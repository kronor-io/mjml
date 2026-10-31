-- | The template syntax: Hamlet's, applied to MJML.
--
-- A template is a sequence of lines whose indentation gives the nesting, as in
-- Hamlet. Each line is one of:
--
--   * a tag, @\<mj-button href=#{url} .primary>Pay now@, whose children are
--     the more indented lines below it and whose text may follow it on the
--     same line;
--   * a control line: @$forall@, @$if@ \/ @$elseif@ \/ @$else@, @$maybe@ \/
--     @$nothing@, @$case@ \/ @$of@, @$with@, or a @$#@ comment;
--   * @^{expr}@ alone, embedding nodes or markup built elsewhere;
--   * text, in which @#{expr}@ interpolates an escaped value and @^{expr}@
--     raw markup. A leading backslash is dropped, so @\\ #{a}@ keeps its space.
--
-- Parsing produces a 'Doc' tree with the Haskell fragments still as source
-- text; "Text.Mjml.Hamlet.TH" turns them into code.
module Text.Mjml.Hamlet.Syntax (
    Pos (..),
    Doc (..),
    Attr (..),
    Piece (..),
    parseTemplate,
) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isSpace)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char


-- | A line and column in the template, both from 1.
data Pos = Pos {line :: Int, column :: Int}
    deriving stock (Show, Eq, Ord)


data Doc
    = DTag Pos Text [Attr] [Doc]
    | DText Pos [Piece]
    | DEmbed Pos String
    | DForall Pos String String [Doc]
    -- ^ Pattern, expression, body.
    | DIf Pos [(Pos, String, [Doc])] (Maybe [Doc])
    | DMaybe Pos String String [Doc] (Maybe [Doc])
    | DCase Pos String [(Pos, String, [Doc])]
    | DWith Pos String String [Doc]
    | DOf Pos String [Doc]
    -- ^ Only while parsing: an @$of@ before it joins its @$case@.
    deriving stock (Show, Eq)


data Attr
    = Attr Pos Text (Maybe [Piece])
    -- ^ 'Nothing' for a bare attribute.
    | ClassAttr Pos [Piece]
    -- ^ @.name@: a class, @css-class@ on MJML elements.
    deriving stock (Show, Eq)


data Piece
    = PLit Text
    | PInterp Pos String
    | PEmbed Pos String
    deriving stock (Show, Eq)


-- | A line before nesting: its indentation and what it holds.
data Line
    = LTag Text [Attr] [Piece]
    | LText [Piece]
    | LEmbed String
    | LForall String String
    | LIf String
    | LElseIf String
    | LElse
    | LMaybe String String
    | LNothing
    | LCase String
    | LOf String
    | LWith String String
    deriving stock (Show)


type Parser = Parsec Void Text


-- | Parse a template. The error is rendered with the template's own line and
-- column numbers; the caller adds where the template sits in its file.
parseTemplate :: Text -> Either (Pos, Text) [Doc]
parseTemplate src = do
    ls <- traverse parseLine (numbered src)
    nest ls


numbered :: Text -> [(Int, Int, Text)]
numbered src =
    [ (n, T.length indent + 1, body)
    | (n, raw) <- zip [1 ..] (T.lines src)
    , let (indent, body) = T.span (== ' ') (T.dropWhileEnd isSpace raw)
    , not (T.null body)
    , not ("$#" `T.isPrefixOf` body)
    ]


parseLine :: (Int, Int, Text) -> Either (Pos, Text) (Pos, Line)
parseLine (n, col, body)
    | "\t" `T.isPrefixOf` body = Left (Pos n col, "tabs are not allowed for indentation; use spaces")
    | otherwise =
        let initial =
                State
                    { stateInput = body
                    , stateOffset = 0
                    , statePosState =
                        PosState
                            { pstateInput = body
                            , pstateOffset = 0
                            , pstateSourcePos = SourcePos "" (mkPos n) (mkPos col)
                            , pstateTabWidth = defaultTabWidth
                            , pstateLinePrefix = ""
                            }
                    , stateParseErrors = []
                    }
         in case snd (runParser' (lineP <* eof) initial) of
                Right l -> Right (Pos n col, l)
                Left bundle -> case bundleErrors bundle of
                    e :| _ ->
                        Left (Pos n (col + errorOffset e), T.strip (T.pack (parseErrorTextPretty e)))


lineP :: Parser Line
lineP =
    choice
        [ controlP
        , tagP
        , try (LEmbed <$> (string "^{" *> balanced) <* eof)
        , LText <$> piecesP (const False)
        ]


controlP :: Parser Line
controlP = do
    void (char '$')
    keyword <- takeWhile1P (Just "a control keyword") isAlphaNum
    let rest = T.unpack . T.strip <$> takeRest
        binding = do
            space1
            src <- takeRest
            case T.breakOn "<-" src of
                (pat, expr)
                    | not (T.null expr) ->
                        pure (T.unpack (T.strip pat), T.unpack (T.strip (T.drop 2 expr)))
                _ -> fail ("$" <> T.unpack keyword <> " needs a binding: $" <> T.unpack keyword <> " pattern <- expression")
        needs what = space1 *> (rest >>= \r -> if null r then fail ("$" <> T.unpack keyword <> " needs " <> what) else pure r)
    case keyword of
        "forall" -> uncurry LForall <$> binding
        "maybe" -> uncurry LMaybe <$> binding
        "with" -> uncurry LWith <$> binding
        "if" -> LIf <$> needs "a condition"
        "elseif" -> LElseIf <$> needs "a condition"
        "else" -> LElse <$ eof
        "nothing" -> LNothing <$ eof
        "case" -> LCase <$> needs "an expression"
        "of" -> LOf <$> needs "a pattern"
        other -> fail ("unknown control keyword $" <> T.unpack other)


tagP :: Parser Line
tagP = do
    void (char '<')
    name <- takeWhile1P (Just "a tag name") (\c -> isAlphaNum c || c == '-' || c == ':' || c == '_')
    as <- many (try (space1 *> attrP))
    space
    void (char '>') <?> "'>' closing the tag"
    inline <- optional (piecesP (const False))
    pure (LTag (T.toLower name) as (maybe [] id inline))


attrP :: Parser Attr
attrP = do
    pos <- getPos
    choice
        [ ClassAttr pos <$> (char '.' *> valueP)
        , do
            name <- takeWhile1P (Just "an attribute name") (\c -> isAlphaNum c || c `elem` ("-_:" :: String))
            value <- optional (char '=' *> valueP)
            pure (Attr pos name value)
        ]


valueP :: Parser [Piece]
valueP =
    choice
        [ between (char '"') (char '"') (piecesP (== '"'))
        , between (char '\'') (char '\'') (piecesP (== '\''))
        , piecesP (\c -> isSpace c || c == '>')
        ]


-- | Literal text and interpolations, up to a character the caller stops at.
piecesP :: (Char -> Bool) -> Parser [Piece]
piecesP stop = do
    merge <$> many piece
  where
    piece =
        choice
            [ try (PInterp <$> getPos <*> (string "#{" *> balanced))
            , try (PEmbed <$> getPos <*> (string "^{" *> balanced))
            , PLit . T.singleton <$> satisfy (not . stop)
            ]
    merge = \case
        PLit a : PLit b : rest -> merge (PLit (a <> b) : rest)
        x : rest -> x : merge rest
        [] -> []


-- | The Haskell source inside @{...}@, up to the matching brace.
balanced :: Parser String
balanced = go (0 :: Int)
  where
    go depth = do
        c <- anySingle <?> "a closing '}'"
        case c of
            '}' | depth == 0 -> pure []
            '}' -> (c :) <$> go (depth - 1)
            '{' -> (c :) <$> go (depth + 1)
            '"' -> do
                s <- stringLit
                (\r -> '"' : s <> r) <$> go depth
            _ -> (c :) <$> go depth
    stringLit = do
        c <- anySingle
        case c of
            '"' -> pure "\""
            '\\' -> do
                e <- anySingle
                (\r -> '\\' : e : r) <$> stringLit
            _ -> (c :) <$> stringLit


getPos :: Parser Pos
getPos = do
    p <- getSourcePos
    pure (Pos (unPos (sourceLine p)) (unPos (sourceColumn p)))


-- | Turn indented lines into a tree, pairing each control line with its
-- continuations (@$else@, @$nothing@, @$of@).
nest :: [(Pos, Line)] -> Either (Pos, Text) [Doc]
nest = fmap fst . block 0
  where
    -- Parse sibling lines whose column is exactly the first one's, returning
    -- the docs and the remaining lines.
    block :: Int -> [(Pos, Line)] -> Either (Pos, Text) ([Doc], [(Pos, Line)])
    block _ [] = Right ([], [])
    block minCol ls@((p, _) : _)
        | column p <= minCol = Right ([], ls)
        | otherwise = siblings (column p) ls

    siblings col = \case
        (p, l) : rest
            | column p == col -> do
                (kids, rest') <- block col rest
                (doc, rest'') <- assemble col p l kids rest'
                (more, rest''') <- siblings col rest''
                pure (doc : more, rest''')
            | column p > col -> Left (p, "unexpected indentation")
        rest -> Right ([], rest)

    -- Continuations at the same column belong to the control line above.
    continuation col = \case
        (p, l) : rest | column p == col -> Just (p, l, rest)
        _ -> Nothing

    assemble col p l kids rest = case l of
        LTag name as inline ->
            let inlineDoc = [DText p inline | not (null inline)]
             in Right (DTag p name as (inlineDoc <> kids), rest)
        LText ps -> do
            noChildren p kids
            Right (DText p (dropBackslash ps), rest)
        LEmbed e -> noChildren p kids >> Right (DEmbed p e, rest)
        LForall pat e -> Right (DForall p pat e kids, rest)
        LWith pat e -> Right (DWith p pat e kids, rest)
        LIf c -> do
            (branches, final, rest') <- ifChain col rest
            Right (DIf p ((p, c, kids) : branches) final, rest')
        LMaybe pat e -> case continuation col rest of
            Just (_, LNothing, rest') -> do
                (kids', rest'') <- block col rest'
                Right (DMaybe p pat e kids (Just kids'), rest'')
            _ -> Right (DMaybe p pat e kids Nothing, rest)
        LCase e -> do
            alts <- traverse asAlternative kids
            if null alts then Left (p, "$case needs at least one nested $of") else Right (DCase p e alts, rest)
        LElseIf _ -> Left (p, "$elseif without a matching $if")
        LElse -> Left (p, "$else without a matching $if")
        LNothing -> Left (p, "$nothing without a matching $maybe")
        LOf pat -> Right (DOf p pat kids, rest)

    ifChain col rest = case continuation col rest of
        Just (p', LElseIf c, rest') -> do
            (kids, rest'') <- block col rest'
            (more, final, rest''') <- ifChain col rest''
            Right ((p', c, kids) : more, final, rest''')
        Just (_, LElse, rest') -> do
            (kids, rest'') <- block col rest'
            Right ([], Just kids, rest'')
        _ -> Right ([], Nothing, rest)

    asAlternative = \case
        DOf p pat kids -> Right (p, pat, kids)
        d -> Left (docPos d, "only $of lines can be nested under $case")

    noChildren p kids = if null kids then Right () else Left (p, "only tags and control lines can have nested lines")

    dropBackslash = \case
        PLit t : more | Just ('\\', t') <- T.uncons t -> [PLit t' | not (T.null t')] <> more
        ps -> ps


docPos :: Doc -> Pos
docPos = \case
    DTag p _ _ _ -> p
    DText p _ -> p
    DEmbed p _ -> p
    DForall p _ _ _ -> p
    DIf p _ _ -> p
    DMaybe p _ _ _ _ -> p
    DCase p _ _ -> p
    DWith p _ _ _ -> p
    DOf p _ _ -> p
