{-# LANGUAGE TemplateHaskell #-}

-- | Code generation: a checked 'Doc' tree becomes an expression of type
-- @['Node']@.
--
-- Interpolated values are escaped as they go into the tree, because MJML
-- copies attribute values and ending-tag content into its output verbatim.
module Text.Mjml.Hamlet.TH (
    compileTemplate,
    Source (..),
) where

import Data.Char (isAlphaNum, isLower, isUpper)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Language.Haskell.TH
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as M
import Text.Parsec qualified as P
import Text.Shakespeare.Base (derefToExp, parseDeref)

import Text.Mjml (Registry)
import Text.Mjml.Hamlet.Check (checkTemplate)
import Text.Mjml.Hamlet.Runtime (ToNodes (..), ToRawHtml (..), ToUrl (..), element, escape, guardMjml, guardStyle, urlAttributes, urlPart)
import Text.Mjml.Hamlet.Syntax


-- | Where the template text came from, for error messages.
data Source = Source
    { file :: FilePath
    , startLine :: Int
    , firstLineColumn :: Int
    -- ^ The column, in the file, of the template's first character.
    }


compileTemplate :: Registry -> Set Text -> Source -> Text -> Q Exp
compileTemplate reg ending src body = case parseTemplate body of
    Left (p, why) -> failAt src p why
    Right docs -> case checkTemplate reg ending docs of
        (p, why) : _ -> failAt src p why
        [] -> structure src ending docs


failAt :: Source -> Pos -> Text -> Q a
failAt src (Pos l c) why =
    let absLine = startLine src + l - 1
        absCol = if l == 1 then firstLineColumn src + c - 1 else c
     in fail (file src <> ":" <> show absLine <> ":" <> show absCol <> ": " <> T.unpack why)


-- | MJML elements, as @['Node']@.
structure :: Source -> Set Text -> [Doc] -> Q Exp
structure src ending docs = [|concat $(listE (map one docs))|]
  where
    one = \case
        DTag _ name as kids
            | name `Set.member` ending ->
                [|[element $(textE name) $(attrsE src name True as) [] $(html src kids)]|]
            | name `elem` ["mj-attributes", "mj-class"] ->
                [|[element $(textE name) $(attrsE src name True as) $(structureRaw kids) ""]|]
            | otherwise ->
                [|[element $(textE name) $(attrsE src name True as) $(structure src ending kids) ""]|]
        DText p _ -> failAt src p "text can only appear inside an ending tag"
        DEmbed p e -> [|toNodes $(expr src p e)|]
        d -> control src (structure src ending) d [|[]|]

    -- @mj-attributes@ children name the elements they set defaults for, so
    -- they are any tag at all.
    structureRaw kids =
        listE
            [ [|element $(textE name) $(attrsE src name True as) [] ""|]
            | DTag _ name as _ <- kids
            ]


-- | HTML inside an ending tag, as 'Text'. Consecutive text lines are joined
-- with a space.
html :: Source -> [Doc] -> Q Exp
html src docs = [|T.concat $(listE (go docs))|]
  where
    go = \case
        DText _ a : rest@(DText _ _ : _) -> pieces src a : textE " " : go rest
        d : rest -> one d : go rest
        [] -> []
    one = \case
        DTag _ name as kids
            | name `elem` voidElements -> [|"<" <> $(textE name) <> $(htmlAttrs name as) <> ">"|]
            | otherwise ->
                [|"<" <> $(textE name) <> $(htmlAttrs name as) <> ">" <> $(html src kids) <> "</" <> $(textE name) <> ">"|]
        DText _ ps -> pieces src ps
        DEmbed p e -> [|toRawHtml $(expr src p e)|]
        d -> control src (html src) d [|""|]
    htmlAttrs name as =
        [|
            T.concat
                ( map
                    (\(k, v) -> " " <> k <> (if T.null v then "" else "=\"" <> v <> "\""))
                    $(attrsE src name False as)
                )
            |]


voidElements :: [Text]
voidElements = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "source", "track", "wbr"]


-- | Control lines, for either kind of content. @body@ compiles nested lines and
-- @empty@ is the content of a branch that renders nothing.
control :: Source -> ([Doc] -> Q Exp) -> Doc -> Q Exp -> Q Exp
control src body d empty = case d of
    DForall p pat e kids -> do
        pt <- patt src p pat
        [|foldMap $(lamE [pure pt] (body kids)) $(expr src p e)|]
    DWith p pat e kids -> do
        pt <- patt src p pat
        caseE (expr src p e) [match (pure pt) (normalB (body kids)) []]
    DIf _ branches final ->
        foldr
            (\(p, c, kids) rest -> condE (expr src p c) (body kids) rest)
            (maybe empty body final)
            branches
    DMaybe p pat e kids nothing -> do
        pt <- patt src p pat
        caseE
            (expr src p e)
            [ match (conP 'Just [pure pt]) (normalB (body kids)) []
            , match (conP 'Nothing []) (normalB (maybe empty body nothing)) []
            ]
    DCase p e alts ->
        caseE
            (expr src p e)
            [ do
                pt <- patt src p' pat
                match (pure pt) (normalB (body kids)) []
            | (p', pat, kids) <- alts
            ]
    DTag p _ _ _ -> failAt src p "unexpected tag"
    DText p _ -> failAt src p "unexpected text"
    DEmbed p _ -> failAt src p "unexpected embed"
    DOf p _ _ -> failAt src p "$of without a matching $case"


-- | Attributes as @[(Text, Text)]@. Classes written with @.name@ are joined
-- into @css-class@ on MJML elements and @class@ on HTML ones.
--
-- A literal value is used as written: the checker has already seen it. An
-- interpolated one goes through the policy in "Text.Mjml.Hamlet.Runtime".
attrsE :: Source -> Text -> Bool -> [Attr] -> Q Exp
attrsE src tag isMjml as =
    let classKey = if isMjml then "css-class" else "class"
        plain = [(name, maybe [] id value) | Attr _ name value <- as, name /= classKey]
        explicitClass = [maybe [] id value | Attr _ name value <- as, name == classKey]
        shorthand = [ps | ClassAttr _ ps <- as]
        classes = explicitClass <> shorthand
        classAttr =
            [ [|Just ($(textE classKey), T.unwords (filter (not . T.null) $(listE (map (pieces src) classes))))|]
            | not (null classes)
            ]
        one (name, ps)
            | not (any interpolated ps) = [|Just ($(textE name), $(pieces src ps))|]
            | name `elem` urlAttributes = case ps of
                [PInterp p e] -> [|Just ($(textE name), toUrl $(expr src p e))|]
                _ -> [|Just ($(textE name), $(urlPieces ps))|]
            | isMjml = [|guardMjml $(textE tag) $(textE name) $(pieces src ps)|]
            | name == "style" = [|guardStyle $(pieces src ps)|]
            | otherwise = [|Just ($(textE name), $(pieces src ps))|]
        urlPieces ps =
            [|
                T.concat
                    $( listE
                        [ case piece of
                            PInterp p e -> [|urlPart $(expr src p e)|]
                            _ -> pieces src [piece]
                        | piece <- ps
                        ]
                     )
                |]
        interpolated = \case
            PLit _ -> False
            _ -> True
     in [|catMaybes $(listE (map one plain <> classAttr))|]


-- | Text with interpolations, as 'Text'.
pieces :: Source -> [Piece] -> Q Exp
pieces src ps = case ps of
    [] -> textE ""
    [PLit t] -> textE t
    _ -> [|T.concat $(listE (map one ps))|]
  where
    one = \case
        PLit t -> textE t
        PInterp p e -> [|escape $(expr src p e)|]
        PEmbed p e -> [|toRawHtml $(expr src p e)|]


textE :: Text -> Q Exp
textE t = [|T.pack $(litE (stringL (T.unpack t)))|]


-- | A Haskell expression, parsed the way Hamlet parses @#{...}@.
expr :: Source -> Pos -> String -> Q Exp
expr src p s = case P.parse (parseDeref <* P.eof) "" (trim s) of
    Right d -> pure (derefToExp [] d)
    Left e -> failAt src p ("cannot parse the expression " <> T.pack (show (trim s)) <> ": " <> T.pack (show e))
  where
    trim = T.unpack . T.strip . T.pack


-- | A pattern in @$forall@, @$maybe@, @$with@ or @$of@: variables,
-- wildcards, constructors, tuples and literals.
patt :: Source -> Pos -> String -> Q Pat
patt src p s = case M.parse (M.space *> pat <* M.eof) "" (T.pack s) of
    Right x -> x
    Left e -> failAt src p ("cannot parse the pattern " <> T.pack (show s) <> ": " <> T.pack (M.errorBundlePretty e))
  where
    lexeme :: M.Parsec Void Text a -> M.Parsec Void Text a
    lexeme x = x <* M.space
    sym = lexeme . M.string
    ident = lexeme (M.takeWhile1P Nothing (\c -> isAlphaNum c || c `elem` ("_'." :: String)))

    pat = M.choice [M.try applied, apat]

    applied = do
        x <- ident
        case T.uncons x of
            Just (c, _) | isUpper c -> do
                args <- M.some apat
                pure (conP (mkName (T.unpack x)) args)
            _ -> fail "not a constructor"

    apat =
        M.choice
            [ do
                ps <- sym "(" *> M.sepBy pat (sym ",") <* sym ")"
                pure $ case ps of
                    [one] -> one
                    _ -> tupP ps
            , do
                lit <- lexeme (M.char '"' *> M.manyTill M.anySingle (M.char '"'))
                pure (litP (stringL lit))
            , do
                x <- ident
                pure $ case T.uncons x of
                    Just (c, _) | isUpper c -> conP (mkName (T.unpack x)) []
                    _ -> simple x
            ]

    simple x
        | x == "_" = wildP
        | T.all (`elem` ['0' .. '9']) x = litP (integerL (read (T.unpack x)))
        | Just (c, _) <- T.uncons x, isLower c || c == '_' = varP (mkName (T.unpack x))
        | otherwise = varP (mkName (T.unpack x))

