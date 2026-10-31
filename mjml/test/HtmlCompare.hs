-- | Compare two HTML documents for the differences that matter to a mail
-- client, ignoring the ones that do not.
--
-- The reference output is MJML's, whose whitespace comes from JavaScript
-- template strings. The rules follow mrml's comparison helper:
--
--   * whitespace inside text and comments is ignored;
--   * attributes are compared as a set, @class@ as a set of classes and
--     @style@ as a set of declarations; an empty @class@, @style@ or @alt@
--     counts as absent;
--   * Outlook conditional comments must match exactly, but a conditional that
--     closes and immediately reopens the same condition equals one that
--     stays open;
--   * the content of @\<style\>@ is compared as CSS rules, not as text;
--   * an empty @\<style\>@ counts as absent.
module HtmlCompare (
    compareHtml,
) where

import Data.Char (isSpace, toLower)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T


data Token
    = Doctype
    | CondStart Text
    | CondEnd Text
    | Comment Text
    | Open Text (Map.Map Text Text)
    | Close Text
    | TextT Text
    | Css [Text]
    deriving stock (Show, Eq)


-- | 'Nothing' when the documents match, otherwise a description of the
-- first difference.
compareHtml :: Text -> Text -> Maybe Text
compareHtml expected generated = go (0 :: Int) (normalize expected) (normalize generated)
  where
    go _ [] [] = Nothing
    go i (e : es) (g : gs)
        | e == g = go (i + 1) es gs
        | otherwise = Just (describe i e g (e : es))
    go i (e : _) [] = Just ("token " <> tshow i <> ": expected " <> render e <> " but output ended")
    go i [] (g : _) = Just ("token " <> tshow i <> ": unexpected " <> render g <> " after the expected end")

    describe i e g _ =
        T.unlines
            [ "token " <> tshow i <> " differs"
            , "  expected:  " <> render e
            , "  generated: " <> render g
            , "  expected context: " <> T.intercalate " " (map render (take 6 (drop (max 0 (i - 5)) (normalize expected))))
            ]

    render = \case
        Doctype -> "<!doctype>"
        CondStart t -> t
        CondEnd t -> t
        Comment t -> "<!--" <> t <> "-->"
        Open n as -> "<" <> n <> T.concat [" " <> k <> "=" <> tshow v | (k, v) <- Map.toList as] <> ">"
        Close n -> "</" <> n <> ">"
        TextT t -> "text " <> tshow t
        Css rs -> "css " <> tshow rs


tshow :: (Show a) => a -> Text
tshow = T.pack . show


normalize :: Text -> [Token]
normalize = dropEmptyStyles . mergeConditions . filter nonEmpty . tokenize
  where
    nonEmpty = \case
        TextT t -> not (T.null t)
        Comment t -> not (T.null t)
        _ -> True


-- | Drop @\<![endif]-->@ immediately followed by a reopening of the
-- condition it closed.
mergeConditions :: [Token] -> [Token]
mergeConditions = go Nothing
  where
    go _ [] = []
    go prev (t : rest) = case t of
        CondStart c -> t : go (Just c) rest
        CondEnd _ -> case rest of
            CondStart c : rest' | Just c == prev -> go prev rest'
            _ -> t : go Nothing rest
        _ -> t : go prev rest


dropEmptyStyles :: [Token] -> [Token]
dropEmptyStyles = \case
    Open "style" _ : Close "style" : rest -> dropEmptyStyles rest
    Open "style" _ : Css [] : Close "style" : rest -> dropEmptyStyles rest
    t : rest -> t : dropEmptyStyles rest
    [] -> []


tokenize :: Text -> [Token]
tokenize input
    | T.null input = []
    | "<!--[if" `T.isPrefixOf` input =
        let (cond, rest) = T.breakOn "]>" input
            afterCond = T.drop 2 rest
         in if "<!-->" `T.isPrefixOf` afterCond
                then CondStart (cond <> "]><!-->") : tokenize (T.drop 5 afterCond)
                else CondStart (cond <> "]>") : tokenize afterCond
    | "<!--<![endif]-->" `T.isPrefixOf` input = CondEnd "<!--<![endif]-->" : tokenize (T.drop 16 input)
    | "<![endif]-->" `T.isPrefixOf` input = CondEnd "<![endif]-->" : tokenize (T.drop 12 input)
    | "<!--" `T.isPrefixOf` input =
        let (body, rest) = T.breakOn "-->" (T.drop 4 input)
         in Comment (squash body) : tokenize (T.drop 3 rest)
    | "<!" `T.isPrefixOf` input =
        let (_, rest) = T.breakOn ">" input in Doctype : tokenize (T.drop 1 rest)
    | "</" `T.isPrefixOf` input =
        let (tagName, rest) = T.breakOn ">" (T.drop 2 input)
         in Close (T.toLower (T.strip tagName)) : tokenize (T.drop 1 rest)
    | Just ('<', after) <- T.uncons input
    , Just (c, _) <- T.uncons after
    , c /= ' ' =
        let (tag, as, rest) = openTag after
         in if tag == "style"
                then
                    let (css, rest') = T.breakOn "</style" rest
                     in Open tag as : Css (cssRules css) : tokenize rest'
                else Open tag as : tokenize rest
    | otherwise =
        let (t, rest) = T.break (== '<') input
            (t', rest') = if T.null t then (T.take 1 input, T.drop 1 input) else (t, rest)
         in TextT (squash t') : tokenize rest'


squash :: Text -> Text
squash = T.filter (not . isSpace)


openTag :: Text -> (Text, Map.Map Text Text, Text)
openTag after =
    let (tagName, rest) = T.break (\x -> x == '>' || x == '/' || isSpace x) after
     in attrs (T.map toLower tagName) [] rest
  where
    attrs n acc s =
        let s' = T.dropWhile (\x -> isSpace x || x == '/') s
         in case T.uncons s' of
                Nothing -> (n, finish acc, "")
                Just ('>', r) -> (n, finish acc, r)
                _ ->
                    let (k, r) = T.break (\x -> x == '=' || x == '>' || isSpace x) s'
                        r1 = T.dropWhile isSpace r
                     in case T.uncons r1 of
                            Just ('=', r2) ->
                                let r3 = T.dropWhile isSpace r2
                                 in case T.uncons r3 of
                                        Just (q, r4)
                                            | q == '"' || q == '\'' ->
                                                let (v, r5) = T.break (== q) r4 in attrs n ((k, v) : acc) (T.drop 1 r5)
                                        _ ->
                                            let (v, r5) = T.break (\x -> x == '>' || isSpace x) r3
                                             in attrs n ((k, v) : acc) r5
                            _ -> attrs n ((k, "") : acc) r1

    finish = Map.fromList . map normal . filter keep . reverse
    keep (k, v) = not (k `elem` ["class", "style", "alt"] && T.null (T.strip v))
    normal (k, v)
        | k == "class" = (k, T.unwords (Set.toList (Set.fromList (T.words v))))
        | k == "style" = (k, T.intercalate ";" (sort (declarations v)))
        | otherwise = (k, v)


declarations :: Text -> [Text]
declarations v =
    [ T.strip prop <> ":" <> T.unwords (T.words val)
    | d <- T.splitOn ";" v
    , let (prop, rest) = T.breakOn ":" d
    , not (T.null rest)
    , let val = T.drop 1 rest
    ]


-- | A stylesheet as a sorted list of normalised rules. A media rule's body is
-- normalised recursively.
cssRules :: Text -> [Text]
cssRules = sort . go . stripComments
  where
    go t =
        let t' = T.strip t
         in if T.null t'
                then []
                else case T.breakOn "{" t' of
                    (prelude, rest)
                        | T.null rest ->
                            -- A statement such as @import, up to its semicolon.
                            [T.unwords (T.words s) | s <- T.splitOn ";" prelude, not (T.null (T.strip s))]
                        | otherwise ->
                            let (statements, selector) = splitStatements prelude
                                (body, after) = matching (T.drop 1 rest)
                                sel = T.unwords (T.words selector)
                                rule
                                    | "@media" `T.isPrefixOf` sel || "@supports" `T.isPrefixOf` sel =
                                        sel <> "{" <> T.intercalate "" (sort (go body)) <> "}"
                                    | otherwise =
                                        normSelector sel <> "{" <> T.intercalate ";" (sort (declarations body)) <> "}"
                             in statements <> [rule] <> go after

    -- Statements ending in ';' before a rule's selector (e.g. @import).
    splitStatements prelude =
        let parts = T.splitOn ";" prelude
         in ( [T.unwords (T.words s) | s <- init parts, not (T.null (T.strip s))]
            , Prelude.last parts
            )

    normSelector = T.intercalate ", " . map (T.unwords . T.words) . T.splitOn ","

    matching = loop (1 :: Int) ""
      where
        loop depth acc t = case T.uncons t of
            Nothing -> (acc, "")
            Just ('{', r) -> loop (depth + 1) (T.snoc acc '{') r
            Just ('}', r)
                | depth == 1 -> (acc, r)
                | otherwise -> loop (depth - 1) (T.snoc acc '}') r
            Just (c, r) -> loop depth (T.snoc acc c) r

    stripComments t = case T.breakOn "/*" t of
        (a, b)
            | T.null b -> a
            | otherwise -> a <> stripComments (T.drop 2 (snd (T.breakOn "*/" b)))
