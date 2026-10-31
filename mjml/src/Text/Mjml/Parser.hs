-- | MJML's XML syntax, read the way @mjml-parser-xml@ reads it.
--
-- That parser is htmlparser2 in HTML mode with entity decoding off, so this is
-- not an XML parser: tag names are lower-cased, attribute values are kept
-- verbatim, and the body of an /ending tag/ (@mj-text@, @mj-button@, …) is not
-- parsed at all but kept as the raw source between its tags.
module Text.Mjml.Parser (
    parseMjml,
    ParseError (..),
) where

import Data.Char (isAlpha, isSpace, toLower)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Unsafe (lengthWord8, takeWord8)

import Text.Mjml.Node (Node (..), comment)


newtype ParseError = ParseError Text
    deriving stock (Show, Eq)


data Token
    = TOpen Text [(Text, Text)] Bool
    -- ^ Name, attributes, self-closing.
    | TClose Text
    | TText Text
    | TComment Text
    | TOther
    deriving stock (Show, Eq)


-- | Parse a document. The set names the ending tags, whose content is kept as
-- raw HTML instead of being parsed into children.
parseMjml :: Set Text -> Text -> Either ParseError Node
parseMjml endingTags source =
    case go source [] Nothing of
        Just root -> Right root
        Nothing -> Left (ParseError "Parsing failed. Check your mjml.")
  where
    -- The stack holds open elements with their children reversed.
    go :: Text -> [Node] -> Maybe Node -> Maybe Node
    go input stack root
        | T.null input = finish stack root
        | otherwise =
            let (tok, rest) = nextToken input
             in case tok of
                    TOpen name attrs selfClosing
                        | name == "mj-include" -> go rest stack root
                        | name `Set.member` endingTags ->
                            let (raw, rest') =
                                    if selfClosing then ("", rest) else scanRaw endingTags name rest
                                node = Node name attrs [] (T.strip raw)
                             in attach node rest' stack root
                        | selfClosing -> attach (Node name attrs [] "") rest stack root
                        | otherwise -> go rest (Node name attrs [] "" : stack) root
                    TClose name
                        | any ((== name) . tagName) stack ->
                            let (popped, stack') = closeTo name stack
                             in case stack' of
                                    [] -> go rest [] (root `orElse` Just popped)
                                    _ -> attachClosed popped rest stack' root
                        | otherwise -> go rest stack root
                    TText t -> case stack of
                        cur : more
                            | not (T.null (T.strip t)) ->
                                go rest (cur {content = T.strip (content cur <> T.strip t)} : more) root
                        _ -> go rest stack root
                    TComment body -> case stack of
                        cur : more -> go rest (cur {children = comment body : children cur} : more) root
                        [] -> go rest stack root
                    TOther -> go rest stack root

    attach node rest stack root = case stack of
        cur : more -> go rest (cur {children = node : children cur} : more) root
        [] -> go rest [] (root `orElse` Just node)

    attachClosed node rest stack root = case stack of
        cur : more -> go rest (cur {children = node : children cur} : more) root
        [] -> go rest [] (root `orElse` Just node)

    -- Pop up to and including the named element, folding each popped
    -- element into its parent.
    closeTo name = \case
        top : more
            | tagName top == name -> (finalize top, more)
            | otherwise -> case more of
                parent : more' -> closeTo name (parent {children = finalize top : children parent} : more')
                [] -> (finalize top, [])
        [] -> (Node name [] [] "", [])

    finish stack root = case stack of
        [] -> root
        [top] -> root `orElse` Just (finalize top)
        top : parent : more -> finish (parent {children = finalize top : children parent} : more) root

    finalize node = node {children = reverse (children node)}

    orElse (Just a) _ = Just a
    orElse Nothing b = b


-- | Read the raw content of an ending tag up to its matching close tag,
-- returning the content and the input after the close tag.
scanRaw :: Set Text -> Text -> Text -> (Text, Text)
scanRaw endingTags _name input = loop (1 :: Int) input
  where
    -- The content is the input up to where the closing tag starts; measuring
    -- in UTF-8 bytes keeps that constant time per token.
    upTo rest = takeWord8 (lengthWord8 input - lengthWord8 rest) input
    loop depth rest
        | T.null rest = (input, "")
        | otherwise =
            let (tok, rest') = nextToken rest
             in case tok of
                    TOpen n _ False
                        | n `Set.member` endingTags -> loop (depth + 1) rest'
                    TClose n
                        | n `Set.member` endingTags ->
                            if depth == 1
                                then (upTo rest, rest')
                                else loop (depth - 1) rest'
                    _ -> loop depth rest'


nextToken :: Text -> (Token, Text)
nextToken input = case T.uncons input of
    Just ('<', after)
        | "!--" `T.isPrefixOf` after ->
            let (body, rest) = T.breakOn "-->" (T.drop 3 after)
             in (TComment body, T.drop 3 rest)
        | "![CDATA[" `T.isPrefixOf` after ->
            let (body, rest) = T.breakOn "]]>" (T.drop 8 after)
             in (TText body, T.drop 3 rest)
        | Just (c, _) <- T.uncons after, c == '!' || c == '?' ->
            let (_, rest) = T.breakOn ">" after in (TOther, T.drop 1 rest)
        | Just ('/', afterSlash) <- T.uncons after
        , Just (c, _) <- T.uncons afterSlash
        , isAlpha c ->
            let (name, rest) = T.break (\x -> x == '>' || isSpace x) afterSlash
                (_, rest') = T.breakOn ">" rest
             in (TClose (T.toLower name), T.drop 1 rest')
        | Just (c, _) <- T.uncons after, isAlpha c -> openTag after
    _ ->
        let (t, rest) = T.break (== '<') input
         in if T.null t
                then (TText (T.take 1 input), T.drop 1 input)
                else (TText t, rest)


openTag :: Text -> (Token, Text)
openTag after =
    let (name, rest) = T.break (\x -> x == '>' || x == '/' || isSpace x) after
     in attrs (T.map toLower name) [] rest
  where
    attrs name acc s =
        let s' = T.dropWhile isSpace s
         in case T.uncons s' of
                Nothing -> (TOpen name (reverse acc) False, "")
                Just ('>', r) -> (TOpen name (reverse acc) False, r)
                Just ('/', r)
                    | Just ('>', r') <- T.uncons (T.dropWhile isSpace r) -> (TOpen name (reverse acc) True, r')
                    | otherwise -> attrs name acc r
                _ ->
                    let (key, r) = T.break (\x -> x == '=' || x == '>' || x == '/' || isSpace x) s'
                        r1 = T.dropWhile isSpace r
                     in case T.uncons r1 of
                            Just ('=', r2) ->
                                let r3 = T.dropWhile isSpace r2
                                 in case T.uncons r3 of
                                        Just (q, r4)
                                            | q == '"' || q == '\'' ->
                                                let (v, r5) = T.break (== q) r4
                                                 in attrs name ((key, v) : acc) (T.drop 1 r5)
                                        _ ->
                                            let (v, r5) = T.break (\x -> x == '>' || isSpace x) r3
                                             in attrs name ((key, v) : acc) r5
                            _
                                | T.null key -> attrs name acc (T.drop 1 r1)
                                | otherwise -> attrs name ((key, "") : acc) r1

