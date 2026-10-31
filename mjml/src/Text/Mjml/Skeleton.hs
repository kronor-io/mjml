-- | The document around the rendered body: MJML's @skeleton@ helper and the
-- head blocks it assembles.
module Text.Mjml.Skeleton (
    skeleton,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Builder.Linear (Builder, fromText)
import Data.Text.Unsafe (lengthWord8)

import Text.Mjml.Render (GlobalData (..))


-- | The whole document around a rendered body, with Outlook conditionals
-- merged, written into one builder. The text before the doctype comes from
-- @mj-raw position="file-start"@.
skeleton :: GlobalData -> Text -> Text -> Builder
skeleton g beforeDoctype body =
    foldMap fromText (outlookChunks (beforeDoctype <> headPart))
        <> foldMap fromText (outlookChunks body)
        <> "\n</html>\n"
  where
    headPart =
      T.concat
        [ "<!doctype html>\n<html lang=\""
        , lang g
        , "\" dir=\""
        , dir g
        , "\" xmlns=\"http://www.w3.org/1999/xhtml\" xmlns:v=\"urn:schemas-microsoft-com:vml\" xmlns:o=\"urn:schemas-microsoft-com:office:office\">\n<head>\n<title>"
        , title g
        , "</title>\n<!--[if !mso]><!-->\n<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">\n<!--<![endif]-->\n"
        , "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">\n"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
        , "<style type=\"text/css\">\n"
        , "#outlook a { padding:0; }\n"
        , "body { margin:0;padding:0;-webkit-text-size-adjust:100%;-ms-text-size-adjust:100%; }\n"
        , "table, td { border-collapse:collapse;mso-table-lspace:0pt;mso-table-rspace:0pt; }\n"
        , "img { border:0;height:auto;line-height:100%; outline:none;text-decoration:none;-ms-interpolation-mode:bicubic; }\n"
        , "p { display:block;margin:13px 0; }\n"
        , "</style>\n"
        , "<!--[if mso]>\n<noscript>\n<xml>\n<o:OfficeDocumentSettings>\n<o:AllowPNG/>\n<o:PixelsPerInch>96</o:PixelsPerInch>\n</o:OfficeDocumentSettings>\n</xml>\n</noscript>\n<![endif]-->\n"
        , "<!--[if lte mso 11]>\n<style type=\"text/css\">\n.mj-outlook-group-fix { width:100% !important; }\n</style>\n<![endif]-->\n"
        , fontsTags g body
        , mediaQueriesTags g
        , styleFromComponents g
        , styleFromTags g
        , T.intercalate "\n" (headRaw g)
        , "\n</head>\n"
        ]


-- | @buildFontsTags@: import a known font when the rendered body, or an
-- inlined style, names it in a @font-family@.
fontsTags :: GlobalData -> Text -> Text
fontsTags g body =
    let used = [url | (fname, url) <- fonts g, usesFont fname]
     in if null used
            then ""
            else
                T.concat
                    [ "<!--[if !mso]><!-->\n"
                    , T.intercalate "\n" ["<link href=\"" <> u <> "\" rel=\"stylesheet\" type=\"text/css\">" | u <- used]
                    , "\n<style type=\"text/css\">\n"
                    , T.intercalate "\n" ["@import url(" <> u <> ");" | u <- used]
                    , "\n</style>\n<!--<![endif]-->\n"
                    ]
  where
    -- MJML matches "[^"]*font-family:[^"]*NAME[^"]*" against the body,
    -- ignoring case: some stretch between two quotes names the font after a
    -- font-family. That is every "font-family:" with a quote somewhere before
    -- it, up to the next quote, which must exist. Inlined styles are matched
    -- with font-family:[^;}]*NAME.
    families = fromBody <> concatMap fromInline (inlineStyle g)
    lowered = asciiLower body
    firstQuote = lengthWord8 (fst (T.breakOn "\"" lowered))
    fromBody =
        [ stretch
        | (before, match) <- T.breakOnAll "font-family:" lowered
        , lengthWord8 before > firstQuote
        , let (stretch, after) = T.break (== '"') (T.drop 12 match)
        , not (T.null after)
        ]
    fromInline style =
        [ T.takeWhile (`notElem` (";}" :: String)) (T.drop 12 match)
        | (_, match) <- T.breakOnAll "font-family:" (asciiLower style)
        ]
    usesFont fname = any (asciiLower fname `T.isInfixOf`) families


-- | Lower-case ASCII letters only. Font names and @font-family@ are ASCII, and
-- unlike 'T.toLower' this never changes the length of the text.
asciiLower :: Text -> Text
asciiLower = T.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)


mediaQueriesTags :: GlobalData -> Text
mediaQueriesTags g
    | null (mediaQueries g) = ""
    | otherwise =
        let grouped prefix =
                [ T.intercalate ", " [prefix <> "." <> c | (c, q') <- mediaQueries g, q' == q] <> " " <> q
                | q <- unique (map snd (mediaQueries g))
                ]
            bp = breakpoint g
         in T.concat
                [ "<style type=\"text/css\">\n@media only screen and (min-width:"
                , bp
                , ") {\n"
                , T.intercalate "\n" (grouped "")
                , "\n}\n</style>\n<style media=\"screen and (min-width:"
                , bp
                , ")\">\n"
                , T.intercalate "\n" (grouped ".moz-text-html ")
                , "\n</style>\n"
                , if forceOWADesktop g
                    then "<style type=\"text/css\">\n" <> T.intercalate "\n" (map ("[owa] " <>) (grouped "")) <> "\n</style>\n"
                    else ""
                ]
  where
    unique = foldr (\x acc -> x : filter (/= x) acc) []


styleFromComponents :: GlobalData -> Text
styleFromComponents g =
    let fs = componentsHeadStyle g <> map snd (headStyle g)
     in if null fs
            then ""
            else "<style type=\"text/css\">" <> T.concat ["\n" <> f (breakpoint g) | f <- fs] <> "\n</style>\n"


styleFromTags :: GlobalData -> Text
styleFromTags g
    | null (styles g) = ""
    | otherwise = "<style type=\"text/css\">" <> T.concat ["\n" <> s | s <- styles g] <> "\n</style>\n"


-- | MJML's @mergeOutlookConditionnals@: join an Outlook conditional that
-- closes and immediately reopens. The result is the text in pieces, to be
-- written out without concatenating it first.
outlookChunks :: Text -> [Text]
outlookChunks = go
  where
    endTag = "<![endif]-->"
    startTag = "<!--[if mso | IE]>"
    go t = case T.breakOn endTag t of
        (before, rest)
            | T.null rest -> [before]
            | otherwise ->
                let after = T.drop (T.length endTag) rest
                    trimmed = T.stripStart after
                 in if startTag `T.isPrefixOf` trimmed
                        then before : go (T.drop (T.length startTag) trimmed)
                        else before : endTag : go after
