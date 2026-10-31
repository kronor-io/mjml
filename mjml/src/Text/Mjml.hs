-- | Render MJML documents to HTML.
--
-- > renderText defaultOptions "<mjml><mj-body><mj-text>Hello</mj-text></mj-body></mjml>"
module Text.Mjml (
    -- * Rendering
    renderText,
    renderNode,
    Options (..),
    defaultOptions,
    MjmlError (..),
    -- * Documents
    Node (..),
    parseText,
    -- * Components
    Registry,
    defaultRegistry,
    endingTagNames,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Builder.Linear (runBuilder)

import Text.Mjml.Component.Head (handleHead)
import Text.Mjml.Components (defaultRegistry, endingTagNames)
import Text.Mjml.Node (Node (..))
import Text.Mjml.Parser (ParseError (..), parseMjml)
import Text.Mjml.Render
import Text.Mjml.Skeleton (skeleton)


data Options = Options
    { components :: Registry
    , webFonts :: [(Text, Text)]
    -- ^ Web fonts imported when a @font-family@ in the output names them.
    }


defaultOptions :: Options
defaultOptions =
    Options
        { components = defaultRegistry
        , webFonts =
            [ ("Open Sans", "https://fonts.googleapis.com/css?family=Open+Sans:300,400,500,700")
            , ("Droid Sans", "https://fonts.googleapis.com/css?family=Droid+Sans:300,400,500,700")
            , ("Lato", "https://fonts.googleapis.com/css?family=Lato:300,400,500,700")
            , ("Roboto", "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700")
            , ("Ubuntu", "https://fonts.googleapis.com/css?family=Ubuntu:300,400,500,700")
            ]
        }


newtype MjmlError = MjmlError Text
    deriving stock (Show, Eq)


parseText :: Options -> Text -> Either MjmlError Node
parseText opts src = case parseMjml (endingTagNames opts.components) src of
    Left (ParseError e) -> Left (MjmlError e)
    Right n -> Right n


renderText :: Options -> Text -> Either MjmlError Text
renderText opts src = parseText opts src >>= renderNode opts


-- | Render a parsed document, rooted at @mjml@.
renderNode :: Options -> Node -> Either MjmlError Text
renderNode opts root
    | tagName root /= "mjml" = Left (MjmlError "Malformed MJML. Check that your structure is correct and enclosed in <mjml> tags.")
    | otherwise = case [c | c <- children root, tagName c == "mj-body"] of
        [] -> Left (MjmlError "Malformed MJML. Check that your structure is correct and enclosed in <mjml> tags.")
        body : _ ->
            let rootAttr k = lookup k (attributes root)
                g0 =
                    (initialGlobalData opts.webFonts)
                        { lang = nonEmptyOr "und" (rootAttr "lang")
                        , dir = nonEmptyOr "auto" (rootAttr "dir")
                        , forceOWADesktop = rootAttr "owa" == Just "desktop"
                        }
                go = do
                    mapM_ handleHead [c | c <- take 1 [h | h <- children root, tagName h == "mj-head"]]
                    mi <- instantiate opts.components "" [] rootProps [] body
                    maybe (pure "") render mi
                (content', gd) = runR go g0
                beforeDoctype =
                    [ T.strip (content c)
                    | c <- children root
                    , tagName c == "mj-raw"
                    , lookup "position" (attributes c) == Just "file-start"
                    ]
                prefix = if null beforeDoctype then "" else T.intercalate "\n" beforeDoctype <> "\n"
             in Right (runBuilder (skeleton gd prefix (runBuilder content')))
  where
    nonEmptyOr d = \case
        Just v | not (T.null v) -> v
        _ -> d
    rootProps = Props 0 True True 1 1 []
