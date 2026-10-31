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
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Control.Monad.Trans.State.Strict qualified

import Text.Mjml.Component.Head (handleHead)
import Text.Mjml.Components (defaultRegistry, endingTagNames)
import Text.Mjml.Node (Node (..))
import Text.Mjml.Parser (ParseError (..), parseMjml)
import Text.Mjml.Render
import Text.Mjml.Skeleton (mergeOutlookConditionals, skeleton)


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
                    g <- getGlobal
                    let body' = applyAttributes g "" body
                    mi <- instantiate opts.components [] rootProps [] body'
                    maybe (pure "") render mi
                (content', gd) = runR go g0
                beforeDoctype =
                    [ T.strip (content c)
                    | c <- children root
                    , tagName c == "mj-raw"
                    , lookup "position" (attributes c) == Just "file-start"
                    ]
                doc = skeleton gd content'
             in Right . mergeOutlookConditionals $
                    (if null beforeDoctype then "" else T.intercalate "\n" beforeDoctype <> "\n") <> doc
  where
    nonEmptyOr d = \case
        Just v | not (T.null v) -> v
        _ -> d
    rootProps = Props 0 True True 1 1 []
    getGlobal = Control.Monad.Trans.State.Strict.get


-- | MJML's @applyAttributes@: fold @mj-attributes@ defaults and @mj-class@
-- values into each element's own attributes.
applyAttributes :: GlobalData -> Text -> Node -> Node
applyAttributes g parentMjClass n =
    let own = attributes n
        mjClasses = maybe [] (T.splitOn " ") (lookup "mj-class" own)
        fromClasses =
            foldl
                ( \acc c ->
                    let values = Map.findWithDefault [] c (classes g)
                        multiple = case (lookup "css-class" acc, lookup "css-class" values) of
                            (Just a, Just b) -> [("css-class", a <> " " <> b)]
                            _ -> []
                     in acc `merge` values `merge` multiple
                )
                []
                mjClasses
        fromParentClasses =
            foldl
                ( \acc c ->
                    acc `merge` maybe [] (Map.findWithDefault [] (tagName n)) (Map.lookup c (classesDefault g))
                )
                []
                (T.splitOn " " parentMjClass)
        nextParent = maybe parentMjClass id (lookup "mj-class" own)
        withoutClass = filter ((/= "mj-class") . fst) own
     in n
            { attributes =
                Map.findWithDefault [] (tagName n) (defaultAttributes g)
                    `merge` fromClasses
                    `merge` fromParentClasses
                    `merge` withoutClass
            , children = map (applyAttributes g nextParent) (children n)
            }
