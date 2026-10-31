-- | The @mj-head@ elements. None of them render anything in place: each
-- records something in 'GlobalData' for the skeleton and the body to use.
module Text.Mjml.Component.Head (
    handleHead,
) where

import Control.Monad.Trans.State.Strict (modify')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T

import Text.Mjml.Node (Node (..))
import Text.Mjml.Render


-- | @mj-head@'s handler: run each child's handler in order.
handleHead :: Node -> R ()
handleHead headNode = mapM_ one (children headNode)
  where
    one n = case tagName n of
        "mj-attributes" -> mapM_ attribute (children n)
        "mj-breakpoint" ->
            mapM_ (\w -> modify' (\g -> g {breakpoint = w})) (lookup "width" (attributes n))
        "mj-font" ->
            let fname = fromMaybe "" (lookup "name" (attributes n))
                href = fromMaybe "" (lookup "href" (attributes n))
             in modify' (\g -> g {fonts = merge (fonts g) [(fname, href)]})
        "mj-preview" -> modify' (\g -> g {preview = T.strip (content n)})
        "mj-title" -> modify' (\g -> g {title = T.strip (content n)})
        "mj-style"
            | lookup "inline" (attributes n) == Just "inline" ->
                modify' (\g -> g {inlineStyle = inlineStyle g <> [T.strip (content n)]})
            | otherwise -> modify' (\g -> g {styles = styles g <> [T.strip (content n)]})
        "mj-raw" -> modify' (\g -> g {headRaw = headRaw g <> [T.strip (content n)]})
        "mj-html-attributes" -> mapM_ selector (children n)
        _ -> pure ()

    attribute n = case tagName n of
        "mj-class" ->
            let cname = fromMaybe "" (lookup "name" (attributes n))
                values = filter ((/= "name") . fst) (attributes n)
                perTag = foldl' (\acc c -> merge acc [(tagName c, attributes c)]) [] (children n)
             in modify' $ \g ->
                    g
                        { classes = Map.insertWith (flip merge) cname values (classes g)
                        , classesDefault =
                            Map.insertWith Map.union cname (Map.fromList perTag) (classesDefault g)
                        }
        tag ->
            modify' $ \g ->
                g {defaultAttributes = Map.insertWith (flip merge) tag (attributes n) (defaultAttributes g)}

    selector n
        | tagName n == "mj-selector"
        , Just path <- lookup "path" (attributes n) =
            let custom =
                    [ (k, T.strip (content c))
                    | c <- children n
                    , tagName c == "mj-html-attribute"
                    , Just k <- [lookup "name" (attributes c)]
                    , not (T.null k)
                    ]
             in modify' (\g -> g {htmlAttributes' = merge (htmlAttributes' g) [(path, custom)]})
        | otherwise = pure ()
