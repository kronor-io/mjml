-- | @mj-body@.
module Text.Mjml.Component.Body (
    mjBody,
) where

import Control.Monad.Trans.State.Strict (get)
import Data.Text qualified as T

import Text.Mjml.Render


mjBody :: Component
mjBody =
    (defComponent "mj-body" build')
        { allowedAttributes =
            [ ("width", "unit(px)")
            , ("background-color", "color")
            , ("id", "string")
            ]
        , defaults = [("width", "600px")]
        }
  where
    build' s =
        let inst =
                (defInstance s (render' inst))
                    { getStyles = \case
                        "body" -> bodyStyles
                        "div" -> bodyStyles
                        _ -> []
                    , childContext = merge (context s) [("containerWidth", attrOr s "width" "")]
                    }
         in inst
      where
        bodyStyles =
            [ style "word-spacing" "normal"
            , ("background-color", attr s "background-color")
            ]

    render' inst = do
        let s = self inst
        g <- get
        kids <- renderChildren inst defChildOptions
        let previewHtml
                | T.null (preview g) = ""
                | otherwise =
                    "<div style=\"display:none;font-size:1px;color:#ffffff;line-height:1px;max-height:0px;max-width:0px;opacity:0;overflow:hidden;\">"
                        <> preview g
                        <> "</div>"
        pure $
            T.concat
                [ "<body"
                , htmlAttributes
                    inst
                    [ ("id", maybe ANone AV (attr s "id"))
                    , ("class", maybe ANone AV (attr s "css-class"))
                    , ("style", AStyleOf "body")
                    ]
                , ">"
                , previewHtml
                , "<div"
                , htmlAttributes inst $
                    [("aria-label", AV (title g)) | not (T.null (title g))]
                        <> [ ("aria-roledescription", AV "email")
                           , ("role", AV "article")
                           , ("lang", AV (lang g))
                           , ("dir", AV (dir g))
                           , ("style", AStyleOf "div")
                           ]
                , ">"
                , kids
                , "</div></body>"
                ]
