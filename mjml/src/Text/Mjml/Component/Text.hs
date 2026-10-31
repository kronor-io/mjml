-- | @mj-text@.
module Text.Mjml.Component.Text (
    mjText,
) where

import Data.Text qualified as T

import Text.Mjml.Render


mjText :: Component
mjText =
    (defComponent "mj-text" build')
        { endingTag = True
        , allowedAttributes =
            [ ("align", "enum(left,right,center,justify)")
            , ("background-color", "color")
            , ("color", "color")
            , ("container-background-color", "color")
            , ("font-family", "string")
            , ("font-size", "unit(px)")
            , ("font-style", "string")
            , ("font-weight", "string")
            , ("height", "unit(px,%)")
            , ("letter-spacing", "unitWithNegative(px,em)")
            , ("line-height", "unit(px,%,)")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("padding", "unit(px,%){1,4}")
            , ("text-decoration", "string")
            , ("text-transform", "string")
            , ("vertical-align", "enum(top,bottom,middle)")
            ]
        , defaults =
            [ ("align", "left")
            , ("color", "#000000")
            , ("font-family", "Ubuntu, Helvetica, Arial, sans-serif")
            , ("font-size", "13px")
            , ("line-height", "1")
            , ("padding", "10px 25px")
            ]
        }
  where
    build' s =
        let inst =
                (defInstance s (pure rendered))
                    { getStyles = \case
                        "text" ->
                            [ ("font-family", attr s "font-family")
                            , ("font-size", attr s "font-size")
                            , ("font-style", attr s "font-style")
                            , ("font-weight", attr s "font-weight")
                            , ("letter-spacing", attr s "letter-spacing")
                            , ("line-height", attr s "line-height")
                            , ("text-align", attr s "align")
                            , ("text-decoration", attr s "text-decoration")
                            , ("text-transform", attr s "text-transform")
                            , ("color", attr s "color")
                            , ("height", attr s "height")
                            ]
                        _ -> []
                    }
            renderContent = "<div" <> htmlAttributes inst [("style", AStyleOf "text")] <> ">" <> getContent s <> "</div>"
            rendered = case attr s "height" of
                Just h
                    | truthy (Just h) ->
                        T.concat
                            [ conditionalTag False $
                                "<table role=\"presentation\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\"><tr><td height=\""
                                    <> h
                                    <> "\" style=\"vertical-align:top;height:"
                                    <> h
                                    <> ";\">"
                            , renderContent
                            , conditionalTag False "</td></tr></table>"
                            ]
                _ -> renderContent
         in inst
