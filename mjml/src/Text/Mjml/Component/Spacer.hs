-- | @mj-spacer@.
module Text.Mjml.Component.Spacer (
    mjSpacer,
) where

import Text.Mjml.Render


mjSpacer :: Component
mjSpacer =
    (defComponent "mj-spacer" build')
        { allowedAttributes =
            [ ("border", "string")
            , ("border-bottom", "string")
            , ("border-left", "string")
            , ("border-right", "string")
            , ("border-top", "string")
            , ("container-background-color", "color")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("padding", "unit(px,%){1,4}")
            , ("height", "unit(px,%)")
            ]
        , defaults = [("height", "20px")]
        }
  where
    build' s =
        let inst =
                (defInstance s (pure ("<div" <> htmlAttributes inst [("style", AStyleOf "div")] <> ">&#8202;</div>")))
                    { getStyles = \case
                        "div" -> [("height", attr s "height"), ("line-height", attr s "height")]
                        _ -> []
                    }
         in inst
