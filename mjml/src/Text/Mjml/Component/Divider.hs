-- | @mj-divider@.
module Text.Mjml.Component.Divider (
    mjDivider,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Text.Mjml.Js (parseIntJs, showNumber)
import Text.Mjml.Render


mjDivider :: Component
mjDivider =
    (defComponent "mj-divider" build')
        { allowedAttributes =
            [ ("border-color", "color")
            , ("border-style", "string")
            , ("border-width", "unit(px)")
            , ("container-background-color", "color")
            , ("padding", "unit(px,%){1,4}")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("width", "unit(px,%)")
            , ("align", "enum(left,center,right)")
            ]
        , defaults =
            [ ("border-color", "#000000")
            , ("border-style", "solid")
            , ("border-width", "4px")
            , ("padding", "10px 25px")
            , ("width", "100%")
            , ("align", "center")
            ]
        }


build' :: Self -> Instance
build' s = inst
  where
    inst = (defInstance s (pure rendered)) {getStyles = stylesFor}

    computeAlign = case attr s "align" of
        Just "left" -> "0px"
        Just "right" -> "0px 0px 0px auto"
        _ -> "0px auto"

    p =
        [ style "border-top" (T.intercalate " " [fromMaybe "" (attr s ("border-" <> a)) | a <- ["style", "width", "color"]])
        , style "font-size" "1px"
        , style "margin" computeAlign
        , ("width", attr s "width")
        ]

    stylesFor = \case
        "p" -> p
        "outlook" -> p `merge` [style "width" outlookWidth]
        _ -> []

    outlookWidth :: Text
    outlookWidth =
        let containerWidth = fromMaybe "" (ctx s "containerWidth")
            paddingSize = shorthandValue s "padding" "left" + shorthandValue s "padding" "right"
            width = fromMaybe "" (attr s "width")
            (parsed, unit) = widthParser True width
         in case unit of
                "%" ->
                    let effectiveWidth = parseIntJs containerWidth - paddingSize
                        percentMultiplier = parseIntJs (showNumber parsed) / 100
                     in showNumber (effectiveWidth * percentMultiplier) <> "px"
                "px" -> width
                _ -> showNumber (parseIntJs containerWidth - paddingSize) <> "px"

    rendered =
        T.concat
            [ "<p"
            , htmlAttributes inst [("style", AStyleOf "p")]
            , "></p>"
            , startConditionalTag
            , "<table"
            , htmlAttributes
                inst
                [ ("align", maybe ANone AV (attr s "align"))
                , ("border", AV "0")
                , ("cellpadding", AV "0")
                , ("cellspacing", AV "0")
                , ("style", AStyleOf "outlook")
                , ("role", AV "presentation")
                , ("width", AV outlookWidth)
                ]
            , "><tr><td style=\"height:0;line-height:0;\">&nbsp;</td></tr></table>"
            , endConditionalTag
            ]
