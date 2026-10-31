-- | @mj-button@.
module Text.Mjml.Component.Button (
    mjButton,
) where

import Data.Text qualified as T

import Text.Mjml.Js (showNumber)
import Text.Mjml.Render


mjButton :: Component
mjButton =
    (defComponent "mj-button" build')
        { endingTag = True
        , allowedAttributes =
            [ ("align", "enum(left,center,right)")
            , ("background-color", "color")
            , ("border-bottom", "string")
            , ("border-left", "string")
            , ("border-radius", "string")
            , ("border-right", "string")
            , ("border-top", "string")
            , ("border", "string")
            , ("color", "color")
            , ("container-background-color", "color")
            , ("font-family", "string")
            , ("font-size", "unit(px)")
            , ("font-style", "string")
            , ("font-weight", "string")
            , ("height", "unit(px,%)")
            , ("href", "string")
            , ("name", "string")
            , ("title", "string")
            , ("inner-padding", "unit(px,%){1,4}")
            , ("letter-spacing", "unitWithNegative(px,em)")
            , ("line-height", "unit(px,%,)")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("padding", "unit(px,%){1,4}")
            , ("rel", "string")
            , ("target", "string")
            , ("text-decoration", "string")
            , ("text-transform", "string")
            , ("vertical-align", "enum(top,bottom,middle)")
            , ("text-align", "enum(left,right,center)")
            , ("width", "unit(px,%)")
            ]
        , defaults =
            [ ("align", "center")
            , ("background-color", "#414141")
            , ("border", "none")
            , ("border-radius", "3px")
            , ("color", "#ffffff")
            , ("font-family", "Ubuntu, Helvetica, Arial, sans-serif")
            , ("font-size", "13px")
            , ("font-weight", "normal")
            , ("inner-padding", "10px 25px")
            , ("line-height", "120%")
            , ("padding", "10px 25px")
            , ("target", "_blank")
            , ("text-decoration", "none")
            , ("text-transform", "none")
            , ("vertical-align", "middle")
            ]
        }


build' :: Self -> Instance
build' s = inst
  where
    inst = (defInstance s (pure rendered)) {getStyles = stylesFor}

    stylesFor = \case
        "table" ->
            [ style "border-collapse" "separate"
            , ("width", attr s "width")
            , style "line-height" "100%"
            ]
        "td" ->
            [ ("border", attr s "border")
            , ("border-bottom", attr s "border-bottom")
            , ("border-left", attr s "border-left")
            , ("border-radius", attr s "border-radius")
            , ("border-right", attr s "border-right")
            , ("border-top", attr s "border-top")
            , style "cursor" "auto"
            , ("font-style", attr s "font-style")
            , ("height", attr s "height")
            , ("mso-padding-alt", attr s "inner-padding")
            , ("text-align", attr s "text-align")
            , ("background", attr s "background-color")
            ]
        "content" ->
            [ style "display" "inline-block"
            , ("width", calculateAWidth (attr s "width"))
            , ("background", attr s "background-color")
            , ("color", attr s "color")
            , ("font-family", attr s "font-family")
            , ("font-size", attr s "font-size")
            , ("font-style", attr s "font-style")
            , ("font-weight", attr s "font-weight")
            , ("line-height", attr s "line-height")
            , ("letter-spacing", attr s "letter-spacing")
            , style "margin" "0"
            , ("text-decoration", attr s "text-decoration")
            , ("text-transform", attr s "text-transform")
            , ("padding", attr s "inner-padding")
            , style "mso-padding-alt" "0px"
            , ("border-radius", attr s "border-radius")
            ]
        _ -> []

    calculateAWidth = \case
        Just width
            | truthy (Just width) ->
                let (parsed, unit) = widthParser True width
                    innerPaddings = shorthandValue s "inner-padding" "left" + shorthandValue s "inner-padding" "right"
                 in if unit /= "px"
                        then Nothing
                        else Just (showNumber (parsed - innerPaddings - borders (boxWidths s)) <> "px")
        _ -> Nothing

    tag = if truthy (attr s "href") then "a" else "p"

    rendered =
        mconcat
            [ "<table"
            , htmlAttributes
                inst
                [ ("border", AV "0")
                , ("cellpadding", AV "0")
                , ("cellspacing", AV "0")
                , ("role", AV "presentation")
                , ("style", AStyleOf "table")
                ]
            , "><tbody><tr><td"
            , htmlAttributes
                inst
                [ ("align", AV "center")
                , ( "bgcolor"
                  , case attr s "background-color" of
                        Just "none" -> ANone
                        v -> maybe ANone AV v
                  )
                , ("role", AV "presentation")
                , ("style", AStyleOf "td")
                , ("valign", maybe ANone AV (attr s "vertical-align"))
                ]
            , "><"
            , tag
            , htmlAttributes
                inst
                [ ("href", maybe ANone AV (attr s "href"))
                , ("name", maybe ANone AV (attr s "name"))
                , ("rel", maybe ANone AV (attr s "rel"))
                , ("title", maybe ANone AV (attr s "title"))
                , ("style", AStyleOf "content")
                , ("target", if tag == "a" then maybe ANone AV (attr s "target") else ANone)
                ]
            , ">"
            , fromText (getContent s)
            , "</"
            , tag
            , "></td></tr></tbody></table>"
            ]
