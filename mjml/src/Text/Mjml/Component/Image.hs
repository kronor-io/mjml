-- | @mj-image@.
module Text.Mjml.Component.Image (
    mjImage,
) where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T

import Text.Mjml.Js (parseIntJs, showNumber)
import Text.Mjml.Render


mjImage :: Component
mjImage =
    (defComponent "mj-image" build')
        { allowedAttributes =
            [ ("alt", "string")
            , ("href", "string")
            , ("name", "string")
            , ("src", "string")
            , ("srcset", "string")
            , ("sizes", "string")
            , ("title", "string")
            , ("rel", "string")
            , ("align", "enum(left,center,right)")
            , ("border", "string")
            , ("border-bottom", "string")
            , ("border-left", "string")
            , ("border-right", "string")
            , ("border-top", "string")
            , ("border-radius", "string")
            , ("container-background-color", "color")
            , ("fluid-on-mobile", "boolean")
            , ("padding", "unit(px,%){1,4}")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("target", "string")
            , ("width", "unit(px)")
            , ("height", "unit(px,auto)")
            , ("max-height", "unit(px,%)")
            , ("font-size", "unit(px)")
            , ("usemap", "string")
            ]
        , defaults =
            [ ("alt", "")
            , ("align", "center")
            , ("border", "0")
            , ("height", "auto")
            , ("padding", "10px 25px")
            , ("target", "_blank")
            , ("font-size", "13px")
            ]
        , componentHeadStyle = Just headStyle'
        }


headStyle' :: Text -> Text
headStyle' bp =
    T.concat
        [ "@media only screen and (max-width:"
        , makeLowerBreakpoint bp
        , ") {\ntable.mj-full-width-mobile { width: 100% !important; }\ntd.mj-full-width-mobile { width: auto !important; }\n}"
        ]


-- | @makeLowerBreakpoint@: one pixel below the breakpoint.
makeLowerBreakpoint :: Text -> Text
makeLowerBreakpoint bp =
    let digits = T.takeWhile isDigit (T.dropWhile (not . isDigit) bp)
     in if T.null digits
            then bp
            else showNumber (parseIntJs digits - 1) <> "px"


build' :: Self -> Instance
build' s = inst
  where
    inst =
        (defInstance s (pure rendered))
            { getStyles = stylesFor
            }

    fullWidth = attr s "full-width" == Just "full-width"
    fluid = truthy (attr s "fluid-on-mobile")

    -- getContentWidth
    contentWidth =
        let width = case attr s "width" of
                Just w | truthy (Just w) -> parseIntJs w
                _ -> 1 / 0
         in min (box (boxWidths s)) width

    stylesFor = \case
        "img" ->
            [ ("border", attr s "border")
            , ("border-left", attr s "border-left")
            , ("border-right", attr s "border-right")
            , ("border-top", attr s "border-top")
            , ("border-bottom", attr s "border-bottom")
            , ("border-radius", attr s "border-radius")
            , style "display" "block"
            , style "outline" "none"
            , style "text-decoration" "none"
            , ("height", attr s "height")
            , ("max-height", attr s "max-height")
            , ("min-width", if fullWidth then Just "100%" else Nothing)
            , style "width" "100%"
            , ("max-width", if fullWidth then Just "100%" else Nothing)
            , ("font-size", attr s "font-size")
            ]
        "td" -> [("width", if fullWidth then Nothing else Just parsedWidthUnit)]
        "table" ->
            [ ("min-width", if fullWidth then Just "100%" else Nothing)
            , ("max-width", if fullWidth then Just "100%" else Nothing)
            , ("width", if fullWidth then Just parsedWidthUnit else Nothing)
            , style "border-collapse" "collapse"
            , style "border-spacing" "0px"
            ]
        _ -> []

    parsedWidthUnit =
        let (parsed, unit) = widthParser True (showNumber contentWidth)
         in showNumber parsed <> unit

    renderImage =
        let img =
                "<img"
                    <> htmlAttributes inst
                        ( [ ("alt", maybe ANone AV (attr s "alt"))
                          , ("src", maybe ANone AV (attr s "src"))
                          , ("srcset", maybe ANone AV (attr s "srcset"))
                          , ("sizes", maybe ANone AV (attr s "sizes"))
                          , ("style", AStyleOf "img")
                          , ("title", maybe ANone AV (attr s "title"))
                          , ("width", AV (showNumber contentWidth))
                          , ("usemap", maybe ANone AV (attr s "usemap"))
                          ]
                            <> case attr s "height" of
                                Just h
                                    | truthy (Just h) ->
                                        [("height", AV (if h == "auto" then h else showNumber (parseIntJs h)))]
                                _ -> []
                        )
                    <> " />"
         in if truthy (attr s "href")
                then
                    T.concat
                        [ "<a"
                        , htmlAttributes
                            inst
                            [ ("href", maybe ANone AV (attr s "href"))
                            , ("target", maybe ANone AV (attr s "target"))
                            , ("rel", maybe ANone AV (attr s "rel"))
                            , ("name", maybe ANone AV (attr s "name"))
                            , ("title", maybe ANone AV (attr s "title"))
                            ]
                        , ">"
                        , img
                        , "</a>"
                        ]
                else img

    fluidClass = if fluid then AV "mj-full-width-mobile" else ANone

    rendered =
        T.concat
            [ "<table"
            , htmlAttributes
                inst
                [ ("border", AV "0")
                , ("cellpadding", AV "0")
                , ("cellspacing", AV "0")
                , ("role", AV "presentation")
                , ("style", AStyleOf "table")
                , ("class", fluidClass)
                ]
            , "><tbody><tr><td"
            , htmlAttributes inst [("style", AStyleOf "td"), ("class", fluidClass)]
            , ">"
            , renderImage
            , "</td></tr></tbody></table>"
            ]
