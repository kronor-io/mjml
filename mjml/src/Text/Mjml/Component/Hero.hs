-- | @mj-hero@.
module Text.Mjml.Component.Hero (
    mjHero,
) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T

import Text.Mjml.Component.Section (setContext)
import Text.Mjml.Js (parseFloatJs, parseIntJs, roundJs, showNumber)
import Text.Mjml.Render


mjHero :: Component
mjHero =
    (defComponent "mj-hero" build')
        { allowedAttributes =
            [ ("mode", "string")
            , ("height", "unit(px,%)")
            , ("background-url", "string")
            , ("background-width", "unit(px,%)")
            , ("background-height", "unit(px,%)")
            , ("background-position", "string")
            , ("border-radius", "string")
            , ("inner-background-color", "color")
            , ("inner-padding", "unit(px,%){1,4}")
            , ("inner-padding-top", "unit(px,%)")
            , ("inner-padding-left", "unit(px,%)")
            , ("inner-padding-right", "unit(px,%)")
            , ("inner-padding-bottom", "unit(px,%)")
            , ("padding", "unit(px,%){1,4}")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("background-color", "color")
            , ("vertical-align", "enum(top,bottom,middle)")
            ]
        , -- MJML also lists null defaults for background-url and the
          -- directional paddings, which is the same as leaving them out.
          defaults =
            [ ("mode", "fixed-height")
            , ("height", "0px")
            , ("background-position", "center center")
            , ("padding", "0px")
            , ("background-color", "#ffffff")
            , ("vertical-align", "top")
            ]
        }


build' :: Self -> Instance
build' s = inst
  where
    inst =
        (defInstance s renderHero)
            { getStyles = stylesFor
            , childContext = setContext [("containerWidth", Just currentContainerWidth)] (context s)
            }

    containerWidth = fromMaybe "" (ctx s "containerWidth")

    currentContainerWidth =
        let paddingSize = shorthandValue s "padding" "left" + shorthandValue s "padding" "right"
         in showNumber (parseFloatJs containerWidth - paddingSize) <> "px"

    background =
        T.intercalate " " . filter (not . T.null) $
            [fromMaybe "" (attr s "background-color")]
                <> if truthy (attr s "background-url")
                    then
                        [ "url('" <> fromMaybe "" (attr s "background-url") <> "')"
                        , "no-repeat"
                        , fromMaybe "" (attr s "background-position") <> " / cover"
                        ]
                    else []

    stylesFor = \case
        "div" -> [style "margin" "0 auto", style "max-width" containerWidth]
        "table" -> [style "width" "100%"]
        "tr" -> [style "vertical-align" "top"]
        "td-fluid" ->
            let ratio =
                    roundJs
                        ( parseIntJs (fromMaybe "" (attr s "background-height"))
                            / parseIntJs (fromMaybe "" (attr s "background-width"))
                            * 100
                        )
             in [ style "width" "0.01%"
                , style "padding-bottom" (showNumber ratio <> "%")
                , style "mso-padding-bottom-alt" "0"
                ]
        "outlook-table" -> [style "width" containerWidth]
        "outlook-td" ->
            [ style "line-height" "0"
            , style "font-size" "0"
            , style "mso-line-height-rule" "exactly"
            ]
        "outlook-inner-table" -> [style "width" currentContainerWidth]
        "outlook-image" ->
            [ style "border" "0"
            , ("height", attr s "background-height")
            , style "mso-position-horizontal" "center"
            , style "position" "absolute"
            , style "top" "0"
            , style "width" (case attr s "background-width" of
                Just w | truthy (Just w) -> w
                _ -> containerWidth)
            , style "z-index" "-3"
            ]
        "outlook-inner-td" ->
            [ ("background-color", attr s "inner-background-color")
            , ("padding", attr s "inner-padding")
            , ("padding-top", attr s "inner-padding-top")
            , ("padding-left", attr s "inner-padding-left")
            , ("padding-right", attr s "inner-padding-right")
            , ("padding-bottom", attr s "inner-padding-bottom")
            ]
        "inner-div" ->
            [ ("background-color", attr s "inner-background-color")
            , ("float", attr s "align")
            , style "margin" "0px auto"
            , ("width", attr s "width")
            , ("padding", attr s "inner-padding")
            , ("padding-top", attr s "inner-padding-top")
            , ("padding-left", attr s "inner-padding-left")
            , ("padding-right", attr s "inner-padding-right")
            , ("padding-bottom", attr s "inner-padding-bottom")
            ]
        "inner-table" -> [style "width" "100%", style "margin" "0px"]
        _ -> []

    renderContent = do
        kids <-
            renderChildren
                inst
                defChildOptions
                    { renderer = \c ->
                        if rawElement (component (self c))
                            then render c
                            else do
                                body <- render c
                                let cs = self c
                                pure $
                                    T.concat
                                        [ "<tr><td"
                                        , htmlAttributes
                                            c
                                            [ ("align", maybe ANone AV (attr cs "align"))
                                            , ("background", maybe ANone AV (attr cs "container-background-color"))
                                            , ("class", maybe ANone AV (attr cs "css-class"))
                                            , ( "style"
                                              , AStyles
                                                    [ ("background", attr cs "container-background-color")
                                                    , style "font-size" "0px"
                                                    , ("padding", attr cs "padding")
                                                    , ("padding-top", attr cs "padding-top")
                                                    , ("padding-right", attr cs "padding-right")
                                                    , ("padding-bottom", attr cs "padding-bottom")
                                                    , ("padding-left", attr cs "padding-left")
                                                    , style "word-break" "break-word"
                                                    ]
                                              )
                                            ]
                                        , ">"
                                        , body
                                        , "</td></tr>"
                                        ]
                    }
        let innerTable =
                htmlAttributes
                    inst
                    [ ("border", AV "0")
                    , ("cellpadding", AV "0")
                    , ("cellspacing", AV "0")
                    , ("role", AV "presentation")
                    , ("style", AStyleOf "inner-table")
                    ]
        pure $
            T.concat
                [ startConditionalTag
                , "<table"
                , htmlAttributes
                    inst
                    [ ("align", maybe ANone AV (attr s "align"))
                    , ("border", AV "0")
                    , ("cellpadding", AV "0")
                    , ("cellspacing", AV "0")
                    , ("style", AStyleOf "outlook-inner-table")
                    , ("width", AV (showNumber (parseIntJs currentContainerWidth)))
                    ]
                , "><tr><td"
                , htmlAttributes inst [("style", AStyleOf "outlook-inner-td")]
                , ">"
                , endConditionalTag
                , "<div"
                , htmlAttributes
                    inst
                    [ ("align", maybe ANone AV (attr s "align"))
                    , ("class", AV "mj-hero-content")
                    , ("style", AStyleOf "inner-div")
                    ]
                , "><table"
                , innerTable
                , "><tbody><tr><td"
                , htmlAttributes inst [("style", AStyleOf "inner-td")]
                , "><table"
                , innerTable
                , "><tbody>"
                , kids
                , "</tbody></table></td></tr></tbody></table></div>"
                , startConditionalTag
                , "</td></tr></table>"
                , endConditionalTag
                ]

    commonStyle =
        [ style "background" background
        , ("background-position", attr s "background-position")
        , style "background-repeat" "no-repeat"
        , ("border-radius", attr s "border-radius")
        , ("padding", attr s "padding")
        , ("padding-top", attr s "padding-top")
        , ("padding-left", attr s "padding-left")
        , ("padding-right", attr s "padding-right")
        , ("padding-bottom", attr s "padding-bottom")
        , ("vertical-align", attr s "vertical-align")
        ]

    backgroundAttr = ("background", maybe ANone AV (attr s "background-url"))

    renderMode = do
        body <- renderContent
        pure $ case attr s "mode" of
            Just "fluid-height" ->
                let magicTd = "<td" <> htmlAttributes inst [("style", AStyleOf "td-fluid")] <> " />"
                 in T.concat
                        [ magicTd
                        , "<td"
                        , htmlAttributes inst [backgroundAttr, ("style", AStyles commonStyle)]
                        , ">"
                        , body
                        , "</td>"
                        , magicTd
                        ]
            _ ->
                let height =
                        parseIntJs (fromMaybe "" (attr s "height"))
                            - shorthandValue s "padding" "top"
                            - shorthandValue s "padding" "bottom"
                 in T.concat
                        [ "<td"
                        , htmlAttributes
                            inst
                            [ backgroundAttr
                            , ("style", AStyles (commonStyle `merge` [style "height" (showNumber height <> "px")]))
                            , ("height", AV (showNumber height))
                            ]
                        , ">"
                        , body
                        , "</td>"
                        ]

    renderHero = do
        mode <- renderMode
        pure $
            T.concat
                [ startConditionalTag
                , "<table"
                , htmlAttributes
                    inst
                    [ ("align", AV "center")
                    , ("border", AV "0")
                    , ("cellpadding", AV "0")
                    , ("cellspacing", AV "0")
                    , ("role", AV "presentation")
                    , ("style", AStyleOf "outlook-table")
                    , ("width", AV (showNumber (parseIntJs containerWidth)))
                    ]
                , "><tr><td"
                , htmlAttributes inst [("style", AStyleOf "outlook-td")]
                , "><v:image"
                , htmlAttributes
                    inst
                    [ ("style", AStyleOf "outlook-image")
                    , ("src", maybe ANone AV (attr s "background-url"))
                    , ("xmlns:v", AV "urn:schemas-microsoft-com:vml")
                    ]
                , " />"
                , endConditionalTag
                , "<div"
                , htmlAttributes
                    inst
                    [ ("align", maybe ANone AV (attr s "align"))
                    , ("class", maybe ANone AV (attr s "css-class"))
                    , ("style", AStyleOf "div")
                    ]
                , "><table"
                , htmlAttributes
                    inst
                    [ ("border", AV "0")
                    , ("cellpadding", AV "0")
                    , ("cellspacing", AV "0")
                    , ("role", AV "presentation")
                    , ("style", AStyleOf "table")
                    ]
                , "><tbody><tr"
                , htmlAttributes inst [("style", AStyleOf "tr")]
                , ">"
                , mode
                , "</tr></tbody></table></div>"
                , startConditionalTag
                , "</td></tr></table>"
                , endConditionalTag
                ]
