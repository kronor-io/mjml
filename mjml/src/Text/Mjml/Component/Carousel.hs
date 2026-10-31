-- | @mj-carousel@ and @mj-carousel-image@.
module Text.Mjml.Component.Carousel (
    mjCarousel,
    mjCarouselImage,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Text.Mjml.Component.Section (setContext)
import Text.Mjml.Js (parseIntJs, showNumber)
import Text.Mjml.Node (Node (..))
import Text.Mjml.Render


-- | The identifier MJML draws at random in the carousel's constructor, kept
-- in the instance's props.
carouselIdKey :: Text
carouselIdKey = "mj-carousel-id"


-- | Which of the image's methods the carousel is calling: MJML renders the
-- same children three times, through @renderRadio@, @renderThumbnail@ and
-- @render@.
imageModeKey :: Text
imageModeKey = "mj-carousel-render"


tshow :: Int -> Text
tshow = T.pack . show


mjCarousel :: Component
mjCarousel =
    (defComponent "mj-carousel" build')
        { allowedAttributes =
            [ ("align", "enum(left,center,right)")
            , ("border-radius", "string")
            , ("container-background-color", "color")
            , ("icon-width", "unit(px,%)")
            , ("left-icon", "string")
            , ("padding", "unit(px,%){1,4}")
            , ("padding-top", "unit(px,%)")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("right-icon", "string")
            , ("thumbnails", "enum(visible,hidden,supported)")
            , ("tb-border", "string")
            , ("tb-border-radius", "string")
            , ("tb-hover-border-color", "color")
            , ("tb-selected-border-color", "color")
            , ("tb-width", "unit(px,%)")
            ]
        , defaults =
            [ ("align", "center")
            , ("border-radius", "6px")
            , ("icon-width", "44px")
            , ("left-icon", "https://i.imgur.com/xTh3hln.png")
            , ("right-icon", "https://i.imgur.com/os7o9kz.png")
            , ("thumbnails", "visible")
            , ("tb-border", "2px solid transparent")
            , ("tb-border-radius", "6px")
            , ("tb-hover-border-color", "#fead0d")
            , ("tb-selected-border-color", "#ccc")
            ]
        , prepare = \s -> do
            i <- nextId
            pure s {props = (props s) {extra = merge (extra (props s)) [(carouselIdKey, i)]}}
        }
  where
    build' s = inst
      where
        carouselId = fromMaybe "" (lookup carouselIdKey (extra (props s)))
        kids = children (node s)
        len = length kids
        a = attrOr s

        inst =
            (defInstance s render')
                { getStyles = \case
                    "carousel.div" ->
                        [ style "display" "table"
                        , style "width" "100%"
                        , style "table-layout" "fixed"
                        , style "text-align" "center"
                        , style "font-size" "0px"
                        ]
                    "carousel.table" ->
                        [ style "caption-side" "top"
                        , style "display" "table-caption"
                        , style "table-layout" "fixed"
                        , style "width" "100%"
                        ]
                    "images.td" -> [style "padding" "0px"]
                    "controls.div" -> [style "display" "none", style "mso-hide" "all"]
                    "controls.img" ->
                        [style "display" "block", ("width", attr s "icon-width"), style "height" "auto"]
                    "controls.td" ->
                        [ style "font-size" "0px"
                        , style "display" "none"
                        , style "mso-hide" "all"
                        , style "padding" "0px"
                        ]
                    _ -> []
                , childContext = setContext [("thumbnails", attr s "thumbnails")] (context s)
                , instanceHeadStyle = Just (const headStyle')
                }

        -- With no parent width in the context MJML's division is NaN, which
        -- lodash's min skips, so the default is always 110px.
        thumbnailsWidth
            | len == 0 = "0"
            | otherwise = case attr s "tb-width" of
                Just w | truthy (Just w) -> w
                _ -> "110px"

        modeOptions mode = defChildOptions {childProps = [(imageModeKey, mode)]}

        generateRadios =
            renderChildren
                inst
                (modeOptions "radio") {childAttributes = [("carouselId", carouselId)]}

        generateThumbnails
            | a "thumbnails" "" `notElem` ["visible", "supported"] = pure ""
            | otherwise =
                renderChildren
                    inst
                    (modeOptions "thumbnail")
                        { childAttributes =
                            [ ("tb-border", a "tb-border" "")
                            , ("tb-border-radius", a "tb-border-radius" "")
                            , ("tb-width", thumbnailsWidth)
                            , ("carouselId", carouselId)
                            ]
                        }

        generateControls direction icon =
            mconcat
                [ "<td"
                , htmlAttributes
                    inst
                    [ ("class", AV ("mj-carousel-" <> carouselId <> "-icons-cell"))
                    , ("style", AStyleOf "controls.td")
                    ]
                , "><div"
                , htmlAttributes
                    inst
                    [ ("class", AV ("mj-carousel-" <> direction <> "-icons"))
                    , ("style", AStyleOf "controls.div")
                    ]
                , ">"
                , mconcat
                    [ mconcat
                        [ "<label"
                        , htmlAttributes
                            inst
                            [ ("for", AV ("mj-carousel-" <> carouselId <> "-radio-" <> tshow i))
                            , ("class", AV ("mj-carousel-" <> direction <> " mj-carousel-" <> direction <> "-" <> tshow i))
                            ]
                        , "><img"
                        , htmlAttributes
                            inst
                            [ ("src", maybe ANone AV icon)
                            , ("alt", AV direction)
                            , ("style", AStyleOf "controls.img")
                            , ("width", AV (showNumber (parseIntJs (a "icon-width" ""))))
                            ]
                        , " /></label>"
                        ]
                    | i <- [1 .. len]
                    ]
                , "</div></td>"
                ]

        generateImages = do
            images <- renderChildren inst defChildOptions {childAttributes = borderRadius}
            pure $
                mconcat
                    [ "<td"
                    , htmlAttributes inst [("style", AStyleOf "images.td")]
                    , "><div"
                    , htmlAttributes inst [("class", AV "mj-carousel-images")]
                    , ">"
                    , images
                    , "</div></td>"
                    ]

        borderRadius = [("border-radius", a "border-radius" "")]

        generateCarousel = do
            images <- generateImages
            pure $
                mconcat
                    [ "<table"
                    , htmlAttributes
                        inst
                        [ ("style", AStyleOf "carousel.table")
                        , ("border", AV "0")
                        , ("cellpadding", AV "0")
                        , ("cellspacing", AV "0")
                        , ("width", AV "100%")
                        , ("role", AV "presentation")
                        , ("class", AV "mj-carousel-main")
                        ]
                    , "><tbody><tr>"
                    , generateControls "previous" (attr s "left-icon")
                    , images
                    , generateControls "next" (attr s "right-icon")
                    , "</tr></tbody></table>"
                    ]

        renderFallback = case kids of
            [] -> pure ""
            k : _ ->
                msoConditionalTag False
                    <$> renderChildren inst defChildOptions {childNodes = Just [k], childAttributes = borderRadius}

        render' = do
            radios <- generateRadios
            thumbnails <- generateThumbnails
            carousel <- generateCarousel
            fallback <- renderFallback
            pure $
                msoConditionalTag
                    True
                    ( mconcat
                        [ "<div"
                        , htmlAttributes inst [("class", AV "mj-carousel")]
                        , ">"
                        , radios
                        , "<div"
                        , htmlAttributes
                            inst
                            [ ("class", AV ("mj-carousel-content mj-carousel-" <> carouselId <> "-content"))
                            , ("style", AStyleOf "carousel.div")
                            ]
                        , ">"
                        , thumbnails
                        , carousel
                        , "</div></div>"
                        ]
                    )
                    <> fallback

        -- componentHeadStyle
        headStyle'
            | len == 0 = ""
            | otherwise = carouselCss <> "\n" <> fallbackCss
        cid = carouselId
        rep str n = T.replicate (max 0 n) str
        range = [0 .. len - 1]
        selectorList f = T.intercalate "," (map f range)
        carouselCss =
            T.unlines
                [ ".mj-carousel { -webkit-user-select: none; -moz-user-select: none; user-select: none; }"
                , ".mj-carousel-" <> cid <> "-icons-cell { display: table-cell !important; width: " <> a "icon-width" "" <> " !important; }"
                , ".mj-carousel-radio, .mj-carousel-next, .mj-carousel-previous { display: none !important; }"
                , ".mj-carousel-thumbnail, .mj-carousel-next, .mj-carousel-previous { touch-action: manipulation; }"
                , selectorList (\i -> ".mj-carousel-" <> cid <> "-radio:checked " <> rep "+ * " i <> "+ .mj-carousel-content .mj-carousel-image")
                    <> " { display: none !important; }"
                , selectorList (\i -> radioChecked i <> "+ .mj-carousel-content .mj-carousel-image-" <> tshow (i + 1))
                    <> " { display: block !important; }"
                , ".mj-carousel-previous-icons,\n.mj-carousel-next-icons,\n"
                    <> selectorList (\i -> radioChecked i <> "+ .mj-carousel-content .mj-carousel-next-" <> tshow ((i + (1 `mod` len) + len) `mod` len + 1))
                    <> ",\n"
                    <> selectorList (\i -> radioChecked i <> "+ .mj-carousel-content .mj-carousel-previous-" <> tshow ((i - (1 `mod` len) + len) `mod` len + 1))
                    <> " { display: block !important; }"
                , selectorList (\i -> radioChecked i <> "+ .mj-carousel-content .mj-carousel-" <> cid <> "-thumbnail-" <> tshow (i + 1))
                    <> " { border-color: "
                    <> a "tb-selected-border-color" ""
                    <> " !important; }"
                , selectorList (\i -> radioChecked i <> "+ .mj-carousel-content .mj-carousel-" <> cid <> "-thumbnail\n          ")
                    <> " { display: inline-block !important; }"
                , ".mj-carousel-image img + div, .mj-carousel-thumbnail img + div { display: none !important; }"
                , selectorList (\i -> ".mj-carousel-" <> cid <> "-thumbnail:hover " <> rep "+ * " (len - i - 1) <> "+ .mj-carousel-main .mj-carousel-image")
                    <> " { display: none !important; }"
                , ".mj-carousel-thumbnail:hover { border-color: " <> a "tb-hover-border-color" "" <> " !important; }"
                , selectorList (\i -> ".mj-carousel-" <> cid <> "-thumbnail-" <> tshow (i + 1) <> ":hover " <> rep "+ * " (len - i - 1) <> "+ .mj-carousel-main .mj-carousel-image-" <> tshow (i + 1))
                    <> " { display: block !important; }"
                ]
        radioChecked i = ".mj-carousel-" <> cid <> "-radio-" <> tshow (i + 1) <> ":checked " <> rep "+ * " (len - i - 1)
        fallbackCss =
            T.unlines
                [ ".mj-carousel noinput { display:block !important; }"
                , ".mj-carousel noinput .mj-carousel-image-1 { display: block !important;  }"
                , ".mj-carousel noinput .mj-carousel-arrows, .mj-carousel noinput .mj-carousel-thumbnails { display: none !important; }"
                , "[owa] .mj-carousel-thumbnail { display: none !important; }"
                , "@media screen yahoo {"
                , ".mj-carousel-" <> cid <> "-icons-cell, .mj-carousel-previous-icons, .mj-carousel-next-icons { display: none !important; }"
                , ".mj-carousel-" <> cid <> "-radio-1:checked " <> rep "+ *" (len - 1) <> "+ .mj-carousel-content .mj-carousel-" <> cid <> "-thumbnail-1 { border-color: transparent; }"
                , "}"
                ]


mjCarouselImage :: Component
mjCarouselImage =
    (defComponent "mj-carousel-image" build')
        { endingTag = True
        , allowedAttributes =
            [ ("alt", "string")
            , ("href", "string")
            , ("rel", "string")
            , ("target", "string")
            , ("title", "string")
            , ("src", "string")
            , ("thumbnails-src", "string")
            , ("border-radius", "string")
            , ("tb-border", "string")
            , ("tb-border-radius", "string")
            ]
        , defaults = [("alt", ""), ("target", "_blank")]
        }
  where
    build' s = inst
      where
        p = props s
        i = index p
        imgIndex = tshow (i + 1)
        containerWidth = fromMaybe "" (ctx s "containerWidth")
        carouselId = attrOr s "carouselId" ""
        opt k = maybe ANone AV (attr s k)

        hasThumbnailsSupported =
            let thumbnails = case attr s "thumbnails" of
                    Just t | truthy (Just t) -> Just t
                    _ -> ctx s "thumbnails"
             in thumbnails == Just "supported"

        inst =
            (defInstance s render')
                { getStyles = \case
                    "images.img" ->
                        [ ("border-radius", attr s "border-radius")
                        , style "display" "block"
                        , style "width" containerWidth
                        , style "max-width" "100%"
                        , style "height" "auto"
                        ]
                    "images.firstImageDiv" -> []
                    "images.otherImageDiv" -> [style "display" "none", style "mso-hide" "all"]
                    "radio.input" -> [style "display" "none", style "mso-hide" "all"]
                    "thumbnails.a" ->
                        [ ("border", attr s "tb-border")
                        , ("border-radius", attr s "tb-border-radius")
                        , style "display" (if hasThumbnailsSupported then "none" else "inline-block")
                        , style "overflow" "hidden"
                        , ("width", attr s "tb-width")
                        ]
                    "thumbnails.img" -> [style "display" "block", style "width" "100%", style "height" "auto"]
                    _ -> []
                }

        render' = pure $ case lookup imageModeKey (extra p) of
            Just "radio" -> renderRadio
            Just "thumbnail" -> renderThumbnail
            _ -> renderImage

        renderThumbnail =
            mconcat
                [ "<a"
                , htmlAttributes
                    inst
                    [ ("style", AStyleOf "thumbnails.a")
                    , ("href", AV ("#" <> imgIndex))
                    , ("target", opt "target")
                    , ( "class"
                      , AV
                            ( "mj-carousel-thumbnail mj-carousel-"
                                <> carouselId
                                <> "-thumbnail mj-carousel-"
                                <> carouselId
                                <> "-thumbnail-"
                                <> imgIndex
                                <> " "
                                <> suffixCssClasses (attr s "css-class") "thumbnail"
                            )
                      )
                    ]
                , "><label"
                , htmlAttributes inst [("for", AV ("mj-carousel-" <> carouselId <> "-radio-" <> imgIndex))]
                , "><img"
                , htmlAttributes
                    inst
                    [ ("style", AStyleOf "thumbnails.img")
                    , ( "src"
                      , case attr s "thumbnails-src" of
                            Just t | truthy (Just t) -> AV t
                            _ -> opt "src"
                      )
                    , ("alt", opt "alt")
                    , ("width", AV (showNumber (parseIntJs (attrOr s "tb-width" ""))))
                    ]
                , " /></label></a>"
                ]

        renderRadio =
            "<input"
                <> htmlAttributes
                    inst
                    [ ("class", AV ("mj-carousel-radio mj-carousel-" <> carouselId <> "-radio mj-carousel-" <> carouselId <> "-radio-" <> imgIndex))
                    , ("checked", if i == 0 then AV "checked" else ANone)
                    , ("type", AV "radio")
                    , ("name", AV ("mj-carousel-radio-" <> carouselId))
                    , ("id", AV ("mj-carousel-" <> carouselId <> "-radio-" <> imgIndex))
                    , ("style", AStyleOf "radio.input")
                    ]
                <> " />"

        renderImage =
            let image =
                    "<img"
                        <> htmlAttributes
                            inst
                            [ ("title", opt "title")
                            , ("src", opt "src")
                            , ("alt", opt "alt")
                            , ("style", AStyleOf "images.img")
                            , ("width", AV (showNumber (parseIntJs containerWidth)))
                            , ("border", AV "0")
                            ]
                        <> " />"
                cssClass = attrOr s "css-class" ""
             in mconcat
                    [ "<div"
                    , htmlAttributes
                        inst
                        [ ("class", AV ("mj-carousel-image mj-carousel-image-" <> imgIndex <> " " <> cssClass))
                        , ("style", AStyleOf (if i == 0 then "images.firstImageDiv" else "images.otherImageDiv"))
                        ]
                    , ">"
                    , case attr s "href" of
                        Just href
                            | truthy (Just href) ->
                                "<a"
                                    <> htmlAttributes inst [("href", AV href), ("rel", opt "rel"), ("target", AV "_blank")]
                                    <> ">"
                                    <> image
                                    <> "</a>"
                        _ -> image
                    , "</div>"
                    ]
