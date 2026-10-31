-- | @mj-social@ and @mj-social-element@.
module Text.Mjml.Component.Social (
    mjSocial,
    mjSocialElement,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Text.Mjml.Js (parseIntJs, showNumber)
import Text.Mjml.Render


mjSocial :: Component
mjSocial =
    (defComponent "mj-social" build')
        { allowedAttributes =
            [ ("align", "enum(left,right,center)")
            , ("border", "string")
            , ("border-radius", "string")
            , ("container-background-color", "color")
            , ("color", "color")
            , ("font-family", "string")
            , ("font-size", "unit(px)")
            , ("font-style", "string")
            , ("font-weight", "string")
            , ("icon-size", "unit(px,%)")
            , ("icon-height", "unit(px,%)")
            , ("icon-padding", "unit(px,%){1,4}")
            , ("inner-padding", "unit(px,%){1,4}")
            , ("line-height", "unit(px,%,)")
            , ("mode", "enum(horizontal,vertical)")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("padding", "unit(px,%){1,4}")
            , ("table-layout", "enum(auto,fixed)")
            , ("text-padding", "unit(px,%){1,4}")
            , ("text-decoration", "string")
            , ("vertical-align", "enum(top,bottom,middle)")
            ]
        , defaults =
            [ ("align", "center")
            , ("border-radius", "3px")
            , ("color", "#333333")
            , ("font-family", "Ubuntu, Helvetica, Arial, sans-serif")
            , ("font-size", "13px")
            , ("icon-size", "20px")
            , ("line-height", "22px")
            , ("mode", "horizontal")
            , ("padding", "10px 25px")
            , ("text-decoration", "none")
            ]
        }
  where
    build' s = inst
      where
        inst =
            (defInstance s render')
                { getStyles = \case
                    "tableVertical" -> [style "margin" "0px"]
                    _ -> []
                }

        render' =
            if attr s "mode" == Just "horizontal" then renderHorizontal else renderVertical

        socialElementAttributes =
            [("padding", p) | Just p <- [attr s "inner-padding"], truthy (Just p)]
                `merge` [ (k, v)
                        | k <-
                            [ "border"
                            , "border-radius"
                            , "color"
                            , "font-family"
                            , "font-size"
                            , "font-weight"
                            , "font-style"
                            , "icon-size"
                            , "icon-height"
                            , "icon-padding"
                            , "text-padding"
                            , "line-height"
                            , "text-decoration"
                            ]
                        , Just v <- [attr s k]
                        ]

        renderHorizontal = do
            kids <-
                renderChildren
                    inst
                    defChildOptions
                        { childAttributes = socialElementAttributes
                        , renderer = \c ->
                            if rawElement (component (self c))
                                then render c
                                else do
                                    body <- render c
                                    pure $
                                        T.concat
                                            [ startConditionalTag
                                            , "<td>"
                                            , endConditionalTag
                                            , "<table"
                                            , htmlAttributes
                                                c
                                                [ ("align", maybe ANone AV (attr s "align"))
                                                , ("border", AV "0")
                                                , ("cellpadding", AV "0")
                                                , ("cellspacing", AV "0")
                                                , ("role", AV "presentation")
                                                , ("style", AStyles [style "float" "none", style "display" "inline-table"])
                                                ]
                                            , "><tbody>"
                                            , body
                                            , "</tbody></table>"
                                            , startConditionalTag
                                            , "</td>"
                                            , endConditionalTag
                                            ]
                        }
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
                        , ("role", AV "presentation")
                        ]
                    , "><tr>"
                    , endConditionalTag
                    , kids
                    , startConditionalTag
                    , "</tr></table>"
                    , endConditionalTag
                    ]

        renderVertical = do
            kids <- renderChildren inst defChildOptions {childAttributes = socialElementAttributes}
            pure $
                T.concat
                    [ "<table"
                    , htmlAttributes
                        inst
                        [ ("border", AV "0")
                        , ("cellpadding", AV "0")
                        , ("cellspacing", AV "0")
                        , ("role", AV "presentation")
                        , ("style", AStyleOf "tableVertical")
                        ]
                    , "><tbody>"
                    , kids
                    , "</tbody></table>"
                    ]


imgBaseUrl :: Text
imgBaseUrl = "https://www.mailjet.com/images/theme/v1/icons/ico-social/"


-- | The networks MJML knows by name, each also available as @NAME-noshare@,
-- which links to the URL as given instead of through a share page.
socialNetworks :: [(Text, Attrs)]
socialNetworks = base <> [(k <> "-noshare", merge v [("share-url", "[[URL]]")]) | (k, v) <- base]
  where
    base =
        [ ("facebook", [("share-url", "https://www.facebook.com/sharer/sharer.php?u=[[URL]]"), ("background-color", "#3b5998"), ("src", imgBaseUrl <> "facebook.png")])
        , ("twitter", [("share-url", "https://twitter.com/intent/tweet?url=[[URL]]"), ("background-color", "#55acee"), ("src", imgBaseUrl <> "twitter.png")])
        , ("x", [("share-url", "https://twitter.com/intent/tweet?url=[[URL]]"), ("background-color", "#000000"), ("src", imgBaseUrl <> "twitter-x.png")])
        , ("google", [("share-url", "https://plus.google.com/share?url=[[URL]]"), ("background-color", "#dc4e41"), ("src", imgBaseUrl <> "google-plus.png")])
        , ("pinterest", [("share-url", "https://pinterest.com/pin/create/button/?url=[[URL]]&media=&description="), ("background-color", "#bd081c"), ("src", imgBaseUrl <> "pinterest.png")])
        , ("linkedin", [("share-url", "https://www.linkedin.com/shareArticle?mini=true&url=[[URL]]&title=&summary=&source="), ("background-color", "#0077b5"), ("src", imgBaseUrl <> "linkedin.png")])
        , ("instagram", [("background-color", "#3f729b"), ("src", imgBaseUrl <> "instagram.png")])
        , ("web", [("src", imgBaseUrl <> "web.png"), ("background-color", "#4BADE9")])
        , ("snapchat", [("src", imgBaseUrl <> "snapchat.png"), ("background-color", "#FFFA54")])
        , ("youtube", [("src", imgBaseUrl <> "youtube.png"), ("background-color", "#EB3323")])
        , ("tumblr", [("src", imgBaseUrl <> "tumblr.png"), ("share-url", "https://www.tumblr.com/widgets/share/tool?canonicalUrl=[[URL]]"), ("background-color", "#344356")])
        , ("github", [("src", imgBaseUrl <> "github.png"), ("background-color", "#000000")])
        , ("xing", [("src", imgBaseUrl <> "xing.png"), ("share-url", "https://www.xing.com/app/user?op=share&url=[[URL]]"), ("background-color", "#296366")])
        , ("vimeo", [("src", imgBaseUrl <> "vimeo.png"), ("background-color", "#53B4E7")])
        , ("medium", [("src", imgBaseUrl <> "medium.png"), ("background-color", "#000000")])
        , ("soundcloud", [("src", imgBaseUrl <> "soundcloud.png"), ("background-color", "#EF7F31")])
        , ("dribbble", [("src", imgBaseUrl <> "dribbble.png"), ("background-color", "#D95988")])
        ]


mjSocialElement :: Component
mjSocialElement =
    (defComponent "mj-social-element" build')
        { endingTag = True
        , allowedAttributes =
            [ ("align", "enum(left,center,right)")
            , ("icon-position", "enum(left,right)")
            , ("background-color", "color")
            , ("color", "color")
            , ("border", "string")
            , ("border-radius", "string")
            , ("font-family", "string")
            , ("font-size", "unit(px)")
            , ("font-style", "string")
            , ("font-weight", "string")
            , ("href", "string")
            , ("icon-size", "unit(px,%)")
            , ("icon-height", "unit(px,%)")
            , ("icon-padding", "unit(px,%){1,4}")
            , ("line-height", "unit(px,%,)")
            , ("name", "string")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("padding", "unit(px,%){1,4}")
            , ("text-padding", "unit(px,%){1,4}")
            , ("rel", "string")
            , ("src", "string")
            , ("srcset", "string")
            , ("sizes", "string")
            , ("alt", "string")
            , ("title", "string")
            , ("target", "string")
            , ("text-decoration", "string")
            , ("vertical-align", "enum(top,middle,bottom)")
            ]
        , defaults =
            [ ("alt", "")
            , ("align", "left")
            , ("icon-position", "left")
            , ("color", "#000")
            , ("border", "0")
            , ("border-radius", "3px")
            , ("font-family", "Ubuntu, Helvetica, Arial, sans-serif")
            , ("font-size", "13px")
            , ("line-height", "1")
            , ("padding", "4px")
            , ("text-padding", "4px 4px 4px 0")
            , ("target", "_blank")
            , ("text-decoration", "none")
            , ("vertical-align", "middle")
            ]
        }
  where
    build' s = inst
      where
        inst = (defInstance s (pure rendered)) {getStyles = stylesFor}

        network = fromMaybe [] (attr s "name" >>= \n -> lookup n socialNetworks)

        -- getSocialAttributes
        href = case attr s "href" of
            Just h
                | truthy (Just h)
                , Just shareUrl <- lookup "share-url" network
                , truthy (Just shareUrl) ->
                    let (before, after) = T.breakOn "[[URL]]" shareUrl
                     in Just (if T.null after then shareUrl else before <> h <> T.drop 7 after)
            other -> other
        social k = case attr s k of
            Just v | truthy (Just v) -> Just v
            _ -> lookup k network
        iconSize = social "icon-size"
        iconHeight = social "icon-height"
        backgroundColor = social "background-color"
        src = social "src"
        srcset = social "srcset"
        sizes = social "sizes"

        stylesFor = \case
            "td" ->
                [ ("padding", attr s "padding")
                , ("padding-top", attr s "padding-top")
                , ("padding-right", attr s "padding-right")
                , ("padding-bottom", attr s "padding-bottom")
                , ("padding-left", attr s "padding-left")
                , ("vertical-align", attr s "vertical-align")
                ]
            "table" ->
                [ ("background", backgroundColor)
                , ("border-radius", attr s "border-radius")
                , ("width", iconSize)
                ]
            "icon" ->
                [ ("padding", attr s "icon-padding")
                , style "font-size" "0"
                , ("height", if truthy iconHeight then iconHeight else iconSize)
                , style "vertical-align" "middle"
                , ("width", iconSize)
                ]
            "img" ->
                [ ("border", attr s "border")
                , ("border-radius", attr s "border-radius")
                , style "display" "block"
                ]
            "tdText" ->
                [ style "vertical-align" "middle"
                , ("padding", attr s "text-padding")
                , ("text-align", attr s "align")
                ]
            "text" ->
                [ ("color", attr s "color")
                , ("font-size", attr s "font-size")
                , ("font-weight", attr s "font-weight")
                , ("font-style", attr s "font-style")
                , ("font-family", attr s "font-family")
                , ("line-height", attr s "line-height")
                , ("text-decoration", attr s "text-decoration")
                ]
            _ -> []

        hasLink = truthy (attr s "href")
        opt = maybe ANone AV

        linkAttrs extra =
            [("href", opt href)]
                <> extra
                <> [("rel", opt (attr s "rel")), ("target", opt (attr s "target"))]

        makeIcon =
            T.concat
                [ "<td"
                , htmlAttributes inst [("style", AStyleOf "td")]
                , "><table"
                , htmlAttributes
                    inst
                    [ ("border", AV "0")
                    , ("cellpadding", AV "0")
                    , ("cellspacing", AV "0")
                    , ("role", AV "presentation")
                    , ("style", AStyleOf "table")
                    ]
                , "><tbody><tr><td"
                , htmlAttributes inst [("style", AStyleOf "icon")]
                , ">"
                , if hasLink then "<a" <> htmlAttributes inst (linkAttrs []) <> ">" else ""
                , "<img"
                , htmlAttributes
                    inst
                    [ ("alt", opt (attr s "alt"))
                    , ("title", opt (attr s "title"))
                    , ("src", opt src)
                    , ("style", AStyleOf "img")
                    , ("width", AV (showNumber (parseIntJs (fromMaybe "" iconSize))))
                    , ("sizes", opt sizes)
                    , ("srcset", opt srcset)
                    ]
                , " />"
                , if hasLink then "</a>" else ""
                , "</td></tr></tbody></table></td>"
                ]

        makeContent
            | T.null (getContent s) = ""
            | otherwise =
                T.concat
                    [ "<td"
                    , htmlAttributes inst [("style", AStyleOf "tdText")]
                    , ">"
                    , if hasLink
                        then "<a" <> htmlAttributes inst (linkAttrs [("style", AStyleOf "text")]) <> ">"
                        else "<span" <> htmlAttributes inst [("style", AStyleOf "text")] <> ">"
                    , getContent s
                    , if hasLink then "</a>" else "</span>"
                    , "</td>"
                    ]

        rendered =
            T.concat
                [ "<tr"
                , htmlAttributes inst [("class", opt (attr s "css-class"))]
                , ">"
                , if attr s "icon-position" == Just "left"
                    then makeIcon <> " " <> makeContent
                    else makeContent <> " " <> makeIcon
                , "</tr>"
                ]
