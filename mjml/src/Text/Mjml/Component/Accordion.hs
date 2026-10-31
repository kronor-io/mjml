-- | @mj-accordion@ and its parts: @mj-accordion-element@,
-- @mj-accordion-title@ and @mj-accordion-text@.
module Text.Mjml.Component.Accordion (
    mjAccordion,
    mjAccordionElement,
    mjAccordionTitle,
    mjAccordionText,
) where

import Data.Text (Text)
import Data.Text qualified as T

import Text.Mjml.Component.Section (setContext)
import Text.Mjml.Node (Node (..), element)
import Text.Mjml.Render


-- | The attributes an accordion and each element hand down to their children.
childrenAttrNames :: [Text]
childrenAttrNames =
    [ "border"
    , "icon-align"
    , "icon-width"
    , "icon-height"
    , "icon-position"
    , "icon-wrapped-url"
    , "icon-wrapped-alt"
    , "icon-unwrapped-url"
    , "icon-unwrapped-alt"
    ]


childrenAttr :: Self -> Attrs
childrenAttr s = [(k, v) | k <- childrenAttrNames, Just v <- [attr s k]]


iconAttributes :: [(Text, Text)]
iconAttributes =
    [ ("icon-align", "enum(top,middle,bottom)")
    , ("icon-width", "unit(px,%)")
    , ("icon-height", "unit(px,%)")
    , ("icon-wrapped-url", "string")
    , ("icon-wrapped-alt", "string")
    , ("icon-unwrapped-url", "string")
    , ("icon-unwrapped-alt", "string")
    , ("icon-position", "enum(left,right)")
    ]


paddingAttributes :: [(Text, Text)]
paddingAttributes =
    [ ("padding-bottom", "unit(px,%)")
    , ("padding-left", "unit(px,%)")
    , ("padding-right", "unit(px,%)")
    , ("padding-top", "unit(px,%)")
    , ("padding", "unit(px,%){1,4}")
    ]


mjAccordion :: Component
mjAccordion =
    (defComponent "mj-accordion" build')
        { allowedAttributes =
            [ ("container-background-color", "color")
            , ("border", "string")
            , ("font-family", "string")
            ]
                <> iconAttributes
                <> paddingAttributes
        , defaults =
            [ ("border", "2px solid black")
            , ("font-family", "Ubuntu, Helvetica, Arial, sans-serif")
            , ("icon-align", "middle")
            , ("icon-wrapped-url", "https://i.imgur.com/bIXv1bk.png")
            , ("icon-wrapped-alt", "+")
            , ("icon-unwrapped-url", "https://i.imgur.com/w4uTygT.png")
            , ("icon-unwrapped-alt", "-")
            , ("icon-position", "right")
            , ("icon-height", "32px")
            , ("icon-width", "32px")
            , ("padding", "10px 25px")
            ]
        , componentHeadStyle = Just (const accordionHeadStyle)
        }
  where
    build' s =
        let inst =
                (defInstance s (render' inst))
                    { getStyles = \case
                        "table" ->
                            [ style "width" "100%"
                            , style "border-collapse" "collapse"
                            , ("border", attr s "border")
                            , style "border-bottom" "none"
                            , ("font-family", attr s "font-family")
                            ]
                        _ -> []
                    , childContext = setContext [("accordionFontFamily", attr s "font-family")] (context s)
                    }
         in inst

    render' inst = do
        kids <- renderChildren inst defChildOptions {childAttributes = childrenAttr (self inst)}
        pure $
            T.concat
                [ "<table"
                , htmlAttributes
                    inst
                    [ ("cellspacing", AV "0")
                    , ("cellpadding", AV "0")
                    , ("class", AV "mj-accordion")
                    , ("style", AStyleOf "table")
                    ]
                , "><tbody>"
                , kids
                , "</tbody></table>"
                ]


accordionHeadStyle :: Text
accordionHeadStyle =
    T.unlines
        [ "noinput.mj-accordion-checkbox { display:block!important; }"
        , "@media yahoo, only screen and (min-width:0) {"
        , "  .mj-accordion-element { display:block; }"
        , "  .mj-accordion-checkbox[type=\"checkbox\"], .mj-accordion-less { display:none!important; }"
        , "  .mj-accordion-checkbox[type=\"checkbox\"] + * .mj-accordion-title { cursor:pointer; touch-action:manipulation; -webkit-user-select:none; -moz-user-select:none; user-select:none; }"
        , "  .mj-accordion-checkbox[type=\"checkbox\"] + * .mj-accordion-content { overflow:hidden; display:none; }"
        , "  .mj-accordion-checkbox[type=\"checkbox\"] + * .mj-accordion-more { display:block!important; }"
        , "  .mj-accordion-checkbox:checked + * .mj-accordion-content { display:block; }"
        , "  .mj-accordion-checkbox:checked + * .mj-accordion-more { display:none!important; }"
        , "  .mj-accordion-checkbox:checked + * .mj-accordion-less { display:block!important; }"
        , "}"
        , ".moz-text-html input.mj-accordion-checkbox + * .mj-accordion-title { cursor: auto; touch-action: auto; -webkit-user-select: auto; -moz-user-select: auto; user-select: auto; }"
        , ".moz-text-html input.mj-accordion-checkbox + * .mj-accordion-content { overflow: hidden; display: block; }"
        , ".moz-text-html input.mj-accordion-checkbox + * .mj-accordion-ico { display: none; }"
        , "@goodbye { @gmail }"
        ]


mjAccordionElement :: Component
mjAccordionElement =
    (defComponent "mj-accordion-element" build')
        { allowedAttributes =
            [ ("background-color", "color")
            , ("border", "string")
            , ("font-family", "string")
            ]
                <> iconAttributes
        }
  where
    build' s =
        let inst =
                (defInstance s (render' inst))
                    { getStyles = \case
                        "td" -> [style "padding" "0px", ("background-color", attr s "background-color")]
                        "label" -> [style "font-size" "13px", ("font-family", attr s "font-family")]
                        "input" -> [style "display" "none"]
                        _ -> []
                    , childContext = setContext [("elementFontFamily", attr s "font-family")] (context s)
                    }
         in inst

    -- handleMissingChildren: a title or text left out is rendered empty, built
    -- outside the tree with only the inherited attributes.
    handleMissingChildren inst = do
        let s = self inst
            kids = children (node s)
            inherited = childrenAttr s
            has t = any ((== t) . tagName) kids
            standalone c =
                render . build c $
                    Self
                        { attrs = defaults c `merge` inherited
                        , props = Props 0 True True 0 0 []
                        , context = childContext inst
                        , node = element (name c) [] []
                        , registry = registry s
                        , component = c
                        , mjClassScope = mjClassScope s
                        }
        title <- if has "mj-accordion-title" then pure [] else (: []) <$> standalone mjAccordionTitle
        body <- renderChildren inst defChildOptions {childAttributes = inherited}
        text <- if has "mj-accordion-text" then pure [] else (: []) <$> standalone mjAccordionText
        pure (T.intercalate "\n" (title <> [body] <> text))

    render' inst = do
        let s = self inst
        inner <- handleMissingChildren inst
        pure $
            T.concat
                [ "<tr"
                , htmlAttributes inst [("class", maybe ANone AV (attr s "css-class"))]
                , "><td"
                , htmlAttributes inst [("style", AStyleOf "td")]
                , "><label"
                , htmlAttributes inst [("class", AV "mj-accordion-element"), ("style", AStyleOf "label")]
                , ">"
                , conditionalTag True $
                    "<input"
                        <> htmlAttributes
                            inst
                            [ ("class", AV "mj-accordion-checkbox")
                            , ("type", AV "checkbox")
                            , ("style", AStyleOf "input")
                            ]
                        <> " />"
                , "<div>"
                , inner
                , "</div></label></td></tr>"
                ]


-- | @resolveFontFamily@: the element's own @font-family@ when it set one,
-- otherwise the enclosing element's, otherwise the accordion's.
--
-- MJML checks the attributes written on the element itself, so an
-- @mj-attributes@ default does not count as the element's own choice.
resolveFontFamily :: Self -> Maybe Text
resolveFontFamily s
    | any ((== "font-family") . fst) (attributes (node s)) = attr s "font-family"
    | truthy (ctx s "elementFontFamily") = ctx s "elementFontFamily"
    | truthy (ctx s "accordionFontFamily") = ctx s "accordionFontFamily"
    | otherwise = Nothing


mjAccordionTitle :: Component
mjAccordionTitle =
    (defComponent "mj-accordion-title" build')
        { endingTag = True
        , allowedAttributes =
            [ ("background-color", "color")
            , ("color", "color")
            , ("font-size", "unit(px)")
            , ("font-family", "string")
            , ("font-weight", "string")
            ]
                <> paddingAttributes
        , defaults = [("font-size", "13px"), ("padding", "16px")]
        }
  where
    build' s =
        let inst =
                (defInstance s (pure rendered))
                    { getStyles = \case
                        "td" ->
                            [ style "width" "100%"
                            , ("background-color", attr s "background-color")
                            , ("color", attr s "color")
                            , ("font-size", attr s "font-size")
                            , ("font-family", resolveFontFamily s)
                            , ("font-weight", attr s "font-weight")
                            , ("padding", attr s "padding")
                            , ("padding-bottom", attr s "padding-bottom")
                            , ("padding-left", attr s "padding-left")
                            , ("padding-right", attr s "padding-right")
                            , ("padding-top", attr s "padding-top")
                            ]
                        "table" -> [style "width" "100%", ("border-bottom", attr s "border")]
                        "td2" ->
                            [ style "padding" "16px"
                            , ("background", attr s "background-color")
                            , ("vertical-align", attr s "icon-align")
                            ]
                        "img" ->
                            [ style "display" "none"
                            , ("width", attr s "icon-width")
                            , ("height", attr s "icon-height")
                            ]
                        _ -> []
                    }
            renderTitle =
                "<td"
                    <> htmlAttributes inst [("class", maybe ANone AV (attr s "css-class")), ("style", AStyleOf "td")]
                    <> ">"
                    <> getContent s
                    <> "</td>"
            renderIcons =
                conditionalTag True $
                    T.concat
                        [ "<td"
                        , htmlAttributes inst [("class", AV "mj-accordion-ico"), ("style", AStyleOf "td2")]
                        , "><img"
                        , htmlAttributes
                            inst
                            [ ("src", maybe ANone AV (attr s "icon-wrapped-url"))
                            , ("alt", maybe ANone AV (attr s "icon-wrapped-alt"))
                            , ("class", AV "mj-accordion-more")
                            , ("style", AStyleOf "img")
                            ]
                        , " /><img"
                        , htmlAttributes
                            inst
                            [ ("src", maybe ANone AV (attr s "icon-unwrapped-url"))
                            , ("alt", maybe ANone AV (attr s "icon-unwrapped-alt"))
                            , ("class", AV "mj-accordion-less")
                            , ("style", AStyleOf "img")
                            ]
                        , " /></td>"
                        ]
            elements =
                if attr s "icon-position" == Just "right"
                    then [renderTitle, renderIcons]
                    else [renderIcons, renderTitle]
            rendered =
                T.concat
                    [ "<div"
                    , htmlAttributes inst [("class", AV "mj-accordion-title")]
                    , "><table"
                    , htmlAttributes
                        inst
                        [("cellspacing", AV "0"), ("cellpadding", AV "0"), ("style", AStyleOf "table")]
                    , "><tbody><tr>"
                    , T.intercalate "\n" elements
                    , "</tr></tbody></table></div>"
                    ]
         in inst


mjAccordionText :: Component
mjAccordionText =
    (defComponent "mj-accordion-text" build')
        { endingTag = True
        , allowedAttributes =
            [ ("background-color", "color")
            , ("font-size", "unit(px)")
            , ("font-family", "string")
            , ("font-weight", "string")
            , ("letter-spacing", "unitWithNegative(px,em)")
            , ("line-height", "unit(px,%,)")
            , ("color", "color")
            ]
                <> paddingAttributes
        , defaults = [("font-size", "13px"), ("line-height", "1"), ("padding", "16px")]
        }
  where
    build' s =
        let inst =
                (defInstance s (pure rendered))
                    { getStyles = \case
                        "td" ->
                            [ ("background", attr s "background-color")
                            , ("font-size", attr s "font-size")
                            , ("font-family", resolveFontFamily s)
                            , ("font-weight", attr s "font-weight")
                            , ("letter-spacing", attr s "letter-spacing")
                            , ("line-height", attr s "line-height")
                            , ("color", attr s "color")
                            , ("padding", attr s "padding")
                            , ("padding-bottom", attr s "padding-bottom")
                            , ("padding-left", attr s "padding-left")
                            , ("padding-right", attr s "padding-right")
                            , ("padding-top", attr s "padding-top")
                            ]
                        "table" -> [style "width" "100%", ("border-bottom", attr s "border")]
                        _ -> []
                    }
            rendered =
                T.concat
                    [ "<div"
                    , htmlAttributes inst [("class", AV "mj-accordion-content")]
                    , "><table"
                    , htmlAttributes
                        inst
                        [("cellspacing", AV "0"), ("cellpadding", AV "0"), ("style", AStyleOf "table")]
                    , "><tbody><tr><td"
                    , htmlAttributes inst [("class", maybe ANone AV (attr s "css-class")), ("style", AStyleOf "td")]
                    , ">"
                    , getContent s
                    , "</td></tr></tbody></table></div>"
                    ]
         in inst
