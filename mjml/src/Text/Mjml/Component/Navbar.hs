-- | @mj-navbar@ and @mj-navbar-link@.
module Text.Mjml.Component.Navbar (
    mjNavbar,
    mjNavbarLink,
) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Text.Mjml.Render


mjNavbar :: Component
mjNavbar =
    (defComponent "mj-navbar" buildNavbar)
        { allowedAttributes =
            [ ("align", "enum(left,center,right)")
            , ("base-url", "string")
            , ("hamburger", "string")
            , ("ico-align", "enum(left,center,right)")
            , ("ico-open", "string")
            , ("ico-close", "string")
            , ("ico-color", "color")
            , ("ico-font-size", "unit(px,%)")
            , ("ico-font-family", "string")
            , ("ico-text-transform", "string")
            , ("ico-padding", "unit(px,%){1,4}")
            , ("ico-padding-left", "unit(px,%)")
            , ("ico-padding-top", "unit(px,%)")
            , ("ico-padding-right", "unit(px,%)")
            , ("ico-padding-bottom", "unit(px,%)")
            , ("padding", "unit(px,%){1,4}")
            , ("padding-left", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-bottom", "unit(px,%)")
            , ("ico-text-decoration", "string")
            , ("ico-line-height", "unit(px,%,)")
            ]
        , -- MJML also lists null defaults for base-url and hamburger.
          defaults =
            [ ("align", "center")
            , ("ico-align", "center")
            , ("ico-open", "&#9776;")
            , ("ico-close", "&#8855;")
            , ("ico-color", "#000000")
            , ("ico-font-size", "30px")
            , ("ico-font-family", "Ubuntu, Helvetica, Arial, sans-serif")
            , ("ico-text-transform", "uppercase")
            , ("ico-padding", "10px")
            , ("ico-text-decoration", "none")
            , ("ico-line-height", "30px")
            ]
        , componentHeadStyle = Just navbarHeadStyle
        }


navbarHeadStyle :: Text -> Text
navbarHeadStyle bp =
    mconcat
        [ "noinput.mj-menu-checkbox { display:block!important; max-height:none!important; visibility:visible!important; }\n"
        , "@media only screen and (max-width:"
        , makeLowerBreakpoint bp
        , ") {\n"
        , ".mj-menu-checkbox[type=\"checkbox\"] ~ .mj-inline-links { display:none!important; }\n"
        , ".mj-menu-checkbox[type=\"checkbox\"]:checked ~ .mj-inline-links,\n"
        , ".mj-menu-checkbox[type=\"checkbox\"] ~ .mj-menu-trigger { display:block!important; max-width:none!important; max-height:none!important; font-size:inherit!important; }\n"
        , ".mj-menu-checkbox[type=\"checkbox\"] ~ .mj-inline-links > a { display:block!important; }\n"
        , ".mj-menu-checkbox[type=\"checkbox\"]:checked ~ .mj-menu-trigger .mj-menu-icon-close { display:block!important; }\n"
        , ".mj-menu-checkbox[type=\"checkbox\"]:checked ~ .mj-menu-trigger .mj-menu-icon-open { display:none!important; }\n"
        , "}\n"
        ]


-- | @makeLowerBreakpoint@: one pixel below the first number in the
-- breakpoint.
makeLowerBreakpoint :: Text -> Text
makeLowerBreakpoint bp =
    let digits = T.takeWhile isDigit (T.dropWhile (not . isDigit) bp)
     in if T.null digits
            then bp
            else T.pack (show (read (T.unpack digits) - 1 :: Integer)) <> "px"


buildNavbar :: Self -> Instance
buildNavbar s = inst
  where
    inst = (defInstance s renderNavbar) {getStyles = stylesFor}

    stylesFor = \case
        "div" -> [("align", attr s "align"), style "width" "100%"]
        "label" ->
            [ style "display" "block"
            , style "cursor" "pointer"
            , style "mso-hide" "all"
            , style "-moz-user-select" "none"
            , style "user-select" "none"
            , ("color", attr s "ico-color")
            , ("font-size", attr s "ico-font-size")
            , ("font-family", attr s "ico-font-family")
            , ("text-transform", attr s "ico-text-transform")
            , ("text-decoration", attr s "ico-text-decoration")
            , ("line-height", attr s "ico-line-height")
            , ("padding", attr s "ico-padding")
            , ("padding-top", attr s "ico-padding-top")
            , ("padding-right", attr s "ico-padding-right")
            , ("padding-bottom", attr s "ico-padding-bottom")
            , ("padding-left", attr s "ico-padding-left")
            ]
        "trigger" ->
            [ style "display" "none"
            , style "max-height" "0px"
            , style "max-width" "0px"
            , style "font-size" "0px"
            , style "overflow" "hidden"
            ]
        "icoOpen" -> [style "mso-hide" "all"]
        "icoClose" -> [style "display" "none", style "mso-hide" "all"]
        _ -> []

    renderHamburger = do
        labelKey <- nextId
        pure $
            mconcat
                [ msoConditionalTag True $
                    "<input type=\"checkbox\" id=\""
                        <> fromText labelKey
                        <> "\" class=\"mj-menu-checkbox\" style=\"display:none !important; max-height:0; visibility:hidden;\" />"
                , "<div"
                , htmlAttributes inst [("class", AV "mj-menu-trigger"), ("style", AStyleOf "trigger")]
                , "><label"
                , htmlAttributes
                    inst
                    [ ("for", AV labelKey)
                    , ("class", AV "mj-menu-label")
                    , ("style", AStyleOf "label")
                    , ("align", maybe ANone AV (attr s "ico-align"))
                    ]
                , "><span"
                , htmlAttributes inst [("class", AV "mj-menu-icon-open"), ("style", AStyleOf "icoOpen")]
                , ">"
                , fromText (fromMaybe "" (attr s "ico-open"))
                , "</span><span"
                , htmlAttributes inst [("class", AV "mj-menu-icon-close"), ("style", AStyleOf "icoClose")]
                , ">"
                , fromText (fromMaybe "" (attr s "ico-close"))
                , "</span></label></div>"
                ]

    renderNavbar = do
        hamburger <- if attr s "hamburger" == Just "hamburger" then renderHamburger else pure ""
        kids <-
            renderChildren
                inst
                defChildOptions {childAttributes = [("navbarBaseUrl", v) | Just v <- [attr s "base-url"]]}
        pure $
            mconcat
                [ hamburger
                , -- MJML passes the result of htmlAttributes('div') as the style,
                  -- which resolves to no styles at all.
                  "<div"
                , htmlAttributes inst [("class", AV "mj-inline-links"), ("style", AV "")]
                , ">"
                , conditionalTag False $
                    "<table role=\"presentation\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" align=\""
                        <> fromText (fromMaybe "" (attr s "align"))
                        <> "\"><tr>"
                , kids
                , conditionalTag False "</tr></table>"
                , "</div>"
                ]


mjNavbarLink :: Component
mjNavbarLink =
    (defComponent "mj-navbar-link" buildLink)
        { endingTag = True
        , allowedAttributes =
            [ ("color", "color")
            , ("font-family", "string")
            , ("font-size", "unit(px)")
            , ("font-style", "string")
            , ("font-weight", "string")
            , ("href", "string")
            , ("name", "string")
            , ("target", "string")
            , ("rel", "string")
            , ("letter-spacing", "unitWithNegative(px,em)")
            , ("line-height", "unit(px,%,)")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("padding", "unit(px,%){1,4}")
            , ("text-decoration", "string")
            , ("text-transform", "string")
            ]
        , defaults =
            [ ("color", "#000000")
            , ("font-family", "Ubuntu, Helvetica, Arial, sans-serif")
            , ("font-size", "13px")
            , ("font-weight", "normal")
            , ("line-height", "22px")
            , ("padding", "15px 10px")
            , ("target", "_blank")
            , ("text-decoration", "none")
            , ("text-transform", "uppercase")
            ]
        }


buildLink :: Self -> Instance
buildLink s = inst
  where
    inst = (defInstance s (pure rendered)) {getStyles = stylesFor}

    paddings =
        [ ("padding", attr s "padding")
        , ("padding-top", attr s "padding-top")
        , ("padding-left", attr s "padding-left")
        , ("padding-right", attr s "padding-right")
        , ("padding-bottom", attr s "padding-bottom")
        ]

    stylesFor = \case
        "a" ->
            [ style "display" "inline-block"
            , ("color", attr s "color")
            , ("font-family", attr s "font-family")
            , ("font-size", attr s "font-size")
            , ("font-style", attr s "font-style")
            , ("font-weight", attr s "font-weight")
            , ("letter-spacing", attr s "letter-spacing")
            , ("line-height", attr s "line-height")
            , ("text-decoration", attr s "text-decoration")
            , ("text-transform", attr s "text-transform")
            ]
                <> paddings
        "td" -> paddings
        _ -> []

    link = case attr s "navbarBaseUrl" of
        Just base | truthy (Just base) -> Just (base <> fromMaybe "undefined" (attr s "href"))
        _ -> attr s "href"

    cssClass = case attr s "css-class" of
        Just c | truthy (Just c) -> " " <> c
        _ -> ""

    renderContent =
        mconcat
            [ "<a"
            , htmlAttributes
                inst
                [ ("class", AV ("mj-link" <> cssClass))
                , ("href", maybe ANone AV link)
                , ("rel", maybe ANone AV (attr s "rel"))
                , ("target", maybe ANone AV (attr s "target"))
                , ("name", maybe ANone AV (attr s "name"))
                , ("style", AStyleOf "a")
                ]
            , ">"
            , fromText (getContent s)
            , "</a>"
            ]

    rendered =
        mconcat
            [ conditionalTag False $
                "<td"
                    <> htmlAttributes
                        inst
                        [ ("style", AStyleOf "td")
                        , ("class", AV (suffixCssClasses (attr s "css-class") "outlook"))
                        ]
                    <> ">"
            , renderContent
            , conditionalTag False "</td>"
            ]
