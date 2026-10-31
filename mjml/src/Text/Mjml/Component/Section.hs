-- | @mj-section@, and the rendering @mj-wrapper@ shares with it.
module Text.Mjml.Component.Section (
    mjSection,
    sectionAllowedAttributes,
    sectionDefaults,
    SectionParts (..),
    sectionParts,
    setContext,
) where

import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T

import Text.Mjml.Js (parseIntJs, showNumber)
import Text.Mjml.Render


mjSection :: Component
mjSection =
    (defComponent "mj-section" build')
        { allowedAttributes = sectionAllowedAttributes
        , defaults = sectionDefaults
        }
  where
    build' s = let parts = sectionParts s (renderWrappedChildren parts) in instanceOf parts

    renderWrappedChildren parts = do
        kids <-
            renderChildren
                (instanceOf parts)
                defChildOptions
                    { renderer = \c ->
                        if rawElement (component (self c))
                            then render c
                            else do
                                body <- render c
                                pure $
                                    T.concat
                                        [ startConditionalTag
                                        , "<td"
                                        , htmlAttributes
                                            c
                                            [ ("align", maybe ANone AV (attr (self c) "align"))
                                            , ("class", AV (suffixCssClasses (attr (self c) "css-class") "outlook"))
                                            , ("style", AStyleOf "tdOutlook")
                                            ]
                                        , ">"
                                        , endConditionalTag
                                        , body
                                        , startConditionalTag
                                        , "</td>"
                                        , endConditionalTag
                                        ]
                    }
        pure (startConditionalTag <> "<tr>" <> endConditionalTag <> kids <> startConditionalTag <> "</tr>" <> endConditionalTag)


sectionAllowedAttributes :: [(Text, Text)]
sectionAllowedAttributes =
    [ ("background-color", "color")
    , ("background-url", "string")
    , ("background-repeat", "enum(repeat,no-repeat)")
    , ("background-size", "string")
    , ("background-position", "string")
    , ("background-position-x", "string")
    , ("background-position-y", "string")
    , ("border", "string")
    , ("border-bottom", "string")
    , ("border-left", "string")
    , ("border-radius", "string")
    , ("border-right", "string")
    , ("border-top", "string")
    , ("direction", "enum(ltr,rtl)")
    , ("full-width", "enum(full-width,false,)")
    , ("gutter", "unit(px,%)")
    , ("padding", "unit(px,%){1,4}")
    , ("padding-top", "unit(px,%)")
    , ("padding-bottom", "unit(px,%)")
    , ("padding-left", "unit(px,%)")
    , ("padding-right", "unit(px,%)")
    , ("text-align", "enum(left,center,right)")
    , ("text-padding", "unit(px,%){1,4}")
    ]


sectionDefaults :: Attrs
sectionDefaults =
    [ ("background-repeat", "repeat")
    , ("background-size", "auto")
    , ("background-position", "top center")
    , ("direction", "ltr")
    , ("padding", "20px 0")
    , ("text-align", "center")
    , ("text-padding", "4px 4px 4px 0")
    ]


-- | Set context keys the way a JavaScript spread does: 'Nothing' is
-- @undefined@, which removes the parent's value.
setContext :: [(Text, Maybe Text)] -> Attrs -> Attrs
setContext kvs base = foldl step base kvs
  where
    step acc (k, Just v) = merge acc [(k, v)]
    step acc (k, Nothing) = filter ((/= k) . fst) acc


-- | The methods of MJML's section class, so @mj-wrapper@ can reuse them with
-- its own children.
data SectionParts = SectionParts
    { instanceOf :: Instance
    , isFullWidth :: Bool
    , hasBackground :: Bool
    , renderBefore :: Text
    , renderAfter :: Text
    , renderSection :: R Text
    , renderWithBackground :: Text -> Text
    }


sectionParts :: Self -> R Text -> SectionParts
sectionParts s wrappedChildren = parts
  where
    parts =
        SectionParts
            { instanceOf = inst
            , isFullWidth = fullWidth
            , hasBackground = hasBg
            , renderBefore = before
            , renderAfter = after
            , renderSection = section
            , renderWithBackground = withBackground
            }

    inst =
        (defInstance s (if fullWidth then renderFull else renderSimple))
            { getStyles = stylesFor
            , childContext =
                setContext
                    [ ("containerWidth", Just (showNumber (box (boxWidths s)) <> "px"))
                    , ("gap", attr s "gap")
                    , ("gutter", attr s "gutter")
                    , ("direction", attr s "direction")
                    ]
                    (context s)
            }

    containerWidth = fromMaybe "" (ctx s "containerWidth")
    fullWidth = attr s "full-width" == Just "full-width"
    hasBorderRadius = case attr s "border-radius" of
        Just v -> not (T.null v)
        Nothing -> False
    isFirstSection = index (props s) == 0
    hasBg = isJust (attr s "background-url")
    hasGap = case ctx s "gap" of
        Just g -> not (T.null g)
        Nothing -> False
    gap = ctx s "gap"

    background
        | truthy (attr s "background-url") =
            [ style "background" backgroundValue
            , style "background-position" backgroundString
            , ("background-repeat", attr s "background-repeat")
            , ("background-size", attr s "background-size")
            ]
        | otherwise =
            [ ("background", attr s "background-color")
            , ("background-color", attr s "background-color")
            ]

    stylesFor = \case
        "tableFullwidth" -> (if fullWidth then background else []) `merge` [style "width" "100%"]
        "table" ->
            (if fullWidth then [] else background)
                `merge` [style "width" "100%"]
                `merge` [style "border-collapse" "separate" | hasBorderRadius]
        "td" ->
            [ ("border", attr s "border")
            , ("border-bottom", attr s "border-bottom")
            , ("border-left", attr s "border-left")
            , ("border-right", attr s "border-right")
            , ("border-top", attr s "border-top")
            , ("border-radius", attr s "border-radius")
            , ("direction", attr s "direction")
            , style "font-size" "0px"
            , ("padding", attr s "padding")
            , ("padding-bottom", attr s "padding-bottom")
            , ("padding-left", attr s "padding-left")
            , ("padding-right", attr s "padding-right")
            , ("padding-top", attr s "padding-top")
            , ("text-align", attr s "text-align")
            ]
        "div" ->
            (if fullWidth then [] else background)
                `merge` [ style "margin" "0px auto"
                        , style "max-width" containerWidth
                        , ("border-radius", attr s "border-radius")
                        ]
                `merge` [style "overflow" "hidden" | hasBorderRadius]
                `merge` [("margin-top", if isFirstSection then Nothing else gap)]
        "innerDiv" -> [style "line-height" "0", style "font-size" "0"]
        _ -> []

    backgroundValue =
        T.intercalate " " . filter (not . T.null) $
            [fromMaybe "" (attr s "background-color")]
                <> if hasBg
                    then
                        [ "url('" <> fromMaybe "" (attr s "background-url") <> "')"
                        , backgroundString
                        , "/ " <> fromMaybe "" (attr s "background-size")
                        , fromMaybe "" (attr s "background-repeat")
                        ]
                    else []

    backgroundString = let (x, y) = backgroundPosition in x <> " " <> y

    backgroundPosition =
        let (x, y) = parseBackgroundPosition
         in (orElse (attr s "background-position-x") x, orElse (attr s "background-position-y") y)
      where
        orElse v d = case v of
            Just t | not (T.null t) -> t
            _ -> d

    parseBackgroundPosition = case T.splitOn " " (fromMaybe "" (attr s "background-position")) of
        [v]
            | v `elem` ["top", "bottom"] -> ("center", v)
            | otherwise -> (v, "center")
        [v1, v2]
            | v1 `elem` ["top", "bottom"] || (v1 == "center" && v2 `elem` ["left", "right"]) -> (v2, v1)
            | otherwise -> (v1, v2)
        _ -> ("center", "top")

    before =
        T.concat
            [ startConditionalTag
            , "<table"
            , htmlAttributes inst $
                [ ("align", AV "center")
                , ("border", AV "0")
                , ("cellpadding", AV "0")
                , ("cellspacing", AV "0")
                , ("class", AV (suffixCssClasses (attr s "css-class") "outlook"))
                , ("role", AV "presentation")
                , ("style", AStyles [style "width" containerWidth, ("padding-top", if isFirstSection then Nothing else gap)])
                , ("width", AV (showNumber (parseIntJs containerWidth)))
                ]
                    <> [ ("bgcolor", AV c)
                       | not hasGap
                       , Just c <- [attr s "background-color"]
                       , truthy (Just c)
                       ]
            , "><tr><td style=\"line-height:0px;font-size:0px;mso-line-height-rule:exactly;\">"
            , endConditionalTag
            ]

    after = startConditionalTag <> "</td></tr></table>" <> endConditionalTag

    section = do
        kids <- wrappedChildren
        pure $
            T.concat
                [ "<div"
                , htmlAttributes
                    inst
                    [ ("class", if fullWidth then ANone else maybe ANone AV (attr s "css-class"))
                    , ("style", AStyleOf "div")
                    ]
                , ">"
                , if hasBg then "<div" <> htmlAttributes inst [("style", AStyleOf "innerDiv")] <> ">" else ""
                , "<table"
                , htmlAttributes
                    inst
                    [ ("align", AV "center")
                    , ("background", if fullWidth then ANone else maybe ANone AV (attr s "background-url"))
                    , ("border", AV "0")
                    , ("cellpadding", AV "0")
                    , ("cellspacing", AV "0")
                    , ("role", AV "presentation")
                    , ("style", AStyleOf "table")
                    ]
                , "><tbody><tr><td"
                , htmlAttributes inst [("style", AStyleOf "td")]
                , ">"
                , startConditionalTag
                , "<table role=\"presentation\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\">"
                , endConditionalTag
                , kids
                , startConditionalTag
                , "</table>"
                , endConditionalTag
                , "</td></tr></tbody></table>"
                , if hasBg then "</div>" else ""
                , "</div>"
                ]

    withBackground body =
        let isPercentage t = case T.breakOnEnd "%" t of
                (pre, "") | Just (num, '%') <- T.unsnoc pre -> isDecimal num
                _ -> False
            isDecimal t = case T.splitOn "." t of
                [a] -> not (T.null a) && T.all (`elem` ['0' .. '9']) a
                [a, b] -> not (T.null a) && not (T.null b) && T.all (`elem` ['0' .. '9']) (a <> b)
                _ -> False
            (rawX, rawY) = backgroundPosition
            bgPosX = case rawX of
                "left" -> "0%"
                "center" -> "50%"
                "right" -> "100%"
                v -> if isPercentage v then v else "50%"
            bgPosY = case rawY of
                "top" -> "0%"
                "center" -> "50%"
                "bottom" -> "100%"
                v -> if isPercentage v then v else "0%"
            repeat' = attr s "background-repeat" == Just "repeat"
            coordinate isX =
                let pos = if isX then bgPosX else bgPosY
                 in if isPercentage pos
                        then
                            let decimal = parseIntJs (T.dropEnd 1 pos) / 100
                                v = if repeat' then decimal else (-50 + decimal * 100) / 100
                             in (showNumber v, showNumber v)
                        else
                            if repeat'
                                then if isX then ("0.5", "0.5") else ("0", "0")
                                else if isX then ("0", "0") else ("-0.5", "-0.5")
            size = fromMaybe "" (attr s "background-size")
            vSize
                | size == "cover" || size == "contain" =
                    [("size", AV "1,1"), ("aspect", AV (if size == "cover" then "atleast" else "atmost"))]
                | size /= "auto" = case T.splitOn " " size of
                    [_] -> [("size", AV size), ("aspect", AV "atmost")]
                    ps -> [("size", AV (T.intercalate "," ps))]
                | otherwise = []
            ((vOriginX, vPosX), (vOriginY, vPosY))
                | size == "auto" = (("0.5", "0.5"), ("0", "0"))
                | otherwise = (coordinate True, coordinate False)
            vmlType
                | size == "auto" = "tile"
                | attr s "background-repeat" == Just "no-repeat" = "frame"
                | otherwise = "tile"
         in T.concat
                [ startConditionalTag
                , "<v:rect"
                , htmlAttributes
                    inst
                    [ ("style", AStyles (if fullWidth then [style "mso-width-percent" "1000"] else [style "width" containerWidth]))
                    , ("xmlns:v", AV "urn:schemas-microsoft-com:vml")
                    , ("fill", AV "true")
                    , ("stroke", AV "false")
                    ]
                , "><v:fill"
                , htmlAttributes inst $
                    [ ("origin", AV (vOriginX <> ", " <> vOriginY))
                    , ("position", AV (vPosX <> ", " <> vPosY))
                    , ("src", maybe ANone AV (attr s "background-url"))
                    , ("color", maybe ANone AV (attr s "background-color"))
                    , ("type", AV vmlType)
                    ]
                        <> vSize
                , " /><v:textbox style=\"mso-fit-shape-to-text:true\" inset=\"0,0,0,0\">"
                , endConditionalTag
                , body
                , startConditionalTag
                , "</v:textbox></v:rect>"
                , endConditionalTag
                ]

    renderFull = do
        sec <- section
        let inner = before <> sec <> after
            body = if hasBg then withBackground inner else inner
        pure $
            T.concat
                [ "<table"
                , htmlAttributes
                    inst
                    [ ("align", AV "center")
                    , ("class", maybe ANone AV (attr s "css-class"))
                    , ("background", maybe ANone AV (attr s "background-url"))
                    , ("border", AV "0")
                    , ("cellpadding", AV "0")
                    , ("cellspacing", AV "0")
                    , ("role", AV "presentation")
                    , ("style", AStyleOf "tableFullwidth")
                    ]
                , "><tbody><tr><td>"
                , body
                , "</td></tr></tbody></table>"
                ]

    renderSimple = do
        sec <- section
        pure (before <> (if hasBg then withBackground sec else sec) <> after)
