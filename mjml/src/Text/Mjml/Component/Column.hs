-- | @mj-column@.
module Text.Mjml.Component.Column (
    mjColumn,
) where

import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T

import Text.Mjml.Component.Section (setContext)
import Text.Mjml.Js (parseFloatJs, parseIntJs, roundJs, showNumber, toFixedNumber)
import Text.Mjml.Render


mjColumn :: Component
mjColumn =
    (defComponent "mj-column" build')
        { allowedAttributes =
            [ ("background-color", "color")
            , ("border", "string")
            , ("border-bottom", "string")
            , ("border-left", "string")
            , ("border-radius", "string")
            , ("border-right", "string")
            , ("border-top", "string")
            , ("direction", "enum(ltr,rtl)")
            , ("inner-background-color", "color")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("inner-border", "string")
            , ("inner-border-bottom", "string")
            , ("inner-border-left", "string")
            , ("inner-border-radius", "string")
            , ("inner-border-right", "string")
            , ("inner-border-top", "string")
            , ("padding", "unit(px,%){1,4}")
            , ("vertical-align", "enum(top,bottom,middle)")
            , ("width", "unit(px,%)")
            ]
        , defaults = [("direction", "ltr"), ("vertical-align", "top")]
        }


normalizeUnitValue :: Double -> Double
normalizeUnitValue = toFixedNumber 6


normalizePxValue :: Double -> Double
normalizePxValue = roundJs


formatPadding :: Double -> Double -> Double -> Double -> Text -> Text
formatPadding t r b l unit
    | unit == "px" = T.unwords [showNumber (normalizePxValue x) <> "px" | x <- [t, r, b, l]]
    | otherwise = T.unwords [showNumber (normalizeUnitValue x) <> unit | x <- [t, r, b, l]]


build' :: Self -> Instance
build' s = inst
  where
    p = props s
    inst =
        (defInstance s renderColumnOuter)
            { getStyles = stylesFor
            , childContext = setContext [("containerWidth", Just childWidth)] (context s)
            }

    containerWidth = fromMaybe "" (ctx s "containerWidth")
    isInGroup = truthy (ctx s "isInGroup")
    direction = ctx s "direction"

    -- getChildContext
    childWidth =
        let bw = boxWidths s
            innerBorders = borderValue s (Just "left") "inner-border" + borderValue s (Just "right") "inner-border"
            allPaddings = paddings bw + borders bw + innerBorders
            width = case attr s "width" of
                Just w | truthy (Just w) -> w
                _ -> showNumber (parseFloatJs containerWidth / fromIntegral (nonRawSiblings p)) <> "px"
            (parsed, unit) = widthParser False width
         in if unit == "%"
                then showNumber (parseFloatJs containerWidth * parsed / 100 - allPaddings) <> "px"
                else showNumber (parsed - allPaddings) <> "px"

    hasBorderRadius = nonEmpty (attr s "border-radius")
    hasInnerBorderRadius = nonEmpty (attr s "inner-border-radius")
    nonEmpty = \case
        Just v -> not (T.null v)
        Nothing -> False

    tableStyle =
        [ ("background-color", attr s "background-color")
        , ("border", attr s "border")
        , ("border-bottom", attr s "border-bottom")
        , ("border-left", attr s "border-left")
        , ("border-radius", attr s "border-radius")
        , ("border-right", attr s "border-right")
        , ("border-top", attr s "border-top")
        , ("vertical-align", attr s "vertical-align")
        ]
            `merge` [style "border-collapse" "separate" | hasBorderRadius]

    stylesFor = \case
        "div" ->
            [ style "font-size" "0px"
            , style "text-align" "left"
            , ("direction", attr s "direction")
            , style "display" "inline-block"
            , ("vertical-align", attr s "vertical-align")
            , style "width" mobileWidth
            ]
                `merge` mobileGutterStyles
        "table" ->
            ( if hasGutter
                then
                    [ ("background-color", attr s "inner-background-color")
                    , ("border", attr s "inner-border")
                    , ("border-bottom", attr s "inner-border-bottom")
                    , ("border-left", attr s "inner-border-left")
                    , ("border-radius", attr s "inner-border-radius")
                    , ("border-right", attr s "inner-border-right")
                    , ("border-top", attr s "inner-border-top")
                    ]
                else tableStyle
            )
                `merge` [style "border-collapse" "separate" | hasInnerBorderRadius]
        "tdOutlook" ->
            [("vertical-align", attr s "vertical-align"), style "width" widthAsPixel]
                `merge` outlookGutterStyles
        "gutter" ->
            tableStyle
                `merge` [ ("padding", attr s "padding")
                        , ("padding-top", attr s "padding-top")
                        , ("padding-right", attr s "padding-right")
                        , ("padding-bottom", attr s "padding-bottom")
                        , ("padding-left", attr s "padding-left")
                        ]
        _ -> []

    mobileWidth
        | attr s "mobileWidth" /= Just "mobileWidth" = "100%"
        | isInGroup && hasColumnGutter =
            let (parsed, unit) = desktopWidth
             in if unit == "%"
                    then showNumber parsed <> "%"
                    else showNumber (normalizeUnitValue (parsed / parseIntJs containerWidth * 100)) <> "%"
        | otherwise = case attr s "width" of
            Nothing -> showNumber (fromInteger (truncate (100 / fromIntegral (nonRawSiblings p) :: Double))) <> "%"
            Just width ->
                let (parsed, unit) = widthParser False width
                 in if unit == "%"
                        then width
                        else showNumber (normalizeUnitValue (parsed / parseIntJs containerWidth * 100)) <> "%"

    widthAsPixel =
        let (parsed, unit) = widthParser False (parsedWidthString)
         in if unit == "%"
                then showNumber (normalizePxValue (parseFloatJs containerWidth * parsed / 100)) <> "px"
                else showNumber (normalizePxValue parsed) <> "px"

    parsedWidth =
        let width = case attr s "width" of
                Just w | truthy (Just w) -> w
                _ -> showNumber (100 / fromIntegral (nonRawSiblings p)) <> "%"
         in widthParser False width

    parsedWidthString = let (v, u) = parsedWidth in showNumber v <> u

    hasColumnGutter = nonEmpty (ctx s "gutter")

    desktopWidth =
        let (parsed, unit) = parsedWidth
         in if not hasColumnGutter
                then (if unit == "px" then normalizePxValue parsed else parsed, unit)
                else
                    let gutter = normalizedGutterValue unit
                        siblings = fromIntegral (sibling p)
                        reduction = gutter * (siblings - 1) / siblings
                        reduced = max 0 (normalizeUnitValue (parsed - reduction))
                     in if unit == "px"
                            then
                                let floorWidth = fromInteger (floor reduced)
                                    fractional = reduced - floorWidth
                                    extraPixels = max 0 (min siblings (roundJs (siblings * fractional)))
                                 in (floorWidth + (if fromIntegral (index p) < extraPixels then 1 else 0), unit)
                            else (reduced, unit)

    normalizedGutterValue targetUnit = case ctx s "gutter" of
        Just g
            | truthy (Just g) ->
                let (parsed, unit) = widthParser False g
                 in if
                        | unit == targetUnit -> parsed
                        | targetUnit == "%" && unit == "px" -> parsed / parseFloatJs containerWidth * 100
                        | targetUnit == "px" && unit == "%" -> parseFloatJs containerWidth * parsed / 100
                        | otherwise -> parsed
        _ -> 0

    desktopUnit = snd parsedWidth

    desktopGutterClassName =
        let gutterUnit = desktopUnit
            gutter = normalizedGutterValue gutterUnit
            unitToken = if gutterUnit == "%" then "per" else gutterUnit
            directionToken = if direction == Just "rtl" then "-rtl" else ""
            normalized = if gutterUnit == "px" then normalizePxValue gutter else gutter
            gutterToken = T.replace "." "-" (showNumber (normalizeUnitValue normalized))
         in mconcat
                [ "mj-column-gutter-"
                , T.pack (show (sibling p))
                , "-"
                , T.pack (show (index p + 1))
                , "-"
                , unitToken
                , "-"
                , gutterToken
                , directionToken
                ]

    desktopPaddingValues unit =
        let gutter = normalizedGutterValue unit
            normalized = if unit == "px" then normalizePxValue gutter else gutter
            isPx = unit == "px"
            halfLeading = if isPx then fromInteger (ceiling (normalized / 2)) else normalized / 2
            halfTrailing = if isPx then fromInteger (floor (normalized / 2)) else normalized / 2
         in if
                | sibling p == 1 -> (0, 0, 0, 0)
                | direction == Just "rtl" ->
                    (0, if isFirst p then 0 else halfTrailing, 0, if isLast p then 0 else halfLeading)
                | otherwise ->
                    (0, if isLast p then 0 else halfLeading, 0, if isFirst p then 0 else halfTrailing)

    desktopPadding =
        let (t, r, b, l) = desktopPaddingValues desktopUnit in formatPadding t r b l desktopUnit

    mobilePadding =
        let half = normalizedGutterValue "%" / 2
         in formatPadding
                (if isFirst p then 0 else half)
                0
                (if isLast p then 0 else half)
                0
                "%"

    mobileGutterStyles
        | not hasColumnGutter = []
        | isInGroup = [style "padding" desktopPadding]
        | otherwise = [style "padding" mobilePadding]

    outlookGutterStyles
        | not hasColumnGutter = []
        | otherwise = let (t, r, b, l) = desktopPaddingValues "px" in [style "padding" (formatPadding t r b l "px")]

    hasGutter = any (isJust . attr s) ["padding", "padding-bottom", "padding-left", "padding-right", "padding-top"]

    columnClass = do
        let (parsed, unit) = if hasColumnGutter then desktopWidth else parsedWidth
            normalized = if unit == "px" then normalizePxValue parsed else parsed
            classNb = T.replace "." "-" (showNumber normalized)
            className = case unit of
                "%" -> "mj-column-per-" <> classNb
                _ -> "mj-column-px-" <> classNb
        addMediaQuery className (Just (showNumber normalized, unit)) Nothing
        if hasColumnGutter && not isInGroup
            then addMediaQuery desktopGutterClassName Nothing (Just desktopPadding)
            else pure ()
        pure className

    renderColumn = do
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
                                    mconcat
                                        [ "<tr><td"
                                        , htmlAttributes
                                            c
                                            [ ("align", maybe ANone AV (attr cs "align"))
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
        pure $
            mconcat
                [ "<table"
                , htmlAttributes
                    inst
                    [ ("border", AV "0")
                    , ("cellpadding", AV "0")
                    , ("cellspacing", AV "0")
                    , ("role", AV "presentation")
                    , ("style", AStyleOf "table")
                    , ("width", AV "100%")
                    ]
                , "><tbody>"
                , kids
                , "</tbody></table>"
                ]

    renderGutter = do
        col <- renderColumn
        pure $
            mconcat
                [ "<table"
                , htmlAttributes inst $
                    [ ("border", AV "0")
                    , ("cellpadding", AV "0")
                    , ("cellspacing", AV "0")
                    , ("role", AV "presentation")
                    , ("width", AV "100%")
                    ]
                        <> [("style", AStyles [style "border-collapse" "separate"]) | hasBorderRadius]
                , "><tbody><tr><td"
                , htmlAttributes inst [("style", AStyleOf "gutter")]
                , ">"
                , col
                , "</td></tr></tbody></table>"
                ]

    renderColumnOuter = do
        defaultClass <- columnClass
        let classesName =
                T.unwords $
                    [defaultClass]
                        <> [desktopGutterClassName | hasColumnGutter]
                        <> ["mj-outlook-group-fix"]
                        <> [c | Just c <- [attr s "css-class"], truthy (Just c)]
        body <- if hasGutter then renderGutter else renderColumn
        pure $
            "<div"
                <> htmlAttributes inst [("class", AV classesName), ("style", AStyleOf "div")]
                <> ">"
                <> body
                <> "</div>"
