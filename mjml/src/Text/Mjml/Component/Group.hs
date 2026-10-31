-- | @mj-group@: columns that stay side by side on mobile.
module Text.Mjml.Component.Group (
    mjGroup,
) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T

import Text.Mjml.Component.Section (setContext)
import Text.Mjml.Js (parseFloatJs, parseIntJs, showNumber)
import Text.Mjml.Node (Node (..))
import Text.Mjml.Render


mjGroup :: Component
mjGroup =
    (defComponent "mj-group" build')
        { allowedAttributes =
            [ ("background-color", "color")
            , ("direction", "enum(ltr,rtl)")
            , ("vertical-align", "enum(top,bottom,middle)")
            , ("width", "unit(px,%)")
            ]
        , defaults = [("direction", "ltr")]
        }


build' :: Self -> Instance
build' s = inst
  where
    p = props s
    inst =
        (defInstance s renderGroup)
            { getStyles = stylesFor
            , childContext = childCtx
            }

    containerWidth = fromMaybe "" (ctx s "containerWidth")

    groupWidth =
        let paddingSize = shorthandValue s "padding" "left" + shorthandValue s "padding" "right"
            width = case attr s "width" of
                Just w | truthy (Just w) -> w
                _ -> showNumber (parseFloatJs containerWidth / fromIntegral (nonRawSiblings p)) <> "px"
            (parsed, unit) = widthParser False width
         in if unit == "%"
                then showNumber (parseFloatJs containerWidth * parsed / 100 - paddingSize) <> "px"
                else showNumber (parsed - paddingSize) <> "px"

    childCtx =
        setContext
            [ ("containerWidth", Just groupWidth)
            , ("nonRawSiblings", Just (T.pack (show (length (children (node s))))))
            , ("isInGroup", Just "true")
            , ("direction", attr s "direction")
            ]
            (context s)

    stylesFor = \case
        "div" ->
            [ style "font-size" "0"
            , style "line-height" "0"
            , style "text-align" "left"
            , style "display" "inline-block"
            , style "width" "100%"
            , ("direction", attr s "direction")
            , ("vertical-align", attr s "vertical-align")
            , ("background-color", attr s "background-color")
            ]
        "tdOutlook" ->
            [ ("vertical-align", attr s "vertical-align")
            , style "width" widthAsPixel
            ]
        _ -> []

    parsedWidth =
        let width = case attr s "width" of
                Just w | truthy (Just w) -> w
                _ -> showNumber (100 / fromIntegral (nonRawSiblings p)) <> "%"
         in widthParser False width

    widthAsPixel =
        let (v, u) = parsedWidth
            (parsed, unit) = widthParser False (showNumber v <> u)
         in if unit == "%"
                then showNumber (parseFloatJs containerWidth * parsed / 100) <> "px"
                else showNumber parsed <> "px"

    columnClass = do
        let (parsed, unit) = parsedWidth
            nb = showNumber (parseIntJs (showNumber parsed))
            className = case unit of
                "%" -> "mj-column-per-" <> nb
                _ -> "mj-column-px-" <> nb
        addMediaQuery className (Just (showNumber parsed, unit)) Nothing
        pure className

    -- The Outlook cell width for a child: a column's pixel width, or its
    -- width attribute.
    elementWidth = \case
        Nothing -> showNumber (parseIntJs containerWidth / parseIntJs (T.pack (show (nonRawSiblings p)))) <> "px"
        Just width ->
            let (parsed, unit) = widthParser False width
             in if unit == "%"
                    -- MJML divides by the group width string, which is not a number.
                    then showNumber (100 * parsed / (0 / 0)) <> "px"
                    else showNumber parsed <> unit

    childWidth c
        | name (component (self c)) == "mj-column" = lookup "width" (getStyles c "tdOutlook") >>= id
        | otherwise = attr (self c) "width"

    renderGroup = do
        cls <- columnClass
        let classesName =
                T.unwords $
                    [cls, "mj-outlook-group-fix"]
                        <> [c | Just c <- [attr s "css-class"], truthy (Just c)]
        kids <-
            renderChildren
                inst
                defChildOptions
                    { childAttributes = [("mobileWidth", "mobileWidth")]
                    , renderer = \c ->
                        if rawElement (component (self c))
                            then render c
                            else do
                                body <- render c
                                let width = case childWidth c of
                                        Just w | not (T.null w) -> Just w
                                        _ -> Nothing
                                pure $
                                    T.concat
                                        [ startConditionalTag
                                        , "<td"
                                        , htmlAttributes
                                            c
                                            [ ( "style"
                                              , AStyles
                                                    [ ("align", attr (self c) "align")
                                                    , ("vertical-align", attr (self c) "vertical-align")
                                                    , style "width" (elementWidth width)
                                                    ]
                                              )
                                            ]
                                        , ">"
                                        , endConditionalTag
                                        , body
                                        , startConditionalTag
                                        , "</td>"
                                        , endConditionalTag
                                        ]
                    }
        let bgcolor = case attr s "background-color" of
                Just "none" -> ANone
                v -> maybe ANone AV v
        pure $
            T.concat
                [ "<div"
                , htmlAttributes inst [("class", AV classesName), ("style", AStyleOf "div")]
                , ">"
                , startConditionalTag
                , "<table"
                , htmlAttributes
                    inst
                    [ ("bgcolor", bgcolor)
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
                , "</div>"
                ]
