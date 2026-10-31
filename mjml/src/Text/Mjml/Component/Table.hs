-- | @mj-table@.
module Text.Mjml.Component.Table (
    mjTable,
) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T

import Text.Mjml.Js (parseFloatJs, showNumber)
import Text.Mjml.Render


mjTable :: Component
mjTable =
    (defComponent "mj-table" build')
        { endingTag = True
        , allowedAttributes =
            [ ("align", "enum(left,right,center)")
            , ("border", "string")
            , ("cellpadding", "integer")
            , ("cellspacing", "integer")
            , ("container-background-color", "color")
            , ("color", "color")
            , ("font-family", "string")
            , ("font-size", "unit(px)")
            , ("font-weight", "string")
            , ("line-height", "unit(px,%,)")
            , ("padding-bottom", "unit(px,%)")
            , ("padding-left", "unit(px,%)")
            , ("padding-right", "unit(px,%)")
            , ("padding-top", "unit(px,%)")
            , ("padding", "unit(px,%){1,4}")
            , ("role", "enum(none,presentation)")
            , ("table-layout", "enum(auto,fixed,initial,inherit)")
            , ("vertical-align", "enum(top,bottom,middle)")
            , ("width", "unit(px,%,auto)")
            ]
        , defaults =
            [ ("align", "left")
            , ("border", "none")
            , ("cellpadding", "0")
            , ("cellspacing", "0")
            , ("color", "#000000")
            , ("font-family", "Ubuntu, Helvetica, Arial, sans-serif")
            , ("font-size", "13px")
            , ("line-height", "22px")
            , ("padding", "10px 25px")
            , ("table-layout", "auto")
            , ("width", "100%")
            ]
        }


build' :: Self -> Instance
build' s = inst
  where
    inst = (defInstance s (pure rendered)) {getStyles = stylesFor}

    hasCellspacing =
        let raw = fromMaybe "undefined" (attr s "cellspacing")
            n = parseFloatJs (T.filter (\c -> isDigit c || c == '.') raw)
         in not (isNaN n) && n > 0

    stylesFor = \case
        "table" ->
            [ ("color", attr s "color")
            , ("font-family", attr s "font-family")
            , ("font-size", attr s "font-size")
            , ("line-height", attr s "line-height")
            , ("table-layout", attr s "table-layout")
            , ("width", attr s "width")
            , ("border", attr s "border")
            ]
                `merge` [style "border-collapse" "separate" | hasCellspacing]
        _ -> []

    width = case attr s "width" of
        Just "auto" -> "auto"
        w ->
            let raw = fromMaybe "" w
                (parsed, unit) = widthParser True raw
             in if unit == "%" then raw else showNumber parsed

    rendered =
        mconcat
            [ "<table"
            , htmlAttributes
                inst
                [ ("cellpadding", maybe ANone AV (attr s "cellpadding"))
                , ("cellspacing", maybe ANone AV (attr s "cellspacing"))
                , ("role", maybe ANone AV (attr s "role"))
                , ("width", AV width)
                , ("border", AV "0")
                , ("style", AStyleOf "table")
                ]
            , ">"
            , fromText (getContent s)
            , "</table>"
            ]
