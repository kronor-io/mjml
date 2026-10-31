-- | What MJML's validator knows about a document: which elements may contain
-- which, which attributes each element takes, and what values those accept.
--
-- The renderer itself never rejects a document, as MJML does not; this module
-- is for tools that want to, such as a compile-time template checker.
module Text.Mjml.Schema (
    allowedChildren,
    allowedAttributesOf,
    globalAttributes,
    checkAttribute,
    checkChild,
    knownTag,
) where

import Data.Char (isDigit, toLower)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

import Text.Mjml.Render (Component (..), Registry)


-- | MJML's @dependencies@: the elements each element may contain. 'Nothing'
-- for an element that accepts anything (@mj-attributes@, whose children are
-- tag names).
allowedChildren :: Map Text (Maybe [Text])
allowedChildren =
    Map.fromList
        [ ("mjml", Just ["mj-body", "mj-head", "mj-raw"])
        , ("mj-accordion", Just ["mj-accordion-element", "mj-raw"])
        , ("mj-accordion-element", Just ["mj-accordion-title", "mj-accordion-text", "mj-raw"])
        , ("mj-accordion-title", Just [])
        , ("mj-accordion-text", Just [])
        , ("mj-attributes", Nothing)
        , ("mj-body", Just ["mj-raw", "mj-section", "mj-wrapper", "mj-hero"])
        , ("mj-button", Just [])
        , ("mj-carousel", Just ["mj-carousel-image"])
        , ("mj-carousel-image", Just [])
        , ("mj-column", Just ["mj-accordion", "mj-button", "mj-carousel", "mj-divider", "mj-image", "mj-raw", "mj-social", "mj-spacer", "mj-table", "mj-text", "mj-navbar"])
        , ("mj-html-attribute", Just [])
        , ("mj-html-attributes", Just ["mj-selector"])
        , ("mj-divider", Just [])
        , ("mj-group", Just ["mj-column", "mj-raw"])
        , ("mj-head", Just ["mj-attributes", "mj-breakpoint", "mj-html-attributes", "mj-font", "mj-preview", "mj-style", "mj-title", "mj-raw"])
        , ("mj-hero", Just ["mj-accordion", "mj-button", "mj-carousel", "mj-divider", "mj-image", "mj-social", "mj-spacer", "mj-table", "mj-text", "mj-navbar", "mj-raw"])
        , ("mj-image", Just [])
        , ("mj-navbar", Just ["mj-navbar-link", "mj-raw"])
        , ("mj-raw", Just [])
        , ("mj-section", Just ["mj-column", "mj-group", "mj-raw"])
        , ("mj-selector", Just ["mj-html-attribute"])
        , ("mj-social", Just ["mj-social-element", "mj-raw"])
        , ("mj-social-element", Just [])
        , ("mj-spacer", Just [])
        , ("mj-table", Just [])
        , ("mj-text", Just [])
        , ("mj-wrapper", Just ["mj-hero", "mj-raw", "mj-section"])
        ]


-- | Attributes every element takes.
globalAttributes :: [Text]
globalAttributes = ["mj-class", "css-class"]


-- | The head elements are handled outside the component registry.
headAttributes :: Map Text [(Text, Text)]
headAttributes =
    Map.fromList
        [ ("mjml", [("lang", "string"), ("dir", "string"), ("owa", "string")])
        , ("mj-head", [])
        , ("mj-attributes", [])
        , ("mj-breakpoint", [("width", "unit(px)")])
        , ("mj-font", [("name", "string"), ("href", "string")])
        , ("mj-preview", [])
        , ("mj-style", [("inline", "string")])
        , ("mj-title", [])
        , ("mj-html-attributes", [])
        , ("mj-selector", [("path", "string")])
        , ("mj-html-attribute", [("name", "string")])
        ]


knownTag :: Registry -> Text -> Bool
knownTag reg t = Map.member t reg || Map.member t headAttributes


-- | The declared attributes of an element and their types, or 'Nothing' for
-- an unknown element.
allowedAttributesOf :: Registry -> Text -> Maybe [(Text, Text)]
allowedAttributesOf reg t = case Map.lookup t reg of
    Just c -> Just (allowedAttributes c)
    Nothing -> Map.lookup t headAttributes


-- | Check one attribute of an element: 'Nothing' when it is fine, otherwise
-- the reason, worded as MJML's validator words it.
checkAttribute :: Registry -> Text -> Text -> Maybe Text -> Maybe Text
checkAttribute reg tag attrName value = case allowedAttributesOf reg tag of
    Nothing -> Nothing
    Just allowed
        | attrName `elem` globalAttributes -> Nothing
        | otherwise -> case lookup attrName allowed of
            Nothing ->
                Just $
                    "Attribute "
                        <> attrName
                        <> " is illegal on <"
                        <> tag
                        <> ">; it takes "
                        <> T.intercalate ", " (map fst allowed <> globalAttributes)
            Just ty -> value >>= \v -> fmap (\m -> "Attribute " <> attrName <> " " <> m) (checkType ty v)


-- | Check that a child may appear inside a parent.
checkChild :: Registry -> Text -> Text -> Maybe Text
checkChild reg parent child
    | not (knownTag reg child) = Nothing
    | otherwise = case Map.lookup parent allowedChildren of
        Just (Just kids)
            | child `notElem` kids ->
                Just $
                    child
                        <> " cannot be used inside "
                        <> parent
                        <> ", only inside: "
                        <> T.intercalate ", " [p | (p, Just ks) <- Map.toList allowedChildren, child `elem` ks]
        _ -> Nothing


-- | MJML's attribute types: @color@, @unit(...)@, @enum(...)@, @boolean@,
-- @integer@ and @string@.
checkType :: Text -> Text -> Maybe Text
checkType ty value
    | "color" `T.isPrefixOf` ty =
        if validColor then Nothing else Just ("has invalid value: " <> value <> " for type Color")
    | "unit" `T.isPrefixOf` ty =
        if validUnit then Nothing else Just ("has invalid value: " <> value <> " for type Unit, only accepts (" <> T.intercalate ", " units <> ") units and " <> T.intercalate " to " counts <> " value(s)")
    | "enum" `T.isPrefixOf` ty =
        if value `elem` params then Nothing else Just ("has invalid value: " <> value <> " for type Enum, only accepts " <> T.intercalate ", " params)
    | "boolean" `T.isPrefixOf` ty =
        if value `elem` ["true", "false"] then Nothing else Just ("has invalid value: " <> value <> " for type Boolean")
    | "integer" `T.isPrefixOf` ty =
        if T.any isDigit value then Nothing else Just ("has invalid value: " <> value <> " for type Integer")
    | otherwise = Nothing
  where
    params = T.splitOn "," (T.takeWhile (/= ')') (T.drop 1 (T.dropWhile (/= '(') ty)))
    units = params
    counts = case T.breakOn "{" ty of
        (_, rest) | not (T.null rest) -> T.splitOn "," (T.takeWhile (/= '}') (T.drop 1 rest))
        _ -> ["1"]
    allowNegative = "unitWithNegative" `T.isPrefixOf` ty
    allowAuto = "auto" `elem` units
    realUnits = filter (/= "auto") units

    validUnit =
        let items = T.splitOn " " (T.dropWhileEnd (== ' ') value)
            n = length items
            (lo, hi) = case map (read . T.unpack) counts :: [Int] of
                [a] -> (a, a)
                [a, b] -> (a, b)
                _ -> (1, 1)
         in n >= lo && n <= hi && all validItem items
    validItem item
        | item == "0" = True
        | allowAuto && item == "auto" = True
        | otherwise =
            let numeric c = isDigit c || c == ',' || c == '.' || (allowNegative && c == '-')
                number = T.takeWhile numeric item
                suffix = T.drop (T.length number) item
             in not (T.null number) && suffix `elem` realUnits

    lowered = T.map toLower value
    validColor =
        ("rgba(" `T.isInfixOf` lowered)
            || ("rgb(" `T.isInfixOf` lowered)
            || hexColor
            || lowered `elem` namedColors
    hexColor = case T.uncons lowered of
        Just ('#', h) -> (T.length h == 3 || T.length h == 6) && T.all (`elem` ("0123456789abcdef" :: String)) h
        _ -> False


namedColors :: [Text]
namedColors =
    T.words
        "aliceblue antiquewhite aqua aquamarine azure beige bisque black blanchedalmond blue blueviolet brown \
        \burlywood cadetblue chartreuse chocolate coral cornflowerblue cornsilk crimson cyan darkblue darkcyan \
        \darkgoldenrod darkgray darkgreen darkgrey darkkhaki darkmagenta darkolivegreen darkorange darkorchid \
        \darkred darksalmon darkseagreen darkslateblue darkslategray darkslategrey darkturquoise darkviolet \
        \deeppink deepskyblue dimgray dimgrey dodgerblue firebrick floralwhite forestgreen fuchsia gainsboro \
        \ghostwhite gold goldenrod gray green greenyellow grey honeydew hotpink indianred indigo inherit ivory \
        \khaki lavender lavenderblush lawngreen lemonchiffon lightblue lightcoral lightcyan lightgoldenrodyellow \
        \lightgray lightgreen lightgrey lightpink lightsalmon lightseagreen lightskyblue lightslategray \
        \lightslategrey lightsteelblue lightyellow lime limegreen linen magenta maroon mediumaquamarine \
        \mediumblue mediumorchid mediumpurple mediumseagreen mediumslateblue mediumspringgreen mediumturquoise \
        \mediumvioletred midnightblue mintcream mistyrose moccasin navajowhite navy oldlace olive olivedrab \
        \orange orangered orchid palegoldenrod palegreen paleturquoise palevioletred papayawhip peachpuff peru \
        \pink plum powderblue purple rebeccapurple red rosybrown royalblue saddlebrown salmon sandybrown \
        \seagreen seashell sienna silver skyblue slateblue slategray slategrey snow springgreen steelblue tan \
        \teal thistle tomato transparent turquoise violet wheat white whitesmoke yellow yellowgreen"
