-- | The machinery every component renders through: MJML's @BodyComponent@,
-- its attribute resolution, and the document-wide data the head collects.
--
-- The structure follows @mjml-core@ closely on purpose. Each MJML component
-- is a JavaScript class whose methods read attributes, compute styles and
-- render template strings; here a component is a function from 'Self' (its
-- attributes, position and inherited context) to an 'Instance' holding the
-- same methods as closures. Keeping that shape is what makes a component easy
-- to check line by line against the original.
module Text.Mjml.Render (
    -- * Attributes
    Attrs,
    merge,
    -- * Styles
    Style,
    Styles,
    style,
    mergeStyles,
    -- * Document data
    GlobalData (..),
    initialGlobalData,
    R,
    runR,
    addMediaQuery,
    addHeadStyle,
    addComponentHeadStyle,
    nextId,
    -- * Components
    Component (..),
    defComponent,
    Registry,
    Props (..),
    Self (..),
    Instance (..),
    defInstance,
    attr,
    attrOr,
    truthy,
    hasAttr,
    getContent,
    ctx,
    -- * Rendering helpers
    Builder,
    fromText,
    fromChar,
    AttrValue (..),
    htmlAttributes,
    renderStyles,
    ChildOptions (..),
    defChildOptions,
    renderChildren,
    instantiate,
    -- * Box model
    shorthandValue,
    borderValue,
    BoxWidths (..),
    boxWidths,
    suffixCssClasses,
    conditionalTag,
    msoConditionalTag,
    startConditionalTag,
    endConditionalTag,
    startNegationConditionalTag,
    endNegationConditionalTag,
    startMsoConditionalTag,
    startMsoNegationConditionalTag,
    widthParser,
) where

import Control.Monad.Trans.State.Strict (State, get, gets, modify', runState)
import Data.Char (isDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Builder.Linear (Builder, fromChar, fromText)

import Text.Mjml.Js (parseFloatJs, parseIntJs)
import Text.Mjml.Node (Node (..))


-- | Attributes in JavaScript object order: a key keeps its first position
-- when a later merge overrides its value.
type Attrs = [(Text, Text)]


-- | @{...a, ...b}@: later keys override earlier ones in place, new keys
-- are appended. Within @b@ a repeated key keeps its first position and its
-- last value, as in an object literal.
merge :: [(Text, v)] -> [(Text, v)] -> [(Text, v)]
merge a [] = a
merge a b =
    [(k, Map.findWithDefault v k later) | (k, v) <- a]
        <> appended Set.empty b
  where
    later = Map.fromList b
    earlier = Set.fromList (map fst a)
    appended _ [] = []
    appended seen ((k, _) : rest)
        | k `Set.member` earlier || k `Set.member` seen = appended seen rest
        | otherwise = (k, later Map.! k) : appended (Set.insert k seen) rest


-- | One CSS declaration; 'Nothing' values are left out, as MJML drops
-- @undefined@ and @null@.
type Style = (Text, Maybe Text)


type Styles = [Style]


style :: Text -> Text -> Style
style k v = (k, Just v)


mergeStyles :: Styles -> Styles -> Styles
mergeStyles = merge


-- | What the head collects and the body adds to while rendering: MJML's
-- @globalData@.
data GlobalData = GlobalData
    { breakpoint :: Text
    , classes :: Map Text Attrs
    , classesDefault :: Map Text (Map Text Attrs)
    , defaultAttributes :: Map Text Attrs
    , fonts :: [(Text, Text)]
    , inlineStyle :: [Text]
    , headStyle :: [(Text, Text -> Text)]
    , componentsHeadStyle :: [Text -> Text]
    , headRaw :: [Text]
    , mediaQueries :: [(Text, Text)]
    , preview :: Text
    , styles :: [Text]
    , title :: Text
    , forceOWADesktop :: Bool
    , lang :: Text
    , dir :: Text
    , htmlAttributes' :: [(Text, Attrs)]
    , idCounter :: Int
    }


initialGlobalData :: [(Text, Text)] -> GlobalData
initialGlobalData defaultFonts =
    GlobalData
        { breakpoint = "480px"
        , classes = Map.empty
        , classesDefault = Map.empty
        , defaultAttributes = Map.empty
        , fonts = defaultFonts
        , inlineStyle = []
        , headStyle = []
        , componentsHeadStyle = []
        , headRaw = []
        , mediaQueries = []
        , preview = ""
        , styles = []
        , title = ""
        , forceOWADesktop = False
        , lang = "und"
        , dir = "auto"
        , htmlAttributes' = []
        , idCounter = 0
        }


type R = State GlobalData


runR :: R a -> GlobalData -> (a, GlobalData)
runR = runState


addMediaQuery :: Text -> Maybe (Text, Text) -> Maybe Text -> R ()
addMediaQuery className widthUnit padding =
    let rule =
            "{"
                <> maybe "" (\(w, u) -> " width:" <> w <> u <> " !important; max-width: " <> w <> u <> ";") widthUnit
                <> maybe "" (\p -> " padding: " <> p <> " !important;") padding
                <> " }"
     in modify' $ \g -> g {mediaQueries = merge (mediaQueries g) [(className, rule)]}


addHeadStyle :: Text -> (Text -> Text) -> R ()
addHeadStyle name f = modify' $ \g -> g {headStyle = merge (headStyle g) [(name, f)]}


addComponentHeadStyle :: (Text -> Text) -> R ()
addComponentHeadStyle f = modify' $ \g -> g {componentsHeadStyle = componentsHeadStyle g <> [f]}


-- | A fresh identifier where MJML would use a random hex string.
nextId :: R Text
nextId = do
    n <- gets idCounter
    modify' $ \g -> g {idCounter = n + 1}
    let s = T.pack (show n)
    pure (T.replicate (16 - T.length s) "0" <> s)


-- | A component definition: MJML's component class.
data Component = Component
    { name :: Text
    , endingTag :: Bool
    , rawElement :: Bool
    , allowedAttributes :: [(Text, Text)]
    -- ^ Attribute names and their MJML type, e.g. @unit(px,%){1,4}@.
    , defaults :: Attrs
    , componentHeadStyle :: Maybe (Text -> Text)
    -- ^ A style block added once per document, given the breakpoint.
    , prepare :: Self -> R Self
    -- ^ Work MJML does in a component's constructor, such as drawing a
    -- random identifier. Runs once per instantiation, before 'build'.
    , build :: Self -> Instance
    }


defComponent :: Text -> (Self -> Instance) -> Component
defComponent n b =
    Component
        { name = n
        , endingTag = False
        , rawElement = False
        , allowedAttributes = []
        , defaults = []
        , componentHeadStyle = Nothing
        , prepare = pure
        , build = b
        }


type Registry = Map Text Component


-- | Where a component sits among its siblings.
data Props = Props
    { index :: Int
    , isFirst :: Bool
    , isLast :: Bool
    , sibling :: Int
    , nonRawSiblings :: Int
    , extra :: Attrs
    -- ^ Anything else a parent passes down through @props@.
    }


-- | Everything a component's methods can see: @this@.
data Self = Self
    { attrs :: Attrs
    , attrIndex :: Map Text Text
    -- ^ 'attrs' again, for lookups.
    , props :: Props
    , context :: Attrs
    -- ^ The child context: @containerWidth@, @gutter@ and so on.
    , node :: Node
    , registry :: Registry
    , component :: Component
    , mjClassScope :: Text
    -- ^ The nearest @mj-class@ above this element, whose per-tag defaults
    -- apply to its children.
    }


data Instance = Instance
    { self :: Self
    , getStyles :: Text -> Styles
    , render :: R Builder
    , childContext :: Attrs
    , instanceHeadStyle :: Maybe (Text -> Text)
    -- ^ A style block this instance adds to the head, given the breakpoint:
    -- MJML's @componentHeadStyle@.
    }


defInstance :: Self -> R Builder -> Instance
defInstance s r =
    Instance
        { self = s
        , getStyles = const []
        , render = r
        , childContext = context s
        , instanceHeadStyle = Nothing
        }


attr :: Self -> Text -> Maybe Text
attr s k = Map.lookup k (attrIndex s)


attrOr :: Self -> Text -> Text -> Text
attrOr s k d = fromMaybe d (attr s k)


-- | JavaScript truthiness of an attribute: present, not empty, and not the
-- string @false@ (which MJML's parser turns into the boolean).
truthy :: Maybe Text -> Bool
truthy = \case
    Just v -> not (T.null v) && v /= "false"
    Nothing -> False


hasAttr :: Self -> Text -> Bool
hasAttr s = isJust . attr s


getContent :: Self -> Text
getContent s = T.strip (content (node s))


ctx :: Self -> Text -> Maybe Text
ctx s k = lookup k (context s)


data AttrValue
    = AV Text
    | AStyleOf Text
    -- ^ A named group from the component's 'getStyles'.
    | AStyles Styles
    | ANone


-- | @htmlAttributes@: render attributes, leaving out absent ones.
htmlAttributes :: Instance -> [(Text, AttrValue)] -> Builder
htmlAttributes inst = foldMap one
  where
    one (k, v) = case v of
        ANone -> mempty
        AV t -> attribute k (fromText t)
        AStyleOf groupName -> attribute k (styleBuilder (getStyles inst groupName))
        AStyles ss -> attribute k (styleBuilder ss)
    attribute k value = fromChar ' ' <> fromText k <> "=\"" <> value <> fromChar '"'


styleBuilder :: Styles -> Builder
styleBuilder = foldMap (\(k, v) -> maybe mempty (\x -> fromText k <> fromChar ':' <> fromText x <> fromChar ';') v)


renderStyles :: Styles -> Text
renderStyles = T.concat . map (\(k, v) -> maybe "" (\x -> k <> ":" <> x <> ";") v)


data ChildOptions = ChildOptions
    { childNodes :: Maybe [Node]
    , childAttributes :: Attrs
    , childProps :: Attrs
    , renderer :: Instance -> R Builder
    }


defChildOptions :: ChildOptions
defChildOptions =
    ChildOptions
        { childNodes = Nothing
        , childAttributes = []
        , childProps = []
        , renderer = render
        }


-- | @renderChildren@.
renderChildren :: Instance -> ChildOptions -> R Builder
renderChildren parent opts = do
    let s = self parent
        kids = fromMaybe (children (node s)) (childNodes opts)
        n = length kids
        isRaw k = maybe False rawElement (Map.lookup (tagName k) (registry s))
        nonRaw = length (filter (not . isRaw) kids)
    outs <- traverse
        ( \(i, k) -> do
            let p =
                    Props
                        { index = i
                        , isFirst = i == 0
                        , isLast = i + 1 == n
                        , sibling = n
                        , nonRawSiblings = nonRaw
                        , extra = childProps opts
                        }
            mi <- instantiate (registry s) (childScope s) (childContext parent) p (childAttributes opts) k
            maybe (pure "") (renderer opts) mi
        )
        (zip [0 ..] kids)
    pure (mconcat outs)


childScope :: Self -> Text
childScope s = fromMaybe (mjClassScope s) (lookup "mj-class" (attributes (node s)))


-- | @initComponent@: build a component for a node, registering its head
-- style. 'Nothing' for a tag no component handles.
--
-- The node's own attributes stay as written; @mj-attributes@ defaults and
-- @mj-class@ values are folded in here, as MJML's @applyAttributes@ does
-- before rendering, so that a component can still tell what was written on
-- the element itself.
instantiate :: Registry -> Text -> Attrs -> Props -> Attrs -> Node -> R (Maybe Instance)
instantiate reg scope context' p inherited n = case Map.lookup (tagName n) reg of
    Nothing -> pure Nothing
    Just c -> do
        g <- get
        let resolved = resolveAttributes c g scope inherited n
        s <- prepare c (Self (Map.toList resolved) resolved p context' n reg c scope)
        let inst = build c s
        traverse_' (addHeadStyle (name c)) (componentHeadStyle c)
        traverse_' addComponentHeadStyle (instanceHeadStyle inst)
        pure (Just inst)
  where
    traverse_' f = maybe (pure ()) f


-- | An element's attributes, from the lowest precedence to the highest: the
-- component's defaults, @mj-all@, what the parent passes down, the tag's
-- @mj-attributes@ defaults, its @mj-class@ values, the per-tag defaults of
-- the enclosing @mj-class@, and what is written on the element. The last four
-- are MJML's @applyAttributes@.
resolveAttributes :: Component -> GlobalData -> Text -> Attrs -> Node -> Map Text Text
resolveAttributes c g parentMjClass inherited n =
    formatAttributes c $
        Map.unions
            [ Map.delete "mj-class" (Map.fromList own)
            , fromParentClasses
            , fromClasses
            , Map.fromList (Map.findWithDefault [] (tagName n) (defaultAttributes g))
            , Map.fromList inherited
            , Map.fromList (Map.findWithDefault [] "mj-all" (defaultAttributes g))
            , Map.fromList (defaults c)
            ]
  where
    own = attributes n
    -- Later classes win; css-class values accumulate instead.
    fromClasses =
        foldl'
            ( \acc cls ->
                let values = Map.fromList (Map.findWithDefault [] cls (classes g))
                    joined = case (Map.lookup "css-class" acc, Map.lookup "css-class" values) of
                        (Just x, Just y) -> Map.singleton "css-class" (x <> " " <> y)
                        _ -> Map.empty
                 in Map.unions [joined, values, acc]
            )
            Map.empty
            (maybe [] (T.splitOn " ") (lookup "mj-class" own))
    fromParentClasses =
        foldl'
            ( \acc cls ->
                Map.union
                    (maybe Map.empty (Map.fromList . Map.findWithDefault [] (tagName n)) (Map.lookup cls (classesDefault g)))
                    acc
            )
            Map.empty
            (T.splitOn " " parentMjClass)


-- | MJML's type coercion on declared attributes. Only colours change: a
-- three-digit hex colour is expanded.
formatAttributes :: Component -> Map Text Text -> Map Text Text
formatAttributes c attrMap =
    foldl'
        (\m (k, ty) -> if "color" `T.isPrefixOf` ty then Map.adjust expandColor k m else m)
        attrMap
        (allowedAttributes c)
  where
    expandColor v = case T.unpack v of
        ['#', a, b, cc] | all isWord [a, b, cc] -> T.pack ['#', a, a, b, b, cc, cc]
        _ -> v
    isWord ch = ch == '_' || isDigit ch || ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')


-- | @getShorthandAttrValue@: one side of a CSS shorthand such as @padding@.
shorthandValue :: Self -> Text -> Text -> Double
shorthandValue s attribute direction =
    case attr s (attribute <> "-" <> direction) of
        Just v | not (T.null v) -> parseIntJs v
        _ -> case attr s attribute of
            Just v | not (T.null v) -> shorthandParser v direction
            _ -> 0


shorthandParser :: Text -> Text -> Double
shorthandParser cssValue direction =
    let parts = take 4 (T.words cssValue)
        pick i = maybe 0 parseIntJs (lookupIndex i parts)
     in case length parts of
            2 -> pick (if direction `elem` ["top", "bottom"] then 0 else 1)
            3 -> pick (case direction of "top" -> 0; "bottom" -> 2; _ -> 1)
            4 -> pick (case direction of "top" -> 0; "right" -> 1; "bottom" -> 2; _ -> 3)
            _ -> parseIntJs cssValue
  where
    lookupIndex i xs = if i < length xs then Just (xs !! i) else Nothing


-- | @getShorthandBorderValue@: the width of a border shorthand.
borderValue :: Self -> Maybe Text -> Text -> Double
borderValue s direction attribute =
    let directional = direction >>= \d -> attr s (attribute <> "-" <> d)
        value = case directional of
            Just v | truthy (Just v) -> v
            _ -> case attr s attribute of
                Just v | truthy (Just v) -> v
                _ -> "0"
     in borderParser value


-- | The first run of digits that starts the string or follows a space.
borderParser :: Text -> Double
borderParser border = go (T.unpack border) True
  where
    go [] _ = 0
    go str@(c : rest) atBoundary
        | atBoundary && isDigit c =
            let ds = takeWhile isDigit str
                n = parseIntJs (T.pack ds)
             in if isNaN n || n == 0 then 0 else n
        | otherwise = go rest (c == ' ')


data BoxWidths = BoxWidths
    { totalWidth :: Double
    , borders :: Double
    , paddings :: Double
    , box :: Double
    }


-- | @getBoxWidths@.
boxWidths :: Self -> BoxWidths
boxWidths s =
    let parsed = parseIntJs (fromMaybe "" (ctx s "containerWidth"))
        pads = shorthandValue s "padding" "right" + shorthandValue s "padding" "left"
        bords = borderValue s (Just "right") "border" + borderValue s (Just "left") "border"
     in BoxWidths parsed bords pads (parsed - pads - bords)


suffixCssClasses :: Maybe Text -> Text -> Text
suffixCssClasses classes suffix = case classes of
    Just c | not (T.null c) -> T.intercalate " " [x <> "-" <> suffix | x <- T.splitOn " " c]
    _ -> ""


startConditionalTag, endConditionalTag, startNegationConditionalTag, endNegationConditionalTag :: Builder
startConditionalTag = "<!--[if mso | IE]>"
endConditionalTag = "<![endif]-->"
startNegationConditionalTag = "<!--[if !mso | IE]><!-->"
endNegationConditionalTag = "<!--<![endif]-->"


startMsoConditionalTag, startMsoNegationConditionalTag :: Builder
startMsoConditionalTag = "<!--[if mso]>"
startMsoNegationConditionalTag = "<!--[if !mso]><!-->"


conditionalTag :: Bool -> Builder -> Builder
conditionalTag negation body =
    (if negation then startNegationConditionalTag else startConditionalTag)
        <> body
        <> (if negation then endNegationConditionalTag else endConditionalTag)


msoConditionalTag :: Bool -> Builder -> Builder
msoConditionalTag negation body =
    (if negation then startMsoNegationConditionalTag else startMsoConditionalTag)
        <> body
        <> (if negation then endNegationConditionalTag else endConditionalTag)


-- | @widthParser@: the number and unit of a width. With the flag set,
-- percentages are truncated to integers, as MJML does by default.
widthParser :: Bool -> Text -> (Double, Text)
widthParser parseFloatToInt width =
    let unit = T.takeWhileEnd (not . isDigit) width
        parser = case unit of
            "%" | not parseFloatToInt -> parseFloatJs
            _ -> parseIntJs
     in (parser width, if T.null unit then "px" else unit)
