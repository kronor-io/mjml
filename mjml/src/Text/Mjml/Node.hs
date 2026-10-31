-- | An MJML document as a tree, in the shape MJML's own parser produces.
module Text.Mjml.Node (
    Node (..),
    element,
    comment,
    lookupAttr,
) where

import Data.Text (Text)


-- | One element of an MJML document.
--
-- Attribute values and 'content' are kept as source text, exactly as MJML
-- keeps them: they are copied into the output unescaped. Anything built from
-- untrusted data must be escaped before it is put here.
data Node = Node
    { tagName :: Text
    , attributes :: [(Text, Text)]
    -- ^ In source order.
    , children :: [Node]
    , content :: Text
    -- ^ The raw inner HTML of an ending tag (@mj-text@, @mj-button@, …), or
    -- the trimmed text of any other element.
    }
    deriving stock (Show, Eq)


element :: Text -> [(Text, Text)] -> [Node] -> Node
element name attrs kids = Node name attrs kids ""


-- | A comment, which MJML carries through the body as an @mj-raw@.
comment :: Text -> Node
comment body = Node "mj-raw" [] [] ("<!--" <> body <> "-->")


lookupAttr :: Text -> Node -> Maybe Text
lookupAttr name = lookup name . attributes
