{-# LANGUAGE TemplateHaskell #-}

-- | MJML templates in Hamlet's syntax, checked at compile time.
--
-- > shipped :: Order -> [Node]
-- > shipped order =
-- >     [mjml|
-- >     <mj-section>
-- >       <mj-column>
-- >         <mj-text font-size="20px">Your order #{order.reference} is on its way
-- >         $forall line <- order.lines
-- >           <mj-text>#{line.name} × #{line.quantity}
-- >         $maybe url <- order.trackingUrl
-- >           <mj-button href=#{url}>Track it
-- >     |]
--
-- Nesting is by indentation, with no closing tags. @$forall@, @$if@,
-- @$maybe@, @$case@ and @$with@ work as in Hamlet; @#{...}@ interpolates an
-- escaped value and @^{...}@ embeds nodes, or markup inside an ending tag.
--
-- A template that names an unknown element, puts an element where MJML does
-- not allow it, uses an attribute the element does not declare, or gives a
-- literal attribute a value of the wrong type does not compile.
module Text.Mjml.Hamlet (
    mjml,
    mjmlFile,
    renderMjml,
    ToNodes (..),
    ToRawHtml (..),
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Loc (..), addDependentFile, location)

import Text.Mjml (MjmlError, Node (..), defaultOptions, defaultRegistry, endingTagNames, renderNode)
import Text.Mjml.Hamlet.Runtime (ToNodes (..), ToRawHtml (..))
import Text.Mjml.Hamlet.TH (Source (..), compileTemplate)


-- | A template, as an expression of type @['Node']@.
mjml :: QuasiQuoter
mjml =
    QuasiQuoter
        { quoteExp = \s -> do
            loc <- location
            let (l, c) = loc_start loc
            compile (Source (loc_filename loc) l (c + length ("[mjml|" :: String))) (T.pack s)
        , quotePat = const (fail "mjml: only expressions are supported")
        , quoteType = const (fail "mjml: only expressions are supported")
        , quoteDec = const (fail "mjml: only expressions are supported")
        }


-- | A template read from a file, recompiled when the file changes.
mjmlFile :: FilePath -> Q Exp
mjmlFile path = do
    addDependentFile path
    body <- runIO (T.readFile path)
    compile (Source path 1 1) body


compile :: Source -> Text -> Q Exp
compile = compileTemplate defaultRegistry (endingTagNames defaultRegistry)


-- | Render a template. A template that is a whole document (rooted at
-- @mjml@) renders as it is; one made of @mj-head@ and @mj-body@ is wrapped
-- in @mjml@, and anything else in @mjml@ and @mj-body@.
renderMjml :: [Node] -> Either MjmlError Text
renderMjml nodes = renderNode defaultOptions $ case nodes of
    [n] | tagName n == "mjml" -> n
    _
        | any ((`elem` ["mj-head", "mj-body"]) . tagName) nodes -> Node "mjml" [] nodes ""
        | otherwise -> Node "mjml" [] [Node "mj-body" [] nodes ""] ""
