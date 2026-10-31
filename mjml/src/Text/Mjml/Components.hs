-- | The components MJML ships with.
module Text.Mjml.Components (
    defaultRegistry,
    endingTagNames,
) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

import Text.Mjml.Component.Body (mjBody)
import Text.Mjml.Component.Button (mjButton)
import Text.Mjml.Component.Column (mjColumn)
import Text.Mjml.Component.Divider (mjDivider)
import Text.Mjml.Component.Group (mjGroup)
import Text.Mjml.Component.Image (mjImage)
import Text.Mjml.Component.Raw (mjRaw)
import Text.Mjml.Component.Section (mjSection)
import Text.Mjml.Component.Social (mjSocial, mjSocialElement)
import Text.Mjml.Component.Spacer (mjSpacer)
import Text.Mjml.Component.Table (mjTable)
import Text.Mjml.Component.Text (mjText)
import Text.Mjml.Component.Wrapper (mjWrapper)
import Text.Mjml.Render (Component (..), Registry)


defaultRegistry :: Registry
defaultRegistry =
    Map.fromList
        [ (name c, c)
        | c <-
            [ mjBody
            , mjColumn
            , mjGroup
            , mjRaw
            , mjSection
            , mjText
            , mjWrapper
            , mjSocial
            , mjSocialElement
            , mjImage
            , mjButton
            , mjDivider
            , mjSpacer
            , mjTable
            ]
        ]


-- | Tags whose content is raw HTML. The head's ending tags are listed here
-- because the head is not rendered through the registry.
endingTagNames :: Registry -> Set Text
endingTagNames reg =
    Set.fromList (map name (filter endingTag (Map.elems reg)))
        <> Set.fromList ["mj-title", "mj-preview", "mj-style", "mj-breakpoint", "mj-html-attribute"]
