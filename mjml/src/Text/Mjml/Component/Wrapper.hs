-- | @mj-wrapper@: a section whose children are sections, each in its own
-- Outlook row.
module Text.Mjml.Component.Wrapper (
    mjWrapper,
) where

import Data.Maybe (fromMaybe)
import Data.Text qualified as T

import Text.Mjml.Component.Section (SectionParts (..), sectionAllowedAttributes, sectionDefaults, sectionParts)
import Text.Mjml.Render


mjWrapper :: Component
mjWrapper =
    (defComponent "mj-wrapper" build')
        { allowedAttributes = sectionAllowedAttributes <> [("gap", "unit(px)")]
        , defaults = sectionDefaults
        }
  where
    build' s = let parts = sectionParts s (renderWrappedChildren parts) in instanceOf parts

    renderWrappedChildren parts =
        let s = self (instanceOf parts)
            containerWidth = fromMaybe "" (ctx s "containerWidth")
         in renderChildren
                (instanceOf parts)
                defChildOptions
                    { renderer = \c ->
                        if rawElement (component (self c))
                            then render c
                            else do
                                body <- render c
                                pure $
                                    mconcat
                                        [ startConditionalTag
                                        , "<tr><td"
                                        , htmlAttributes
                                            c
                                            [ ("align", maybe ANone AV (attr (self c) "align"))
                                            , ("class", AV (suffixCssClasses (attr (self c) "css-class") "outlook"))
                                            , ("width", AV containerWidth)
                                            ]
                                        , ">"
                                        , endConditionalTag
                                        , body
                                        , startConditionalTag
                                        , "</td></tr>"
                                        , endConditionalTag
                                        ]
                    }
