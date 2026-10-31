-- | @mj-raw@: content copied into the output untouched.
module Text.Mjml.Component.Raw (
    mjRaw,
) where

import Text.Mjml.Render


mjRaw :: Component
mjRaw =
    (defComponent "mj-raw" (\s -> defInstance s (pure (fromText (getContent s)))))
        { endingTag = True
        , rawElement = True
        , allowedAttributes = [("position", "enum(file-start)")]
        }
