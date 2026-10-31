# mjml-hamlet

[MJML](https://mjml.io) email templates written in Hamlet's syntax and checked
at compile time. Rendering goes through the [`mjml`](../mjml) package, so
there is no Node.js involved at build time or at runtime.

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Text.Mjml.Hamlet (mjml, renderMjml)

shipped :: Order -> Either MjmlError Text
shipped order =
    renderMjml
        [mjml|
        <mj-section>
          <mj-column>
            <mj-text font-size="20px">Your order #{order.reference} is on its way
            $forall line <- order.lines
              <mj-text>#{line.name} × #{line.quantity}
            $maybe url <- order.trackingUrl
              <mj-button href=#{url} .primary>Track it
        |]
```

## Syntax

As in Hamlet, nesting is given by indentation and there are no closing tags.

* `<mj-text align="center">Hi` is an element; text after the tag on the same
  line is its content. Attribute values may be quoted, unquoted, or an
  interpolation (`href=#{url}`). `.name` adds a class (`css-class` on MJML
  elements, `class` on HTML ones).
* `#{expr}` interpolates any `ToMarkup` value, escaped. Expressions are
  Hamlet's: variables, application, operators, literals and record fields
  (`order.lines`), but no type annotations or lambdas.
* `^{expr}` embeds what was built elsewhere: nodes (`Node`, `[Node]`,
  `Maybe Node`) among MJML elements, or blaze `Markup` inside an ending tag.
* `$forall pattern <- expr`, `$if` / `$elseif` / `$else`,
  `$maybe pattern <- expr` / `$nothing`, `$case expr` with nested
  `$of pattern` lines, and `$with pattern <- expr` work as in Hamlet. `$#`
  starts a comment line.
* Inside an ending tag (`mj-text`, `mj-button`, `mj-raw`, `mj-table`, …) the
  nested lines are HTML. Consecutive text lines are joined with a space; a
  leading `\` keeps a line's leading space.

`mjmlFile "templates/shipped.mjml"` reads the same syntax from a file and
recompiles when the file changes.

A template is a list of nodes. `renderMjml` renders a whole document rooted at
`<mjml>` as it is, wraps a template made of `<mj-head>` and `<mj-body>` in
`<mjml>`, and wraps anything else in `<mjml><mj-body>`, so a template can be
just the sections of an email.

## What does not compile

The checks MJML's validator makes at runtime happen at compile time, with the
template's line and column:

```
Shipped.hs:12:39: Attribute align has invalid value: middle for type Enum, only accepts left, right, center, justify
```

* an unknown element, or HTML outside an ending tag;
* an element inside a parent MJML does not allow (`mj-text` directly in
  `mj-section`);
* an attribute the element does not declare;
* a literal attribute value of the wrong type (units, colours, enumerations);
* text outside an ending tag.

## License

MIT.
