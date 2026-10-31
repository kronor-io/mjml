# MJML for Haskell

[MJML](https://mjml.io) is a markup language for responsive email: you write
sections, columns, text and buttons, and it produces the table-based HTML that
renders properly in Outlook, Gmail and the rest. This repository renders MJML
in Haskell, with no Node.js.

| Package | |
|---|---|
| [`mjml`](mjml) | Renders MJML documents to HTML, matching the reference implementation's output. |
| [`mjml-hamlet`](mjml-hamlet) | Writes MJML templates in Hamlet's syntax, with MJML's validation done at compile time. |

## Development

```sh
cabal build all
cabal test all
```

The `mjml` test suite renders every document in `mjml/test/fixtures` and
compares the result with the HTML that MJML 5.4.1 itself produced for it. To
regenerate that HTML, for instance after upgrading MJML in
`scripts/package.json`:

```sh
npm ci --prefix scripts
node scripts/update-fixtures.mjs
```

## License

MIT
