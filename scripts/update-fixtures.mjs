// Regenerate the expected HTML of every fixture with the reference MJML
// implementation. Run from the repository root after `npm ci --prefix scripts`:
//
//   node scripts/update-fixtures.mjs [name-filter...]
//
// The output is written as MJML produces it, untouched: the Haskell test suite
// compares structurally, so MJML's template-string whitespace does not matter.
import { readdir, readFile, writeFile } from "node:fs/promises";
import { createRequire } from "node:module";
import path from "node:path";

const require = createRequire(path.resolve("scripts/package.json"));
const mjml2html = require("mjml");

const dir = "mjml/test/fixtures";
const only = process.argv.slice(2);

for (const file of (await readdir(dir)).filter((f) => f.endsWith(".mjml")).sort()) {
  const name = file.slice(0, -".mjml".length);
  if (only.length && !only.some((o) => name.includes(o))) continue;
  const source = await readFile(path.join(dir, file), "utf8");
  try {
    const { html } = await mjml2html(source, { validationLevel: "skip" });
    await writeFile(path.join(dir, name + ".html"), html);
  } catch (e) {
    console.error(name + ": " + e.message);
    process.exitCode = 1;
  }
}
