// Regenerate the expected HTML of every fixture with the reference MJML
// implementation. Run from the repository root after `npm ci --prefix scripts`:
//
//   node scripts/update-fixtures.mjs [name-filter...]
//
// The output is written as MJML produces it, except for the identifiers MJML
// draws at random (accordion, navbar, carousel), which become 00000000 so the
// files are stable. The Haskell test suite zeroes its own identifiers the same
// way, and compares structurally, so MJML's whitespace does not matter.
import { readdir, readFile, writeFile } from "node:fs/promises";
import { createRequire } from "node:module";
import path from "node:path";

const require = createRequire(path.resolve("scripts/package.json"));
const mjml2html = require("mjml");

const dir = "mjml/test/fixtures";
const only = process.argv.slice(2);

const zeroIds = (html) => {
  let out = html;
  for (const [, id] of html.matchAll(/for="([a-zA-Z0-9]+)"/g)) {
    out = out.replaceAll('for="' + id + '"', 'for="00000000"');
    out = out.replaceAll('id="' + id + '"', 'id="00000000"');
  }
  for (const [, id] of html.matchAll(/mj-carousel-([a-zA-Z0-9]{12,16})-/g)) {
    out = out.replaceAll("mj-carousel-" + id + "-", "mj-carousel-00000000-");
    out = out.replaceAll("mj-carousel-radio-" + id, "mj-carousel-radio-00000000");
  }
  return out;
};

for (const file of (await readdir(dir)).filter((f) => f.endsWith(".mjml")).sort()) {
  const name = file.slice(0, -".mjml".length);
  if (only.length && !only.some((o) => name.includes(o))) continue;
  const source = await readFile(path.join(dir, file), "utf8");
  try {
    const { html } = await mjml2html(source, { validationLevel: "skip" });
    await writeFile(path.join(dir, name + ".html"), zeroIds(html));
  } catch (e) {
    console.error(name + ": " + e.message);
    process.exitCode = 1;
  }
}
