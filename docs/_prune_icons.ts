// _prune_icons.ts
// Quarto runs this script after every render (project: post-render in
// _quarto.yml), with the program it carries for that purpose; nothing has to
// be installed.
//
// Quarto embeds the whole Bootstrap Icons style sheet in each page: about two
// thousand rules of the form  .bi-name::before { content: "..." }, one per
// icon, among them the logos of many companies and products. The pages of
// this project use none of those icons, or a handful. This script removes from
// each rendered page the rules of the icons that the page does not mention
// anywhere else, which also makes every page about 100 kB smaller.

const rule = /\.bi-([a-z0-9-]+)::before\s*\{\s*content:\s*"\\[0-9a-f]+";?\s*\}\n?/g;

// Quarto names the rendered files, one per line, relative to the project folder
const project = Deno.env.get("QUARTO_PROJECT_DIR") ?? ".";
const outputs = (Deno.env.get("QUARTO_PROJECT_OUTPUT_FILES") ?? "")
  .split("\n")
  .map((file) => file.trim())
  .filter((file) => file.endsWith(".html"));

function located(file: string): string {
  try {
    Deno.statSync(file);
    return file;
  } catch (_notHere) {
    return `${project}/${file}`;                  // a missing file stops the script below
  }
}

for (const output of outputs) {
  const file = located(output);
  const page = Deno.readTextFileSync(file);
  const rest = page.replace(rule, "");            // the page without the icon rules
  // Every "bi-..." that the rest of the page mentions, found in one pass: the
  // pages hold many megabytes of figures, and looking for each of the two
  // thousand names in turn took 20 seconds on a page of 20 MB
  const mentioned = new Set<string>();
  for (const mention of rest.matchAll(/bi-([a-z0-9-]+)/g)) mentioned.add(mention[1]);
  // An icon is in use when a mention starts with its name ("bi-arrow-right"
  // keeps the rule of "arrow" too, as a search for "bi-arrow" would)
  const used = (name: string) => [...mentioned].some((mention) => mention.startsWith(name));
  let removed = 0;
  const pruned = page.replace(rule, (text: string, name: string) => {
    if (used(name)) return text;
    removed += 1;
    return "";
  });
  if (removed > 0) {
    Deno.writeTextFileSync(file, pruned);
    console.log(`${output}: ${removed} unused icon rules removed`);
  }
}
