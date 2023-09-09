import { glob } from "glob";
import { readFile, writeFile } from "node:fs/promises";

await Promise.all(
  (await glob(`../posts/**/*.md`)).map(async (path) => {
    const content = await readFile(path, "utf-8");
    const title = content.split("\n")[0].replace("# ", "");

    await writeFile(
      path,
      [
        "---",
        "layout: ../../layouts/Default.astro",
        `title: ${title}`,
        "---",
        content,
      ].join("\n"),
    );
  }),
);
