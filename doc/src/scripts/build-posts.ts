import { glob } from "glob";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname } from "node:path";

await Promise.all(
  (await glob("../posts/**/*.md")).map(async (path) => {
    const content = await readFile(path, "utf-8");
    const title = content.split("\n")[0].replace("# ", "");
    path = path.replace("../", "src/pages/");

    await mkdir(dirname(path), { recursive: true });
    await writeFile(
      path,
      [
        "---",
        "layout: ../../../layouts/Default.astro",
        `title: ${title}`,
        "---",
        "",
        content,
      ].join("\n"),
    );
  }),
);
