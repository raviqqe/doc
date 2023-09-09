import { glob } from "glob";
import { mkdir, readFile, writeFile } from "node:fs/promises";

await mkdir("src/pages/posts", { recursive: true });
await Promise.all(
  (await glob(`../posts/**/*.md`)).map(async (path) => {
    const content = await readFile(path, "utf-8");
    const title = content.split("\n")[0].replace("# ", "");

    await writeFile(
      path.replace("../", "src/pages/"),
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
