import { glob, mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname, join, relative } from "node:path";
import { argv } from "node:process";

const [, , directory] = argv;

await Promise.all(
  (await Array.fromAsync(glob(join("..", directory, "**/*.md")))).map(
    async (path) => {
      const content = await readFile(path, "utf-8");
      path = join("src/pages", relative("..", path));

      await mkdir(dirname(path), { recursive: true });
      await writeFile(
        path,
        [
          "---",
          `layout: ${relative(dirname(path), "src/layouts/Default.astro")}`,
          `title: ${JSON.stringify(content.split("\n")[0].replace("# ", ""))}`,
          "---",
          "",
          content,
        ].join("\n"),
      );
    },
  ),
);
