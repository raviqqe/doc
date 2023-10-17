import { glob } from "glob";
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { dirname } from "node:path";
import { argv } from "node:process";

const [, directory] = argv;

await Promise.all(
  (await glob(`../${directory}/**/*.md`)).map(async (path) => {
    const content = await readFile(path, "utf-8");
    path = path.replace("../", "src/pages/");

    await mkdir(dirname(path), { recursive: true });
    await writeFile(
      path,
      [
        "---",
        "layout: ../../../layouts/Default.astro",
        `title: ${content.split("\n")[0].replace("# ", "")}`,
        "---",
        "",
        content,
      ].join("\n"),
    );
  }),
);
