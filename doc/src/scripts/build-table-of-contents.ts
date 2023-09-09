import { glob } from "glob";
import { readFile, writeFile } from "node:fs/promises";
import { exec } from "node:child_process";
import { promisify } from "node:util";
import { sortBy } from "lodash-unified";

await writeFile(
  "src/components/TableOfContents.md",
  sortBy(
    await Promise.all(
      (await glob("../**/*.md", { ignore: ["node_modules/**", "src/**"] }))
        .filter((path) => !path.includes("README"))
        .map(async (path) => {
          const title = (await readFile(path, "utf-8"))
            .split("\n")[0]
            .replace("# ", "");
          const htmlPath = path.replace(/^..\//, "").replace(".md", ".html");
          const time = (
            await promisify(exec)(
              `git log --format=format:%ci --name-only --diff-filter=A ${path}`,
            )
          ).stdout
            .split(" ")[0]
            .replaceAll("-", "/");

          return { title, path: htmlPath, time };
        }),
    ),
    "time",
  )
    .reverse()
    .map(({ title, path, time }) => `- [${title}](${path}) (${time})`)
    .join("\n"),
);
