import { glob } from "glob";
import { readFile, writeFile } from "node:fs/promises";
import { exec } from "node:child_process";
import { promisify } from "node:util";
import { sortBy } from "lodash-unified";

const writeToc = async (directory: string, component: string) =>
  await writeFile(
    `src/components/${component}.md`,
    sortBy(
      await Promise.all(
        (await glob(`../${directory}/**/*.md`)).map(async (path) => ({
          title: (await readFile(path, "utf-8"))
            .split("\n")[0]
            .replace("# ", ""),
          path: path.replace(/^..\//, "").replace(".md", ""),
          time: (
            await promisify(exec)(
              `git log --format=format:%ci --follow --name-only --diff-filter=A ${path}`,
            )
          ).stdout
            .split(" ")[0]
            .replaceAll("-", "/"),
        })),
      ),
      "time",
    )
      .reverse()
      .map(({ title, path, time }) => `- [${title}](${path}) (${time})`)
      .join("\n"),
  );

await writeToc("posts", "Posts");
await writeToc("slides", "Slides");
