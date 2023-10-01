import { glob } from "glob";
import { readFile, stat, writeFile } from "node:fs/promises";
import { exec } from "node:child_process";
import { promisify } from "node:util";
import { chain, sortBy } from "lodash-unified";
import { join } from "node:path";

const writeToc = async (directory: string, component: string) =>
  await writeFile(
    `src/components/${component}.md`,
    chain(
      await Promise.all(
        (await glob(`../${directory}/**/*.md`)).map(async (path) => {
          const htmlPath = path.replace(/^..\//, "").replace(".md", "");
          const pdfPath = htmlPath + ".pdf";

          return {
            title: (await readFile(path, "utf-8"))
              .split("\n")[0]
              .replace("# ", ""),
            htmlPath,
            pdfPath: (await stat(join("public", pdfPath)).catch(() => null))
              ? pdfPath
              : null,
            time: (
              await promisify(exec)(
                `git log --format=format:%ci --follow --name-only --diff-filter=A ${path}`,
              )
            ).stdout
              .split(" ")[0]
              .replaceAll("-", "/"),
          };
        }),
      ),
    )
      .sortBy("time")
      .groupBy(({ time }) => Number(time.split("/")[0]))
      .toPairsIn()
      .value()
      .reverse()
      .map(([year, posts]) => [
        `# ${year}`,
        posts.map(
          ({ title, htmlPath, pdfPath, time }) =>
            `- [${title}](${htmlPath}) (${
              pdfPath ? `[PDF](${pdfPath}), ` : ""
            }${time})`,
        ),
      ])
      .join("\n"),
  );

await writeToc("posts", "Posts");
await writeToc("slides", "Slides");
