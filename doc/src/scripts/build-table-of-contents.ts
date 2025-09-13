import { exec } from "node:child_process";
import { readFile, stat, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { promisify } from "node:util";
import { groupBy, sortBy } from "es-toolkit";
import { glob } from "glob";

const excludedPattern = /slides\/papers/;

const writeToc = async (directory: string, component: string) =>
  writeFile(
    `src/components/${component}.md`,
    [
      ...Object.entries(
        groupBy(
          sortBy(
            await Promise.all(
              (await glob(`../${directory}/**/*.md`))
                .values()
                .filter((path) => !excludedPattern.test(path))
                .map(async (path) => {
                  const htmlPath = path.replace(/^..\//, "").replace(".md", "");
                  const pdfPath = `${htmlPath}.pdf`;

                  return {
                    htmlPath,
                    pdfPath: (await stat(join("public", pdfPath)).catch(
                      () => null,
                    ))
                      ? pdfPath
                      : null,
                    time: (
                      await promisify(exec)(
                        `git log --format=format:%ci --follow --name-only --diff-filter=A ${path}`,
                      )
                    ).stdout
                      .split(" ")[0]
                      .replaceAll("-", "/"),
                    title: (await readFile(path, "utf-8"))
                      .split("\n")[0]
                      .replace("# ", ""),
                  };
                }),
            ),
            ["time"],
          ),
          ({ time }) => Number(time.split("/")[0]),
        ),
      ),
    ]
      .sort()
      .reverse()
      .flatMap(([year, posts]) => [
        `### ${year}`,
        ...posts
          .reverse()
          .map(
            ({ htmlPath, pdfPath, time, title }) =>
              `- [${title}](${htmlPath}) (${pdfPath ? `[PDF](${pdfPath}), ` : ""
              }${time})`,
          ),
      ])
      .join("\n"),
  );

await writeToc("notes", "Notes");
await writeToc("posts", "Posts");
await writeToc("slides", "Slides");
