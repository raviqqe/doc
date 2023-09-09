import { glob } from "glob";
import { readFile, writeFile } from "node:fs/promises";

await writeFile(
  "src/components/TableOfContents.md",
  (
    await Promise.all(
      (
        await glob("**/*.md", {
          ignore: ["node_modules/**", "doc/**"],
          root: "..",
        })
      )
        .filter((path) => !path.includes("/doc/"))
        .map(async (path) => {
          const title = (await readFile(path, "utf-8"))
            .split("\n")[0]
            .replace("# ", "");

          return `- [${title}](${path})`;
        }),
    )
  ).join("\n"),
);
