import { glob } from "glob";
import { readFile, writeFile } from "node:fs/promises";

await writeFile(
  "src/components/TableOfContents.md",
  (
    await Promise.all(
      (await glob("../**/*.md", { ignore: ["node_modules/**", "**/src/**"] }))
        .filter((path) => !path.includes("README"))
        .map(async (path) => {
          const title = (await readFile(path, "utf-8"))
            .split("\n")[0]
            .replace("# ", "");
          path = path.replace(/^..\//, "").replace(".md", ".html");

          return `- [${title}](${path})`;
        }),
    )
  ).join("\n"),
);
