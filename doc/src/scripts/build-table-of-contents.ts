import { glob } from "glob";

console.log(
  (await glob("../**/*.md", { ignore: ["node_modules", "doc"] })).filter(
    (path) => !path.includes("/doc/"),
  ),
);
