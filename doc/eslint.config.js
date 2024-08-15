import { default as configurations } from "@raviqqe/eslint-config";

export default [
  ...configurations,
  {
    ignores: ["src/env.d.ts"],
  },
];
