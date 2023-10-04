import { defineConfig } from "astro/config";
import mdx from "@astrojs/mdx";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import remarkToc from "remark-toc";

export default defineConfig({
  base: "/doc",
  image: {
    service: { entrypoint: "astro/assets/services/sharp" },
    remotePatterns: [{ protocol: "https" }],
  },
  integrations: [
    mdx({ remarkPlugins: [remarkToc] }),
    prefetch({ selector: "a", intentSelector: "a" }),
    sitemap(),
  ],
  markdown: {
    remarkPlugins: [remarkToc],
  },
  site: "https://raviqqe.gitub.io/doc",
});
