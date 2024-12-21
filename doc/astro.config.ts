import { defineConfig, sharpImageService } from "astro/config";
import mdx from "@astrojs/mdx";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import remarkToc from "remark-toc";

export default defineConfig({
  base: "/doc",
  image: {
    service: sharpImageService(),
    remotePatterns: [{ protocol: "https" }],
  },
  integrations: [
    mdx(),
    prefetch({ selector: "a", intentSelector: "a" }),
    sitemap(),
  ],
  markdown: {
    remarkPlugins: [remarkToc],
  },
  site: "https://raviqqe.github.io/doc",
});
