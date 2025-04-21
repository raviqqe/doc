import { defineConfig } from "astro/config";
import mdx from "@astrojs/mdx";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import rehypeMermaid from "rehype-mermaid";
import remarkToc from "remark-toc";
import rehypeSlug from "rehype-slug";
import rehypeAutoLinkHeadings from "rehype-autolink-headings";

export default defineConfig({
  base: "/doc",
  experimental: {
    responsiveImages: true,
  },
  image: {
    experimentalLayout: "full-width",
    remotePatterns: [{ protocol: "https" }],
  },
  integrations: [
    mdx(),
    prefetch({ selector: "a", intentSelector: "a" }),
    sitemap(),
  ],
  markdown: {
    remarkPlugins: [remarkToc],
    rehypePlugins: [
      [rehypeMermaid, { dark: true, colorScheme: "dark" }],
      rehypeSlug,
      rehypeAutoLinkHeadings,
    ],
  },
  site: "https://raviqqe.github.io/doc",
});
