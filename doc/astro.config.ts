import { defineConfig, sharpImageService } from "astro/config";
import mdx from "@astrojs/mdx";
import prefetch from "@astrojs/prefetch";
import sitemap from "@astrojs/sitemap";
import remarkToc from "remark-toc";
import remarkMermaid from "remark-mermaidjs";
import rehypeSlug from "rehype-slug";
import rehypeAutoLinkHeadings from "rehype-autolink-headings";

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
    remarkPlugins: [remarkMermaid, remarkToc],
    rehypePlugins: [rehypeSlug, rehypeAutoLinkHeadings],
  },
  site: "https://raviqqe.github.io/doc",
});
