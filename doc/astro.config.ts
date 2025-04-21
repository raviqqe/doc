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
    syntaxHighlight: {
      type: "shiki",
      excludeLangs: ["mermaid"],
    },
    remarkPlugins: [remarkToc],
    rehypePlugins: [
      [
        rehypeMermaid,
        {
          colorScheme: "dark",
          dark: true,
          strategy: "img-svg",
        },
      ],
      rehypeSlug,
      rehypeAutoLinkHeadings,
    ],
  },
  site: "https://raviqqe.github.io/doc",
});
