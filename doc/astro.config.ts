import mdx from "@astrojs/mdx";
import sitemap from "@astrojs/sitemap";
import { defineConfig } from "astro/config";
import rehypeAutoLinkHeadings from "rehype-autolink-headings";
import rehypeMermaid from "rehype-mermaid";
import rehypeSlug from "rehype-slug";
import remarkToc from "remark-toc";

export default defineConfig({
  base: "/doc",
  image: {
    layout: "full-width",
    remotePatterns: [{ protocol: "https" }],
    responsiveStyles: true,
  },
  integrations: [mdx(), sitemap()],
  markdown: {
    rehypePlugins: [
      [
        rehypeMermaid,
        {
          dark: true,
          strategy: "img-svg",
        },
      ],
      rehypeSlug,
      rehypeAutoLinkHeadings,
    ],
    remarkPlugins: [remarkToc],
    syntaxHighlight: {
      excludeLangs: ["mermaid"],
      type: "shiki",
    },
  },
  prefetch: true,
  site: "https://raviqqe.github.io/doc",
});
