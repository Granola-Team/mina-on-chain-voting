import react from "@vitejs/plugin-react";
import { defineConfig } from "vite";
import { createHtmlPlugin } from "vite-plugin-html";
import { ViteEjsPlugin } from "vite-plugin-ejs";
import path from "path";

export default defineConfig({
  root: "src",
  build: {
    outDir: "../build",
  },
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
  server: {
    port: 3000,
  },
  preview: {
    port: 3000,
  },
  plugins: [
    ViteEjsPlugin({
      isDev: true,
    }),
    createHtmlPlugin({
      template: "./index.html",
    }),
    react({
      include: "**/*.{jsx,tsx}",
    }),
  ],
});
