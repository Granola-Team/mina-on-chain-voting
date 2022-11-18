import { defineConfig } from "vitest/config";
import path, { resolve } from "path";

export default defineConfig({
  test: {
    globals: true,
    environment: "jsdom",
    coverage: {
      all: true,
      reporter: ["text", "json", "html"],
      reportsDirectory: "./tests/__coverage__",
      include: ["src"],
      exclude: [
        "src/pages/**",
        "src/types/**",
        "src/env.d.ts",
        "src/index.tsx",
      ],
    },
    setupFiles: [resolve(__dirname, "src/vitest.setup.ts")],
  },
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
});
