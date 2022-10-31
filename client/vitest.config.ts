import { defineConfig } from "vitest/config";
import path, { resolve } from "path";

export default defineConfig({
  test: {
    globals: true,
    environment: "jsdom",
    coverage: {
      reporter: ["text", "json", "html"],
      reportsDirectory: "./tests/__coverage__",
    },
    setupFiles: [resolve(__dirname, "src/vitest.setup.ts")],
  },
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
});
