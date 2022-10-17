import path from "path";
import { defineConfig } from "vitest/config";
import react from '@vitejs/plugin-react' 

export default defineConfig({
  plugins: [react()],
  test: {
    globals: true,
    environment: "happy-dom",
    coverage: {
      reporter: ['text', 'json', 'html'],
      reportsDirectory: "./tests/__coverage__",
    },
  },
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
});
