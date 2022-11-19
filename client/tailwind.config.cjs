/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/index.html", "./src/**/*.{js,jsx,ts,tsx}"],
  darkMode: "class",
  theme: {
    extend: {
      screens: {
        xs: "375px",
        sm: "576px",
        md: "768px",
        lg: "1024px",
        xl: "1200px",
      },
      textColor: {
        "OrangeMINA": "#FF603B",
      },
      fontFamily: {
        sans: [
          "Inter Regular",
          "-apple-system",
          "BlinkMacSystemFont",
          "Segoe UI",
          "Roboto",
          "Oxygen",
          "Ubuntu",
          "Cantarell",
          "Fira Sans",
          "Droid Sans",
          "Helvetica Neue",
          "sans-serif",
        ],
        serif: [
          "Constantia",
          "Lucida Bright",
          "Lucidabright",
          "Lucida Serif",
          "Lucida",
          "DejaVu Serif",
          "Bitstream Vera Serif",
          "Liberation Serif",
          "Georgia",
          "serif",
        ],
        mono: [
          "Menlo",
          "Monaco",
          "Consolas",
          "Liberation Mono",
          "Courier New",
          "monospace",
        ],
      },
      backgroundImage: {
        gradient:
          "linear-gradient(49deg,#2d4de0 0,#9f71f0 30%,#fc6277 58%,#d100cf 95%)",
      },
    },
  },
  plugins: [],
  presets: [require("windy-radix-palette")],
};
