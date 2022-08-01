import App from "./App";
import * as React from "react";
import { createRoot } from "react-dom/client";
import "./assets/css/styles.css";

const root = createRoot(document.getElementById("root") as HTMLElement);

root.render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
);
