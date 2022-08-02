import React, { useEffect } from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";

import { isDarkMode, setTheme } from "@/utils/theme";
import { useAppStore } from "@/store/app.store";
import { Home } from "@/pages/Home";
import { isDev } from "./utils/devMode";

const App = () => {
  const setDarkMode = useAppStore((state) => state.setDarkMode);

  useEffect(() => {
    if (isDev()) {
      console.warn("Development mode activated. 🚀");
    }
    setTheme();
    setDarkMode(isDarkMode());
  }, []);

  return (
    <Router>
      <Routes>
        <Route path="/" element={<Home />} />
      </Routes>
    </Router>
  );
};

export default App;
