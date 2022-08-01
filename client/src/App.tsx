import React, { useEffect } from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";

import { isDarkMode, setTheme } from "@/utils/theme";
import { useAppStore } from "@/store/app.store";
import { isDev } from "@/utils/devMode";
import { Home } from "@/pages/Home";

const App = () => {
  const setDarkMode = useAppStore((state) => state.setDarkMode);
  const state = useAppStore();

  useEffect(() => {
    if (isDev()) {
      console.log(state);
    }
  }, [state]);

  useEffect(() => {
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
