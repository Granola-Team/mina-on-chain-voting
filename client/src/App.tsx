import React, { useEffect } from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import {
  ReactQueryClient,
  QueryClientProvider,
  ReactQueryDevtools,
} from "@/queries";

import { isDarkMode, setTheme } from "@/utils/theme";
import { useAppStore } from "@/store/app.store";
import { Home } from "@/pages/Home";

const App = () => {
  const setDarkMode = useAppStore((state) => state.setDarkMode);
  const isDev = useAppStore((state) => state.devMode);

  useEffect(() => {
    if (isDev) {
      console.warn("Development mode activated. 🚀");
    }
    setTheme();
    setDarkMode(isDarkMode());
  }, []);

  return (
    <QueryClientProvider client={ReactQueryClient}>
      <Router>
        <Routes>
          <Route path="/" element={<Home />} />
        </Routes>
      </Router>
      {isDev ? <ReactQueryDevtools /> : null}
    </QueryClientProvider>
  );
};

export default App;
