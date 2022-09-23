import React, { useEffect } from "react";
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import {
  ReactQueryClient,
  QueryClientProvider,
  ReactQueryDevtools,
} from "@/query";

import { isDarkMode, setTheme } from "@/utils/theme";
import { useAppStore } from "@/App.store";
import { Home, Keyword } from "@/pages";

const App = () => {
  const setDarkMode = useAppStore((state) => state.setDarkMode);

  useEffect(() => {
    setTheme();
    setDarkMode(isDarkMode());
  }, [setDarkMode]);

  return (
    <QueryClientProvider client={ReactQueryClient}>
      <Router>
        <Routes>
          <Route path="/:key" element={<Keyword />} />
          <Route path="/" element={<Home />} />
        </Routes>
      </Router>
      {import.meta.env.DEV ? <ReactQueryDevtools /> : null}
    </QueryClientProvider>
  );
};

export default App;
