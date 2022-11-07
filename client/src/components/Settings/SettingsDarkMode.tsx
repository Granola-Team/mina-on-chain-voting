import React from "react";
import shallow from "zustand/shallow";

import { useAppStore } from "@/App.store";
import { changeTheme } from "@/utils/theme";

export const SettingsDarkMode = () => {
  const { darkMode, setDarkMode } = useAppStore(
    (state) => ({ darkMode: state.darkMode, setDarkMode: state.setDarkMode }),
    shallow,
  );

  const handleToggle = (e: React.ChangeEvent<HTMLInputElement>) => {
    setDarkMode(e.target.checked);
    changeTheme(darkMode);
  };

  return (
    <div className="flex items-center justify-between">
      <div className="flex flex-col">
        <span className="font-semibold">Dark Mode</span>
        <span className="mt-1 text-sm text-gray-11">
          Enable/Disable UID-Mode
        </span>
      </div>
      <label
        htmlFor="theme-toggle"
        className="flex items-center cursor-pointer"
      >
        <div className="relative">
          <input
            checked={darkMode}
            onChange={handleToggle}
            type="checkbox"
            id="theme-toggle"
            className="sr-only"
            data-testid="toggle"
          />
          <div
            className={`rounded-full p-[2px] ${
              darkMode ? "bg-gradient animate-gradient" : ""
            }`}
          >
            <div className="block h-[22px] bg-gray-8 rounded-full w-14" />
          </div>
          <div
            className={`absolute top-0 w-[26px] h-[26px] transition bg-gray-9 rounded-full dot ${
              !darkMode ? "border border-gray-9" : ""
            }`}
          />
        </div>
      </label>
    </div>
  );
};
