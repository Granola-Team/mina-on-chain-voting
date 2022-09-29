import React from "react";

import { SettingsDarkMode } from "./SettingsDarkMode";

export const SettingsMenu = () => {
  return (
    <div className="flex flex-col gap-4 pt-4">
      <SettingsDarkMode />
    </div>
  );
};
