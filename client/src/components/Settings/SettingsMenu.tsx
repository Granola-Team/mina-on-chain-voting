import React from "react";

import { useFilterParams } from "@/hooks/useFilterParams";

import { SettingsDarkMode } from "./SettingsDarkMode";
import { SettingsNetwork } from "./SettingsNetwork";

export const SettingsMenu = () => {
  const [searchParams] = useFilterParams();
  const admin = searchParams.get("admin");

  return (
    <div className="flex flex-col gap-4 pt-4">
      <SettingsDarkMode />
      {admin === "true" && <SettingsNetwork />}
    </div>
  );
};
