import React from "react";

import { SettingsButton } from "./SettingsButton";

export const SettingsNetwork = () => {
  return (
    <div className="flex items-center justify-between gap-4">
      <SettingsButton title="Mainnet" />
      <SettingsButton title="Devnet" />
    </div>
  );
};
