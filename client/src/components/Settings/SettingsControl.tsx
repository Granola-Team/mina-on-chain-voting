import React from "react";
import { CogIcon } from "@heroicons/react/solid";

import { useAppStore } from "@/store/app.store";

export const SettingsControl = () => {
  const settingsActive = useAppStore((state) => state.settingsActive);
  const setSettingsState = useAppStore((state) => state.setSettingsState);

  return (
    <button
      onClick={() => {
        setSettingsState(!settingsActive);
      }}
    >
      <div className="group px-3 py-1.5 hover:bg-gray-4 transition-all duration-200 rounded-md">
        <CogIcon className="w-[25px] h-[25px] text-gray-9 group-hover:text-gray-12 transition-all duration-200" />
      </div>
    </button>
  );
};
