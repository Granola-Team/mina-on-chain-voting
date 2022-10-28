import React from "react";
import shallow from "zustand/shallow";

import { CogIcon } from "@heroicons/react/solid";

import { useAppStore } from "@/App.store";

export const SettingsControl = () => {
  const { settingsActive, setSettingsState } = useAppStore(
    (state) => ({
      settingsActive: state.settingsActive,
      setSettingsState: state.setSettingsState,
    }),
    shallow,
  );

  return (
    <button
      type="button"
      aria-describedby="false"
      onClick={() => {
        setSettingsState(!settingsActive);
      }}
    >
      <div className="group px-2 md:px-3 py-1.5 hover:bg-gray-4 transition-all duration-200 rounded-md">
        <CogIcon className="w-[25px] h-[25px] text-gray-9 group-hover:text-gray-12 transition-all duration-200" />
      </div>
    </button>
  );
};
