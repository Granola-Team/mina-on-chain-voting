import React from "react";

import type { SettingsButtonProps } from "@/types";
import { useAppStore } from "@/App.store";

export const SettingsButton: React.FC<SettingsButtonProps> = ({ title }) => {
  const { network, setNetwork } = useAppStore((state) => state);
  const active = title === network;

  return (
    <div className="flex items-center justify-center w-full">
      <button
        type="button"
        disabled={active}
        onClick={() => {
          setNetwork(title);
        }}
        className={`w-full h-full py-3 transition-all duration-200 hover:opacity-80 rounded-lg ${
          active
            ? "border-2 border-green-8 bg-green-6"
            : "border-2 border-gray-8"
        }`}
      >
        <span className="font-medium">{title}</span>
      </button>
    </div>
  );
};
