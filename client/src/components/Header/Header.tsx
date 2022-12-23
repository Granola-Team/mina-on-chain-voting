import React from "react";
import { useNavigate } from "react-router-dom";
import { useMediaQuery } from "@react-hook/media-query";

import { Settings, SettingsControl } from "@/components/Settings";

export const Header = React.memo(() => {
  const isMobile = useMediaQuery("only screen and (max-width: 768px)");
  const navigate = useNavigate();

  if (isMobile) {
    return (
      <header className="border-b border-gray-7">
        <div
          className="flex items-center justify-between py-4 px-3.5"
          data-testid="header-container"
        >
          <a
            onClick={() => {
              navigate("/");
            }}
            href=""
          >
            <h1 className="text-base">On Chain Voting for MIP1: Remove supercharge rewards</h1>
          </a>
          <div className="flex items-center gap-1">
            <SettingsControl />
          </div>
        </div>
        <Settings />
      </header>
    );
  }

  return (
    <header className="border-b border-gray-7">
      <div
        className="flex items-center justify-between px-5 py-4 lg:px-10"
        data-testid="header-container"
      >
        <div className="flex items-center">
          <a
              href="https://github.com/MinaProtocol/MIPs/blob/main/MIPS/mip-remove-supercharged-rewards.md"
          >
            <h1 className="text-base">On Chain Voting for MIP1: Remove supercharge rewards</h1>
          </a>
        </div>
        <SettingsControl />
      </div>
      <Settings />
    </header>
  );
});
