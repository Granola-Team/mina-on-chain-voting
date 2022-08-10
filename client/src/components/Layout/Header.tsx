import React from "react";
import { useNavigate } from "react-router-dom";

import { Settings, SettingsControl } from "@/components/Settings";
import { Search, SearchControl } from "@/components/Search";

export const Header = React.memo(() => {
  const navigate = useNavigate();

  return (
    <header className="border-b border-gray-7">
      <div className="flex items-center justify-between px-5 py-4 lg:px-10">
        <div className="flex items-center">
          <a
            onClick={() => {
              navigate("/");
            }}
            href=""
          >
            <h1>Mina On-Chain Signals</h1>
          </a>
          <span className="h-5 border-r ml-9 border-gray-7" />
          <SearchControl />
        </div>
        <SettingsControl />
      </div>
      <Search />
      <Settings />
    </header>
  );
});
