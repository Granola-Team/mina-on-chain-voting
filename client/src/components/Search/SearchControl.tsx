import React from "react";
import shallow from "zustand/shallow";
import { SearchIcon } from "@heroicons/react/solid";

import { useAppStore } from "@/App.store";

export const SearchControl = () => {
  const { searchActive, setSearchState } = useAppStore(
    (state) => ({
      searchActive: state.searchActive,
      setSearchState: state.setSearchState,
    }),
    shallow,
  );

  return (
    <button
      type="button"
      className="ml-4"
      onClick={() => {
        setSearchState(!searchActive);
      }}
    >
      <div className="group px-2 md:px-3 py-1.5 hover:bg-gray-4 transition-all duration-200 rounded-md">
        <SearchIcon className="w-[25px] h-[25px] text-gray-9 group-hover:text-gray-12 transition-all duration-200" />
      </div>
    </button>
  );
};
