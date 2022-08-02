import React from "react";

import type { RouteFilterType } from "@/types";

import { TableNavElement } from "./TableNavElement";

const navElements: { title: string; filter: RouteFilterType }[] = [
  {
    title: "All Signals",
    filter: "all",
  },
  {
    title: "Settled",
    filter: "settled",
  },
  {
    title: "Unsettled",
    filter: "unsettled",
  },
  {
    title: "Invalid",
    filter: "invalid",
  },
];

export const TableNavigation = () => {
  return (
    <div className="bg-gray-2 w-full rounded-xl py-4 px-6 border border-gray-7">
      <div className="flex items-center justify-between">
        <nav className="flex items-center">
          {navElements.map((nav, index) => (
            <React.Fragment key={index}>
              {index !== 0 && (
                <span className="h-4 border-r ml-4 border-gray-7" />
              )}
              <TableNavElement title={nav.title} filter={nav.filter} />
            </React.Fragment>
          ))}
        </nav>
        <span className="text-content medium">Sort</span>
      </div>
    </div>
  );
};
