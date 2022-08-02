import React, { useEffect, useState } from "react";
import { useLocation } from "react-router-dom";

import type { TableNavElementProps } from "@/types";
import { useFilterParams } from "@/hooks/useFilterParams";

export const TableNavElement: React.FC<TableNavElementProps> = ({
  title,
  filter,
}) => {
  const [searchParams, executeRoute] = useFilterParams();
  const [active, setActive] = useState(false);
  const location = useLocation();

  useEffect(() => {
    if (
      filter === searchParams.get("filter") ||
      (searchParams.get("filter") === null && filter === "all")
    ) {
      setActive(true);
    } else {
      setActive(false);
    }
  }, [location]);

  return (
    <button
      type="button"
      disabled={active}
      className={`relative w-20 ${filter !== "all" ? "ml-4" : ""}`}
      onClick={() => {
        executeRoute({ filter });
      }}
    >
      <span
        className={`text-content ${
          active
            ? "medium"
            : "text-gray-11 hover:text-gray-12 transition-colors duration-100"
        }`}
      >
        {title}
      </span>
      {active ? (
        <div className="absolute inset-x-0 -bottom-4 w-full h-[2.5px] bg-gradient animate-gradient" />
      ) : (
        <div />
      )}
    </button>
  );
};
