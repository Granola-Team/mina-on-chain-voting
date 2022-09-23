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
      (searchParams.get("filter") === null && filter === "All")
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
      className={`relative md:w-20 ${filter !== "All" ? "ml-4" : ""}`}
      onClick={() => {
        executeRoute({ filter });
      }}
    >
      <span
        className={`text-[0.8rem] md:text-[0.95rem] md:leading-6 ${
          active
            ? "medium"
            : "text-gray-11 hover:text-gray-12 transition-colors duration-100"
        }`}
      >
        {title}
      </span>
      {active ? (
        <div className="absolute inset-x-0 -bottom-4 w-full h-[2px] rounded-xl bg-gradient animate-gradient" />
      ) : (
        <div />
      )}
    </button>
  );
};
