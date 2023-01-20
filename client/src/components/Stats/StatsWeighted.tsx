import React, { useEffect, useState } from "react";
import moment from "moment";

import type { StatsWeightedProps } from "@/types";
import { IconTooltip } from "@/components/Tooltip";
import { useKeywordStore } from "@/pages/Keyword/Keyword.store";

const createPercent = (v: number, t: number): string => {
  const val = (v / t) * 100;
  if (Number.isNaN(val)) {
    return "N/A";
  }
  return val.toFixed(2);
};

export const StatsWeighted: React.FC<StatsWeightedProps> = ({ network }) => {
  const now = moment(new Date()).format("YYYY-MM-DD | hh:mm:ss Z");
  const { stats: _stats } = useKeywordStore((state) => ({
    stats: state.stats,
  }));

  const [stats, setStats] = useState({
    yes: "0",
    no: "0",
  });

  useEffect(() => {
    if (_stats) {
      const total = _stats.yes + _stats.no;
      setStats({
        yes: createPercent(_stats.yes, total),
        no: createPercent(_stats.no, total),
      });
    }
  }, [_stats]);

  return (
    <div className="content-full-width px-2 md:px-4 lg:px-8">
      <div className="bg-gray-2 border border-gray-7 rounded-xl w-full">
        <div className="flex flex-col justify-center px-3 md:px-6 py-2 gap-1">
          <div className="flex-col flex md:flex-row items-center justify-between">
            <div className="flex items-center gap-1.5">
              <span className="semibold text-lg md:text-xl leading-8">
                Voting Results
              </span>
              <IconTooltip css="h-[1.1rem] w-[1.1rem] md:mt-0.5">
                <div className="flex flex-col items-start justify-center">
                  <span className="medium text-sm">Information:</span>
                  <div className="text-[0.725rem]">
                    <div>
                      - Results include valid votes calculated based on their
                      account&apos;s weighted stake.
                    </div>
                    <div>
                      - Accounts that have delegated their stake then voted will
                      not count in the results.
                    </div>
                    <div className="semibold">
                      Refer to the FAQ for more detail
                    </div>
                  </div>
                </div>
              </IconTooltip>
            </div>

            <span className="text-[0.65rem] md:text-xs text-gray-10">
              {network[0].toUpperCase() + network.slice(1).toLowerCase()} at{" "}
              {now} UTC
            </span>
          </div>
          <div className="flex flex-col gap-1 pb-2">
            <div className="flex items-center justify-between">
              <div className="flex flex-col items-start">
                <span className="text-xs md:text-sm medium text-gray-11 md:-mb-1">
                  Yes
                </span>
                <span className="text-sm md:text-lg semibold text-green-11">
                  {stats.yes}%
                </span>
              </div>
              <div className="flex flex-col items-end">
                <span className="text-xs md:text-sm medium text-gray-11 md:-mb-1">
                  No
                </span>
                <span className="text-sm md:text-lg semibold text-red-11">
                  {stats.no}%
                </span>
              </div>
            </div>
            <div className="flex items-center w-full self-center h-5">
              <div
                style={{
                  width: `${stats.yes === "N/A" ? "50" : stats.yes}%`,
                }}
                className={`bg-green-11 opacity-80 h-full ${
                  stats.yes === "100.00" ? "rounded-md" : "rounded-l-md"
                }`}
              />
              <div
                style={{
                  width: `${stats.no === "N/A" ? "50" : stats.no}%`,
                }}
                className={`bg-red-11 h-full ${
                  stats.no === "100.00" ? "rounded-md" : "rounded-r-md"
                }`}
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
