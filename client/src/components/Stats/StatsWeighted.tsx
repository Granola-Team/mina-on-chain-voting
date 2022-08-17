import React from "react";
import moment from "moment";

import type { StatsWeightedProps } from "@/types";
import { IconTooltip } from "@/components/Tooltip";

const createPercent = (v: number, t: number): string => {
  const val = (v / t) * 100;
  if (Number.isNaN(val)) {
    return "XXX";
  }
  return val.toFixed(2);
};

export const StatsWeighted: React.FC<StatsWeightedProps> = ({ stats }) => {
  const now = moment(new Date()).format("YYYY-MM-DD | hh:mm:ss Z");
  const total = stats.yes + stats.no;
  const yesPercent = createPercent(stats.yes, total);
  const noPercent = createPercent(stats.no, total);

  return (
    <div className="content-full-width px-2 md:px-4 lg:px-8 mt-4 md:mt-6">
      <div className="bg-gray-2 border border-gray-7 rounded-xl w-full">
        <div className="flex flex-col justify-center px-3 md:px-6 py-2 gap-1">
          <div className="flex-col flex md:flex-row items-center justify-between">
            <div className="flex items-center gap-1.5">
              <span className="semibold text-lg md:text-xl leading-8">
                Signal Results
              </span>
              <IconTooltip css="h-[1.1rem] w-[1.1rem] md:mt-0.5">
                <div className="flex flex-col items-start justify-center">
                  <span className="medium text-sm">Information:</span>
                  <div className="text-[0.725rem]">
                    We&apos;re counting all
                    <span className="inline semibold"> Settled</span> and
                    <span className="inline semibold"> Unsettled</span> votes
                    that adhere to our signalling convention.
                  </div>
                </div>
              </IconTooltip>
            </div>

            <span className="text-[0.65rem] md:text-xs text-gray-10">
              Updated at: {now}
            </span>
          </div>
          <div className="flex flex-col gap-1 pb-2">
            <div className="flex items-center justify-between">
              <div className="flex flex-col items-start">
                <span className="text-xs md:text-sm medium text-gray-11 md:-mb-1">
                  Yes
                </span>
                <span className="text-sm md:text-lg semibold text-green-11">
                  {yesPercent}%
                </span>
              </div>
              <div className="flex flex-col items-end">
                <span className="text-xs md:text-sm medium text-gray-11 md:-mb-1">
                  No
                </span>
                <span className="text-sm md:text-lg semibold text-red-11">
                  {noPercent}%
                </span>
              </div>
            </div>
            <div className="flex items-center w-full self-center h-5">
              <div
                style={{
                  width: `${yesPercent === "XXX" ? "50" : yesPercent}%`,
                }}
                className={`bg-green-11 h-full ${
                  yesPercent === "100.00" ? "rounded-md" : "rounded-l-md"
                }`}
              />
              <div
                style={{
                  width: `${noPercent === "XXX" ? "50" : noPercent}%`,
                }}
                className={`bg-red-11 h-full ${
                  noPercent === "100.00" ? "rounded-md" : "rounded-r-md"
                }`}
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
