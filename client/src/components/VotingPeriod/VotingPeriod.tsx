/* eslint-disable no-mixed-operators */
import React from "react";
import moment from "moment";

interface Props {
  start: string;
  end: string;
}

export const VotingPeriod: React.FC<Props> = ({ start, end }) => {
  const _start = Number.parseInt(start, 10);
  const _end = Number.parseInt(end, 10);

  const now = moment(new Date()).utc();
  const startDate = moment(new Date(_start)).utc();
  const endDate = moment(new Date(_end)).utc();
  const duration = moment.duration(endDate.diff(now));
  const isDone = now.isAfter(endDate);

  return (
    <div className="content-full-width px-2 md:px-4 lg:px-8">
      <div className="bg-gray-2 border border-gray-7 rounded-xl w-full">
        <div className="flex flex-col justify-center px-3 md:px-6 py-2 gap-1.5">
          <div className="flex-col flex md:flex-row items-center justify-between">
            <div className="flex items-center gap-1.5">
              <span className="semibold text-lg md:text-xl leading-8">
                Voting Period
              </span>
            </div>
            <span className="text-[0.65rem] md:text-xs text-gray-10">
              Updated at {now.format("YYYY-MM-DD | hh:mm:ss A")} UTC
            </span>
          </div>
          <div className="flex flex-col gap-1 pb-2">
            <div className="flex-col flex md:flex-row items-center justify-between">
              <div className="flex flex-col items-start">
                <span className="text-xs md:text-sm medium text-gray-11 md:-mb-1">
                  Start Date
                </span>
                <span className="text-sm md:text-lg semibold text-blue-400/80">
                  {startDate.format("YYYY-MM-DD | hh:mm:ss A").toString()} UTC
                </span>
              </div>
              <div className="flex flex-col items-end">
                <span className="text-xs md:text-sm medium text-gray-11 md:-mb-1">
                  End Date
                </span>
                <span className="text-sm md:text-lg semibold text-violet-600">
                  {endDate.format("YYYY-MM-DD | hh:mm:ss A").toString()} UTC
                </span>
              </div>
            </div>
          </div>
          <div className="flex flex-col gap-1 pb-2">
            <div className="flex items-center w-full self-center h-5">
              <div
                style={{
                  marginRight: `${50}%`,
                }}
                className="w-full bg-gradient-to-r from-blue-400/80 to-violet-600 h-full rounded-lg"
                data-testid="bar-percentage"
              />
            </div>
          </div>
          <div className="flex flex-col items-left">
            <span className="text-xs md:text-sm medium text-gray-11 mb-1">
              {isDone
                ? "The Voting Period has ended."
                : `Remaining time: ${duration.months()}M ${duration.days()}D ${duration.hours()}H ${duration.minutes()}m`}
            </span>
          </div>
        </div>
      </div>
    </div>
  );
};
