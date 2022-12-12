import React from "react";
import moment from "moment";
import { IconTooltip } from "../Tooltip";

interface Props {
  epoch: number;
  slot: number;
}

export const EpochTiming: React.FC<Props> = ({ epoch, slot }) => {
  const now = moment(new Date()).utc().format("YYYY-MM-DD | hh:mm:ss A");
  const percentage = (slot / 7140) * 89.78;
  const oppositePercentage = 100 - percentage;
  const begSlotMinutes = slot * 3;
  const begEpoch = moment().utc().subtract(begSlotMinutes, "minutes")
    .format("YYYY-MM-DD | hh:mm:ss A");
  const endSlotMinutes = (7140 - slot) * 3;
  const endEpoch = moment().utc().add(endSlotMinutes, "minutes")
    .format("YYYY-MM-DD | hh:mm:ss A");
  const days = Math.floor(endSlotMinutes / 60 / 24);
  const hours = Math.floor(((endSlotMinutes / 60 / 24) - days) * 24);
  const minutes = Math.floor(((((endSlotMinutes / 60 / 24) - days) * 24) - hours) * 60);

  return (
    <div className="content-full-width px-2 md:px-4 lg:px-8">
      <div className="bg-gray-2 border border-gray-7 rounded-xl w-full">
        <div className="flex flex-col justify-center px-3 md:px-6 py-2 gap-1.5">
          <div className="flex-col flex md:flex-row items-center justify-between">
            <div className="flex items-center gap-1.5">
              <span className="semibold text-lg md:text-xl leading-8">
                Voting Period
              </span>

              <IconTooltip css="h-[1.1rem] w-[1.1rem] md:mt-0.5">
                <div className="flex flex-col items-start justify-center">
                  <span className="medium text-sm">Information:</span>
                  <div className="text-xs">
                    There are
                    <span className="inline semibold"> 7140 slots</span> per
                    epoch. A new
                    <span className="inline semibold"> slot</span> is allocated
                    approx. every 3 minutes.
                  </div>
                </div>
              </IconTooltip>
            </div>

            <span className="semibold text-lg leading-8">
              Epoch {epoch} | Slot {slot}
            </span>

            <span className="text-[0.65rem] md:text-xs text-gray-10">
              Updated at {now} UTC
            </span>
          </div>
          <div className="flex flex-col gap-1 pb-2">
            <div className="flex-col flex md:flex-row items-center justify-between">
              <div className="flex flex-col items-start">
                <span className="text-xs md:text-sm medium text-gray-11 md:-mb-1">
                  Start Date
                </span>
                <span className="text-sm md:text-lg semibold text-blue-400/80">
                  {begEpoch.toString()} UTC
                </span>
              </div>
              <div className="flex flex-col items-end">
                <span className="text-xs md:text-sm medium text-gray-11 md:-mb-1">
                  End Date
                </span>
                <span className="text-sm md:text-lg semibold text-violet-600">
                  {endEpoch.toString()} UTC
                </span>
              </div>
            </div>
          </div>
          <div className="flex flex-col gap-1 pb-2">
            <div className="flex items-center w-full self-center h-5">
              <div
                style={{
                  marginRight: `${oppositePercentage}%`,
                }}
                className="w-full bg-gradient-to-r from-blue-400/80 to-violet-600 h-full rounded-lg"
                data-testid="bar-percentage"
              />
            </div>
          </div>
          <div className="flex flex-col items-left">
            <span className="text-xs md:text-sm medium text-gray-11 mb-1">
              Next epoch estimated to begin in {days} days {hours} hours {minutes} minutes
            </span>
          </div>
        </div>
      </div>
    </div>
  );
};
