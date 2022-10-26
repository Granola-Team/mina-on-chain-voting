import React from "react";
import moment from "moment";
import { IconTooltip } from "../Tooltip";

interface Props {
  epoch: number;
  slot: number;
}

export const EpochTiming: React.FC<Props> = ({ epoch, slot }) => {
  const now = moment(new Date()).format("YYYY-MM-DD | hh:mm:ss Z");
  const percentage = (slot / 7140) * 89.78;

  return (
    <div className="content-full-width px-2 md:px-4 lg:px-8">
      <div className="bg-gray-2 border border-gray-7 rounded-xl w-full">
        <div className="flex flex-col justify-center px-3 md:px-6 py-2 gap-1.5">
          <div className="flex-col flex md:flex-row items-center justify-between">
            <div className="flex items-center gap-1.5">
              <span className="semibold text-lg md:text-xl leading-8">
                Signal Timing
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
              Epoch {epoch} - Slot {slot}
            </span>

            <span className="text-[0.65rem] md:text-xs text-gray-10">
              Updated at {now}
            </span>
          </div>
          <div className="flex flex-col gap-1 pb-2">
            <div className="flex items-center w-full self-center h-5">
              <div className="w-full bg-gradient-to-r from-blue-400/80 to-violet-600 h-full rounded-lg" />
              <div
                style={{
                  marginLeft: `${percentage}%`,
                }}
                className="w-[5px] h-[20px] bg-black absolute"
              />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
