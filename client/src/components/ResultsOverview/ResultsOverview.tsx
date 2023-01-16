import React from "react";
import { IconTooltip } from "../Tooltip";

export const ResultsOverview: React.FC<{ totalVotes: number }> = ({
  totalVotes,
}) => {
  return (
    <div className="content-full-width px-2 md:px-4 lg:px-8 mt-4 md:mt-6">
      <div className="bg-gray-2 border border-gray-7 rounded-xl w-full">
        <div className="flex flex-row justify-between px-3 md:px-6 py-2 gap-1.5">
          <div className="flex flex-col items-start justify-start md:gap-1">
            <div className="flex-col flex md:flex-row items-center justify-between">
              <span className="semibold text-lg md:text-xl leading-8">
                Results Overview
              </span>
            </div>
            <p className="font-semibold">
              <span className="text-[0.875rem] font-normal">
                Please read the&nbsp;
                <a
                  href="https://docs.google.com/document/d/1cMq2QoE_n61QGycsxRhCPirIAWNDXeWf"
                  className="text-OrangeMINA hover:opacity-80 transition-all duration-200"
                >
                  <u>FAQ</u>
                </a>
                &nbsp;to understand how the results were calculated
              </span>
            </p>

            <p className="font-semibold">
              <span className="text-[0.875rem] font-normal">
                The results can be verified on-chain. Please read these&nbsp;
                <a
                  href="#"
                  className="text-OrangeMINA hover:opacity-80 transition-all duration-200"
                >
                  <u>instructions</u>
                </a>
                &nbsp;for doing so.
              </span>
            </p>

            <p className="font-semibold">
              <span className="text-[0.875rem] font-normal">
                If you find an issue, a bug or simply have a question, please
                feel free to write us some&nbsp;
                <a
                  target="_blank"
                  rel="noopener noreferrer"
                  href="https://docs.google.com/forms/d/e/1FAIpQLSeKoyUIVU3OrJ7hkakwHnOeWz9R8gRe-pUeduXeMyfFsmW6iQ/viewform?usp=sf_link"
                  className="text-OrangeMINA hover:opacity-80 transition-all duration-200"
                >
                  <u>feedback</u>
                </a>
                . We would love to hear your thoughts!
              </span>
            </p>
          </div>
        </div>
      </div>
      <div className="bg-gray-2 border border-gray-7 rounded-xl w-[15%] ml-2">
        <div className="flex flex-col items-center justify-center w-full h-full">
          <div className="flex flex-row items-center justify-center">
            <span className="semibold text-xl leading-8">Total Votes</span>
            <IconTooltip css="h-[1.1rem] w-[1.1rem] ml-[2.5px] mt-[1px]">
              <div className="flex flex-row items-start justify-center">
                <div className="text-[0.725rem]">
                  <span className="inline semibold"> Total Votes</span> is the
                  sum of all valid votes per unique account that were received
                  during the voting period. Duplicate votes are not included in
                  the total.
                </div>
              </div>
            </IconTooltip>
          </div>
          <div className="flex flex-col items-center justify-center">
            <span className="semibold text-2xl">{totalVotes}</span>
          </div>
        </div>
      </div>
    </div>
  );
};
