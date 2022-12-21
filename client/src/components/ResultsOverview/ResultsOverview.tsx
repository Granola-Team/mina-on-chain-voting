import React from "react";

export const ResultsOverview: React.FC = () => {
  return (
    <div className="content-full-width px-2 md:px-4 lg:pl-8 mt-4 md:mt-6">
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
          </div>
        </div>
      </div>
    </div>
  );
};
