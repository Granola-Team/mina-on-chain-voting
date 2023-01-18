import React from "react";

export const ResultsOverview = () => {
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
                If an account delegates and then also votes (having delegated),
                then this direct vote has 0 weight since the account has already
                delegated away its stake. This 0 weighted vote will not be
                counted in the results. “Stake Delegated” will display in the
                Weighted Stake and Weighted Stake % columns in this scenario.
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
    </div>
  );
};
