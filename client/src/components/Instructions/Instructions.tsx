import { useKeywordStore } from "@/pages/Keyword/Keyword.store";
import React from "react";
import shallow from "zustand/shallow";

export const Instructions: React.FC = () => {
  const { key } = useKeywordStore((state) => ({ key: state.key }), shallow);

  return (
    <div className="content-full-width px-2 md:px-4 lg:px-8 mt-4 md:mt-6">
      <div className="bg-gray-2 border border-gray-7 rounded-xl w-full">
        <div className="flex flex-row justify-between px-3 md:px-6 py-2 gap-1.5">
          <div className="flex flex-col items-start justify-start md:gap-1">
            <div className="flex-col flex md:flex-row items-center justify-between">
              <span className="semibold text-lg md:text-xl leading-8">
                Instructions
              </span>
            </div>
            <div className="font-semibold">
              {" "}
              <span className="text-[0.875rem] font-normal">
                Send yourself a transaction with the keyword(s) in the memo
                field
                <ul className="list-disc list-inside">
                  <li>
                    To vote in favor of the proposal, enter in the memo field{" "}
                    <span className="font-semibold">&apos;{key}&apos;</span>
                  </li>
                  <li>
                    To vote against the proposal enter in the memo field{" "}
                    <span className="font-semibold"> &apos;no {key}&apos;</span>
                  </li>
                </ul>
              </span>
            </div>

            <p className="font-semibold">
              <span className="text-[0.875rem] font-normal">
                Refresh the dashboard - your vote should appear in the dashboard
                within 10-15 minutes
              </span>
            </p>

            <p className="font-semibold">
              <span className="text-[0.875rem] font-normal">
                See Frequently Asked Questions (
                <a
                  href="https://faucet.minaprotocol.com/"
                  className="text-OrangeMINA hover:opacity-80 transition-all duration-200"
                >
                  <u>FAQs</u>
                </a>
                ) for more information, including how to send yourself a
                transaction
              </span>
            </p>
          </div>
        </div>
      </div>
    </div>
  );
};
