import { useKeywordStore } from "@/pages/Keyword/Keyword.store";
import React from "react";
import shallow from "zustand/shallow";

export const Instructions: React.FC = () => {
  const { key } = useKeywordStore((state) => ({ key: state.key }), shallow);

  return (
    <div className="content-full-width px-2 md:px-4 lg:px-8 mt-4 md:mt-6">
      <div className="bg-gray-2 border border-gray-7 rounded-xl w-full">
        <div className="flex flex-col justify-center px-3 md:px-6 py-2 gap-1.5">
          <div className="flex-col flex md:flex-row items-center justify-between">
            <span className="semibold text-lg md:text-xl leading-8">
              Instructions For Testnet
            </span>
          </div>
          <div className="flex flex-col items-start justify-start md:gap-1">
            <p className="font-semibold">
              Step 1 -{" "}
              <span className="text-[0.875rem] font-normal">
                Create your wallet (Auro, Clorio & Staking Power)
              </span>
            </p>

            <p className="font-semibold">
              Step 2 -{" "}
              <span className="text-[0.875rem] font-normal">
                Get testnet MINA from the official{" "}
                <a
                  href="https://faucet.minaprotocol.com/"
                  className="text-OrangeMINA hover:opacity-80 transition-all duration-200"
                >
                  <u>Faucet</u>
                </a>
              </span>
            </p>

            <p className="font-semibold">
              Step 3 -{" "}
              <span className="text-[0.875rem] font-normal">
                To vote, send yourself a transaction with the memo{" "}
                <span className="font-semibold">&apos;{key}&apos;</span>
              </span>
            </p>

            <p>
              <span className="text-[0.875rem] font-normal">
                Your vote should appear in 10-15 minutes
              </span>
            </p>
          </div>
          <div className="flex-col flex md:flex-row items-center justify-between">
            <span className="semibold text-lg md:text-xl leading-8">
              Instructions For Mainnet
            </span>
          </div>
          <div className="flex flex-col items-start justify-start md:gap-1">
            <p className="font-semibold">
              {" "}
              <span className="text-[0.875rem] font-normal">
              Send yourself a transaction with the keyword(s) in the memo field
              <ul>
                To vote in favor of the proposal enter {" "} in the memo field
                To vote against the proposal enter no {" "} in the memo field
                <span className="font-semibold">&apos;{key}&apos;</span>
              </ul>
              </span>
            </p>

            <p className="font-semibold">
              <span className="text-[0.875rem] font-normal">
                Refresh the dashboard - your vote should appear in the dashboard within 10-15 minutes
                <span className="font-semibold">&apos;{key}&apos;</span>
              </span>
            </p>

            <p className="font-semibold">
              <span className="text-[0.875rem] font-normal">
              See Frequently Asked Questions for more information ({" "}), including how to send yourself a transaction
                <a
                  href="https://docs.google.com/document/d/1cMq2QoE_n61QGycsxRhCPirIAWNDXeWf/edit"
                  className="font-semibold hover:opacity-80 transition-all duration-200"
                >
                  FAQs
                </a>
              </span>
            </p>

          </div>
        </div>
      </div>
    </div>
  );
};
