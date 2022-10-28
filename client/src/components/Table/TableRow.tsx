import React from "react";
import moment from "moment";
import makeBlockie from "ethereum-blockies-base64";
import { useMediaQuery } from "@react-hook/media-query";

import type { TableRowProps } from "@/types";

import { TableBubble } from "./TableBubble";

export const createPercent = (v: number, t: number): string => {
  const val = (v / t) * 100;
  if (Number.isNaN(val)) {
    return "XXX";
  }
  return val.toFixed(2);
};

export const TableRow: React.FC<TableRowProps> = ({ signal, stats }) => {
  const isMobile = useMediaQuery("only screen and (max-width: 768px)");
  const percent: () => string = (): string => {
    if (
      signal.signal_status === "Settled" ||
      signal.signal_status === "Unsettled"
    ) {
      const total = stats.yes + stats.no;
      if (signal.delegations) {
        return createPercent(
          parseFloat(signal.delegations.delegated_balance),
          total,
        );
      }
    }
    return "---";
  };

  if (isMobile) {
    return (
      <div className="w-full flex items-center justify-center py-4">
        <div className="flex flex-col items-start justify-center gap-0.5 w-full px-2">
          <div className="flex items-center gap-1">
            <img
              className="h-4 w-4 rounded-full opacity-70"
              src={makeBlockie(signal.account)}
            />
            <span className="text-[0.6rem] sm:text-[0.7rem] sm:leading-5 text-grayA-12 text-ellipsis overflow-hidden whitespace-nowrap medium mt-0.5">
              {signal.account}
            </span>
          </div>

          <div className="flex items-center justify-between w-full mt-0.5">
            <div className="flex flex-col gap-0.5">
              <div className="flex items-center justify-between">
                <span className="grid-table-content-mobile semibold">
                  {signal.height} -{" "}
                  {moment(new Date(signal.timestamp)).format(
                    "DD/MM/YYYY - hh:mm",
                  )}
                </span>
              </div>
              <span className="grid-table-content-mobile semibold italic">
                {signal.memo}
              </span>
            </div>

            <div className="flex items-center gap-2">
              <TableBubble status={signal.status}>
                <span className="grid-table-content-mobile medium">
                  {signal.status}
                </span>
              </TableBubble>
              <TableBubble status={signal.signal_status}>
                <span className="grid-table-content-mobile medium">
                  {signal.signal_status}
                </span>
              </TableBubble>
            </div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="grid-table-el w-full py-2 hover:bg-grayA-3 transition-colors duration-100">
      <div className="place-self-center">
        <span className="grid-table-content semibold">{signal.height}</span>
      </div>
      <div className="hidden lg:block place-self-center">
        <span className="grid-table-content semibold">
          {moment(new Date(signal.timestamp)).format("DD/MM/YYYY - hh:mm")}
        </span>
      </div>
      <div className="flex items-center gap-2 lg:gap-3">
        <img
          className="h-5 w-5 lg:w-6 lg:h-6 rounded-full opacity-70"
          src={makeBlockie(signal.account)}
        />
        <span className="grid-table-content medium">{signal.account}</span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content">
          {signal.delegations
            ? `${parseFloat(signal.delegations.delegated_balance).toFixed(4)}`
            : "---"}
        </span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content">
          {signal.delegations ? `${percent()}` : "---"}
        </span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content medium">
          {signal.delegations ? signal.delegations.total_delegators : "---"}
        </span>
      </div>
      <div className="place-self-center">
        <span className="grid-table-content medium">{signal.memo}</span>
      </div>
      <div className="place-self-center">
        <TableBubble status={signal.status}>
          <span className="grid-table-content medium">{signal.status}</span>
        </TableBubble>
      </div>
      <div className="place-self-center">
        <TableBubble status={signal.signal_status}>
          <span className="grid-table-content medium">
            {signal.signal_status}
          </span>
        </TableBubble>
      </div>
    </div>
  );
};
