import { expect, vi } from "vitest";
import "@testing-library/jest-dom";
import { Table } from "./Table";
import { TableHeader } from "./TableHeader";
import { Layout } from "../Layout/Layout";
import { BrowserRouter as Router } from "react-router-dom";
import type { BlockStatus, DelegationEntity, SignalStatus } from "@/types";
import { cleanup, fireEvent, render } from "@testing-library/react";
import React from "react";
import { TableNavigation } from "./TableNavigation";
afterEach(cleanup);

test("TableHeader is rendering", async () => {
    const rendered = render(
        <Router>
            <Layout>
                <TableHeader />
            </Layout>
        </Router>,
    );
    expect(rendered.getByText("Timestamp")).toBeInTheDocument();
    expect(rendered.getByText("Account")).toBeInTheDocument();
    expect(rendered.getByText("Delegated Stake")).toBeInTheDocument();
    expect(rendered.getByText("Delegated Stake %")).toBeInTheDocument();
    expect(rendered.getByText("Classification")).toBeInTheDocument();
});

const createPercent = (v: number, t: number): string => {
    const val = (v / t) * 100;
    if (Number.isNaN(val)) {
      return "XXX";
    }
    return val.toFixed(2);
};

test("TableRow createPercent function", async () => {
    expect(createPercent(15, 20)).toBe("75.00");
    expect(createPercent(12, 87)).toBe("13.79");
});

const stats = {
    yes: 15,
    no: 10,
};

const data = [
        {
            height: 10,
            timestamp: 12,
            account: "bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu",
            memo: "magenta",
            status: "Canonical" as BlockStatus,
            signal_status: "Settled" as SignalStatus,
            delegations: {
                delegated_balance: "ten",
                total_delegators: 2,
            } as DelegationEntity,
        },
        {
            height: 8,
            timestamp: 11,
            account: "ab1qlg2ayye0h6hf5u26vn3mdgcadvcr0838tcjefu",
            memo: "no magenta",
            status: "Pending" as BlockStatus,
            signal_status: "Unsettled" as SignalStatus,
            delegations: {
                delegated_balance: "ten",
                total_delegators: 2,
            } as DelegationEntity,
        },
        {
            height: 7,
            timestamp: 10,
            account: "5u2ab1qlg2aye0h6yhf6vn3mdgcadvcr0838tcjefu",
            memo: "no@#$%magenta",
            status: "Orphaned" as BlockStatus,
            signal_status: "Invalid" as SignalStatus,
            delegations: null,
        },
];

describe("testing TableRow, TableBubble, TableNavigation, and TableNavElement", () => {
    const query = "magenta";
    const isLoading = false;

    test("TableRow and TableBubble colors working", async () => {
        const rendered = render(
            <Router>
                <Layout>
                    <Table data={data} query={query} isLoading={isLoading} stats={stats} />
                </Layout>
            </Router>,
        );

        expect(rendered.getByText("Pending").closest("div#flex"))
            .toHaveAttribute("class", "flex items-center justify-center border py-0.5 rounded-3xl w-[4.5rem] lg:w-24 bg-yellowA-4 border-yellowA-7");
        expect(rendered.getByText("Canonical").closest("div#flex"))
            .toHaveAttribute("class", "flex items-center justify-center border py-0.5 rounded-3xl w-[4.5rem] lg:w-24 bg-greenA-4 border-greenA-7");
        expect(rendered.getByText("Orphaned").closest("div#flex"))
            .toHaveAttribute("class", "flex items-center justify-center border py-0.5 rounded-3xl w-[4.5rem] lg:w-24 bg-redA-4 border-redA-7");
    });

    test("TableNavigation and TableNavElement working", async () => {
        const setStateMock = vi.fn();
        const useStateMock: any = (useState: any) => [useState, setStateMock];
        vi.spyOn(React, "useState").mockImplementation(useStateMock);

        const rendered = render(
            <Router>
                <Layout>
                    <TableNavigation />
                </Layout>
            </Router>,
        );

        const SettledCheck = rendered.getByRole("button", { name: "Settled" });
        expect(SettledCheck).toBeInTheDocument();
        fireEvent.click(SettledCheck);
        expect(setStateMock).toHaveBeenCalled();

        const UnsettledCheck = rendered.getByRole("button", { name: "Unsettled" });
        expect(UnsettledCheck).toBeInTheDocument();
        fireEvent.click(UnsettledCheck);
        expect(setStateMock).toHaveBeenCalled();

        const InvalidCheck = rendered.getByRole("button", { name: "Invalid" });
        expect(InvalidCheck).toBeInTheDocument();
        fireEvent.click(InvalidCheck);
        expect(setStateMock).toHaveBeenCalled();
    });
});
