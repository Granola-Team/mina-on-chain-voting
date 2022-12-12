import {expect, vi} from "vitest";
import "@testing-library/jest-dom";
import React from "react";

import {TableHeader} from "./TableHeader";
import {Layout} from "../Layout/Layout";
import {BrowserRouter as Router} from "react-router-dom";
import {TableBody} from "./TableBody";
import type {BlockStatus, SignalStatus, DelegationEntity} from "@/types";
import {TableRow} from "./TableRow";
import {Table} from "./Table";
import {cleanup, fireEvent, render} from "@testing-library/react";

afterEach(cleanup);

describe("Table Tests", () => {
    const props = {
        stats: {
            yes: 15,
            no: 10,
        },
        query: "magenta",
        isLoading: false,
        data: [
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
        ],
    };

    test("should render", () => {
        render(<TableBody {...props} />);
    });

    test("should render props", () => {
        const view = render(<TableBody {...props} />);
        const stringOne = "magenta";
        const stringTwo = "Canonical";
        const stringThree = "Unsettled";
        const stringFour = "Invalid";
        const stringFive = "5u2ab1qlg2aye0h6yhf6vn3mdgcadvcr0838tcjefu";
        const elementOne = view.getByText(stringOne);
        expect(elementOne).toBeInTheDocument();
        const elementFive = view.getByText(stringFive);
        expect(elementFive).toBeInTheDocument();
    });

//    test("TableBubble colors working", () => {
//        const view = render(<TableBody {...props} />);
//        expect(view.getByText("Pending").closest("div")).toHaveAttribute(
//            "class",
//            "flex items-center justify-center border py-0.5 rounded-3xl w-[4.5rem] lg:w-24 bg-yellowA-4 border-yellowA-7",
//        );
//        expect(view.getByText("Canonical").closest("div")).toHaveAttribute(
//            "class",
//            "flex items-center justify-center border py-0.5 rounded-3xl w-[4.5rem] lg:w-24 bg-greenA-4 border-greenA-7",
//        );
//        expect(view.getByText("Orphaned").closest("div")).toHaveAttribute(
//            "class",
//            "flex items-center justify-center border py-0.5 rounded-3xl w-[4.5rem] lg:w-24 bg-redA-4 border-redA-7",
//        );
//    });

//    test("Table file", () => {
//        const rendered = render(
//            <Router>
//                <Table {...props}>
//                    <TableNavigation/>
//                    <TableBody {...props} />
//                </Table>
//            </Router>,
//        );
//        expect(rendered.getByText("Pending").closest("div")).toHaveAttribute(
//            "class",
//            "flex items-center justify-center border py-0.5 rounded-3xl w-[4.5rem] lg:w-24 bg-yellowA-4 border-yellowA-7",
//        );
//    });
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
    expect(
        createPercent(
            "hello" as unknown as number,
            "helloTwo" as unknown as number,
        ),
    ).toBe("XXX");
});

describe("StatsWeighted Tests", () => {
    const props = {
        signal: {
            height: 10,
            timestamp: 12,
            account: "bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu",
            memo: "magenta",
            status: "Canonical" as BlockStatus,
            signal_status: "Settled" as SignalStatus,
            delegations: {
                delegated_balance: "10.761231314",
                total_delegators: 2,
            } as DelegationEntity,
        },
        stats: {
            yes: 15,
            no: 10,
        },
    };

    test("should render", () => {
        render(<TableRow {...props} />);
    });

    test("TableHeader is rendering", async () => {
        const rendered = render(
            <Router>
                <Layout>
                    <TableHeader/>
                </Layout>
            </Router>,
        );
        expect(rendered.getByText("Timestamp")).toBeInTheDocument();
        expect(rendered.getByText("Account")).toBeInTheDocument();
    });
});

describe("Testing isLoading", () => {
    const props = {
        stats: {
            yes: 15,
            no: 10,
        },
        query: "magenta",
        isLoading: true,
        data: [
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
        ],
    };

    test("isLoading in TableBody", () => {
        render(
            <Router>
                <Table {...props}>
                    <TableBody {...props} />
                </Table>
            </Router>,
        );
    });
});
