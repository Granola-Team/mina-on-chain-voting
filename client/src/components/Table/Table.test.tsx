import { expect } from "vitest";
import "@testing-library/jest-dom";
import { Table } from "./Table";
import { TableRow } from "./TableRow"
import { TableNavigation } from "./TableNavigation";
import { TableBody } from "./TableBody";
import { TableHeader } from "./TableHeader";
import { Layout } from "../Layout/Layout";
import { BrowserRouter as Router } from "react-router-dom";
import { cleanup, fireEvent, render } from "@testing-library/react";
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

describe("testing TableRow, TableBubble, TableNavigation and TableNavElement", () => {

    const stats = {
        yes: 15,
        no: 10,
    };

    const data = {
        signals: [
            {
                height: 10,
                timestamp: 12,
                account: "bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu",
                memo: "magenta",
                status: "Canonical",
                signal_status: "Settled",
                delegations: {
                    delegated_balance: "ten",
                    total_delegators: 2,
                }
            },
            {
                height: 8,
                timestamp: 11,
                account: "ab1qlg2ayye0h6hf5u26vn3mdgcadvcr0838tcjefu",
                memo: "no magenta",
                status: "Pending",
                signal_status: "Unsettled",
                delegations: {
                    delegated_balance: "ten",
                    total_delegators: 2,
                }
            },
            {
                height: 7,
                timestamp: 10,
                account: "5u2ab1qlg2aye0h6yhf6vn3mdgcadvcr0838tcjefu",
                memo: "no@#$%magenta",
                status: "Orphaned",
                signal_status: "Invalid",
                delegations: null,
            },
        ],
        stats: stats,
    };

    const query = "test";
    const isLoading = false;

    test("TableRow and TableBubble colors working", async () => {
        const rendered = render(
            <Router>
                <Layout>
                    <Table data={data} query={query} isLoading={isLoading} stats={stats} />
                </Layout>
            </Router>,
        );

        expect(rendered.getByText("Canonical")).toHaveAttribute("bg-greenA-4");
        expect(rendered.getByText("Pending")).toHaveAttribute("bg-yellowA-4");
        expect(rendered.getByText("Orphaned")).toHaveAttribute("bg-redA-4");
    });

    test("TableNavigation and TableNavElement working", async () => {
        const rendered = render(
            <Router>
                <Layout>
                    <Table data={data} query={query} isLoading={isLoading} stats={stats} />
                </Layout>
            </Router>,
        );

        const SettledCheck = rendered.getByText("Settled");
        expect(SettledCheck).toBeInTheDocument();
        fireEvent.click(SettledCheck);
        expect(screen.queryByText("Unsettled")).not.toBeInTheDocument();

        const UnsettledCheck = rendered.getByText("Settled");
        expect(UnsettledCheck).toBeInTheDocument();
        fireEvent.click(UnsettledCheck);
        expect(screen.queryByText("Invalid")).not.toBeInTheDocument();

        const InvalidCheck = rendered.getByText("Settled");
        expect(InvalidCheck).toBeInTheDocument();
        fireEvent.click(InvalidCheck);
        expect(screen.queryByText("Settled")).not.toBeInTheDocument();
    });

});

describe("testing TableBody error working properly", () => {

    const stats = {
        yes: 15,
        no: 10,
    };

    const data = {
        signals: [
            {
                height: 10,
                timestamp: 12,
                account: "bc1qlg2ayye0h6hf5u26vn3mdgcadvcr3808tcjefu",
                memo: "magenta",
                status: "Canonical",
                signal_status: "Settled",
                delegations: {
                    delegated_balance: "ten",
                    total_delegators: 2,
                }
            },
            {
                height: 8,
                timestamp: 11,
                account: "ab1qlg2ayye0h6hf5u26vn3mdgcadvcr0838tcjefu",
                memo: "no magenta",
                status: "Pending",
                signal_status: "Unsettled",
                delegations: {
                    delegated_balance: "ten",
                    total_delegators: 2,
                }
            },
            {
                height: 7,
                timestamp: 10,
                account: "5u2ab1qlg2aye0h6yhf6vn3mdgcadvcr0838tcjefu",
                memo: "no@#$%magenta",
                status: "Orphaned",
                signal_status: "Invalid",
                delegations: null,
            },
        ],
        stats: stats,
    };

    const query = "test";
    const isLoading = false;

    test("TableBody throws error when appropiate", async () => {
        const rendered = render(
            <Router>
                <Layout>
                    <Table data={data} query={query} isLoading={isLoading} stats={stats} />
                </Layout>
            </Router>,
        );

        // need to finish this last test

    });

});
