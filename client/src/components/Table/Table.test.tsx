import { expect } from "vitest";
import "@testing-library/jest-dom";
import { Table } from "./Table";
import { TableRow } from "./TableRow"
import { TableNavigation } from "./TableNavigation";
import { TableBody } from "./TableBody";
import { TableHeader } from "./TableHeader";
import { Layout } from "./Layout";
import { BrowserRouter as Router } from "react-router-dom";
import { cleanup, render } from "@testing-library/react";
afterEach(cleanup);

const createPercent = (v: number, t: number): string => {
    const val = (v / t) * 100;
    if (Number.isNaN(val)) {
      return "XXX";
    }
    return val.toFixed(2);
};

test("createPercent function in StatsWeighted", async () => {
    expect(createPercent(15, 20)).toBe("75.00");
    expect(createPercent(12, 87)).toBe("13.79");
});

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

/*
const createTestProps = (props?: object): any => ({
    ...props,
});

const renderTest = () => {
    const props = createTestProps();
    const { getByTestId } = render(
        <Router>
            <Table {...props}>
                <div data-testid="child" />
            </Table>
        </Router>
    );
    const container = getByTestId("table-container");
    return {
        getByTestId,
        container
    };
};

describe("TableContainer", () => {

    describe("rendering", () => {
        test("a container", () => {
            const { container } = renderTest();
            expect(container).toHaveStyle(`
                boxSizing: 'border-box';
                display: flex;
                flexWrap: 'wrap';
                width: '100%';
            `);
        });

        test("children are passed through", () => {
            const { container, getByTestId } = renderTest();
            expect(container.children.length).toBe(1);
            expect(getByTestId("child")).toBeDefined();
        });
    });
});
*/