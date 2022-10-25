import { expect } from "vitest";
import "@testing-library/jest-dom";
import { Table, TableRow } from "./Table";
// import type { ModalProps } from "@/types";
import { BrowserRouter as Router } from "react-router-dom";
import { cleanup, render } from "@testing-library/react";
afterEach(cleanup);

test("createPercent function in TableRow", async() => {
    expect(TableRow.createPercent(15, 20).toBeEqual(75.00));
    expect(TableRow.createPercent(12, 80).toBeEqual((12/80*100).toFixed(2)));
    expect(TableRow.createPercent(12, "80").toBeEqual("XXX"));
}); // gives TypeError: Cannot read properties of undefined (reading 'createPercent')



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