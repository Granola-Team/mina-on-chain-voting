import { expect } from "vitest";
import "@testing-library/jest-dom";
import { createPercent } from "./StatsWeighted"
// import type { ModalProps } from "@/types";
import { cleanup, render } from "@testing-library/react";
afterEach(cleanup);

test("createPercent function in StatsWeighted", async() => {
    expect(createPercent(15, 20)).toBe("75.00");
    expect(createPercent(12, 87)).toBe("13.79");
    expect(createPercent(12, "80")).toBe("XXX");
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