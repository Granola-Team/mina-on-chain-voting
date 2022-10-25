import { expect } from "vitest";
import "@testing-library/jest-dom";
import { createPercent, StatsWeighted } from "./StatsWeighted";
import { Layout } from "../Layout/Layout";
import { BrowserRouter as Router } from "react-router-dom";
import { fireEvent, render } from "@testing-library/react";
import { stat } from "fs";

test("createPercent function in StatsWeighted", async() => {
    expect(createPercent(15, 20)).toBe("75.00");
    expect(createPercent(12, 87)).toBe("13.79");
});

test("Signals Results bar is rendering", async() => {
    const bar = render(
        <Router>
            <Layout>
                <StatsWeighted stats={stats}>
            </Layout>
        </Router>
    );
    expect(bar.getByText("Signal Results")).toBeInTheDocument();
    expect(bar).toMatchSnapshot();
});

test("Tooltip in Signals Results bar is rendering", async() => {
    const rendered = render(
        <Router>
            <Layout>
                <StatsWeighted stats={undefined} />
            </Layout>
        </Router>
    );
    fireEvent.mouseOver(await rendered.findByTestId("connection-sign"));
    expect(await rendered
        .findByText("signals that adhere to our signalling convention"))
        .toBeInTheDocument();
});
