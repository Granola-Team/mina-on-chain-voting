import { expect } from "vitest";
import "@testing-library/jest-dom";
import { IconTooltip } from "./IconTooltip"
import { Layout } from "../Layout/Layout"
import { StatsWeighted } from "../Stats/StatsWeighted";
import { BrowserRouter as Router } from "react-router-dom";
import { fireEvent, render } from "@testing-library/react";

test("createPercent function in StatsWeighted", async() => {
    // I probably need to delete children in the component below
    const rendered = render(
        <Router>
            <Layout>
                <StatsWeighted stats={undefined} />
            </Layout>
        </Router>
    ); // this test could probably go in statsweighted component test
    // but then I would need to think what to test for this component
    // To hover element and show tooltip --> may need to add data-testid="connection-sign" to IconTooltip.tsx file
    fireEvent.mouseOver(await rendered.findByTestId("connection-sign"));
    expect(await rendered
        .findByText("signals that adhere to our signalling convention"))
        .toBeInTheDocument();
});

