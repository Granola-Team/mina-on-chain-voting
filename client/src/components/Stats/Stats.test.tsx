import { expect } from "vitest";
import "@testing-library/jest-dom";
import { StatsWeighted } from "./StatsWeighted";
import { Layout } from "../Layout/Layout";
import { BrowserRouter as Router } from "react-router-dom";
import { fireEvent, render } from "@testing-library/react";
import { IconTooltip } from "../Tooltip";
import * as TooltipPrimitive from "@radix-ui/react-tooltip";
import { keyframes } from "@stitches/react"; // might need to add styled back
// import {createStyled} from '@stitches/react'
const { createStyled } = require('@stitches/react'); // using require () because importing didn't work
// import {matchers} from 'jest-stitches';
const { matchers } = require('jest-stitches'); // using require () because importing didn't work
const {styled, css} = createStyled({})
// Add the custom matchers provided by 'jest-stitches'
expect.extend(matchers)

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

const stats = {
  yes: 15,
  no: 20,
};

test("Signals Results bar is rendering", async () => {

    const AddStyle = styled(IconTooltip, {
      [`& ${IconTooltip}`]: {
        length: 400,
      },
    });

    const bar = render(
        <Router>
        <Layout>
            <AddStyle>
              <StatsWeighted stats={stats} network={""} />
            </AddStyle>
        </Layout>
        </Router>
    );
    console.log(bar);
    expect(bar.getByText("Signal Results")).toBeInTheDocument();
    expect(bar.getByText("Yes")).toBeInTheDocument();
    expect(bar.getByText("No")).toBeInTheDocument();
});

/*        /////// [`& ${IconTooltip}`]: { length = 40 }

const StyledToolTip = styled(TooltipPrimitive.Content, {
  borderRadius: 10,
  padding: "10px 15px",
  maxWidth: "18rem",
  userSelect: "none",
  "@media (prefers-reduced-motion: no-preference)": {
    animationDuration: "400ms",
    animationTimingFunction: "cubic-bezier(0.16, 1, 0.3, 1)",
    willChange: "transform, opacity",
    '&[data-state="delayed-open"]': {
      '&[data-side="top"]': {
        animationName: keyframes({
          "0%": { opacity: 0, transform: "translateY(-2px)" },
          "100%": { opacity: 1, transform: "translateY(0)" },
        }),
      },
      '&[data-side="right"]': {
        animationName: keyframes({
          "0%": { opacity: 0, transform: "translateX(2px)" },
          "100%": { opacity: 1, transform: "translateX(0)" },
        }),
      },
      '&[data-side="bottom"]': {
        animationName: keyframes({
          "0%": { opacity: 0, transform: "translateX(2px)" },
          "100%": { opacity: 1, transform: "translateX(0)" },
        }),
      },
      '&[data-side="left"]': {
        animationName: keyframes({
          "0%": { opacity: 0, transform: "translateX(-2px)" },
          "100%": { opacity: 1, transform: "translateX(0)" },
        }),
      },
    },
  },
});

test("Tooltip in Signals Results bar is rendering", async () => {
    const rendered = render(
      <Router>
        <Layout>
          <StatsWeighted stats={stats} network={""} >
            <StyledToolTip>
              <IconTooltip css={""} children={undefined} />
            </StyledToolTip>
          </StatsWeighted>
        </Layout>
      </Router>,
    );
    fireEvent.mouseOver(await rendered.findByTestId("connection-sign"));
    expect(
      await rendered.findByText(
        "signals that adhere to our signalling convention",
      ),
    ).toBeInTheDocument();
});
*/