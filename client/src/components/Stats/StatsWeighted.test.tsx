import "@testing-library/jest-dom";
import { render } from "@testing-library/react";
import { StatsWeighted } from "./StatsWeighted";

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

describe("StatsWeighted Tests", () => {
  const props = {
    stats: {
      yes: 50,
      no: 25,
    },
    network: "mainnet",
  };

  test("should render", () => {
    render(<StatsWeighted {...props} />);
  });

  test("should render props", () => {
    const total = props.stats.yes + props.stats.no;
    const yesPercent = createPercent(props.stats.yes, total);
    const noPercent = createPercent(props.stats.no, total);

    const view = render(<StatsWeighted {...props} />);
    const string = `${yesPercent}%`;
    const stringTwo = `${noPercent}%`;
    const element = view.getByText(string);
    const elementTwo = view.getByText(stringTwo);
    expect(element).toBeInTheDocument();
    expect(elementTwo).toBeInTheDocument();
  });
});

/*
test("Signals Results bar is rendering", async () => {
  const length = keyframes({
    '0%': { length: 0 },
    '100%': { length: 400 },
  });
  const MoreStyle = styled('IconTooltip', {
    '&:animationName': {
      length: `${length} 400ms`,
    },
  });
  const AddStyle = styled("IconTooltip", {
    [`& ${IconTooltip}`]: {
      length: 400,
      animationName: `${IconTooltip} 400ms`,
    },
  });

  const bar = render(
        <Router>
        <Layout>
            <MoreStyle>
              <AddStyle>
              <StatsWeighted stats={stats} network={""} />
            </AddStyle>
            </MoreStyle>
        </Layout>
        </Router>
    );

    expect(bar.getByText("Signal Results")).toBeInTheDocument();
    expect(bar.getByText("Yes")).toBeInTheDocument();
    expect(bar.getByText("No")).toBeInTheDocument();
});
/*
test("Tooltip in Signals Results bar is rendering", async () => {
  const scaleUp = keyframes({
    '0%': { transform: 'scale(1)' },
    '100%': { transform: 'scale(1.5)' },
  });
  const AddStyle = styled("IconTooltip", {
    [`& ${IconTooltip}`]: {
      length: 400,
      animation: `${IconTooltip} 400ms`,
    },
  });
  const MoreStyle = styled('IconTooltip', {
    '&:length': {
      animation: `${scaleUp} 400ms`,
    },
  });
      const rendered = render(
        <Router>
        <Layout>
            <MoreStyle>
            <AddStyle>
              <StatsWeighted stats={stats} network={""} />
            </AddStyle>
            </MoreStyle>
        </Layout>
        </Router>
    );
    fireEvent.mouseOver(await rendered.findByTestId("connection-sign"));
    expect(
      await rendered.findByText(
        "signals that adhere to our signalling convention",
      ),
    ).toBeInTheDocument();
});
*/