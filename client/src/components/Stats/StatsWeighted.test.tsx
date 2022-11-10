import "@testing-library/jest-dom";
import { render, screen } from "@testing-library/react";
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
  expect(createPercent("hello" as unknown as number, "helloTwo" as unknown as number)).toBe("XXX");
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

  test("should render props and checks signals bar functionality", () => {
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

    const title = view.getByText("Signal Results");
    expect(title).toBeInTheDocument();
    const network = props.network[0].toUpperCase() + props.network.slice(1).toLowerCase();
    expect(network).toBe("Mainnet");

    expect(string).toBe("66.67%");
    expect(screen.getByText("66.67%")).toHaveAttribute("class", "text-sm md:text-lg semibold text-green-11");
  });
});
