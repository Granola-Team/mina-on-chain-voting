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
