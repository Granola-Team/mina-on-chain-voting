import "@testing-library/jest-dom";
import { render } from "@testing-library/react";
import { EpochTiming } from "./EpochTiming";

describe("EpochTiming Tests", () => {
  const props = {
    epoch: 24,
    slot: 5000,
  };

  test("should render", () => {
    render(<EpochTiming {...props} />);
  });

  test("should render props", () => {
    const view = render(<EpochTiming {...props} />);
    const string = "Epoch 24 - Slot 5000";
    const element = view.getByText(string);
    expect(element).toBeInTheDocument();
  });
});
