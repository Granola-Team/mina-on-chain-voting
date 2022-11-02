import "@testing-library/jest-dom";
import { render } from "@testing-library/react";
import moment from "moment";
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

  test("title renders", () => {
    const view = render(<EpochTiming {...props} />);
    expect(view.getByText("Signal Timing")).toBeInTheDocument();
  });

  test("bar meter percentage", () => {
    const view = render(<EpochTiming {...props} />);
    const percentage = (props.slot / 7140) * 89.78;
    expect(view.getByTestId("bar-percentage"))
      .toHaveAttribute("style", `margin-left: ${percentage}%;` );
  });
});
