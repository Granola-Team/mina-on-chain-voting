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
    const string = "Epoch 24 | Slot 5000";
    const element = view.getByText(string);
    expect(element).toBeInTheDocument();
  });

  test("title renders", () => {
    const view = render(<EpochTiming {...props} />);
    expect(view.getByText("Voting Period")).toBeInTheDocument();
  });

  test("bar meter percentage", () => {
    const view = render(<EpochTiming {...props} />);
    const percentage = (props.slot / 7140) * 89.78;
    const oppositePercentage = 100 - percentage;
    expect(view.getByTestId("bar-percentage"))
      .toHaveAttribute("style", `margin-right: ${oppositePercentage}%;`);
  });

  test("countdown displays", () => {
    const view = render(<EpochTiming {...props} />);

    const endSlotMinutes = (7140 - props.slot) * 3;
    const days = Math.floor(endSlotMinutes / 60 / 24);
    const hours = Math.floor(((endSlotMinutes / 60 / 24) - days) * 24);
    const minutes = Math.floor(((((endSlotMinutes / 60 / 24) - days) * 24) - hours) * 60);

    const stringTwo = `Next epoch estimated to begin in ${days} days ${hours} hours ${minutes} minutes`;
    const elementTwo = view.getByText(stringTwo);
    expect(elementTwo).toBeInTheDocument();
  });
});
