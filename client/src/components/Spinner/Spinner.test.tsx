import { expect } from "vitest";
import { render } from "@testing-library/react";
import { Spinner } from "./Spinner";

test("Spinner is rendered", async () => {
  const spinner = render(
    <Spinner />
  );
  expect(spinner).toMatchSnapshot();
});
