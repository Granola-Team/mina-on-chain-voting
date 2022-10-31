import { expect, vi } from "vitest";
import "@testing-library/jest-dom";
import { TableHeader } from "./TableHeader";
import { Layout } from "../Layout/Layout";
import { BrowserRouter as Router } from "react-router-dom";
import { cleanup, fireEvent, render } from "@testing-library/react";
import React from "react";
import { TableNavigation } from "./TableNavigation";

afterEach(cleanup);

test("TableHeader is rendering", async () => {
  const rendered = render(
    <Router>
      <Layout>
        <TableHeader />
      </Layout>
    </Router>,
  );
  expect(rendered.getByText("Timestamp")).toBeInTheDocument();
  expect(rendered.getByText("Account")).toBeInTheDocument();
  expect(rendered.getByText("Delegated Stake")).toBeInTheDocument();
  expect(rendered.getByText("Delegated Stake %")).toBeInTheDocument();
  expect(rendered.getByText("Classification")).toBeInTheDocument();
});

const createPercent = (v: number, t: number): string => {
  const val = (v / t) * 100;
  if (Number.isNaN(val)) {
    return "XXX";
  }
  return val.toFixed(2);
};

test("TableRow createPercent function", async () => {
  expect(createPercent(15, 20)).toBe("75.00");
  expect(createPercent(12, 87)).toBe("13.79");
});

describe("testing TableRow, TableBubble, TableNavigation, and TableNavElement", () => {
  test("TableNavigation and TableNavElement working", async () => {
    const setStateMock = vi.fn();
    const useStateMock: any = (useState: any) => [useState, setStateMock];
    vi.spyOn(React, "useState").mockImplementation(useStateMock);

    const rendered = render(
      <Router>
        <Layout>
          <TableNavigation />
        </Layout>
      </Router>,
    );

    const SettledCheck = rendered.getByRole("button", { name: "Settled" });
    expect(SettledCheck).toBeInTheDocument();
    fireEvent.click(SettledCheck);
    expect(setStateMock).toHaveBeenCalled();

    const UnsettledCheck = rendered.getByRole("button", { name: "Unsettled" });
    expect(UnsettledCheck).toBeInTheDocument();
    fireEvent.click(UnsettledCheck);
    expect(setStateMock).toHaveBeenCalled();

    const InvalidCheck = rendered.getByRole("button", { name: "Invalid" });
    expect(InvalidCheck).toBeInTheDocument();
    fireEvent.click(InvalidCheck);
    expect(setStateMock).toHaveBeenCalled();
  });
});
