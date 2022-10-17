import { expect } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Layout } from "./Layout";
import type { ComponentWithChildren } from "@/types";
import { shallow } from "enzyme";

test("Header is rendered", async () => {
    const layout = shallow(<Layout children={undefined} />);
    expect(layout.getElements()).toMatchSnapshot();
  });

test("Footer is rendered", async () => {
    const { getByText } = render(<Layout children={undefined} />);
    expect(getByText("Made with ❤️ by Granola")).toBeInTheDocument();
});

test("Header is rendered by grabbing ", async () => {
    const { getByText } = render(<Layout children={undefined} />);
    expect(getByText("Mina On-Chain Signals")).toBeInTheDocument();
});

