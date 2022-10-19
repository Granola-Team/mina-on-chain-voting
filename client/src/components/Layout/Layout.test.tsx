import React from "react";
import { expect } from "vitest";
import "@testing-library/jest-dom";
import { Layout } from "./Layout";
import type { ComponentWithChildren } from "@/types";
import { Header } from "../Header";
import { Footer } from "../Footer";
import { screen, render } from "@testing-library/react";
import { Router } from "react-router-dom";

import { configure, shallow } from "enzyme";
import Adapter from "enzyme-adapter-react-16";


configure({ adapter: new Adapter() });
test("Header is rendered", async () => {
    const layout = shallow(<Layout><Header /></Layout>);
    expect(layout.getElements()).toMatchSnapshot();
});

test("Footer is rendered", async () => {
    const layout = shallow(<Layout><Footer /></Layout>);
    expect(layout.getElements()).toMatchSnapshot();
});

// below is code that looks like it should work for RTL

/*
test("Footer is rendered", async () => {
    render(<Layout><Footer/></Layout>);
    expect(screen.getByText("Made with ❤️ by Granola")).toBeInTheDocument();
});

test("Header is rendered", async () => {
    render(<Layout><Header/></Layout>);
    expect(screen.getByText("Mina On-Chain Signals")).toBeInTheDocument();
});
*/