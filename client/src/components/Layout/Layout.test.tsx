import { expect } from "vitest";
import { screen, fireEvent, render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Layout } from "./Layout";
import type { ComponentWithChildren } from "@/types";
import { shallow, configure } from "enzyme";
import Adapter from "enzyme-adapter-react-16";
import { Header } from "../Header";
import { Footer } from "../Footer";

configure({adapter: new Adapter()});
test("Header is rendered", async () => {
    const layout = shallow(<Layout><div><Header /></div> </Layout>);
    expect(layout.getElements()).toMatchSnapshot();
});

test("Footer is rendered", async () => {
    const layout = shallow(<Layout><div><Footer /></div> </Layout>);
    expect(layout.getElements()).toMatchSnapshot();
});

test("Children are passed", async () => {
    const MyComponent: React.FC<ComponentWithChildren> = ({ children }) => {
        return (
            <div>{children}</div>
            );
        };
    const layout = shallow(<Layout><div><MyComponent children={undefined} /></div> </Layout>);
    expect(layout.getElements()).toMatchSnapshot();
});
