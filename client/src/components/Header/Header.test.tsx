import { expect } from "vitest";
import { render } from "@testing-library/react";
import "@testing-library/jest-dom";
import { Header } from "./Header";
import { SearchControl } from "../Search/SearchControl";
import { SettingsControl } from "../Settings/SettingsControl";
import { Search } from "../Search/Search";
import { Settings } from "../Settings/Settings";
import { shallow } from "enzyme";

// simulate window resize
function fireResize(width: any) {
    window.innerWidth = width;
    window.dispatchEvent(new Event("resize"));
}
// Test component that uses the Hook
function EffecfulComponent() {
    const viewport = <Header />;
    return <span>{viewport}</span>;
}

test("Header listen to window resize and set viewport size responsively", async () => {
    const { container, rerender } = render(<EffecfulComponent />);
    const span = container.firstChild;

    fireResize(767);

    rerender(<EffecfulComponent />);
    expect(span.textContent).toBe("isMobile");

    fireResize(769);

    rerender(<EffecfulComponent />);
    expect(span.textContent).toBe("!isMobile");
});

test("renders Child components", async () => {
    const wrapper = shallow(<Header store={store} />);
    expect(wrapper.containsMatchingElement(<SearchControl />)).toEqual(true);
});

test("renders Child components", async () => {
    const wrapper = shallow(<Header store={store} />);
    expect(wrapper.containsMatchingElement(<SettingsControl />)).toEqual(true);
});

test("renders Child components", async () => {
    const wrapper = shallow(<Header store={store} />);
    expect(wrapper.containsMatchingElement(<Search />)).toEqual(true);
});

test("renders Child components", async () => {
    const wrapper = shallow(<Header store={store} />);
    expect(wrapper.containsMatchingElement(<Settings />)).toEqual(true);
});
