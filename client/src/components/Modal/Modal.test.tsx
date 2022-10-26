import { expect } from "vitest";
import "@testing-library/jest-dom";
import { Modal } from "./Modal";
import { Transition } from "@headlessui/react";
import React from "react";
import { cleanup, fireEvent, render } from "@testing-library/react";
afterEach(cleanup);

const createTestProps = (props?: object): any => ({
    ...props,
});

const renderTest = () => {
    const props = createTestProps();
    const { queryByTestId } = render(
        <Transition show={false} as={React.Fragment}>
            <Modal {...props}>
                <div data-testid="child" />
            </Modal>
        </Transition>
    );
    const container = queryByTestId("modal-container");
    return {
        queryByTestId,
        container
    };
};

describe("ModalContainer", () => {

    describe("rendering", () => {
    test("a container", () => {
        const { container } = renderTest();
        expect(container!).toHaveStyle(`
            boxSizing: 'border-box';
            display: flex;
            flexWrap: 'wrap';
            width: '100%';
        `);
    });

    test("children are passed through", () => {
        const { container, queryByTestId } = renderTest();
        expect((container! as HTMLElement).children.length).toBe(1);
        expect(queryByTestId("child")).toBeDefined();
    });

    });
});
