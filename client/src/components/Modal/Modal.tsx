import React from "react";
import { Dialog, Transition } from "@headlessui/react";

import type { ModalProps } from "@/types";

export const Modal: React.FC<ModalProps> = ({ children, state, setState }) => {
  return (
    <Transition.Root show={state} as={React.Fragment}>
      <Dialog
        as="div"
        className="absolute z-10 inset-0"
        onClose={() => {
          setState(false);
        }}
      >
        <Transition.Child
          as={React.Fragment}
          enter="ease-out duration-300"
          enterFrom="opacity-0"
          enterTo="opacity-100"
          leave="ease-in duration-200"
          leaveFrom="opacity-100"
          leaveTo="opacity-0"
        >
          <div className="absolute inset-0 transition-opacity backdrop-blur-[8px] bg-gray-600/5 dark:bg-gray-900/5 dark:backdrop-blur-md" />
        </Transition.Child>

        <div className="absolute top-44 left-0 right-0 w-full z-10"
        data-testid="modal-container">
          <div className="flex items-center justify-center p-2 text-center lg:p-0">
            {children}
          </div>
        </div>
      </Dialog>
    </Transition.Root>
  );
};
