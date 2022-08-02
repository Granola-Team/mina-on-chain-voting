import React from "react";
import { Dialog, Transition } from "@headlessui/react";

export const Modal: React.FC<{
  children: React.ReactNode;
  state: boolean;
  setState: (v: boolean) => void;
}> = ({ children, state, setState }) => {
  return (
    <Transition.Root show={state} as={React.Fragment}>
      <Dialog
        as="div"
        className="relative z-10"
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
          <div className="fixed inset-0 transition-opacity bg-gray-200/10 backdrop-blur-sm dark:bg-gray-600/10 dark:backdrop-blur-md" />
        </Transition.Child>

        <div className="fixed left-0 right-0 top-40 z-10 overflow-y-auto">
          <div className="flex items-end justify-center min-h-full p-4 text-center sm:items-center sm:p-0">
            {children}
          </div>
        </div>
      </Dialog>
    </Transition.Root>
  );
};
