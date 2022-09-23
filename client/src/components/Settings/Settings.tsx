import React from "react";
import shallow from "zustand/shallow";
import { Dialog, Transition } from "@headlessui/react";
import { AdjustmentsIcon, XIcon } from "@heroicons/react/outline";

import { useAppStore } from "@/App.store";

import { SettingsMenu } from "./SettingsMenu";
import { Modal } from "@/components/Modal";

export const Settings = () => {
  const { settingsActive, setSettingsState } = useAppStore(
    (state) => ({
      settingsActive: state.settingsActive,
      setSettingsState: state.setSettingsState,
    }),
    shallow,
  );

  return (
    <Modal state={settingsActive} setState={setSettingsState}>
      <Transition.Child
        as={React.Fragment}
        enter="ease-out duration-300"
        enterFrom="opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
        enterTo="opacity-100 translate-y-0 sm:scale-100"
        leave="ease-in duration-200"
        leaveFrom="opacity-100 translate-y-0 sm:scale-100"
        leaveTo="opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
      >
        <Dialog.Panel className="relative w-full overflow-hidden text-left border rounded-lg shadow-lg border-gray-7 sm:my-8 sm:max-w-lg">
          <div className="px-4 pt-5 pb-4 bg-gray-3 sm:p-6 sm:pb-4">
            <div className="pb-3 border-b sm:flex sm:items-center border-b-grayA-5">
              <div className="hidden md:flex items-center justify-center flex-shrink-0 w-12 h-12 mx-auto rounded-full bg-grayA-5 sm:mx-0 sm:h-10 sm:w-10">
                <AdjustmentsIcon
                  className="w-6 h-6 text-gray-12"
                  aria-hidden="true"
                />
              </div>
              <div className="flex items-center justify-between w-full mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left">
                <Dialog.Title
                  as="h3"
                  className="text-xl font-semibold leading-6"
                >
                  OSC Controls
                </Dialog.Title>
                <XIcon
                  onClick={() => {
                    setSettingsState(false);
                  }}
                  className="w-6 h-6 cursor-pointer text-gray-12"
                  aria-hidden="true"
                />
              </div>
            </div>
            <SettingsMenu />
          </div>
        </Dialog.Panel>
      </Transition.Child>
    </Modal>
  );
};
