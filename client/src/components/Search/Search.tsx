import React, { Fragment } from "react";
import { Dialog, Transition } from "@headlessui/react";

import { useFilterParams } from "@/hooks/useFilterParams";
import { useAppStore } from "@/store/app.store";

import { Modal } from "@/components/Modal";

export const Search = () => {
  const searchActive = useAppStore((state) => state.searchActive);
  const setSearchState = useAppStore((state) => state.setSearchState);
  const [, executeRoute] = useFilterParams();

  const submitHandler = (e: React.SyntheticEvent) => {
    e.preventDefault();
    const target = e.target as typeof e.target & {
      search: { value: string };
    };
    executeRoute({ key: target.search.value.toLowerCase() });
    setSearchState(false);
  };

  return (
    <Modal state={searchActive} setState={setSearchState}>
      <Transition.Child
        as={Fragment}
        enter="ease-out duration-300"
        enterFrom="opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
        enterTo="opacity-100 translate-y-0 sm:scale-100"
        leave="ease-in duration-200"
        leaveFrom="opacity-100 translate-y-0 sm:scale-100"
        leaveTo="opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
      >
        <Dialog.Panel className="relative w-full overflow-hidden text-center sm:my-8 sm:max-w-5xl">
          <Dialog.Title as="h2" className="mb-8 text-2xl font-bold leading-6">
            What are you looking for?
          </Dialog.Title>
          <form onSubmit={submitHandler}>
            <div className="flex flex-col overflow-auto border rounded-xl md:overflow-visible md:mb-14 bg-gray-3 border-gray-7">
              <div className="flex items-center px-6 py-4">
                <div className="flex-1 mr-8 leading-none">
                  <input
                    className="hidden w-full bg-transparent outline-none sm:block overflow-ellipsis placeholder:text-gray-10 text-gray-11"
                    type="text"
                    name="search"
                    autoComplete="off"
                    placeholder="Enter a keyword to search for... 🔍 "
                  />
                </div>
                <button
                  type="button"
                  className="hidden px-4 py-2 mr-8 font-normal text-center transition-all duration-200 rounded-md md:block hover:bg-gray-6"
                >
                  <span className="text-gray-10">Advanced Search</span>
                </button>

                <button
                  type="submit"
                  className="relative hidden px-4 py-2 transition-all duration-200 rounded-md md:block bg-gradient animate-gradient hover:opacity-[0.85]"
                >
                  <span className="font-semibold text-gray-1 dark:text-gray-12">
                    Search
                  </span>
                </button>
              </div>
            </div>
          </form>
        </Dialog.Panel>
      </Transition.Child>
    </Modal>
  );
};
