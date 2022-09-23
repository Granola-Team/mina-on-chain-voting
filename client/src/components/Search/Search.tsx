import React from "react";
import shallow from "zustand/shallow";
import { useNavigate } from "react-router-dom";
import { Dialog, Transition } from "@headlessui/react";

import { useAppStore } from "@/App.store";

import { Modal } from "@/components/Modal";

export const Search = () => {
  const { searchActive, setSearchState } = useAppStore(
    (state) => ({
      searchActive: state.searchActive,
      setSearchState: state.setSearchState,
    }),
    shallow,
  );
  const navigate = useNavigate();

  const submitHandler = (e: React.SyntheticEvent) => {
    e.preventDefault();
    const target = e.target as typeof e.target & {
      search: { value: string };
    };
    navigate(`/${target.search.value.toLowerCase()}`, { replace: true });
    setSearchState(false);
  };

  return (
    <Modal state={searchActive} setState={setSearchState}>
      <Transition.Child
        as={React.Fragment}
        enter="ease-out duration-300"
        enterFrom="opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
        enterTo="opacity-100 translate-y-0 sm:scale-100"
        leave="ease-in duration-200"
        leaveFrom="opacity-100 translate-y-0 sm:scale-100"
        leaveTo="opacity-0 translate-y-4 sm:translate-y-0 sm:scale-95"
      >
        <Dialog.Panel className="relative w-full overflow-hidden text-center sm:my-8 sm:max-w-5xl">
          <Dialog.Title
            as="h2"
            className="mb-8 text-xl lg:text-2xl font-bold leading-6"
          >
            What are you looking for?
          </Dialog.Title>
          <form onSubmit={submitHandler}>
            <div className="flex flex-col overflow-auto border rounded-xl md:overflow-visible md:mb-14 bg-gray-3 border-gray-7">
              <div className="flex items-center px-6 py-3 md:py-4">
                <div className="flex-1 mr-8 leading-none">
                  <input
                    className="w-full bg-transparent outline-none overflow-ellipsis placeholder:text-xs md:placeholder:text-base placeholder:text-gray-10 text-gray-11"
                    type="text"
                    name="search"
                    autoComplete="off"
                    placeholder="Enter a keyword to search for... 🔍 "
                  />
                </div>
                <button
                  type="button"
                  className="hidden md:block px-4 py-2 mr-8 font-normal text-center transition-all duration-200 rounded-md hover:bg-gray-6"
                >
                  <span className="text-gray-10">Advanced Search</span>
                </button>

                <button
                  type="submit"
                  className="flex items-center relative transition-all duration-200 rounded-md bg-gradient animate-gradient hover:opacity-[0.85]"
                >
                  <span className="text-xs p-2 md:px-4 md:py-2 md:text-base md:font-semibold text-gray-1 dark:text-gray-12">
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
