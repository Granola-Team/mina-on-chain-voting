import create from "zustand";

import type { DataEntity } from "@/types";

interface KeywordPageStore {
  signals: DataEntity | undefined;
  isLoading: boolean;
  setSignals: (d: DataEntity) => void;
  setIsLoading: (value: boolean) => void;
}

export const useKeywordStore = create<KeywordPageStore>((set) => ({
  signals: undefined,
  isLoading: false,
  setSignals: (signals: DataEntity) => {
    set(() => ({ signals }));
  },
  setIsLoading: (v: boolean) => {
    set(() => ({ isLoading: v }));
  },
}));
