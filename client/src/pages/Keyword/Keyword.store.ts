import create from "zustand";
import type { SignalEntity, StatsEntity } from "@/types";

interface KeywordPageStore {
  signals: SignalEntity[] | null;
  stats: StatsEntity | null;
  isLoading: boolean;
  setSignals: (x: SignalEntity[]) => void;
  setStats: (x: StatsEntity | null) => void;
  setIsLoading: (value: boolean) => void;
}

export const useKeywordStore = create<KeywordPageStore>((set) => ({
  signals: null,
  stats: null,
  isLoading: false,
  setSignals: (signals: SignalEntity[]) => {
    set(() => ({ signals }));
  },
  setStats: (stats: StatsEntity | null) => {
    set(() => ({ stats }));
  },
  setIsLoading: (v: boolean) => {
    set(() => ({ isLoading: v }));
  },
}));
