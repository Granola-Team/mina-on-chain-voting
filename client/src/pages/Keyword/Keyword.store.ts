import create from "zustand";
import type { SignalEntity, StatsEntity } from "@/types";

interface Timing {
  epoch: number | null;
  slot: number | null;
}

interface KeywordPageStore {
  signals: SignalEntity[] | null;
  stats: StatsEntity | null;
  isLoading: boolean;
  timing: Timing;
  setSignals: (x: SignalEntity[]) => void;
  setStats: (x: StatsEntity | null) => void;
  setIsLoading: (value: boolean) => void;
  setTiming: (x: Timing) => void;
}

export const useKeywordStore = create<KeywordPageStore>((set) => ({
  signals: null,
  stats: null,
  isLoading: false,
  timing: { epoch: null, slot: null },
  setSignals: (signals: SignalEntity[]) => {
    set(() => ({ signals }));
  },
  setStats: (stats: StatsEntity | null) => {
    set(() => ({ stats }));
  },
  setIsLoading: (isLoading: boolean) => {
    set(() => ({ isLoading }));
  },
  setTiming: (timing: Timing) => {
    set({ timing });
  },
}));
