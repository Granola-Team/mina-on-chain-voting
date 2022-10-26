import create from "zustand";
import type { SignalEntity, StatsEntity } from "@/types";

interface Timing {
  epoch: number | null;
  slot: number | null;
}

interface KeywordPageStore {
  key: string | null;
  signals: SignalEntity[] | null;
  stats: StatsEntity | null;
  isLoading: boolean;
  timing: Timing;
  setKey: (x: string) => void;
  setSignals: (x: SignalEntity[]) => void;
  setStats: (x: StatsEntity | null) => void;
  setIsLoading: (value: boolean) => void;
  setTiming: (x: Timing) => void;
}

export const useKeywordStore = create<KeywordPageStore>((set) => ({
  key: null,
  signals: null,
  stats: null,
  isLoading: false,
  timing: { epoch: null, slot: null },
  setKey: (key: string) => {
    set({ key });
  },
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
