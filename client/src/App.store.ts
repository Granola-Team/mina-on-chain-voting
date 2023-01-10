import create from "zustand";
import type { SignalEntity } from "./types";

interface AppStore {
  darkMode: boolean;
  settingsActive: boolean;
  searchActive: boolean;
  tableRange: number[];
  slice: SignalEntity[];
  setDarkMode: (value: boolean) => void;
  setSettingsState: (value: boolean) => void;
  setSearchState: (value: boolean) => void;
  setTableRange: (value: number[]) => void;
  setSlice: (value: SignalEntity[]) => void;
}

export const useAppStore = create<AppStore>((set) => ({
  darkMode: false,
  settingsActive: false,
  searchActive: false,
  isLoading: false,
  tableRange: [],
  slice: [],
  setDarkMode: (v: boolean) => {
    set(() => ({ darkMode: v }));
  },
  setSettingsState: (v: boolean) => {
    set(() => ({ settingsActive: v }));
  },
  setSearchState: (v: boolean) => {
    set(() => ({ searchActive: v }));
  },
  setTableRange: (v: number[]) => {
    set(() => ({ tableRange: v }));
  },
  setSlice: (v: SignalEntity[]) => {
    set(() => ({ slice: v }));
  },
}));
