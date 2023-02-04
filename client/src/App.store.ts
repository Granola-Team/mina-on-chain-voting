import create from "zustand";


interface AppStore {
  darkMode: boolean;
  settingsActive: boolean;
  searchActive: boolean;
  setDarkMode: (value: boolean) => void;
  setSettingsState: (value: boolean) => void;
  setSearchState: (value: boolean) => void;
}

export const useAppStore = create<AppStore>((set) => ({
  darkMode: false,
  settingsActive: false,
  searchActive: false,
  isLoading: false,
  setDarkMode: (v: boolean) => {
    set(() => ({ darkMode: v }));
  },
  setSettingsState: (v: boolean) => {
    set(() => ({ settingsActive: v }));
  },
  setSearchState: (v: boolean) => {
    set(() => ({ searchActive: v }));
  },
}));
