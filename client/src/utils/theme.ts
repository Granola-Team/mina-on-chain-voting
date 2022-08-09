/**
 * Checks if darkmode is activated & changes the theme accordingly.
 */
export const changeTheme = (state: boolean): void => {
  if (state) {
    document.documentElement.classList.remove("dark");
    document.body.classList.remove("theme-dark");
    document.body.classList.add("theme-light");
    localStorage.setItem("theme", "light");
  } else {
    document.documentElement.classList.add("dark");
    document.body.classList.remove("theme-light");
    document.body.classList.add("theme-dark");
    localStorage.setItem("theme", "dark");
  }
};

/**
 * Checks if localstorage for native theme & sets darkmode accordingly.
 */
export const setTheme = (): void => {
  if (
    localStorage.theme === "dark" ||
    (!("theme" in localStorage) &&
      window.matchMedia("(prefers-color-scheme: dark)").matches)
  ) {
    document.documentElement.classList.add("dark");
    document.body.classList.remove("theme-light");
    document.body.classList.add("theme-dark");
  } else {
    document.documentElement.classList.remove("dark");
    document.body.classList.remove("theme-dark");
    document.body.classList.add("theme-light");
  }
};

/**
 * Checks if darkmode is represented in our current HTML Classlist.
 */
export const isDarkMode = (): boolean => {
  if (document.documentElement.classList.contains("dark")) {
    return true;
  } else {
    return false;
  }
};
