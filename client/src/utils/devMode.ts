import React from "react";

/**
 * Checks if React is in development mode.
 * @returns true/false
 */
export const isDev = (): boolean => {
  return "_self" in React.createElement("div");
};
