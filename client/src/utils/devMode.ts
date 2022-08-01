import React from "react";

export const isDev = () => {
  return "_self" in React.createElement("div");
};
