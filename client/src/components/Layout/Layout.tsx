import React from "react";

import type { ComponentWithChildren } from "@/types";

import { Header } from "../Header";

export const Layout: React.FC<ComponentWithChildren> = ({ children }) => {
  return (
    <>
      <Header />
      <main className="flex flex-col items-center gap-2 pb-6">{children}</main>
    </>
  );
};
