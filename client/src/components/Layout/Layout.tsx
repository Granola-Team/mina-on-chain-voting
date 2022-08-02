import React, { FC, ReactNode } from "react";

import { Header } from "./Header";

export const Layout: FC<{ children: ReactNode }> = ({ children }) => {
  return (
    <>
      <Header />
      <main className="flex flex-col gap-6">{children}</main>
    </>
  );
};
