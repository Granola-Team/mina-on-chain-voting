import React, { FC, ReactNode } from "react";

import { Header } from "./Header";

export const Layout: FC<{ children: ReactNode }> = ({ children }) => {
  return (
    <>
      <Header />
      <main>{children}</main>
    </>
  );
};
