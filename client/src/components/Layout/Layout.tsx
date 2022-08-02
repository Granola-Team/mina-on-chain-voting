import React from "react";

import { Header } from "./Header";

export const Layout: React.FC<{ children: React.ReactNode }> = ({
  children,
}) => {
  return (
    <>
      <Header />
      <main className="flex flex-col gap-6">{children}</main>
    </>
  );
};
