import React from "react";

import type { ComponentWithChildren } from "@/types";

import { Header } from "../Header";
import { Footer } from "../Footer";

export const Layout: React.FC<ComponentWithChildren> = ({ children }) => {
  return (
    <div className="flex flex-col min-h-screen">
      <Header />
      <main className="flex flex-col items-center gap-2 pb-6 mb-auto">
        {children}
      </main>
      <Footer />
    </div>
  );
};
