import React from "react";
import { useNavigate } from "react-router-dom";
import { useMediaQuery } from "@react-hook/media-query";

import { Settings, SettingsControl } from "@/components/Settings";
import { Search, SearchControl } from "@/components/Search";

export const Footer = React.memo(() => {
  const isMobile = useMediaQuery("only screen and (max-width: 768px)");
  const navigate = useNavigate();

  if (isMobile) {
    return (
      <footer>
        <div className="absolute inset-x-0 bottom-2">
            <div className="mx-auto pt-10 flex flex-col items-center gap-4">
                <a
                    onClick={() => {
                    navigate("/");
                    }}
                    href="https://github.com/Granola-Team/mina-signaling"
                >
                    <h5 className="text-base">If you want to run the code yourself, click here</h5>
                </a>
                <a
                    onClick={() => {
                    navigate("/");
                    }}
                    href="https://granola.team"
                >
                    <h6>Made by Granola</h6>
                </a>
          </div>
        </div>
      </footer>
    );
  }

  return (
    <footer>
        <div className="absolute inset-x-0 bottom-2">
            <div className="mx-auto pt-10 flex flex-col items-center gap-4">
                <a
                onClick={() => {
                    navigate("/");
                }}
                href="https://github.com/Granola-Team/mina-signaling"
                >
                    <h5>If you want to run the code yourself, click here</h5>
                </a>
                <a
                    onClick={() => {
                    navigate("/");
                    }}
                    href="https://granola.team"
                >
                    <h6>Made by Granola</h6>
                </a>
            </div>
        </div>
    </footer>
  );
});
