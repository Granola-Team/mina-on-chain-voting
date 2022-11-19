import React from "react";

export const Footer = React.memo(() => {
  return (
    <footer className="py-1.5 border border-t border-gray-7">
      <div className="mx-auto flex flex-col items-center">
        <a
          href="https://github.com/Granola-Team/onchain-signalling"
          className="text-OrangeMINA -mb-1"
        >
          <span className="text-[0.85rem]"><u>GitHub</u></span>
        </a>
        <a href="https://granola.team">
          <span className="text-OrangeMINA text-[0.7rem]"><u>Made with ❤️ by Granola</u></span>
        </a>
      </div>
    </footer>
  );
});
