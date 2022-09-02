import React from "react";

export const Footer = React.memo(() => {
  return (
    <footer className="py-1.5 border border-t border-gray-7">
      <div className="mx-auto flex flex-col items-center">
        <a
          href="https://github.com/Granola-Team/mina-signaling"
          className="-mb-1"
        >
          <span className="text-[0.85rem]">Github</span>
        </a>
        <a href="https://granola.team">
          <span className="text-[0.7rem]">Made with ❤️ by Granola</span>
        </a>
      </div>
    </footer>
  );
});
