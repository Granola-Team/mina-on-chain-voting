import React from "react";
import * as TooltipPrimitive from "@radix-ui/react-tooltip";
import { styled, keyframes } from "@stitches/react";
import { gray, grayDark } from "@radix-ui/colors";
import { QuestionMarkCircledIcon } from "@radix-ui/react-icons";

import type { ComponentWithChildren } from "@/types";
import { useAppStore } from "@/App.store";

const StyledContent = styled(TooltipPrimitive.Content, {
  borderRadius: 10,
  padding: "10px 15px",
  maxWidth: "18rem",
  userSelect: "none",
  "@media (prefers-reduced-motion: no-preference)": {
    animationDuration: "400ms",
    animationTimingFunction: "cubic-bezier(0.16, 1, 0.3, 1)",
    willChange: "transform, opacity",
    '&[data-state="delayed-open"]': {
      '&[data-side="top"]': {
        animationName: keyframes({
          "0%": { opacity: 0, transform: "translateY(-2px)" },
          "100%": { opacity: 1, transform: "translateY(0)" },
        }),
      },
      '&[data-side="right"]': {
        animationName: keyframes({
          "0%": { opacity: 0, transform: "translateX(2px)" },
          "100%": { opacity: 1, transform: "translateX(0)" },
        }),
      },
      '&[data-side="bottom"]': {
        animationName: keyframes({
          "0%": { opacity: 0, transform: "translateX(2px)" },
          "100%": { opacity: 1, transform: "translateX(0)" },
        }),
      },
      '&[data-side="left"]': {
        animationName: keyframes({
          "0%": { opacity: 0, transform: "translateX(-2px)" },
          "100%": { opacity: 1, transform: "translateX(0)" },
        }),
      },
    },
  },
});

const StyledArrow = styled(TooltipPrimitive.Arrow, {});

const Content: React.FC<ComponentWithChildren> = ({ children }) => {
  return (
    <TooltipPrimitive.Portal>
      <StyledContent
        css={{
          backgroundColor: grayDark.gray4,
          border: "1px solid",
          borderColor: grayDark.gray7,
          color: grayDark.gray12,
        }}
      >
        {children}
        <StyledArrow css={{ fill: grayDark.gray7 }} />
      </StyledContent>
    </TooltipPrimitive.Portal>
  );
};

export const Provider = TooltipPrimitive.Provider;
export const Tooltip = TooltipPrimitive.Root;
export const TooltipTrigger = TooltipPrimitive.Trigger;
export const TooltipContent = Content;

interface TooltipProps extends ComponentWithChildren {
  css: string;
}

export const IconTooltip: React.FC<TooltipProps> = ({ children, css }) => {
  const darkMode = useAppStore((state) => state.darkMode);

  return (
    <Provider delayDuration={150}>
      <Tooltip>
        <TooltipTrigger asChild>
          <QuestionMarkCircledIcon
            className={css ?? ""}
            color={darkMode ? grayDark.gray10 : gray.gray10}
          />
        </TooltipTrigger>
        <TooltipContent>{children}</TooltipContent>
      </Tooltip>
    </Provider>
  );
};
