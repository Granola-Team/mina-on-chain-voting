import { Typography } from '@mui/material';

export interface ButtonProps {
  default?: boolean;
}

export const Button = ({ default: _default }: ButtonProps) => {
  return <Typography>Button!</Typography>;
};
