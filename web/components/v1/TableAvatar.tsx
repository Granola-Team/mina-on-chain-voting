import { Avatar } from '@mui/material';

import makeBlockie from 'ethereum-blockies-base64';

export type TableAvatarProps = {
  seed: string;
};

export const TableAvatar = ({ seed }: TableAvatarProps) => {
  return <Avatar alt="identicon" src={makeBlockie(seed)} sx={{ width: 24, height: 24 }} />;
};
