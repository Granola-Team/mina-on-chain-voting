import { Box, Typography } from '@mui/material';

export type StatusBubbleProps = {
  type: 'Canonical' | 'Pending';
};

export const StatusBubble = ({ type }: StatusBubbleProps) => {
  if (type === 'Canonical') {
    return (
      <Box
        mx="auto"
        maxWidth="6rem"
        py={0.25}
        border={1}
        borderColor="hsla(153.8, 100%, 62.7%, 0.26)"
        borderRadius={4}
        sx={{ backgroundColor: 'hsla(157.7, 98.1%, 59.6%, 0.11)' }}
      >
        <Typography fontSize={12.5} fontWeight={500}>
          Canonical
        </Typography>
      </Box>
    );
  }

  return (
    <Box
      mx="auto"
      maxWidth="6rem"
      py={0.25}
      border={1}
      borderColor="hsla(42.2, 100%, 51.8%, 0.44)"
      borderRadius={4}
      sx={{ backgroundColor: '#FF83004D' }}
    >
      <Typography fontSize={12.5} fontWeight={500}>
        Pending
      </Typography>
    </Box>
  );
};
