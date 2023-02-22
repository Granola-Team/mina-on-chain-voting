import { Stack, Table, TableBody, TableCell, TableContainer, TableHead, TableRow, Typography } from '@mui/material';

import { useTheme } from 'components/provider';

import { format, setMinutes, setSeconds } from 'date-fns';
import type { MinaProposalParserResponse } from 'models';

import { StatusBubble } from './StatusBubble';

export type ResultsTableProps = {
  votes: MinaProposalParserResponse['votes'];
  createPercent: (value: number) => void;
};

export const ResultsTable = ({ votes, createPercent }: ResultsTableProps) => {
  const { theme } = useTheme();

  return (
    <TableContainer
      sx={{
        border: 1,
        borderRadius: 2,
        borderColor: 'hsl(0, 0%, 24.3%)',
        backgroundColor: theme.key === 'dark' ? '#1C1C1C' : 'hsl(0, 0%, 100%)',
      }}
    >
      <Table sx={{ minWidth: 650 }}>
        <TableHead>
          <TableRow>
            <TableCell align="center" sx={{ py: 1.1 }}>
              <Typography variant="body2" fontWeight={600}>
                Blockheight
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.1 }}>
              <Typography variant="body2" fontWeight={600}>
                Timestamp
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.1 }}>
              <Typography variant="body2" fontWeight={600}>
                Account
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.1 }}>
              <Typography variant="body2" fontWeight={600}>
                Weighted Stake
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.1 }}>
              <Typography variant="body2" fontWeight={600}>
                Weighted Stake %
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.1 }}>
              <Typography variant="body2" fontWeight={600}>
                Memo
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.1 }}>
              <Typography variant="body2" fontWeight={600}>
                Voting Status
              </Typography>
            </TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {votes.map((row) => (
            <TableRow key={row.id} sx={{ '&:last-child td, &:last-child th': { border: 0 } }}>
              <TableCell align="center" sx={{ py: 1.2 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {row.height}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.2 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {format(setMinutes(setSeconds(new Date(row.timestamp * 1000), 0), 3), 'MM/dd/yyyy - HH:mm')}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.2 }}>
                <Stack spacing={1} direction="row" justifyContent="center" alignItems="center">
                  {/* <Avatar alt="identicon" src={makeBlockie(row.account)} sx={{ width: 24, height: 24 }} /> */}
                  <Typography fontSize={13} fontWeight={500}>
                    {row.account}
                  </Typography>
                </Stack>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.2 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {row.stake_weight > 0 ? row.stake_weight.toFixed(4) : 'Stake Delegated'}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.2 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {row.stake_weight ? `${createPercent(row.stake_weight)}` : 'Stake Delegated'}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.2 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {row.memo}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.2 }}>
                {row.status === 'Canonical' && <StatusBubble type="Canonical" />}
                {row.status === 'Pending' && <StatusBubble type="Pending" />}
              </TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </TableContainer>
  );
};
