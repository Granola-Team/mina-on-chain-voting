import { Table, TableBody, TableCell, TableContainer, TableHead, TableRow, Typography } from '@mui/material';

import { format, setMinutes, setSeconds } from 'date-fns';

export type BlockStatus = 'Canonical' | 'Orphaned' | 'Pending';

export type Vote = {
  id: number;
  account: string;
  hash: string;
  memo: string;
  height: number;
  timestamp: number;
  status: BlockStatus;
  stake_weight?: number;
};

export type VotesTableProps = {
  votes: Vote[];
};

export const VotesTable = ({ votes }: VotesTableProps) => {
  return (
    <TableContainer sx={{ border: 1, borderRadius: 2, borderColor: 'hsl(0, 0%, 24.3%)' }}>
      <Table sx={{ minWidth: 650 }}>
        <TableHead>
          <TableRow>
            <TableCell align="center" sx={{ py: 1.5 }}>
              <Typography variant="body2" fontWeight={600}>
                Blockheight
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.5 }}>
              <Typography variant="body2" fontWeight={600}>
                Timestamp
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.5 }}>
              <Typography variant="body2" fontWeight={600}>
                Account
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.5 }}>
              <Typography variant="body2" fontWeight={600}>
                Transaction Hash
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.5 }}>
              <Typography variant="body2" fontWeight={600}>
                Memo
              </Typography>
            </TableCell>
            <TableCell align="center" sx={{ py: 1.5 }}>
              <Typography variant="body2" fontWeight={600}>
                Voting Status
              </Typography>
            </TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {votes.map((row) => (
            <TableRow key={row.id} sx={{ '&:last-child td, &:last-child th': { border: 0 } }}>
              <TableCell align="center" sx={{ py: 1.35 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {row.height}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.35 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {format(setMinutes(setSeconds(new Date(row.timestamp * 1000), 0), 3), 'MM/dd/yyyy - HH:mm')}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.35 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {row.account}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.35 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {row.hash}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.35 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {row.memo}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.35 }}>
                <Typography fontSize={13} fontWeight={500}>
                  {row.status}
                </Typography>
              </TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </TableContainer>
  );
};
