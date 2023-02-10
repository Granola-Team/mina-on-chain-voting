import { Table, TableBody, TableCell, TableContainer, TableHead, TableRow, Typography } from '@mui/material';

import { format, setMinutes, setSeconds } from 'date-fns';

type BlockStatus = 'Canonical' | 'Orphaned' | 'Pending';

type Vote = {
  id: number;
  account: string;
  hash: string;
  memo: string;
  height: number;
  timestamp: number;
  status: BlockStatus;
  stake_weight?: number;
};

const dummy = [
  {
    id: 1,
    account: 'B62qj4sYygXUKhKKitLtbmrxN1PWx7Xd7C5R2tyqWtSGA5bctZAuVDw',
    hash: 'CkpZxqP7pM5nRTy9dcJaCsLGxg1iFYLstr1zGaScwjfV1iBbgW8FY',
    memo: 'no cftest-2',
    height: 212888,
    status: 'Canonical',
    timestamp: 1675989172,
  },
  {
    id: 2,
    account: 'B62qj4sYygXUKhKKitLtbmrxN1PWx7Xd7C5R2tyqWtSGA5bctZAuVDw',
    hash: 'CkpZxqP7pM5nRTy9dcJaCsLGxg1iFYLstr1zGaScwjfV1iBbgW8FY',
    memo: 'no cftest-2',
    height: 212888,
    status: 'Canonical',
    timestamp: 1675989172,
  },
  {
    id: 3,
    account: 'B62qj4sYygXUKhKKitLtbmrxN1PWx7Xd7C5R2tyqWtSGA5bctZAuVDw',
    hash: 'CkpZxqP7pM5nRTy9dcJaCsLGxg1iFYLstr1zGaScwjfV1iBbgW8FY',
    memo: 'no cftest-2',
    height: 212888,
    status: 'Canonical',
    timestamp: 1675989172,
  },
] satisfies Vote[];

export const VotesTable = () => {
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
          {dummy.map((row) => (
            <TableRow key={row.id} sx={{ '&:last-child td, &:last-child th': { border: 0 } }}>
              <TableCell align="center" sx={{ py: 1.5 }}>
                <Typography fontSize={13.5} fontWeight={500}>
                  {row.height}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.5 }}>
                <Typography fontSize={13.5} fontWeight={500}>
                  {format(setMinutes(setSeconds(new Date(row.timestamp * 1000), 0), 3), 'MM/dd/yyyy - HH:mm')}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.5 }}>
                <Typography fontSize={13.5} fontWeight={500}>
                  {row.account}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.5 }}>
                <Typography fontSize={13.5} fontWeight={500}>
                  {row.hash}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.5 }}>
                <Typography fontSize={13.5} fontWeight={500}>
                  {row.memo}
                </Typography>
              </TableCell>
              <TableCell align="center" sx={{ py: 1.5 }}>
                <Typography fontSize={13.5} fontWeight={500}>
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
