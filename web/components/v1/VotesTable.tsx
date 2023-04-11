import { useEffect, useState } from 'react';

import {
  Pagination,
  Stack,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Typography,
} from '@mui/material';

import { useTheme } from 'components/provider';

import { format, setMinutes, setSeconds } from 'date-fns';
import type { ProposalParserOutcome } from 'models';

import { StatusBubble } from './StatusBubble';
import { TableAvatar } from './TableAvatar';

export type VotesTableProps = {
  votes: ProposalParserOutcome['votes'];
};

export const VotesTable = ({ votes }: VotesTableProps) => {
  const { theme } = useTheme();
  const [currentPage, setCurrentPage] = useState(1);
  const rowsPerPage = 20;

  useEffect(() => {
    setCurrentPage(1);
  }, [votes, rowsPerPage]);

  const handlePageChange = (event: React.ChangeEvent<unknown>, page: number) => {
    setCurrentPage(page);
  };

  const startIndex = (currentPage - 1) * rowsPerPage;
  const endIndex = startIndex + rowsPerPage;
  const pageVotes = votes.slice(startIndex, endIndex);
  const pageCount = Math.ceil(votes.length / rowsPerPage);

  return (
    <>
      <TableContainer
        sx={{
          border: 1,
          borderRadius: 2,
          borderColor: 'hsl(0, 0%, 24.3%)',
          backgroundColor: theme.key === 'dark' ? '#1C1C1C' : 'hsl(0, 0%, 100%)',
        }}
      >
        <Table
          sx={{
            minWidth: 650,
          }}
        >
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
                  Transaction Hash
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
            {pageVotes.length === 0 && (
              <TableRow>
                <TableCell colSpan={6}>
                  <Stack py={2} sx={{ width: '100%' }}>
                    <Typography fontSize={14} fontWeight={500} align="center">
                      There is no data yet
                    </Typography>
                  </Stack>
                </TableCell>
              </TableRow>
            )}
            {pageVotes.length > 0 &&
              pageVotes.map((vote) => (
                <TableRow key={vote.account} sx={{ '&:last-child td, &:last-child th': { border: 0 } }}>
                  <TableCell align="center" sx={{ py: 1.2 }}>
                    <Typography fontSize={13} fontWeight={500}>
                      {vote.height}
                    </Typography>
                  </TableCell>
                  <TableCell align="center" sx={{ py: 1.2 }}>
                    <Typography fontSize={13} fontWeight={500}>
                      {format(setMinutes(setSeconds(new Date(vote.timestamp), 0), 3), 'MM/dd/yyyy - HH:mm')}
                    </Typography>
                  </TableCell>
                  <TableCell align="left" sx={{ py: 1.2 }}>
                    <Stack spacing={1} direction="row" justifyContent="left" alignItems="left">
                      <TableAvatar seed={vote.account} />
                      <Typography fontSize={13} fontWeight={500}>
                        {vote.account}
                      </Typography>
                    </Stack>
                  </TableCell>
                  <TableCell align="left" sx={{ py: 1.2 }}>
                    <Typography fontSize={13} fontWeight={500}>
                      {vote.hash}
                    </Typography>
                  </TableCell>
                  <TableCell align="center" sx={{ py: 1.2 }}>
                    <Typography fontSize={13} fontWeight={500}>
                      {vote.memo}
                    </Typography>
                  </TableCell>
                  <TableCell align="center" sx={{ py: 1.2 }}>
                    {vote.status === 'Canonical' && <StatusBubble type="Canonical" />}
                    {vote.status === 'Pending' && <StatusBubble type="Pending" />}
                  </TableCell>
                </TableRow>
              ))}
          </TableBody>
        </Table>
      </TableContainer>
      {votes.length > 0 && (
        <Stack direction="row" spacing={2} justifyContent="center" alignItems="center">
          <Pagination
            count={pageCount}
            page={currentPage}
            onChange={handlePageChange}
            variant="outlined"
            color="primary"
          />
        </Stack>
      )}
    </>
  );
};
