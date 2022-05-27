import React, { useState, useEffect } from 'react';
import DataTable from 'react-data-table-component';
import styled from 'styled-components';

import api from '../service/api';

const TextField = styled.input`
  height: 32px;
  width: 200px;
  border-radius: 3px;
  border-top-left-radius: 5px;
  border-bottom-left-radius: 5px;
  border-top-right-radius: 0;
  border-bottom-right-radius: 0;
  border: 1px solid #e5e5e5;
  padding: 0 32px 0 16px;

  &:hover {
    cursor: pointer;
  }
`;

const ClearButton = styled.button`
  border-top-left-radius: 0;
  border-bottom-left-radius: 0;
  border-top-right-radius: 5px;
  border-bottom-right-radius: 5px;
  border-color: #2979FF;
  border: none;
  background-color: #2979FF;
  color: #fff;
  height: 34px;
  width: 50px;
  text-align: center;
  display: flex;
  align-items: center;
  justify-content: center;
`;

const FilterComponent = ({ filterText, onFilter, onClear }) => (
    <>
      <TextField id="search" type="text" placeholder="Filtrar por nome" aria-label="Search Input" value={filterText} onChange={onFilter} />
      <ClearButton type="button" onClick={onClear}>X</ClearButton>
    </>
  );

const columns = [
  {
    name: 'ID',
    selector: 'id',
    sortable: true,
  },
  {
    name: 'Nome',
    selector: 'name',
    sortable: true,
  },
  {
    name: 'Details',
    button: true,
    cell: () => <button className="btn"><i class="fas fa-eye"></i></button>,
  }
];

export default function Datatable() {
    const [data, setData] = useState([]);
    const [filterText, setFilterText] = React.useState('');
    const [resetPaginationToggle, setResetPaginationToggle] = React.useState(false);
    const filteredItems = data.filter(item => item.name && item.name.toLowerCase().includes(filterText.toLowerCase()));

    const subHeaderComponentMemo = React.useMemo(() => {
        const handleClear = () => {
          if (filterText) {
            setResetPaginationToggle(!resetPaginationToggle);
            setFilterText('');
          }
        };
    
        return <FilterComponent onFilter={e => setFilterText(e.target.value)} onClear={handleClear} filterText={filterText} />;
      }, [filterText, resetPaginationToggle]);

    useEffect(() => {
        api.get('comments')
            .then((response) => {
                setData(response.data)
                console.log(response.data)
            })
    }, []);
    
    return (
        <>
        <div className="container">
            <DataTable 
                title="Data-Table React"
                columns={columns}
                data={filteredItems}

                paginationResetDefaultPage={resetPaginationToggle} // optionally, a hook to reset pagination to page 1
                subHeader
                subHeaderComponent={subHeaderComponentMemo}

                noDataComponent={<NoData />}

                compact={true}
                responsive={true}
                compact={true}
                pagination={true}
                striped={true}
                highlightOnHover={true}
                direction="left"
            />
        </div>
            
        </>
    )
}

const NoData = () => {
  return(
    <>
      <p>Não há registros para exibir</p>
    </>
  );  
}
