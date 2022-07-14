import React from "react";

export function Datatable({ data, filter }: any) {
  const columns = data[0] && Object.keys(data[0]);
  return (
  <table cellPadding={0}> 
    <thead>
      <tr>{data[0] && columns.map((heading : any) => <th>{heading}</th>)}</tr>
    </thead>
    <tbody>
      {filter(data).map((row : any) => ( 
      <tr>
        {columns.map((column : any) => (
          <td>{row[column]}</td>
      ))}
      </tr>
    ))} 
    </tbody>
  </table>
  );
}

