import React from "react";

export default function Datatable({ data, filter }) {
  const columns = data[0] && Object.keys(data[0]);
  return (
  <table cellPadding={0} cellSpace={0}> 
    <thead>
      <tr>{data[0] && columns.map(heading => <th>{heading}</th>)}</tr>
    </thead>
    <tbody>
      {filter(data).map((row) => ( 
      <tr>
        {columns.map((column) => (
          <td>{row[column]}</td>
      ))}
      </tr>
    ))}
    </tbody>
  </table>
  );
}
