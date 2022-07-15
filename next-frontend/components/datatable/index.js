import React from "react";

export default function Datatable({ data, filter }) {
  const columns = data[0] && Object.keys(data[0]);
  return (
  <table cellPadding={0} cellSpace={0}> 
    <thead>
      <tr>{data[0] && columns.map((heading, index) => <th key={index}>{heading}</th>)}</tr>
    </thead>
    <tbody>
      {filter(data).map((row, i1) => ( 
      <tr key={i1}>
        {columns.map((column, i2) => (
          <td key={i2}>{row[column]}</td>
      ))}
      </tr>
    ))}
    </tbody>
  </table>
  );
}
