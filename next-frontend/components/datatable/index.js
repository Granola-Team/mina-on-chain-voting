"use strict";
exports.__esModule = true;
exports.Datatable = void 0;
var react_1 = require("react");
function Datatable(_a) {
    var data = _a.data, filter = _a.filter;
    var columns = data[0] && Object.keys(data[0]);
    return (<table cellPadding={0}> 
    <thead>
      <tr>{data[0] && columns.map(function (heading) { return <th>{heading}</th>; })}</tr>
    </thead>
    <tbody>
      {filter(data).map(function (row) { return (<tr>
        {columns.map(function (column) { return (<td>{row[column]}</td>); })}
      </tr>); })} 
    </tbody>
  </table>);
}
exports.Datatable = Datatable;
