import React from 'react';
import { DateTime } from 'luxon';
import {Table, TableHead, TableCell, TableRow,
        TableBody} from '@material-ui/core';

import {store} from '../index';
import {TYPE_TO_UNIT_HTML} from '../lib/meterTypes';


var getMeter = meterId =>
    store.getState().meters.find(meter => meter.id === meterId);
var getMeterName = reading => getMeter(reading.meter).name;

var measureRow = row =>
    <TableRow>
        <TableCell scope="row">
            {DateTime.fromISO(row.timestamp).toLocaleString()}
        </TableCell>
        <TableCell>{getMeterName(row)}</TableCell>
        <TableCell numeric>{row.value}</TableCell>
    </TableRow>;

var MeasureTable = ({rows, type}) =>
    <Table>
        <TableHead>
            <TableRow>
                <TableCell>Data</TableCell>
                <TableCell>Licznik</TableCell>
                <TableCell>Wartość [{TYPE_TO_UNIT_HTML[type]}]</TableCell>
            </TableRow>
        </TableHead>
        <TableBody>
            {rows.map(measureRow)}
        </TableBody>
    </Table>;

export default MeasureTable;
