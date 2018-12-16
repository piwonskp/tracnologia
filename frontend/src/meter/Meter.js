import React from 'react';
import { connect } from 'react-redux';
import {Table, TableHead, TableCell, TableRow, TableBody, Button, CardContent,
        CardHeader, CardActions
       } from '@material-ui/core';
import {Add} from '@material-ui/icons';

import {SingleCard} from '../lib/Grid';
import {openMeterAdd} from '../actions';
import AddMeterDialog from './MeterDialog';
import {TYPE_TO_DISPLAY} from '../lib/meterTypes';


var meterRow = row =>
    <TableRow>
        <TableCell>{row.name}</TableCell>
        <TableCell numeric>{TYPE_TO_DISPLAY[row.type]}</TableCell>
    </TableRow>;

var MeterTable = ({rows}) =>
    <Table>
        <TableHead>
            <TableRow>
                <TableCell>Licznik</TableCell>
                <TableCell>Typ</TableCell>
            </TableRow>
        </TableHead>
        <TableBody>{rows.map(meterRow)}</TableBody>
    </Table>;

var Meter = props =>
    <SingleCard>
        <CardHeader title="Liczniki" />
        <CardContent>
            <MeterTable rows={props.meters} />
        </CardContent>
        <CardActions>
            <Button onClick={props.openMeterAdd}>
                <Add />Dodaj licznik
            </Button>
        </CardActions>
        <AddMeterDialog />
    </SingleCard>;
Meter = connect(state => ({meters: state.meters}),
                dispatch =>({
                    openMeterAdd: () => dispatch(openMeterAdd())
                }))(Meter);

export default Meter;
