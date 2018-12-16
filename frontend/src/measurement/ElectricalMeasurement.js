import React, { Component } from 'react';
import { connect } from 'react-redux';
import {lifecycle} from 'recompose';
import {CardHeader, CardContent} from '@material-ui/core';

import {SingleCard} from '../lib/Grid';
import MeasureTable from '../lib/MeasureTable';
import {getElectrical} from './api';
import {ELECTRICAL} from '../lib/meterTypes';


var ElectricalMeasurement = ({rows}) =>
    <SingleCard>
        <CardHeader title="Pomiary prądu" />
        <CardContent>
            <MeasureTable type={ELECTRICAL} rows={rows} />
        </CardContent>
    </SingleCard>;
ElectricalMeasurement = lifecycle({
    componentDidMount() {getElectrical();}
})(ElectricalMeasurement);

ElectricalMeasurement = connect(
    state => ({
        rows: state.electricalMeasures
    }))(ElectricalMeasurement);


export default ElectricalMeasurement;
