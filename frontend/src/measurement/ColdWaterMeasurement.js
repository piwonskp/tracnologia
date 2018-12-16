import React, { Component } from 'react';
import { connect } from 'react-redux';
import {lifecycle} from 'recompose';
import {CardContent, CardHeader} from '@material-ui/core';

import {SingleCard} from '../lib/Grid';
import MeasureTable from '../lib/MeasureTable';
import {getColdWater} from './api';
import {COLD_WATER} from '../lib/meterTypes';


var ColdWaterMeasurement = ({rows}) =>
    <SingleCard>
        <CardHeader title="Pomiary zimnej wody" />
        <CardContent>
            <MeasureTable type={COLD_WATER} rows={rows} />
        </CardContent>
    </SingleCard>;

ColdWaterMeasurement = lifecycle({
    componentDidMount() {getColdWater();}
})(ColdWaterMeasurement);

ColdWaterMeasurement = connect(
    state => ({
        rows: state.coldWaterMeasures
    }))(ColdWaterMeasurement);

export default ColdWaterMeasurement;
