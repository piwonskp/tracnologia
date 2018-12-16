import React, { Component } from 'react';
import { connect } from 'react-redux';
import {lifecycle} from 'recompose';
import {CardContent, CardHeader} from '@material-ui/core';

import {SingleCard} from '../lib/Grid';
import MeasureTable from '../lib/MeasureTable';
import {getHotWater} from './api';
import {HOT_WATER} from '../lib/meterTypes';


var HotWaterMeasurement = ({rows}) =>
    <SingleCard>
        <CardHeader title="Pomiary ciepłej wody" />
        <CardContent>
            <MeasureTable type={HOT_WATER} rows={rows} />
        </CardContent>
    </SingleCard>;

HotWaterMeasurement = lifecycle({
    componentDidMount() {getHotWater();}
})(HotWaterMeasurement);

HotWaterMeasurement = connect(
    state => ({
        rows: state.hotWaterMeasures
    }))(HotWaterMeasurement);

export default HotWaterMeasurement;
