import React, {Fragment} from 'react';
import { connect } from 'react-redux';
import {lifecycle} from 'recompose';
import {Typography} from '@material-ui/core';

import {store} from '../index';
import {get} from '../lib/api';
import {setRecentReadings} from '../actions';
import {UserAuth} from '../lib/Authorization';
import {CenteredGrid, CardContentItem, SingleCardContent } from '../lib/Grid';
import {COLD_WATER, HOT_WATER, ELECTRICAL} from '../lib/meterTypes';
import MeasureTable from '../lib/MeasureTable';


var fetchRecentReadings = () =>
    get('recent-readings',
        readings => store.dispatch(setRecentReadings(readings)));

var RecentReadingsTable = ({coldWater, hotWater, electrical}) =>
    <Fragment>
        <SingleCardContent>
            <Typography variant="subheading">Zimna woda</Typography>
            <MeasureTable type={COLD_WATER} rows={coldWater} />
        </SingleCardContent>
        <SingleCardContent>
            <Typography variant="subheading">Ciepła woda</Typography>
            <MeasureTable type={HOT_WATER} rows={hotWater} />
        </SingleCardContent>
        <SingleCardContent>
            <Typography variant="subheading">Prąd</Typography>
            <MeasureTable type={ELECTRICAL} rows={electrical} />
        </SingleCardContent>
    </Fragment>;

RecentReadingsTable = connect(state => ({
    coldWater: state.coldWaterMeasures.slice(0, 5),
    hotWater: state.hotWaterMeasures.slice(0, 5),
    electrical: state.electricalMeasures.slice(0, 5)
}))(RecentReadingsTable);

var RecentReadings= () =>
    <Fragment>
        <Typography variant="title">
            Ostatnie pomiary
        </Typography>
        <CenteredGrid gridTemplateColumns="repeat(auto-fit, minmax(400px, 1fr))">
            <RecentReadingsTable />
        </CenteredGrid>
    </Fragment>;
RecentReadings = lifecycle({componentDidMount: fetchRecentReadings})(RecentReadings);
RecentReadings = UserAuth(RecentReadings);

export {RecentReadings};
