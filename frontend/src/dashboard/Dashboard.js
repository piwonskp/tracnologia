import React, { Fragment } from 'react';

import {RecentReadings} from './RecentReadings';
import {Statistics} from './Statistics';


var Dashboard = () =>
    <Fragment>
        <Statistics />
        <RecentReadings />
    </Fragment>;

export default Dashboard;

