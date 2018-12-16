import React, {Fragment} from 'react';
import {Typography, CardContent, Tabs, Tab, BottomNavigation, Card,
        BottomNavigationAction} from '@material-ui/core';
import 'react-vis/dist/style.css';

import {UserAuth} from '../lib/Authorization';
import {SingleCard, CardItem} from '../lib/Grid';
import {TYPE_TO_DISPLAY, COLD_WATER} from '../lib/meterTypes';
import {Timeline} from './timeline';
import {MonthlyUsage, DailyUsage} from './timePeriodStatistics';
import {mkState} from '../lib/recompose';


var renderNav = ([type, label]) =>
    <BottomNavigationAction value={type} label={label} />;
var statsNav = Object.entries(TYPE_TO_DISPLAY).map(renderNav);
var TypeNav = (type, setType) =>
    <BottomNavigation value={type} onChange={(_, val) => setType(val)} showLabels>
        {statsNav}
    </BottomNavigation>;

const UserTab = UserAuth(Tab);

var UserViews = ({view, type}) =>
    <Fragment>
        {view == 2 && DailyUsage(type)}
    </Fragment>;
UserViews = UserAuth(UserViews);

var StatsTabs = ({view, setView}) =>
    <Tabs value={view} onChange={(_, val) => setView(val)}>
        <Tab label="Zużycie" />
        <Tab label="Miesięczne" />
        <UserTab label="Dzienne" />
    </Tabs>;

var statisticsState = {type: COLD_WATER, view: 0};
var Statistics = ({type, view, setType, setView}) =>
    <Card>
        <CardContent>
            <StatsTabs view={view} setView={setView} />
            {view == 0 && Timeline()}
            {view == 1 && MonthlyUsage(type)}
            <UserViews view={view} type={type} />

            {view != 0 && TypeNav(type, setType)}
        </CardContent>
    </Card>;
Statistics = mkState(statisticsState)(Statistics);

export {Statistics};
