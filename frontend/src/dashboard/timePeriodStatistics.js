import React from 'react';
import {partial, zipObject, toLower} from 'lodash';
import {HorizontalBarSeries, XAxis, YAxis, LabelSeries, ChartLabel
       } from 'react-vis';
import {timeFormatLocale, timeFormatDefaultLocale, timeFormat
       } from 'd3-time-format';
import plLocale from 'd3-time-format/locale/pl-PL';
import enLocale from 'd3-time-format/locale/en-US';
import { withTheme } from '@material-ui/core/styles';

import {COLD_WATER, HOT_WATER, ELECTRICAL, TYPE_TO_DISPLAY, TYPE_TO_UNIT_SVG
       } from '../lib/meterTypes';
import {get} from '../lib/api';
import {fetchOnMount, BasePlot} from './lib';
import {mkState} from '../lib/recompose';


timeFormatDefaultLocale(plLocale);
var en = timeFormatLocale(enLocale);

var statsType = type => 'stats/type/' + type;
var dailyStats = type => partial(get, statsType(type) + '/daily');
var monthlyStats = type => partial(get, statsType(type) + '/monthly');
var timelineStats = partial(get, 'stats/timeline');

var monthKeys = enLocale.months.map(toLower);
var weekdayKeys = enLocale.days.map(toLower);
var getMonthName = month => timeFormat("%B")(en.parse("%B")(month));
var enToLocaleWeekday = zipObject(weekdayKeys, plLocale.days);
var getWeekdayName = weekday => enToLocaleWeekday[weekday];

var toPlotData = (keys, toLabel) => data =>
    keys.slice().reverse().map(k => ({y: toLabel(k), x: data[k]}));
var monthPlotData = toPlotData(monthKeys, getMonthName);
var dayPlotData = toPlotData(weekdayKeys, getWeekdayName);

var labelData = data => data.map(v => Object.assign({label: v.x}, v));
var UsageLabel = ({type, theme}) =>
    <tspan font-weight="bold" fill={theme.palette.secondary.main}>
        Zużycie [{TYPE_TO_UNIT_SVG[type]}]
    </tspan>;
UsageLabel = withTheme()(UsageLabel);

var QuantifiedUsage = type => ({usage}) =>
    <BasePlot yType="ordinal">
        <XAxis />
        <ChartLabel xPercent={0.5} yPercent={0.65}
            text={<UsageLabel type={type} />} />
        <YAxis />
        <HorizontalBarSeries data={usage} />
        <LabelSeries data={labelData(usage)} labelAnchorY="middle" />
    </BasePlot>;
QuantifiedUsage = (QuantifiedUsage);

var usageStats = (fetch, toPlot, type) => React.createElement(mkState({usage: []})(
    fetchOnMount(fetch, resp => ({usage: toPlot(resp)}))(QuantifiedUsage(type))));

export var DailyUsage = type => usageStats(dailyStats(type), dayPlotData, type);
export var MonthlyUsage =
    type => usageStats(monthlyStats(type), monthPlotData, type);

