import React, {Fragment} from 'react';
import {partial, mapValues} from 'lodash';
import { DateTime } from 'luxon';
import {compose} from 'recompose';
import {timeFormat} from 'd3-time-format';
import {timeYear} from 'd3-time';
import {XAxis, YAxis, LineMarkSeries, DiscreteColorLegend, Hint} from 'react-vis';
import { withTheme } from '@material-ui/core/styles';

import {COLD_WATER, HOT_WATER, ELECTRICAL, TYPE_TO_DISPLAY, TYPE_TO_UNIT_HTML
       } from '../lib/meterTypes';
import {get} from '../lib/api';
import {mkState} from '../lib/recompose';
import {fetchOnMount, BasePlot} from './lib';


var timelineStats = partial(get, 'stats/timeline');

//XXX: react-vis implements hardcoded legend color
var DefaultTextColor = ({theme, ...props}) =>
    <span style={{color: theme.palette.text.primary}} {...props} />;
DefaultTextColor = withTheme()(DefaultTextColor);
var themedText = text => <DefaultTextColor>{text}</DefaultTextColor>;

var legendItems =
    Object.values(TYPE_TO_DISPLAY).map(t => ({title: themedText(t) }));

var toHint = v => [
    {title: "Data", value: timeFormat('%B %Y')(v.x)},
    {title: "Zużycie", value: <span>{v.y} {TYPE_TO_UNIT_HTML[v.type]}</span>}];

var addType = type => data => data.map(v => Object.assign({type: type}, v));
var TimelinePlot = ({coldWater, hotWater, electrical, hint, setHint}) =>
    <Fragment>
        <BasePlot xType="time" style={{marginBottom: 200}}>
            <LineMarkSeries data={addType(COLD_WATER)(coldWater)}
                onValueMouseOver={v => setHint(v)}
                onValueMouseOut={() => setHint(null)} />
            <LineMarkSeries data={addType(HOT_WATER)(hotWater)}
                onValueMouseOver={v => setHint(v)}
                onValueMouseOut={() => setHint(null)} />
            <LineMarkSeries data={addType(ELECTRICAL)(electrical)}
                onValueMouseOver={v => setHint(v)}
                onValueMouseOut={() => setHint(null)} />
            <XAxis tickTotal={timeYear.every(1)} />
            <YAxis />
            {hint ? <Hint value={hint} format={toHint} /> : null}
        </BasePlot>
        <DiscreteColorLegend orientation="horizontal" items={legendItems} />
    </Fragment>;
var timelinePlotState = {hint: null, coldWater: [], hotWater: [], electrical: []};

var toTimelinePoint = ({date, usage}) => ({x: DateTime.fromISO(date), y: usage});
var timelinePlotData = list => list.map(toTimelinePoint);

var Timeline = () => React.createElement(compose(
    mkState(timelinePlotState), fetchOnMount(
    timelineStats, resp => mapValues(resp, timelinePlotData)))(TimelinePlot));

export {Timeline};
