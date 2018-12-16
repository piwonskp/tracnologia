import { handleActions } from 'redux-actions';
import {pascalCase} from 'change-case';

import {openMeasureAdd, closeMeasureAdd, setUserContext, logout,
        setMeters, setColdWater, setHotWater, setElectrical, openMeterAdd,
        setRecentReadings, closeMeterAdd, addMeter, addReading,
        toggleTheme, toggleMenu, setUsers, setStats, addUser
       } from '../actions';
import * as types from '../lib/meterTypes';


var defaultState = {
    newMeasureMeter: null,
    newMeterDialog: false,
    user: null,
    meters: [],
    coldWaterMeasures: [],
    hotWaterMeasures: [],
    electricalMeasures: [],
    lightTheme: true,
    menuOpened: false,
    users: [],
    dailyStats: {},
    monthlyStats: {}
};

var setByKey = (key, value) => state => ({...state, [key]: value});
var setVal = key => (state, {payload}) => setByKey(key, payload)(state);
var nullify = key => state => ({...state, [key]: null});
var addByKey = (key, state, item) =>
    ({...state, [key]: [item].concat(state[key])});
var addItem = key => (state, {payload}) => addByKey(key, state, payload);
var toggle = key => state => ({...state, [key]: !state[key]});

var userContextSetter = (state, {payload}) =>
    ({...state, user: payload.user, meters: payload.meters});

var typeToKey = {
    [types.COLD_WATER]: "coldWaterMeasures",
    [types.HOT_WATER]: "hotWaterMeasures",
    [types.ELECTRICAL]: "electricalMeasures"
};

var setTypeReadings = (state, [type, val]) =>
    ({...state, [typeToKey[pascalCase(type)]]: val});
var setReadings = (state, action) =>
    Object.entries(action.payload).reduce(setTypeReadings, state);

var addNewReading = (state, {payload: {reading, type}}) =>
    addByKey(typeToKey[type], state, reading);

var setDailyStats = ({daily}) => setByKey("dailyStats", daily);
var setMonthlyStats = ({monthly}) => setByKey("monthlyStats", monthly);
var statsFuns = [setDailyStats, setMonthlyStats];

var reducer = handleActions({
    [openMeasureAdd]: setVal("newMeasureMeter"),
    [closeMeasureAdd]: nullify("newMeasureMeter"),
    [setUserContext]: userContextSetter,
    [logout]: ({lightTheme}) => ({...defaultState, lightTheme}),
    [setMeters]: setVal("meters"),
    [setColdWater]: setVal("coldWaterMeasures"),
    [setHotWater]: setVal("hotWaterMeasures"),
    [setElectrical]: setVal("electricalMeasures"),
    [openMeterAdd]: state => ({...state, newMeterDialog: true}),
    [closeMeterAdd]: state => ({...state, newMeterDialog: false}),
    [addMeter]: addItem("meters"),
    [addReading]: addNewReading,
    [setRecentReadings]: setReadings,
    [toggleTheme]: toggle("lightTheme"),
    [toggleMenu]: toggle("menuOpened"),
    [setUsers]: setVal("users"),
    [addUser]: addItem("users"),
    [setStats]: (state, {payload}) =>
        statsFuns.reduce((state, fun) => fun(payload)(state), state)
}, defaultState);

export default reducer;
