import { createAction } from 'redux-actions';


export const openMeasureAdd = createAction('OPEN_MEASURE_ADD');
export const closeMeasureAdd = createAction('CLOSE_MEASURE_ADD');
export const setUserContext = createAction('SET_USER_CONTEXT');
export const logout = createAction('LOGOUT');
export const setMeters = createAction('SET_METERS');
export const setColdWater = createAction('SET_COLD_WATER');
export const setHotWater = createAction('SET_HOT_WATER');
export const setElectrical = createAction('SET_ELECTRICAL');
export const setRecentReadings = createAction('SET_RECENT_READINGS');
export const openMeterAdd = createAction('OPEN_METER_ADD');
export const closeMeterAdd = createAction('CLOSE_METER_ADD');
export const addMeter = createAction('ADD_METER');
export const addReading = createAction('ADD_READING');
export const toggleTheme = createAction('TOGGLE_THEME');
export const toggleMenu = createAction('TOGGLE_MENU');
export const setUsers = createAction('SET_USERS');
export const setStats = createAction('SET_STATS');
export const addUser = createAction('ADD_USER');
