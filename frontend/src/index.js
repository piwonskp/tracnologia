import React from 'react';
import ReactDOM from 'react-dom';
import { Provider, connect } from 'react-redux';
import { createStore } from 'redux';
import { Router } from 'react-router-dom';
import { MuiThemeProvider, createMuiTheme } from '@material-ui/core/styles';
import CssBaseline from '@material-ui/core/CssBaseline';
import {MuiPickersUtilsProvider} from 'material-ui-pickers';
import LuxonUtils from '@date-io/luxon';
import LuxonSettings from 'luxon/src/settings.js';
import {purple} from '@material-ui/core/colors';

import './index.css';
import App from './App';
import registerServiceWorker from './registerServiceWorker';
import reducer from './reducers/root';
import {history} from './lib/api';


var locale = 'pl';
LuxonSettings.defaultLocale = locale;

export const store = createStore(reducer,
                          window.__REDUX_DEVTOOLS_EXTENSION__ &&
                          window.__REDUX_DEVTOOLS_EXTENSION__()
);

const darkTheme = createMuiTheme({
  palette: {
      primary: {
          main: '#84ffff'
      },
    type: 'dark'
  }
});

const lightTheme = createMuiTheme({
    palette: {
        primary: {
            main: '#84ffff'
        },
        type: 'light'
    }
});

var ThemedApp = theme =>
    <MuiThemeProvider theme={theme}>
        <CssBaseline />
        <Router history={history}>
            <App />
        </Router>
    </MuiThemeProvider>;
ThemedApp = connect(
    state => state.lightTheme ? lightTheme : darkTheme)(ThemedApp);

ReactDOM.render(
    <Provider store={store}>
            <MuiPickersUtilsProvider utils={LuxonUtils}
            locale={locale}>
                <ThemedApp />
            </MuiPickersUtilsProvider>
    </Provider>,
    document.getElementById('root'));
registerServiceWorker();
