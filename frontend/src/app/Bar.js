import React, {Fragment} from 'react';
import { connect } from 'react-redux';
import {Link} from 'react-router-dom';
import {AppBar, Toolbar, Grid, Button, IconButton, BottomNavigation,
        BottomNavigationAction, Switch} from '@material-ui/core';
import {AccountCircle, Menu} from '@material-ui/icons';
import {GithubCircle, WeatherNight, WeatherSunny} from 'mdi-material-ui';
import Cookies from 'js-cookie';

import {store} from '../index';
import * as actions from '../actions';
import {userAuth, anonymousAuth} from '../lib/Authorization';


var LogInButton = () =>
    <Button color="inherit" component={Link} to="/login">
        Zaloguj się
    </Button>;
LogInButton = anonymousAuth(LogInButton);

function logout(){
    store.dispatch(actions.logout());
    Cookies.remove('XSRF-TOKEN');
}

var UserBar = user =>
    <Fragment>
        <Link to='/profile'>
            <IconButton><AccountCircle /></IconButton>
        </Link>
        <Button component={Link} to="/" onClick={logout}>
            Wyloguj
        </Button>
    </Fragment>;
UserBar = connect(state => state.user)(UserBar);
UserBar = userAuth(UserBar);

var ThemeSwitchIcon = ({isLight}) => isLight ? <WeatherNight /> : <WeatherSunny />;
ThemeSwitchIcon = connect(state => ({isLight: state.lightTheme}))(ThemeSwitchIcon);

var ThemeSwitch = ({toggleTheme}) =>
    <IconButton onClick={toggleTheme}><ThemeSwitchIcon /></IconButton>;
ThemeSwitch = connect(null, dispatch => (
    {toggleTheme: () => dispatch(actions.toggleTheme())}
))(ThemeSwitch);

const barStyle = {
    backgroundColor: "rgba(132, 255, 255, 0.3)",
    boxShadow: "none",
    backdropFilter: "blur(5px)"
};

var Bar = ({toggleMenu}) =>
    <AppBar position="sticky" style={barStyle}>
        <Toolbar>
            <Grid container>
                <IconButton onClick={toggleMenu}><Menu /></IconButton>
            </Grid>
            <ThemeSwitch />
            <LogInButton />
            <UserBar />
        </Toolbar>
    </AppBar>;
Bar = connect(null, dispatch => ({
    toggleMenu: () => dispatch(actions.toggleMenu())}))(Bar);

var Footer = () =>
    <BottomNavigation>
        <a href="https://github.com/piwonskp/tracnologia">
            <BottomNavigationAction label="Github" icon={<GithubCircle />} />
        </a>
    </BottomNavigation>;

export {Bar, Footer};
