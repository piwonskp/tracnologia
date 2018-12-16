import React, { Fragment, Component } from 'react';
import {Route, Switch} from 'react-router-dom';

import './App.css';
import {store} from './index';
import {userAuth, UserAuth, AdminAuth, adminAuth} from './lib/Authorization';
import {get} from './lib/api';
import {setUserContext} from './actions';
import {Bar, Footer} from './app/Bar';
import Menu from './app/Menu';
import Dashboard from './dashboard/Dashboard';
import Login from './auth/Login';
import HotWaterMeasurement from './measurement/HotWaterMeasurement';
import ColdWaterMeasurement from './measurement/ColdWaterMeasurement';
import ElectricalMeasurement from './measurement/ElectricalMeasurement';
import Meter from './meter/Meter';
import UserList from './admin/Users';
import Profile from './profile/Profile';


var authorizeRoute = authHOC => ({props: {component, ...rest}}) =>
    <Route {...rest} component={authHOC(component)} />;
var authorizeRoutes = authHOC => routes => routes.map(authorizeRoute(authHOC));

var UserRoutes =
    [
            <Route path='/profile' component={Profile} />,
            <Route path="/measurement/cold-water"
        component={ColdWaterMeasurement} />,
            <Route path='/measurement/hot-water'
        component={HotWaterMeasurement} />,
            <Route path="/measurement/electrical"
        component={ElectricalMeasurement} />,
            <Route path="/meter" component={Meter} />,
    ];
UserRoutes = authorizeRoutes(userAuth)(UserRoutes);

var AdminRoutes =
    [
        <Route path="/admin/users" component={UserList} />
    ];
AdminRoutes = authorizeRoutes(adminAuth)(AdminRoutes);

var Main = () =>
    <main>
        <Switch>
            <Route exact path="/" component={Dashboard} />
            <Route path="/login" component={Login} />
            {UserRoutes}
            {AdminRoutes}
        </Switch>
    </main>;

var fetchUserContext = () =>
    get('get-context', ctx => store.dispatch(setUserContext(ctx)));


class App extends Component {
    constructor(props){
        fetchUserContext();
        super(props);
    }
  render() {
    return (
        <div className="App">
            <Bar />
            <Menu />
            <Main />
            <Footer />
        </div>
    );
  }
}

export default (App);
