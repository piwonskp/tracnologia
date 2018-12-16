import React, { Fragment } from 'react';
import { connect } from 'react-redux';
import {Link} from 'react-router-dom';
import {defaultProps} from 'recompose';
import {Drawer, Toolbar, Typography, MenuList, ListSubheader, MenuItem,
        ListItemIcon, ListItemText, IconButton, Switch,
        Divider} from '@material-ui/core';
import {Home, OfflineBolt, AddBox, Person} from '@material-ui/icons';
import {Water, Speedometer} from 'mdi-material-ui';
import {groupBy} from 'lodash';

import {openMeasureAdd, toggleMenu} from '../actions';
import {UserAuth, userAuth, AdminAuth, adminAuth} from '../lib/Authorization';
import {COLD_WATER, HOT_WATER, ELECTRICAL} from '../lib/meterTypes';
import AddMeasureDialog from './AddMeasureDialog';


const Logo = () =>
      <Toolbar>
        <Link to="/">
            <Typography variant="title" className="fullwidth"
                noWrap style={{fontFamily: 'Aclonica'}}>
                Tracnologia
            </Typography>
        </Link>
    </Toolbar>;

var UserNavigation = () =>
    <Fragment>
        <MenuLink to="/measurement/cold-water">
            <ListItemIcon><Water /></ListItemIcon>
            <ListItemText primary="Pomiary zimnej wody" />
        </MenuLink>
        <MenuLink to="/measurement/hot-water">
            <ListItemIcon><Water /></ListItemIcon>
            <ListItemText primary="Pomiary ciepłej wody" />
        </MenuLink>
        <MenuLink to="/measurement/electrical">
            <ListItemIcon><OfflineBolt /></ListItemIcon>
            <ListItemText primary="Pomiary prądu" />
        </MenuLink>
        <MenuLink to="/meter">
            <ListItemIcon><Speedometer /></ListItemIcon>
            <ListItemText primary="Liczniki" />
        </MenuLink>
    </Fragment>;
UserNavigation = userAuth(UserNavigation);

var AdminNavigation = () =>
    <Fragment>
    <MenuLink to="/admin/users">
        <ListItemIcon><Person /></ListItemIcon>
        <ListItemText primary="Użytkownicy" />
    </MenuLink>
    </Fragment>;
AdminNavigation = adminAuth(AdminNavigation);

var Navigation = () =>
    <MenuList>
        <MenuLink to='/'>
            <ListItemIcon><Home /></ListItemIcon>
            <ListItemText primary="Strona główna" />
        </MenuLink>
        <UserNavigation />
        <AdminNavigation />
    </MenuList>;

var MenuLink = ({children, to}) =>
    <Link to={to}>
        <MenuItem button>
            {children}
        </MenuItem>
    </Link>;

var AddMeasureMenuItem = ({meter, openMeasureAdd}) =>
    <MenuItem button  onClick={() => openMeasureAdd(meter)}>
        <ListItemIcon><AddBox /></ListItemIcon>
        <ListItemText primary={meter.name} />
    </MenuItem>;
AddMeasureMenuItem = connect(
    () => {},
    dispatch => ({
        openMeasureAdd: meter => dispatch(openMeasureAdd(meter))
    }))(AddMeasureMenuItem);
var addMeasureMenuItem = meter => <AddMeasureMenuItem meter={meter} />;

var listSubheader = text => <ListSubheader>{text}</ListSubheader>;

var meterList = (subheader, meters) =>
    <MenuList subheader={listSubheader(subheader)} >
        {meters.map(addMeasureMenuItem)}
    </MenuList>;

var typeToSubheader = {
    [COLD_WATER]: "Liczniki zimnej wody",
    [HOT_WATER]: "Liczniki ciepłej wody",
    [ELECTRICAL]: "Liczniki elektryczności"
};

var renderMeterTypes =
    meters => Object.entries(typeToSubheader).map(
        ([type, subheader]) =>
            meterList(subheader, (meters[type] || [])
                     )
);

var metersMenu = m => renderMeterTypes(groupBy(m, 'type'));

var Meters = ({meters}) =>
    <Fragment>
        {metersMenu(meters)}
        <AddMeasureDialog />
    </Fragment>;
Meters = connect(state => ({meters: state.meters}))(Meters);
Meters = userAuth(Meters);

var Menu = ({isOpen, toggleMenu}) =>
    <Drawer open={isOpen} onClose={toggleMenu}>
        <Logo />
        <Divider />
        <Navigation />
        <Divider />
        <Meters />
    </Drawer>;
Menu = connect(state => ({isOpen: state.menuOpened}),
               dispatch => ({toggleMenu: () => dispatch(toggleMenu())}))(Menu);

export default Menu;
