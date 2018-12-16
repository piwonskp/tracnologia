import React, {Fragment} from 'react';
import {connect} from 'react-redux';
import {compose, withProps} from 'recompose';
import {Typography, Divider, CardHeader, withStyles, withTheme,
        CardContent, List, ListItem, ListItemText, ListSubheader, FormGroup,
        CardActions, Button, ExpansionPanel, ExpansionPanelSummary,
        ExpansionPanelDetails, ExpansionPanelActions, Snackbar
       } from '@material-ui/core';
import {Description, ExpandMore} from '@material-ui/icons';

import {SingleCard, SingleItem} from '../lib/Grid';
import {ChangePassword} from './ChangePassword';


var BasicInfo = ({email, name}) =>
    <ExpansionPanel defaultExpanded>
        <ExpansionPanelSummary expandIcon={<ExpandMore />}>
        <Typography variant="h6">
            <Description /> Podstawowe informacje
        </Typography>
        </ExpansionPanelSummary>
        <ExpansionPanelDetails>
            <List>
                <ListItem>
                    <ListItemText primary={`Email: ${email}`} />
                </ListItem>
                <ListItem>
                    <ListItemText primary={`Imię: ${name}`} />
                </ListItem>
            </List>
        </ExpansionPanelDetails>;
   </ExpansionPanel>;

var Profile = ({user}) =>
    <SingleItem>
        <div>
            <BasicInfo email={user.email} name={user.name} />
            <ChangePassword />
        </div>
    </SingleItem>;
Profile = connect(state => ({user: state.user}))(Profile);

export default Profile;
