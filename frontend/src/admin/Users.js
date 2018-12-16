import React, { Component } from 'react';
import { connect } from 'react-redux';
import {compose, lifecycle} from 'recompose';
import {Table, TableHead, TableCell, TableRow, TableBody, CardHeader, CardActions,
       Button} from '@material-ui/core';
import {Add} from '@material-ui/icons';

import {store} from '../index';
import {SingleCard} from '../lib/Grid';
import {get} from '../lib/api';
import {setUsers} from '../actions';
import {mkState} from '../lib/recompose';
import {AddUser} from './AddUser';


var row = user =>
    <TableRow>
        <TableCell>{user.email}</TableCell>
        <TableCell>{user.name}</TableCell>
    </TableRow>;

var UserTable = ({users}) =>
    <Table>
        <TableHead>
            <TableRow>
                <TableCell>Email</TableCell>
                <TableCell>Imie</TableCell>
            </TableRow>
        </TableHead>
        <TableBody>
            {users.map(row)}
        </TableBody>
    </Table>;

var fetchUsers = () => get('users', users => store.dispatch(setUsers(users)));

var UserList = ({users, userDialog, setUserDialog}) =>
    <SingleCard>
        <CardHeader title="Użytkownicy" />
        <UserTable users={users} />
        <CardActions>
            <Button onClick={() => setUserDialog(true)}>
                <Add />Dodaj użytkownika
            </Button>
        </CardActions>
        <AddUser isOpen={userDialog} close={() => setUserDialog(false)} />
    </SingleCard>;
UserList = compose(mkState({userDialog: false}),
                   lifecycle({componentDidMount() {fetchUsers();}})
                  )(UserList);
UserList = connect(state => ({users: state.users}))(UserList);


export default UserList;
