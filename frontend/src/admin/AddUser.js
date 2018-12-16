import React from 'react';
import {flow} from 'lodash';
import {Button, Dialog, DialogTitle, DialogActions, DialogContent, FormGroup
       } from '@material-ui/core';
import { Formik, Form, Field } from 'formik';
import {TextField} from 'formik-material-ui';

import {store} from '../index';
import {post} from '../lib/api';
import {addUser} from '../actions';

var Fields = () =>
    <FormGroup>
        <Field name="email" type="email" component={TextField} label="Email" />
        <Field name="name" component={TextField} label="Imię" />
        <Field name="password" type="password" component={TextField} label="Hasło" />
    </FormGroup>;

var AddUserForm = () =>
    <Form>
        <DialogContent>
            <Fields />
        </DialogContent>
        <DialogActions>
            <Button type="submit">Dodaj</Button>
        </DialogActions>;
    </Form>;

const send = user => post('add-user', user, u => store.dispatch(addUser(u)));

const submit = close => flow([send, close]);

export var AddUser = ({isOpen, close}) =>
    <Dialog open={isOpen} onClose={close}>
        <DialogTitle>Dodaj nowego użytkownika</DialogTitle>
        <Formik initialValues={{email: null, password: null, name:null}}
            render={AddUserForm} onSubmit={submit(close)}/>
    </Dialog>;
