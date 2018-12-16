import React from 'react';
import {Grid,Typography, Button, Snackbar, FormGroup,
        Card, CardContent, CardActions, CardHeader} from '@material-ui/core';
import { Formik, Form, Field } from 'formik';
import {TextField} from 'formik-material-ui';
import {flow, partial} from 'lodash';
import {nest} from 'recompose';

import {store} from '../index';
import {postLogin} from './AuthAPI';
import {setUserContext} from '../actions';
import {SingleCard} from '../lib/Grid';


const INVALID_CREDENTIALS = "Nieprawidłowy adres email lub hasło";

function loginFailed (setErrors, setFieldValue){
    setFieldValue('password', "", false);
    setErrors({email: true, password:true});
}

function loggedIn(history, userContext){
    store.dispatch(setUserContext(userContext));
    history.push('/');
}

var submit = history => (data, {setErrors, setFieldValue, setSubmitting}) =>
    postLogin(data, partial(loggedIn, history), flow([
        () => loginFailed(setErrors, setFieldValue), () => setSubmitting(false)]));

var form = ({errors, isSubmitting}) =>
    <Form>
        <Snackbar message={INVALID_CREDENTIALS}
            open={errors.email || errors.password}/>
        <CardHeader title="Logowanie" />
        <CardContent>
            <FormGroup>
                <Field type="email" label="Email" name="email"
                    component={TextField}/>
                <Field type="password" label="Hasło" component={TextField}
                    name="password"/>
            </FormGroup>
        </CardContent>
        <CardActions>
            <Button variant="contained" color="primary" type="submit"
                disabled={isSubmitting}>
                Zaloguj się
            </Button>
        </CardActions>
    </Form>;

var Login = ({history}) =>
    <SingleCard>
        <Formik initialValues={{ email: '', password: '' }}
            onSubmit={submit(history)} render={form} />
    </SingleCard>;

export default Login;
