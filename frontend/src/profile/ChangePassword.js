import React from 'react';
import {flow} from 'lodash';
import {defaultProps} from 'recompose';
import {Typography, Divider, CardHeader, withStyles, withTheme,
        CardContent, List, ListItem, ListItemText, ListSubheader, FormGroup,
        CardActions, Button, ExpansionPanel, ExpansionPanelSummary,
        ExpansionPanelDetails, ExpansionPanelActions, Snackbar
       } from '@material-ui/core';
import {ExpandMore} from '@material-ui/icons';
import { Formik, Form, Field } from 'formik';
import {TextField} from 'formik-material-ui';
import {LockReset} from 'mdi-material-ui';
import * as yup from 'yup';

import {post} from '../lib/api';
import {mkState} from '../lib/recompose';


const PASS_DID_NOT_MATCH = "Podane hasła nie pokrywają się";

const PasswordChangeSchema = yup.object().shape({
    newPassword: yup.string().oneOf([yup.ref('newPassword1')], PASS_DID_NOT_MATCH),
    newPassword1: yup.string().oneOf([yup.ref('newPassword')], PASS_DID_NOT_MATCH)
});

const INITIAL_VALUES = {password: '', newPassword: '', newPassword1: ''};

var submit = setSuccess => ({password, newPassword}, {setFieldValue, setValues, setFieldError, resetForm, setSubmitting}) =>
    post('change-password', {password, newPassword},
    flow([setSuccess, resetForm, () => setSubmitting(false)])).catch(flow([
    // XXX: Workaround. setValues() validates fields automatically overwriting backend validation errors
    () => Object.entries(INITIAL_VALUES).map(([k, v]) => setFieldValue(k, v, false)),
    () => setFieldError('password', "Podano nieprawidłowe hasło"),
    () => setSubmitting(false)
]));

var PasswordChangeForm = ({values, errors, isSubmitting}) =>
    <Form>
        <ExpansionPanelDetails>
            <FormGroup>
                <PasswordField name="password" label="Obecne hasło" />
                <PasswordField name="newPassword" label="Nowe hasło" />
                <PasswordField name="newPassword1" label="Powtórz nowe hasło" />
            </FormGroup>
        </ExpansionPanelDetails>
        <ExpansionPanelActions>
            <Button variant="contained" color="primary" type="submit"
                disabled={isSubmitting}>
                Zmień hasło
            </Button>
        </ExpansionPanelActions>
    </Form>;

var ChangePassword = ({changed, setChanged}) =>
    <ExpansionPanel>
        <ExpansionPanelSummary expandIcon={<ExpandMore />}>
            <Typography variant="h6"><LockReset /> Zmiana hasła</Typography>
        </ExpansionPanelSummary>
        <Formik render={PasswordChangeForm} initialValues ={INITIAL_VALUES}
            onSubmit={submit(() => setChanged(true))}
            validationSchema={PasswordChangeSchema} />

        <Snackbar message="Hasło zostało pomyślnie zmienione" open={changed}
            onClose={() => setChanged(false)} />
    </ExpansionPanel>;
ChangePassword = mkState({changed: false})(ChangePassword);

const PasswordField = defaultProps({type: "password", component: TextField})(Field);

export {ChangePassword};
