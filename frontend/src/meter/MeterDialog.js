import React from 'react';
import { connect } from 'react-redux';
import {Dialog, DialogContent, DialogTitle, DialogContentText,
        DialogActions, Button, MenuItem, InputLabel
       } from '@material-ui/core';
import { Formik, Form, Field } from 'formik';
import {TextField, Select} from 'formik-material-ui';

import {store} from '../index';
import {post} from '../lib/api';
import {closeMeterAdd, addMeter} from '../actions';
import {COLD_WATER} from '../lib/meterTypes';
import {TYPE_TO_DISPLAY} from '../lib/meterTypes';


var postMeter = meter =>
    post('meter/add', meter, meter => store.dispatch(addMeter(meter)));


var typeMenuItems = () => Object.entries(TYPE_TO_DISPLAY).map(
    ([itemType, text]) =>
        <MenuItem value={itemType}>{text}</MenuItem>
);

function submit(meter){
    postMeter(meter);
    store.dispatch(closeMeterAdd());
}

var form = ({isSubmitting}) =>
    <Form>
        <DialogContent>
            <DialogContentText>
                <InputLabel htmlFor="new-meter-type-select">
                    Typ
                </InputLabel>
                <div>
                    <Field component={Select} name="type"
                        inputProps={{id: "new-meter-type-select"}}>
                        {typeMenuItems()}
                    </Field>
                </div>
                <Field label="Nazwa" name="name" component={TextField}/>
            </DialogContentText>
        </DialogContent>
        <DialogActions>
            <Button type="submit" disabled={isSubmitting}>
                Dodaj
            </Button>
        </DialogActions>
    </Form>;

var AddMeterDialog  = ({isOpen, closeDialog}) =>
    <Dialog open={isOpen} onClose={closeDialog}>
        <DialogTitle>Dodaj nowy licznik</DialogTitle>
        <Formik initialValues={{type: COLD_WATER, name: ""}}
            onSubmit={submit} render={form} />
    </Dialog>;
AddMeterDialog = connect(
    state => ({isOpen: state.newMeterDialog}),
    dispatch => ({closeDialog: () => dispatch(closeMeterAdd())})
)(AddMeterDialog);

export default AddMeterDialog;
