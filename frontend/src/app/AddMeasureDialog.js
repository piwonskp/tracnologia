import React, { Fragment } from 'react';
import { connect } from 'react-redux';
import {nest, withProps, defaultProps} from 'recompose';
import {Dialog, DialogTitle, DialogContent, InputAdornment, DialogContentText,
        DialogActions, Typography, Button, List, ListItem, ListItemText
       } from '@material-ui/core';
import {ArrowBackIos, ArrowForwardIos, DateRange,
        AccessTime, Cancel, CheckCircle} from '@material-ui/icons';
import { DateTimePicker } from 'material-ui-pickers';
import { Formik, Form, Field } from 'formik';
import {TextField} from 'formik-material-ui';
import FormGroup from '@material-ui/core/FormGroup';

import {store} from '../index';
import {post} from '../lib/api';
import {closeMeasureAdd, addReading} from '../actions';
import {TYPE_TO_DISPLAY, TYPE_TO_UNIT_HTML} from '../lib/meterTypes';

const DialogText = nest(DialogContent, DialogContentText);

var DateTimeField = defaultProps({
    leftArrowIcon: <ArrowBackIos />, rightArrowIcon: <ArrowForwardIos />,
    dateRangeIcon: <DateRange />, timeIcon: <AccessTime />,
    cancelLabel: <Fragment><Cancel />Anuluj</Fragment>,
    okLabel: <Fragment><CheckCircle />OK</Fragment>
})(DateTimePicker);

var DateTime = withProps(props => ({
    name: "timestamp",
    render: ({field}) =>
        <DateTimeField {...field} {...props} label="Data pomiaru" />}))(Field);

var meterDesc = meter =>
    <List disablePadding>
        <ListItem>
            <ListItemText primary={`Licznik: ${meter.name}`} />
        </ListItem>
        <ListItem>
            <ListItemText primary={`Typ: ${TYPE_TO_DISPLAY[meter.type]}`} />
        </ListItem>
    </List>;

var Description = ({meter}) =>
    <DialogText>
        {meter ? meterDesc(meter) : null}
    </DialogText>;

var unitLabel = type =>
    <InputAdornment position="end">{TYPE_TO_UNIT_HTML[type]}</InputAdornment>;

var Content = ({type, setFieldValue}) =>
    <DialogText>
        <FormGroup>
            <DateTime label="Data pomiaru"
            onChange={date => setFieldValue('timestamp', date)} />
            <Field name="value" type="number" component={TextField}
            label="Wartość odczytu"
            InputProps={{endAdornment: unitLabel(type), required: true}} />
        </FormGroup>
    </DialogText>;

var Actions = () =>
    <DialogActions>
        <Button type="submit">Dodaj</Button>
    </DialogActions>;

var form = ({values, setFieldValue}) =>
    <Form>
        <Content type={values.meter.type} setFieldValue={setFieldValue} />
        <Actions />
    </Form>;

var send = (reading, type) =>
    post('add-meterreading', reading,
         reading => store.dispatch(addReading({reading, type})));

var submit = closeDialog => function(reading){
    send({...reading, meter: reading.meter.id}, reading.meter.type);
    closeDialog();
};

var AddMeasureDialog = ({meter, closeDialog, timestamp}) =>
    <Dialog open={Boolean(meter)} onClose={closeDialog}>
        <DialogTitle>Dodaj nowy pomiar</DialogTitle>
        <Description meter={meter} />
        <Formik onSubmit={submit(closeDialog)} render={form}
            initialValues={{
                value: null, meter: meter, timestamp: new Date()
            }}/>
    </Dialog>;

AddMeasureDialog = connect(
    state => ({meter: state.newMeasureMeter}),
    dispatch => ({closeDialog: () => dispatch(closeMeasureAdd())})
)(AddMeasureDialog);

export default AddMeasureDialog;
