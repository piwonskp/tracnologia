import React, {Fragment} from 'react';
import {mapValues} from 'lodash';


export const COLD_WATER = "ColdWater";
export const HOT_WATER = "HotWater";
export const ELECTRICAL = "Electrical";

export const TYPE_TO_DISPLAY = {
    [COLD_WATER]: "Zimna woda",
    [HOT_WATER]: "Ciepła woda",
    [ELECTRICAL]: "Elektryczność"
};

const Sup = props => <sup {...props} />;
const TSpanSup = props => <tspan baseline-shift="super" {...props} />;

const waterUnit = Sup => <Fragment>dm<Sup>3</Sup></Fragment>;
const HTML_WATER = waterUnit(Sup);
const SVG_WATER = waterUnit(TSpanSup);
const ELECTRICAL_UNIT = 'Wh';
export const TYPE_TO_UNIT_HTML = mapValues({
    [COLD_WATER]: HTML_WATER,
    [HOT_WATER]: HTML_WATER,
    [ELECTRICAL]: ELECTRICAL_UNIT
}, unit => <span>{unit}</span>);

export const TYPE_TO_UNIT_SVG = {
    [COLD_WATER]: SVG_WATER,
    [HOT_WATER]: SVG_WATER,
    [ELECTRICAL]: ELECTRICAL_UNIT
};
