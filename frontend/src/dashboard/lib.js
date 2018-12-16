import React from 'react';
import {FlexibleWidthXYPlot} from 'react-vis';

import {compose, withState, lifecycle} from 'recompose';


export var fetchOnMount = (fetch, getState) => lifecycle({
    componentDidMount() {fetch(resp => this.setState(getState(resp)));}
});

export const BasePlot = props =>
    <FlexibleWidthXYPlot height={320} margin={{left: 110}} {...props} />;
