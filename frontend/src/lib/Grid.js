import React from 'react';
import {Grid, Card, CardContent} from '@material-ui/core';
import {nest} from 'recompose';

export const CenteredGrid = props =>
    <div style={{display: "grid", gridGap: 16, marginTop: 16, marginBottom: 16,
                 gridTemplateColumns: props.gridTemplateColumns}} {...props} />;

const toItemStyle = ({gridRow, gridColumn}) =>
      ({gridRow: gridRow, gridColumn: gridColumn});
export const GridItem = props => <div style={toItemStyle(props)} {...props} />;

export const CardItem = nest(GridItem, Card);
export const CardContentItem = nest(CardItem, CardContent);

export const FlexItem = props =>
    <div style={{display: "flex", justifyContent: "center"}} {...props} />;
export const SingleItem = nest(CenteredGrid, FlexItem);
export const SingleCard = nest(SingleItem, Card);
export const SingleCardContent = nest(SingleCard, CardContent);
