import React from 'react';
import { connect } from 'react-redux';


var authorize = (predicate, user, Component, props) =>
    predicate(user) ? <Component {...props} /> : null;

var authorization = predicate =>
    WrappedComponent =>
    connect(
        state => ({user: state.user})
    )(
        ({user, ...props}) =>
            authorize(predicate, user, WrappedComponent, props)
    );

const anonymousOnly = user => !user;
const atLeastUser = user => user;
const isAdmin = user => atLeastUser(user) && user.isAdmin;

export const anonymousAuth = authorization(anonymousOnly);
export const userAuth = authorization(atLeastUser);
export const adminAuth = authorization(isAdmin);

var AuthorizationComponent = React.Fragment;
var Authorization = predicate => authorization(predicate, AuthorizationComponent);

export const AnonymousAuth = Authorization(anonymousOnly);
export const UserAuth = Authorization(atLeastUser);
export const AdminAuth = Authorization(isAdmin);

