import {compose, withState} from 'recompose';
import {pascalCase} from 'change-case';


const toRecompose = ([name, defaultValue]) =>
      withState(name, "set" + pascalCase(name), defaultValue);
const stateArgs = defaultState => Object.entries(defaultState).map(toRecompose);
export const mkState = s => compose(...stateArgs(s));
