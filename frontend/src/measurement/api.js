import {store} from '../index';
import {get as baseGet} from '../lib/api';
import {setColdWater, setHotWater, setElectrical} from '../actions';


var mkUrl = type => "measure-types/" + type + "/meterreading/list";
var get = (type, fun) => baseGet(mkUrl(type), fun);

export var getColdWater =
    () => get('cold-water', resp => store.dispatch(setColdWater(resp)));
export var getHotWater =
    () => get('hot-water', resp => store.dispatch(setHotWater(resp)));
export var getElectrical =
    () => get('electrical', resp => store.dispatch(setElectrical(resp)));
