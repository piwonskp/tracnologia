import Cookies from 'js-cookie';
import { createBrowserHistory } from 'history';


export const history = createBrowserHistory();

export const API_ROOT = process.env.REACT_APP_BACKEND_URL + 'api/v1/';
var merge = Object.assign;

const BASE_OPTIONS = {credentials: 'include'};
var baseHeaders = () => ({"X-XSRF-TOKEN": Cookies.get("XSRF-TOKEN")});
var addBaseHeaders = headers => merge(baseHeaders(), headers);

var getOptions = () => ({headers: baseHeaders(), ...BASE_OPTIONS});

const POST_HEADERS = {"Content-Type": "application/json"};
var mkBody = data => ({method: "POST", body: JSON.stringify(data)});
var getPostHeaders = () => addBaseHeaders(POST_HEADERS);
var postOptions = data =>
    ({headers: getPostHeaders(), ...merge({}, BASE_OPTIONS, (mkBody(data)))});

export var post = (url, data, action) =>
    request(url, action, postOptions(data));

export var get = (url, action) => request(url, action, getOptions());

export var request = (url, action, options = {}) =>
    fetch(API_ROOT + url, options)
    .then(resp => resp.ok ? resp : Promise.reject(resp))
    .then(resp => resp.status != 204 ? resp.json() : null,
          resp => resp.status == 401 ?
          resp.text().then(txt => txt ? Promise.reject(JSON.parse(txt)) : resp)
          : resp)
    .then(action);
