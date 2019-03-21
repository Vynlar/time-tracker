import { Elm } from './src/Main.elm';
import { setToken, getToken } from './storage';
import { EDOM } from 'constants';

const urlParams = new URLSearchParams(window.location.search);
const oAuthCode = urlParams.get('code');

const app = Elm.Main.init({ node: document.getElementById('root'), flags: {
  simpleInOutAppId: process.env.SIMPLE_IN_OUT_APP_ID,
  oAuthCode,
  token: getToken(),
}});

app.ports.setToken.subscribe(token => {
  setToken(token);
});