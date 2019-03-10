import { Elm } from './src/Main.elm';

const urlParams = new URLSearchParams(window.location.search);
const oAuthCode = urlParams.get('code');

Elm.Main.init({ node: document.getElementById('root'), flags: {
  simpleInOutAppId: process.env.SIMPLE_IN_OUT_APP_ID,
  simpleInOutSecret: process.env.SIMPLE_IN_OUT_SECRET,
  oAuthCode,
}});
