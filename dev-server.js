const proxy = require('http-proxy-middleware');
const Bundler = require('parcel-bundler');
const app = require('express')();

const file = "index.html";
const options = {};

const bundler = new Bundler(file, options);

app.use('/api', proxy('/api', {
  target: "http://localhost:4000",
}));

app.use(bundler.middleware());

app.listen(1234);
