"use strict";

exports.logAny = function (a) {
  return function () {
    console.log(a);
    return {};
  };
};

exports.startServer = function (errback, callback) {
  var express = require('express');
  var app = express();
  var bodyParser = require('body-parser');

  // Always make req.body available as a String
  app.use(bodyParser.text(function() { return true; }));

  app.use(express.static(__dirname));

  app.get('/', function (req, res) {
    res.send('<html><script src="tmp/test.js"></script></html>');
  });

  app.get('/arrayview', function(req, res) {
    res.send('TODO');
  });

  app.get('/not-json', function(req, res) {
    res.header({'content-type': 'text/plain'});
    res.send('This is not JSON');
  });

  app.get('/mirror', function(req, res) {
    res.json(true);
  });

  app.all('/mirror', function(req, res) {
    res.json(JSON.parse(req.body));
  });

  app.put('/put', function(req, res) {
    res.json({ foo: 1, bar: JSON.parse(req.body) });
  });

  app.post('/unauthorized', function(req,res) {
    res.status(401).send();
  });

  var server = app.listen(function () {
    callback({ port: server.address().port, server: server });
  });
  server.on('error', function (error) {
    errback(error);
  });

  return function (cancelError, onCancelerError, onCancelerSuccess) {
    onCancelerSuccess();
  };
};

exports.stopServer = function (server) {
  return function (errback, callback) {
    server.close(function (err) {
      if (err) errback(err);
      else callback();
    });
    return function (cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
  };
};
