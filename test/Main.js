import express from 'express'
import bodyParser from 'body-parser'

export const logAny = function (a) {
  return function () {
    console.log(a);
    return {};
  };
};

export const startServer = function (errback, callback) {
  // var express = require('express');
  var app = express();
  // var bodyParser = require('body-parser');

  // Always make req.body available as a String
  // app.use(bodyParser.text(function() { return true; }));
  app.use(bodyParser.json({
    strict: false
  }))

  // app.use(express.static(__dirname));

  app.get('/', function (req, res) {
    res.send('<html><script src="tmp/test.js"></script></html>');
  });

  app.get('/json-404', function(req, res) {
    res.status(404).send();
  });

  app.get('/arrayview', function(req, res) {
    res.send('TODO');
  });

  app.post('/register', function(req, res) {
    res.set('Authorization', 'Bearer: fake_token');
    res.json({result: "Thanks"});
  });

  app.get('/not-json', function(req, res) {
    res.header({'content-type': 'text/plain'});
    res.send('This is not JSON');
  });

  app.get('/mirror', function(req, res) {
    res.json(true);
  });

  app.all('/mirror', function(req, res) {
    res.json(req.body);
  });

  app.put('/put', function(req, res) {
    res.json({ foo: 1, bar: req.body });
  });

  app.post('/unauthorized', function(req,res) {
    res.status(401).send();
  });

  app.get('/wrong-json', function(req, res) {
    res.json({ bar: 2 });
  })

  var retry = 0
  app.delete('/timed_fails', function(req, res) {
    if (retry < 3) {
      retry++
      return
    }
    res.json({ foo: "bar" })
  })

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

export const stopServer = function (server) {
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
