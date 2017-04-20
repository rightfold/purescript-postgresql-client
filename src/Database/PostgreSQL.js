'use strict';

var Control_Monad_Aff = require('../Control.Monad.Aff');
var pg = require('pg');

exports.newPool = function(config) {
    return function(onSuccess, onError) {
        onSuccess(new pg.Pool(config));
        return Control_Monad_Aff.nonCanceler;
    };
};

exports.withConnection = function(pool) {
    return function(body) {
        return function(onSuccess, onError) {
            pool.connect(function(err, client, done) {
                if (err !== null) {
                    onError(err);
                    return;
                }
                body(client)(function(r) {
                    done();
                    onSuccess(r);
                }, function(e) {
                    done(e);
                    onError(e);
                });
            });
            return Control_Monad_Aff.nonCanceler;
        };
    };
};

exports._query = function(client) {
    return function(sql) {
        return function(values) {
            return function(onSuccess, onError) {
                client.query({
                    text: sql,
                    values: values,
                    rowMode: 'array',
                }, function(err, result) {
                    if (err !== null) {
                        onError(err);
                        return;
                    }
                    onSuccess(result.rows);
                });
                return Control_Monad_Aff.nonCanceler;
            };
        };
    };
};

exports.instantToString = function(i) {
    return new Date(i).toUTCString();
};

exports.unsafeIsBuffer = function(x) {
    return x instanceof Buffer;
};

exports['null'] = null;
