'use strict';

var pg = require('pg');

exports.ffiNewPool = function(config) {
    return function() {
        return new pg.Pool(config);
    };
};

exports.ffiConnect = function (pool) {
    return function (onError, onSuccess) {
        var p = pool.connect(
            ).then(function(client) {
                onSuccess({
                    connection: client,
                    done: function() {
                        return client.release();
                    }
                });
            }).catch(function(err) {
                onError(err);
            });

        return function (cancelError, cancelerError, cancelerSuccess) {
          p.cancel();
          cancelerSuccess();
        };
    };
};

exports.ffiUnsafeQuery = function(client) {
    return function(sql) {
        return function(values) {
            return function(onError, onSuccess) {
                var q = client.query({
                        text: sql,
                        values: values,
                        rowMode: 'array',
                    }).catch(function(err) {
                        onError(err);
                    }).then(function(result) {
                        onSuccess(result.rows);
                    });

                return function (cancelError, cancelerError, cancelerSuccess) {
                    q.cancel();
                    cancelerSuccess();
                };
            };
        };
    };
};
