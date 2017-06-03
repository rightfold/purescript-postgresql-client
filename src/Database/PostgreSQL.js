'use strict';

var pg = require('pg');

exports.ffiNewPool = function(config) {
    return function() {
        return new pg.Pool(config);
    };
};

exports.ffiConnect = function(pool) {
    return function(onError) {
        return function(onSuccess) {
            return function() {
                pool.connect(function(err, client, done) {
                    if (err !== null) {
                        onError(err)();
                        return;
                    }
                    onSuccess({connection: client, done: done})();
                });
            };
        };
    };
};

exports.ffiUnsafeQuery = function(client) {
    return function(sql) {
        return function(values) {
            return function(onError) {
                return function(onSuccess) {
                    return function() {
                        client.query({
                            text: sql,
                            values: values,
                            rowMode: 'array',
                        }, function(err, result) {
                            if (err !== null) {
                                onError(err)();
                                return;
                            }
                            onSuccess(result.rows)();
                        });
                    };
                };
            };
        };
    };
};
