'use strict';

var pg = require('pg');

exports.ffiNewPool = function(config) {
    return function() {
        return new pg.Pool(config);
    };
};

exports.ffiConnect = function (config) {
    return function (pool) {
        return function (onError, onSuccess) {
            var p = pool.connect().then(function(client) {
                onSuccess(config.right({
                    connection: client,
                    done: function() {
                        return client.release();
                    }
                }));
            }).catch(function(err) {
                var pgError = config.nullableLeft(err)
                if (pgError) {
                    onSuccess(pgError)
                } else {
                    onError(err);
                }
            });

            return function (cancelError, cancelerError, cancelerSuccess) {
                p.cancel();
                cancelerSuccess();
            };
        };
    };
};

exports.ffiUnsafeQuery = function(config) {
    return function(client) {
        return function(sql) {
            return function(values) {
                return function(onError, onSuccess) {
                    var q = client.query({
                        text: sql,
                        values: values,
                        rowMode: 'array',
                    }).then(function(result) {
                        onSuccess(config.right(result))
                    }).catch(function(err) {
                        var pgError = config.nullableLeft(err);
                        if (pgError) {
                            onSuccess(pgError)
                        } else {
                            onError(err);
                        }
                    });

                    return function (cancelError, cancelerError, cancelerSuccess) {
                        q.cancel();
                        cancelerSuccess();
                    };
                };
            };
        };
    };
};

exports.ffiSQLState = function (error) {
    return error.code || null;
}

exports.ffiErrorDetail = function (error) {
    return {
        severity: error.severity || '',
        code: error.code || '',
        message: error.message || '',
        detail: error.detail || '',
        hint: error.hint || '',
        position: error.position || '',
        internalPosition: error.internalPosition || '',
        internalQuery: error.internalQuery || '',
        where_: error.where || '',
        schema: error.schema || '',
        table: error.table || '',
        column: error.column || '',
        dataType: error.dataType || '',
        constraint: error.constraint || '',
        file: error.file || '',
        line: error.line || '',
        routine: error.routine || ''
    };
}
