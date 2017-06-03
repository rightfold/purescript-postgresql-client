'use strict';

exports['null'] = null;

exports.instantToString = function(i) {
    return new Date(i).toUTCString();
};

exports.unsafeIsBuffer = function(x) {
    return x instanceof Buffer;
};
