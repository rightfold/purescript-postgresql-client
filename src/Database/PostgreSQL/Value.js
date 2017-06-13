'use strict';
var types = require('pg').types;

var DATE_OID = 1082;
var parseFn = function (val) {
    return val;
}

types.setTypeParser(DATE_OID, parseFn)

exports['null'] = null;

exports.instantToString = function (i) {
    return new Date(i).toUTCString();
};

exports.unsafeIsBuffer = function (x) {
    return x instanceof Buffer;
};
