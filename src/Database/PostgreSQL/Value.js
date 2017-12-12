'use strict';

exports['null'] = null;

exports.instantToString = function (i) {
    return new Date(i).toUTCString();
};

exports.unsafeIsBuffer = function (x) {
    return x instanceof Buffer;
};

exports.compensateTZ = function (date) {
    var offset = date.getTimezoneOffset();
    return new Date(date.getTime() - (offset * 60 * 1000));
}