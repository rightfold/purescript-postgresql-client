/* global Buffer, exports, require */
/* jshint -W097 */

'use strict';

exports['null'] = null;

exports.instantToString = function(i) {
    return new Date(i).toUTCString();
};

exports.instantFromString = function(Left) {
  return function(Right) {
    return function(s) {
      try {
        return Right(Date.parse(s));
      } catch(e) {
        return Left("Date string parsing failed: \"" + s + "\", with: " + e);
      }
    };
  };
};

exports.unsafeIsBuffer = function(x) {
    return x instanceof Buffer;
};
