'use strict';

// pg does strange thing converting DATE
// value to js Date, so we have
// to prevent this craziness
var pg = require('pg');
var DATE_OID = 1082;
pg.types.setTypeParser(DATE_OID, function(dateString) { return dateString; });

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
