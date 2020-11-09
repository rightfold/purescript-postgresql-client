var pg = require('pg');

"use sctrict";

exports.ffiNew = function(config) {
    return function() {
        return new pg.Pool(config);
    };
};


exports.totalCount = function(pool) {
  return function() {
    return pool.totalCount;
  };
};

exports.idleCount = function(pool) {
  return function() {
    return pool.idleCount;
  };
};

exports.waitingCount = function(pool) {
  return function() {
    return pool.waitingCount;
  };
};
