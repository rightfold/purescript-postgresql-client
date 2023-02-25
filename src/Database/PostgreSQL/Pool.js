import pg from 'pg';

export const ffiNew = function(config) {
    return function() {
        return new pg.Pool(config);
    };
};


export const totalCount = function(pool) {
  return function() {
    return pool.totalCount;
  };
};

export const idleCount = function(pool) {
  return function() {
    return pool.idleCount;
  };
};

export const waitingCount = function(pool) {
  return function() {
    return pool.waitingCount;
  };
};
