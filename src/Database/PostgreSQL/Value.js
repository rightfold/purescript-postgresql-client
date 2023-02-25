/* global Buffer, exports, require */
/* jshint -W097 */

export const null_ = null;

export const instantToString = function(i) {
    return new Date(i).toUTCString();
};

export const instantFromString = function(Left) {
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

export const unsafeIsBuffer = function(x) {
    return x instanceof Buffer;
};
