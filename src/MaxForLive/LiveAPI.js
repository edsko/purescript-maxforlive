exports.new = function(path) {
  return function() {
    return new LiveAPI(null, path);
  }
}

exports.id = function(obj) {
  return obj.id;
}

exports.objectType = function(obj) {
  return obj.type;
}

exports.sameIdImpl = function(id1, id2) {
  return id1 === id2;
}

exports.unquotedPath = function(obj) {
  return obj.unquotedpath;
}

exports.getCount = function(path, liveAPI) {
  return liveAPI.getcount(path);
}
