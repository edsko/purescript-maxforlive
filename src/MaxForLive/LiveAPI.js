exports.withPath = function(path) {
  return function() {
    return new LiveAPI(null, path);
  }
}

exports.withId = function(id) {
  return function() {
    return new LiveAPI(null, id);
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

exports.idToString = function(id) {
  return String(id);
}

exports.unquotedPath = function(obj) {
  return obj.unquotedpath;
}

exports.getCount = function(path, liveAPI) {
  return liveAPI.getcount(path);
}

exports.grabControl = function(liveAPI) {
  return function(control) {
    return function() {
      liveAPI.call("grab_control", control);
    }
  }
}

exports.releaseControl = function(liveAPI) {
  return function(control) {
    return function() {
      liveAPI.call("release_control", control);
    }
  }
}

exports.getControlId = function(liveAPI) {
  return function(control) {
    return function() {
      return liveAPI.call("get_control", control);
    }
  }
}

exports.setButtonMatrixColor = function(liveAPI) {
  return function(args) {
    return function() {
      liveAPI.call("send_value", args.col, args.row, args.color);
    }
  }
}
