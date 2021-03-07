exports.withPath = function(path) {
  return function() {
    return new LiveAPI(null, path);
  }
}

exports.parentOfType = function(just, nothing, parentType, path) {
  var parentObj  = null;
  var parentPath = path;

  var i = 0;

  // We're a bit paranoid here; for justification, see
  // https://cycling74.com/forums/livemax-hang-live-at-100-cpu-when-saving-patch
  var loopProtection = 20;

  do {
    loopProtection--;
    parentPath = parentPath + " canonical_parent";
    parentObj  = new LiveAPI(null, parentPath);
  } while( (parentObj.type !== parentType)
        && (parentObj.id != 0)
        && loopProtection > 0
         );

  if(loopProtection == 0 || parentObj.id == 0) {
    return nothing;
  } else {
    return just(parentObj);
  }
}

exports.withId = function(id) {
  return function() {
    return new LiveAPI(null, id);
  }
}

exports.id = function(obj) {
  var id = obj.id;

  if(typeof(id) === 'string') {
    // For some reason, IDs are returned as strings 🤦‍♂️
    return parseInt(id);
  } else if(typeof(id) === 'number') {
    return id;
  } else {
    error("id: unexpected ID", id, "of type", typeof(id), "\n");
  }
}

exports.objectType = function(obj) {
  return obj.type;
}

exports.unquotedPath = function(obj) {
  return obj.unquotedpath;
}

exports.getCount = function(path, liveAPI) {
  return liveAPI.getcount(path);
}

exports.idFromMax = function(id) {
  if(typeof(id) === 'number') {
    return id;
  } else {
    error("idFromMax: unexpected ID", id, "of type", typeof(id), "\n");
  }
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
  return function(button) {
    return function(color) {
      return function() {
        liveAPI.call("send_value", button.col, button.row, color);
      }
    }
  }
}
