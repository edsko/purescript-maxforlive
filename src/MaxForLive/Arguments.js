exports.getArgImpl = function(arguments, i) {
  if(i < arguments.length) {
    return arguments[i];
  } else {
    throw ( "getArgImpl: Argument "
          + i
          + " out of range ("
          + arguments.length
          + ")\n"
          );
  }
}

exports.getRemainingArgsImpl = function(arguments, i) {
  var remaining = [];
  var x = 0;

  for(x = 0; i + x < arguments.length; x++) {
    remaining[x] = arguments[x + i];
  }

  return remaining;
}
