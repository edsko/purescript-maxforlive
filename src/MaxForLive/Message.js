exports.mkMaxMessage = function(msg, payload) {
  // If the payload is an array, insert the message at the front
  // This is the format that `route` and co expect
  if(Array.isArray(payload)) {
    return [msg].concat(payload);
  } else {
    return [msg, payload];
  }
}
