exports.setHandlerImpl = function(inlet, message, handler) {
  if(messageHandlers[inlet] === undefined) {
    messageHandlers[inlet] = {};
  }

  messageHandlers[inlet][message] = handler;
}
