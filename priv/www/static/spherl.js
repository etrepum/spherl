var WS = window.WebSocket || window.MozWebSocket,
    console = window.console || {log: function () {}},
    d3 = window.d3;
(function () {
  if (!WS) {
    d3.select('body').append('p').text('WebSockets not supported!');
    return;
  }
  var body = d3.select('body');
  var devices = {};
  var update = function () {
  };
  var handlers = {
    device_list: function (lst) {
      devices = lst;
    },
    clock: function (now) {
    },
    device_connected: function (name) {
      devices[name] = true;
    },
    device_disconnected: function (name) {
      devices[name] = false;
    }
  };
  var handle_event = function (evt) {
    var f = handlers[evt[0]];
    if (f) {
      f.apply(handlers, evt.slice(1));
    }
    body.append('div').text(JSON.stringify(evt));
  };
  var send = function (body) {
    ws.send(JSON.stringify(body));
  };
  var ws = new WS('ws://' + window.location.host + '/ws');
  ws.onopen = function () {
    body.append('div').text('connected');
  };
  ws.onclose = function () {
    body.append('div').text('disconnected');
  };
  ws.onerror = function (err) {
    body.append('div').text('error: ' + JSON.stringify(err));
  };
  ws.onmessage = function (evt) {
    var msg = JSON.parse(evt.data);
    console.log(msg);
    (msg.events || []).forEach(handle_event);
    update();
  };
  window._getState = function () {
    return {ws: ws,
            devices: devices};
  };
})();