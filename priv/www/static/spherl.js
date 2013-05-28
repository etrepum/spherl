var WS = window.WebSocket || window.MozWebSocket,
    console = window.console || {log: function () {}},
    d3 = window.d3;
(function () {
  if (!WS) {
    d3.select('body').append('p').text('WebSockets not supported!');
    return;
  }
  var body = d3.select('body');
  var devicesTableBody = body.select('table.devices tbody');
  var devices = {};
  var deviceList = [];
  d3.select('form').on('submit', function () { d3.event.preventDefault(); console.log('form submit'); });
  devicesTableBody.on('click', function () {
    var elem = d3.event.target;
    if (elem.tagName !== 'BUTTON') {
      return;
    }
    d3.event.preventDefault();
    while (elem !== null && elem !== this) {
      if (elem.tagName == 'TR') {
        d3.select(elem).data().forEach(function (data) {
          send([data[1] ? 'disconnect' : 'connect', data[0]]);
        });
        return;
      }
      elem = elem.parentNode;
    }
  });
  var id = function (x) { return x; };
  var indexKey = function (_d, i) { return i; };
  var first = function (x) { return x[0]; };
  var update = function () {
    deviceList = Object.keys(devices).map(function (k) {
      return [k, devices[k]];
    });
    deviceList.sort(function (a, b) { return a[0].localeCompare(b[0]); });
    var tr = devicesTableBody.selectAll('tr')
          .data(deviceList, first);
    tr.enter().append('tr');
    var td = tr.selectAll('td').data(id, indexKey);
    td.enter().append('td').filter(':nth-child(2)').append('button');
    td.filter(':nth-child(1)').text(id);
    td.select('button').text(function (enabled) { return (enabled ? 'Disconnect' : 'Connect'); });
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
    console.log(evt);
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
    (msg.events || []).forEach(handle_event);
    update();
  };
  window._getState = function () {
    return {ws: ws,
            devices: devices,
            deviceList: deviceList,
            update: update};
  };
})();