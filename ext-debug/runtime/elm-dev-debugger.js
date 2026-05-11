(function () {
  "use strict";

  var scope = typeof window !== "undefined" ? window : this;
  var existing = scope.__ELM_DEV_DEBUGGER__;

  if (existing && existing.version) {
    return;
  }

    var runtimeVersion = "0.1.3-json-tree";

  if (typeof console !== "undefined" && console.info) {
    console.info("[elm-dev][debugger] Elm Dev Debugger runtime loaded", runtimeVersion);
  }

  function now() {
    return typeof performance !== "undefined" && performance.now
      ? performance.now()
      : Date.now();
  }

  function messageSummary(value) {
    if (typeof value === "boolean") return value ? "True" : "False";
    if (typeof value === "number") return String(value);
    if (typeof value === "string") return JSON.stringify(value);
    if (value instanceof String) return "'" + String(value) + "'";
    if (!value || typeof value !== "object" || !("$" in value)) return "<internals>";

    var tag = value.$;
    if (typeof tag !== "string") return "<internals>";
    if (tag === "::" || tag === "[]") return "List";
    if (tag.charAt(0) === "#") return "Tuple";

    var keys = Object.keys(value).filter(function (key) { return key !== "$"; });
    if (keys.length === 0) return tag;
    if (keys.length === 1) return tag + " " + messageSummary(value[keys[0]]);
    return tag + " ...";
  }

  function inspect(value, depth) {
    if (depth <= 0) return { kind: "opaque", label: "..." };

    if (typeof value === "boolean") return { kind: "bool", value: value };
    if (typeof value === "number") return { kind: "number", value: String(value) };
    if (typeof value === "string") return { kind: "string", value: value };
    if (value instanceof String) return { kind: "char", value: String(value) };
    if (typeof value === "function") return { kind: "opaque", label: "<function>" };
    if (value == null) return { kind: "opaque", label: "<null>" };

      if (typeof value === "object" && "$" in value) {
        var tag = value.$;

        if (tag === "Set_elm_builtin") {
          return inspect(value.a, depth - 1);
        }

        if (tag === "Dict_elm_builtin") {
          return inspect(value.a, depth - 1);
        }

      if (tag === "RBEmpty_elm_builtin" || tag === "RBNode_elm_builtin") {
        var entries = rbTreeToEntries(value, depth - 1);
        var isSet = entries.every(function (entry) { return isUnit(entry.value); });

        if (isSet) {
          return { kind: "set", items: entries.map(function (entry) { return entry.key; }) };
        }

        return { kind: "dict", entries: entries };
      }

      if (tag === "Array_elm_builtin") {
        return { kind: "array", items: elmArrayToItems(value, depth - 1) };
      }

      if (tag === "[]") return { kind: "list", items: [] };

      if (tag === "::") {
        var items = [];
        for (var list = value; list && list.b; list = list.b) {
          items.push(inspect(list.a, depth - 1));
        }
        return { kind: "list", items: items };
      }

      if (typeof tag === "number") return { kind: "opaque", label: "<internals>" };

      var args = [];
      for (var key in value) {
        if (key !== "$") args.push(inspect(value[key], depth - 1));
      }

      return {
        kind: "constructor",
        name: tag.charAt(0) === "#" ? null : tag,
        args: args
      };
    }

    if (typeof value === "object") {
      var fields = [];
      for (var field in value) {
        fields.push({ name: field.charAt(0) === "_" ? field.slice(1) : field, value: inspect(value[field], depth - 1) });
      }
      return { kind: "record", fields: fields };
    }

    return { kind: "opaque", label: "<internals>" };
  }

  function isUnit(value) {
    return value && typeof value === "object" && value.$ === "#0";
  }

  function rbTreeToEntries(tree, depth) {
    if (!tree || tree.$ === "RBEmpty_elm_builtin") return [];
    if (tree.$ !== "RBNode_elm_builtin") return [];

    return rbTreeToEntries(tree.d, depth)
      .concat([{ key: inspect(tree.b, depth), value: inspect(tree.c, depth) }])
      .concat(rbTreeToEntries(tree.e, depth));
  }

  function elmArrayToItems(array, depth) {
    var items = [];

    function walk(node) {
      if (!node) return;

      if (Array.isArray(node)) {
        for (var i = 0; i < node.length; i++) {
          walk(node[i]);
        }
        return;
      }

      if (node.$ === "Array_elm_builtin") {
        walk(node.b || node.c || node.d);
        return;
      }

      items.push(inspect(node, depth));
    }

    walk(array.b || array.c || array.d);
    return items;
  }

  function createRuntime() {
    var events = [];
    var listeners = [];
    var apps = [];
    var appById = {};
    var nextAppId = 1;
    var currentApp = null;
    var debuggerApp = null;
    var fallbackPanel = null;

    installLazyProfiler(function () { return currentApp; });

    function publishToDebugger(event, store) {
      if (typeof event.id !== "number") {
        event.id = events.length;
      }

      if (store !== false) {
        events.push(event);
      }

      for (var i = 0; i < listeners.length; i++) listeners[i](event);

      debuggerApp = debuggerApp || startDebuggerApp(events);
      if (debuggerApp) {
        debuggerApp.send(event);
      } else {
        fallbackPanel = fallbackPanel || installFallbackPanel(events);
        fallbackPanel && fallbackPanel.render();
      }

      return event.id;
    }

    function createApp(kind, metadata, args) {
      var app = createDebuggerApp(
        "elm-dev-app-" + nextAppId++,
        kind,
        metadata,
        args,
        publishToDebugger,
        function (callback) { return runWithApp(app, callback); }
      );

      apps.push(app);
      appById[app.id] = app.agent;
      return app;
    }

    function runWithApp(app, callback) {
      var previous = currentApp;
      currentApp = app;
      try {
        return callback();
      } finally {
        currentApp = previous;
      }
    }

    return {
      version: runtimeVersion,
      events: events,
      listApps: function () {
        return apps.map(function (app) { return app.summary(); });
      },
      getApp: function (id) {
        return appById[id];
      },
      subscribe: function (listener) {
        listeners.push(listener);
        for (var i = 0; i < events.length; i++) listener(events[i]);
        return function () {
          listeners = listeners.filter(function (candidate) { return candidate !== listener; });
        };
      },
      createApp: createApp,
      runWithApp: runWithApp,
      installLazyProfiler: function () { installLazyProfiler(function () { return currentApp; }); },
      inspect: inspect,
      messageSummary: messageSummary
    };
  }

  function createDebuggerApp(id, kind, metadata, args, publishToDebugger, runWithApp) {
    var events = [];
    var listeners = [];
    var frameScheduled = false;
    var frameDuration = 0;
    var frameRenderDuration = 0;
    var messageCount = 0;
    var latestModel = null;
    var lastMessage = null;
    var lastUpdate = null;
    var lastFrame = null;
    var latestLazy = [];
    var appStartedAt = now();
    var lazyProfiler = createLazyProfiler(publishPerformance);
    var app = {
      id: id,
      agent: null,
      summary: summary,
      recordInit: recordInit,
      recordUpdate: recordUpdate,
      recordRender: recordRender,
      recordLazyOpportunity: lazyProfiler.recordOpportunity,
      recordLazyRender: lazyProfiler.recordRender,
      run: runWithApp
    };

    app.agent = {
      id: id,
      latest: latest,
      listen: listen
    };

    return app;

    function summary() {
      return {
        id: id,
        kind: kind,
        module: moduleNameFromMetadata(metadata),
        node: nodeLabel(args),
        startedAt: appStartedAt,
        messageCount: messageCount
      };
    }

    function latest() {
      return {
        appId: id,
        model: latestModel || undefined,
        lastMessage: lastMessage || undefined,
        lastUpdate: lastUpdate || undefined,
        lastFrame: lastFrame || undefined,
        lazy: latestLazy
      };
    }

    function listen(listener) {
      listeners.push(listener);
      return function () {
        listeners = listeners.filter(function (candidate) { return candidate !== listener; });
      };
    }

    function publish(event, store, visualEvent) {
      event.appId = id;
      var outgoing = visualEvent || event;
      outgoing.appId = id;
      event.id = publishToDebugger(outgoing, store);
      outgoing.id = event.id;

      if (store !== false) events.push(event);

      for (var i = 0; i < listeners.length; i++) listeners[i](event);
    }

    function recordInit(model, initMetadata) {
      latestModel = inspect(model, 1000);
      publish({ type: "init", metadata: initMetadata || metadata || null, model: latestModel });
    }

    function recordUpdate(msg, before, after, duration) {
      messageCount++;
      lastMessage = inspect(msg, 1000);
      latestModel = inspect(after, 1000);
      lastUpdate = {
        type: "update",
        message: messageSummary(msg),
        rawMessage: lastMessage,
        modelBefore: inspect(before, 1000),
        modelAfter: latestModel,
        duration: duration
      };
      publish(lastUpdate);
      frameDuration += duration;
      scheduleFrameMarker();
    }

    function recordRender(duration) {
      frameRenderDuration += duration;
      scheduleFrameMarker();
    }

    function scheduleFrameMarker() {
      if (frameScheduled) return;
      frameScheduled = true;

      var schedule = typeof requestAnimationFrame === "function"
        ? requestAnimationFrame
        : function (callback) { return setTimeout(callback, 16); };

      schedule(function () {
        var duration = frameDuration;
        var renderDuration = frameRenderDuration;
        frameDuration = 0;
        frameRenderDuration = 0;
        frameScheduled = false;
        lastFrame = { type: "frame", duration: duration, updateDuration: duration, renderDuration: renderDuration };
        publish(lastFrame);
      });
    }

    function publishPerformance(snapshot) {
      latestLazy = snapshot;
      publish(
        { type: "lazy", stats: snapshot },
        false,
        { type: "performance", appId: id, lazy: snapshot }
      );
    }
  }

  function moduleNameFromMetadata(metadata) {
    return metadata && metadata.module ? metadata.module : undefined;
  }

  function nodeLabel(args) {
    var node = args && args["node"];
    if (!node) return undefined;
    if (node.id) return "#" + node.id;
    if (node.tagName) return node.tagName.toLowerCase();
    return undefined;
  }

  function createLazyProfiler(publishSnapshot) {
    var fnIds = typeof WeakMap === "function" ? new WeakMap() : null;
    var fnPairs = [];
    var statsById = {};
    var nextId = 1;
    var scheduled = false;

    function getFunctionId(fn) {
      if (fnIds) {
        var existing = fnIds.get(fn);
        if (existing) return existing;
        fnIds.set(fn, nextId);
        return nextId++;
      }

      for (var i = 0; i < fnPairs.length; i++) {
        if (fnPairs[i].fn === fn) return fnPairs[i].id;
      }

      fnPairs.push({ fn: fn, id: nextId });
      return nextId++;
    }

    function getStat(refs) {
      var fn = refs && refs[0];
      var id = typeof fn === "function" ? getFunctionId(fn) : 0;
      var stat = statsById[id];

      if (!stat) {
        stat = statsById[id] = {
          id: id,
          name: functionLabel(fn, id),
          arity: Math.max(0, refs ? refs.length - 1 : 0),
          calls: 0,
          renders: 0,
          totalRender: 0,
          maxRender: 0
        };
      }

      return stat;
    }

    function recordOpportunity(refs) {
      var stat = getStat(refs);
      stat.calls++;
      schedule();
      return stat.id;
    }

    function recordRender(id, duration) {
      var stat = statsById[id];
      if (!stat) return;

      stat.renders++;
      stat.totalRender += duration;
      if (duration > stat.maxRender) stat.maxRender = duration;
      schedule();
    }

    function schedule() {
      if (scheduled) return;
      scheduled = true;

      var scheduleFrame = typeof requestAnimationFrame === "function"
        ? requestAnimationFrame
        : function (callback) { return setTimeout(callback, 16); };

      scheduleFrame(function () {
        scheduled = false;
        publishSnapshot(snapshot());
      });
    }

    function snapshot() {
      var items = [];

      for (var id in statsById) {
        if (!Object.prototype.hasOwnProperty.call(statsById, id)) continue;
        var stat = statsById[id];
        items.push({
          id: stat.id,
          name: stat.name,
          arity: stat.arity,
          calls: stat.calls,
          renders: stat.renders,
          hits: Math.max(0, stat.calls - stat.renders),
          avoidedRenders: Math.max(0, stat.calls - stat.renders),
          avgRender: stat.renders ? stat.totalRender / stat.renders : 0,
          maxRender: stat.maxRender
        });
      }

      return items;
    }

    return {
      recordOpportunity: recordOpportunity,
      recordRender: recordRender
    };
  }

  function functionLabel(fn, id) {
    if (typeof fn !== "function") return "<unknown lazy>";

    var name = fn.displayName || fn.name || "";
    if ((!name || name === "wrapper") && fn.f && typeof fn.f === "function") {
      name = fn.f.displayName || fn.f.name || name;
    }

    if (!name) return "<anonymous lazy #" + id + ">";

    return elmFunctionName(name) || name;
  }

  function elmFunctionName(name) {
    if (name.charAt(0) !== "$") return null;

    var parts = name.split("$").filter(function (part) { return part !== ""; });
    if (parts.length < 4) return null;

    // Generated Elm globals look like $author$project$Module$Name$function.
    // Dropping author/project leaves the useful module-qualified name.
    return parts.slice(2).join(".");
  }

  function installLazyProfiler(getCurrentApp) {
    if (typeof _VirtualDom_thunk !== "function" || _VirtualDom_thunk.elmDevProfiled) return;

    var originalThunk = _VirtualDom_thunk;

    _VirtualDom_thunk = function (refs, thunk) {
      var app = getCurrentApp && getCurrentApp();
      var id = app && app.recordLazyOpportunity ? app.recordLazyOpportunity(refs) : null;
      var rendered = false;

      return originalThunk(refs, function () {
        var start = now();
        try {
          return thunk();
        } finally {
          if (!rendered && app && app.recordLazyRender && id !== null) {
            rendered = true;
            app.recordLazyRender(id, now() - start);
          }
        }
      });
    };

    _VirtualDom_thunk.elmDevProfiled = true;
  }

  function startDebuggerApp(events) {
    if (typeof document === "undefined") return null;
    if (!scope.Elm || !scope.Elm.ElmDev || !scope.Elm.ElmDev.Debugger) return null;

    var host = document.getElementById("elm-dev-debugger-root");
    if (!host) {
      host = document.createElement("div");
      host.id = "elm-dev-debugger-root";
      document.body.appendChild(host);
    }

    host.setAttribute("data-debugger", "elm-dev");
    setDebuggerOpen(host, false);

    var mount = document.getElementById("elm-dev-debugger-mount");
    if (!mount) {
      mount = document.createElement("div");
      mount.id = "elm-dev-debugger-mount";
      mount.style.cssText = "width:100%;height:100%";
      host.appendChild(mount);
    }

    var app = scope.Elm.ElmDev.Debugger.init({ node: mount, flags: null });
    var port = app.ports && app.ports.fromRuntime;
    if (!port || typeof port.send !== "function") return null;

    var commandPort = app.ports && app.ports.toRuntime;
    if (commandPort && typeof commandPort.subscribe === "function") {
      commandPort.subscribe(function (command) {
        if (command && command.type === "setOpen") {
          setDebuggerOpen(host, !!command.open);
        } else if (command && command.type === "copyText") {
          copyText(command.text || "");
        }
      });
    }

    return {
      send: function (event) {
        port.send(event);
      }
    };
  }

  function copyText(text) {
    if (typeof navigator !== "undefined" && navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text);
      return;
    }

    if (typeof document === "undefined") return;

    var input = document.createElement("textarea");
    input.value = text;
    input.setAttribute("readonly", "readonly");
    input.style.cssText = "position:fixed;left:-9999px;top:-9999px";
    document.body.appendChild(input);
    input.select();
    try { document.execCommand("copy"); } catch (_) {}
    document.body.removeChild(input);
  }

  function setDebuggerOpen(root, open) {
    if (open) {
      root.style.cssText = "position:fixed;left:24px;right:24px;top:24px;bottom:24px;width:auto;height:auto;z-index:2147483647;box-shadow:0 18px 80px rgba(0,0,0,.45);pointer-events:auto";
    } else {
      root.style.cssText = "position:fixed;left:16px;bottom:16px;width:168px;height:42px;z-index:2147483647;pointer-events:auto";
    }
  }

  function installFallbackPanel(events) {
    if (typeof document === "undefined") return null;

    var root = document.createElement("div");
    root.style.cssText = "position:fixed;right:0;bottom:0;width:420px;height:45vh;background:#202124;color:#f1f3f4;font:12px monospace;z-index:2147483647;border:1px solid #3c4043;display:flex;box-shadow:0 0 24px rgba(0,0,0,.35)";

    var timeline = document.createElement("div");
    timeline.style.cssText = "width:160px;overflow:auto;border-right:1px solid #3c4043";

    var details = document.createElement("pre");
    details.style.cssText = "flex:1;margin:0;padding:10px;overflow:auto;white-space:pre-wrap";

    root.appendChild(timeline);
    root.appendChild(details);

    function renderEvent(event) {
      if (!event) {
        details.textContent = "Waiting for Elm debugger events...";
        return;
      }

      if (event.type === "init") {
        details.textContent = "INIT\n\nMODEL\n" + debugValueToText(event.model, 0);
        return;
      }

      if (event.type === "update") {
        details.textContent = "MESSAGE\n" + event.message + "\n\nDURATION\n" + event.duration.toFixed(2) + "ms\n\nMODEL BEFORE\n" + debugValueToText(event.modelBefore, 0) + "\n\nMODEL AFTER\n" + debugValueToText(event.modelAfter, 0);
        return;
      }

      details.textContent = JSON.stringify(event, null, 2);
    }

    function render() {
      if (!root.parentNode) document.body.appendChild(root);
      timeline.textContent = "";

      for (var i = 0; i < events.length; i++) {
        (function (event) {
          var row = document.createElement("button");
          row.style.cssText = "display:block;width:100%;padding:7px 8px;border:0;background:transparent;color:inherit;text-align:left;font:inherit;cursor:pointer;overflow:hidden;text-overflow:ellipsis;white-space:nowrap";
          row.textContent = event.id + "  " + (event.type === "update" ? event.message : event.type);
          row.onclick = function () { renderEvent(event); };
          timeline.appendChild(row);
        })(events[i]);
      }

      renderEvent(events[events.length - 1]);
    }

    return { render: render };
  }

  function debugValueToText(value, indent) {
    var pad = new Array(indent + 1).join("  ");

    if (!value) return "<unknown>";

    switch (value.kind) {
      case "bool": return value.value ? "True" : "False";
      case "number": return value.value;
      case "string": return JSON.stringify(value.value);
      case "char": return "'" + value.value + "'";
      case "opaque": return value.label;
      case "list":
        return "[\n" + value.items.map(function (item) { return pad + "  " + debugValueToText(item, indent + 1); }).join("\n") + "\n" + pad + "]";
      case "array":
        return "Array [\n" + value.items.map(function (item) { return pad + "  " + debugValueToText(item, indent + 1); }).join("\n") + "\n" + pad + "]";
      case "set":
        return "Set [\n" + value.items.map(function (item) { return pad + "  " + debugValueToText(item, indent + 1); }).join("\n") + "\n" + pad + "]";
      case "dict":
        return "Dict [\n" + value.entries.map(function (entry) { return pad + "  " + debugValueToText(entry.key, indent + 1) + " => " + debugValueToText(entry.value, indent + 1); }).join("\n") + "\n" + pad + "]";
      case "constructor":
        return (value.name || "Tuple") + (value.args.length ? " " + value.args.map(function (arg) { return debugValueToText(arg, indent); }).join(" ") : "");
      case "record":
        return "{\n" + value.fields.map(function (field) { return pad + "  " + field.name + " = " + debugValueToText(field.value, indent + 1); }).join("\n") + "\n" + pad + "}";
      default:
        return "<unknown>";
    }
  }

  scope.__ELM_DEV_DEBUGGER__ = createRuntime();
})();


var _Debugger_element = F4(function (impl, flagDecoder, debugMetadata, args) {
  var runtime = window.__ELM_DEV_DEBUGGER__;
  var app = runtime && runtime.createApp
    ? runtime.createApp("Browser.element", debugMetadata, args)
    : null;

  return _Platform_initialize(
    flagDecoder,
    args,
    elmDevDebuggerField(impl, "init"),
    elmDevDebuggerUpdate(elmDevDebuggerField(impl, "update"), app),
    elmDevDebuggerField(impl, "subscriptions"),
    function (sendToApp, initialModel) {
      runtime && runtime.installLazyProfiler && runtime.installLazyProfiler();
      var view = elmDevDebuggerField(impl, "view");
      var domNode = args && args["node"] ? args["node"] : _Debug_crash(0);
      var currNode = _VirtualDom_virtualize(domNode);

      app && app.recordInit(initialModel, debugMetadata);

      return elmDevMakeAnimator(initialModel, function (model) {
        var start = elmDevNow();
        try {
          if (app && app.run) {
            app.run(function () {
              var nextNode = view(model);
              var patches = _VirtualDom_diff(currNode, nextNode);
              domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
              currNode = nextNode;
            });
          } else {
            var nextNode = view(model);
            var patches = _VirtualDom_diff(currNode, nextNode);
            domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
            currNode = nextNode;
          }
        } finally {
          app && app.recordRender(elmDevNow() - start);
        }
      });
    }
  );
});


var _Debugger_document = F4(function (impl, flagDecoder, debugMetadata, args) {
  var runtime = window.__ELM_DEV_DEBUGGER__;
  var app = runtime && runtime.createApp
    ? runtime.createApp("Browser.document", debugMetadata, args)
    : null;

  return _Platform_initialize(
    flagDecoder,
    args,
    elmDevDebuggerField(impl, "init"),
    elmDevDebuggerUpdate(elmDevDebuggerField(impl, "update"), app),
    elmDevDebuggerField(impl, "subscriptions"),
    function (sendToApp, initialModel) {
      runtime && runtime.installLazyProfiler && runtime.installLazyProfiler();
      var setup = elmDevDebuggerField(impl, "setup");
      var divertHrefToApp = setup && setup(sendToApp);
      var view = elmDevDebuggerField(impl, "view");
      var title = _VirtualDom_doc.title;
      var bodyNode = _VirtualDom_doc.body;
      var currNode = _VirtualDom_virtualize(bodyNode);

      app && app.recordInit(initialModel, debugMetadata);

      return elmDevMakeAnimator(initialModel, function (model) {
        var start = elmDevNow();
        try {
          if (app && app.run) {
            app.run(function () {
              _VirtualDom_divertHrefToApp = divertHrefToApp;
              var doc = view(model);
              var nextNode = _VirtualDom_node("body")(_List_Nil)(doc.__$body || doc.e || doc.body);
              var patches = _VirtualDom_diff(currNode, nextNode);
              bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
              currNode = nextNode;

              var nextTitle = doc.__$title || doc.d || doc.title;
              if (title !== nextTitle) {
                _VirtualDom_doc.title = title = nextTitle;
              }
            });
          } else {
            _VirtualDom_divertHrefToApp = divertHrefToApp;
            var doc = view(model);
            var nextNode = _VirtualDom_node("body")(_List_Nil)(doc.__$body || doc.e || doc.body);
            var patches = _VirtualDom_diff(currNode, nextNode);
            bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
            currNode = nextNode;

            var nextTitle = doc.__$title || doc.d || doc.title;
            if (title !== nextTitle) {
              _VirtualDom_doc.title = title = nextTitle;
            }
          }
        } finally {
          _VirtualDom_divertHrefToApp = 0;
          app && app.recordRender(elmDevNow() - start);
        }
      });
    }
  );
});


function elmDevMakeAnimator(initialModel, draw) {
  if (_Browser_makeAnimator.length === 1) {
    var stepper = _Browser_makeAnimator(draw);
    stepper(initialModel);
    return stepper;
  }

  return _Browser_makeAnimator(initialModel, draw);
}


function elmDevNow() {
  return typeof performance !== "undefined" && performance.now
    ? performance.now()
    : Date.now();
}


function elmDevDebuggerUpdate(update, app) {
  return F2(function (msg, model) {
    var start = elmDevNow();
    var pair;

    if (app && app.run) {
      pair = app.run(function () { return A2(update, msg, model); });
    } else {
      pair = A2(update, msg, model);
    }

    app && app.recordUpdate(msg, model, pair.a, elmDevNow() - start);

    return pair;
  });
}


function elmDevDebuggerField(record, field) {
  return record["__$" + field] || record[field];
}
