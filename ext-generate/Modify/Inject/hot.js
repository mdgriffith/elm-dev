

(function () {
    var wsUrl = "{{ ELM_DEV_SERVER_WS_URL }}";
    var socket;
    function applyCode(code) {
        console.info('[elm-dev][hot] applying compiled js (length:', code && code.length, ')');
        var f = new Function(code);
        var newScope = {};
        f.call(newScope);
        if (window.Elm && window.Elm.hot && typeof window.Elm.hot.reload === 'function') {
            console.info('[elm-dev][hot] calling Elm.hot.reload');
            window.Elm.hot.reload(newScope);
        } else {
            console.info('[elm-dev][hot] Elm.hot.reload missing, full reload');
            location.reload();
        }
    }

    function connect() {
        socket = new WebSocket(wsUrl);
        socket.onopen = function () { console.info('[elm-dev][hot] connected'); };
        socket.onmessage = function (ev) {
            try {
                var raw = ev && ev.data;
                console.info('[elm-dev][hot] raw message:', raw);
                var msg = JSON.parse(raw);
                console.info('[elm-dev][hot] parsed message:', msg);

                // window.dispatchEvent(new CustomEvent('elm-dev:hot-message', { detail: msg }));

                // New DevWS format: { msg: "Compiled" | "CompilationError", details: ... }
                if (msg && typeof msg.msg === 'string') {
                    if (msg.msg === 'Compiled') {
                        if (msg.details && typeof msg.details === 'string' && msg.details.length > 0) {
                            console.info('[elm-dev][hot] Compiled message with inline code; applying');
                            applyCode(msg.details);
                        }
                    } else if (msg.msg === 'CompilationError') {
                        console.info('[elm-dev][hot] compilation error received:', msg.details);
                        // Do not reload on error; keep current app running
                    }
                    return;
                }

                // Legacy format support
                var shouldApply = msg && (msg.type === 'hot-apply' || msg.type === 'compile-success');
                if (!shouldApply) { return; }
                fetchAndApply();
            } catch (err) {
                console.info('[elm-dev][hot] onmessage error', err);
            }
        };
        socket.onclose = function () {
            console.info('[elm-dev][hot] disconnected, reconnecting');
            setTimeout(connect, 1000);
        };
        socket.onerror = function (err) {
            console.info('[elm-dev][hot] error, reconnecting', err);
            socket.close();
        };
    }
    connect();
})();