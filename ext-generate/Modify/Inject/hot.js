; (function () {
    try {
        var wsUrl = "{{ ELM_DEV_SERVER_WS_URL }}";
        var socket;
        function connect() {
            try {
                socket = new WebSocket(wsUrl);
                socket.onopen = function () { try { console.info('[elm-dev][hot] connected'); } catch (_) { } };
                socket.onmessage = function (ev) {
                    var msg = null;
                    try { msg = JSON.parse(ev.data); } catch (_) { return; }
                    try { window.dispatchEvent(new CustomEvent('elm-dev:hot-message', { detail: msg })); } catch (_) { }
                    // If runtime provides hot apply hook, use it; else reload on success messages.
                    try {
                        if (window.Elm && window.Elm.Hot && typeof window.Elm.Hot.apply === 'function') {
                            window.Elm.Hot.apply(msg);
                        } else {
                            if (msg && (msg.type === 'hot-apply' || msg.type === 'compile-success')) { location.reload(); }
                        }
                    } catch (_e) { /* last resort */ try { location.reload(); } catch (_) { } }
                };
                socket.onclose = function () { setTimeout(connect, 1000); };
                socket.onerror = function () { try { socket.close(); } catch (_) { } };
            } catch (_err) { setTimeout(connect, 2000); }
        }
        connect();
    } catch (_outer) { /* ignore */ }
})();


