# ext-debug

`ext-debug` contains the Elm Dev Debugger runtime assets.

The initial goal is to recreate the useful parts of Elm's built-in debugger in-browser:

- capture messages and model snapshots
- inspect message and model values
- scrub through the message timeline

The debugger UI is intended to be a separately compiled Elm app. The injected JavaScript runtime keeps raw Elm values in the user app process and sends JSON summaries to the debugger app over ports.

## Agent API

The debugger should expose a small browser-side API for tools and AI agents that need to observe Elm applications without scraping the DOM or console output.

Multiple Elm applications can run on the same page, so `window.__ELM_DEV_DEBUGGER__` should act as a registry of app instances.

### Registry

```ts
type ElmDevDebuggerRegistry = {
  version: string
  listApps(): AppSummary[]
  getApp(id: string): DebuggerApp | undefined
}
```

```ts
type AppSummary = {
  id: string
  kind: "Browser.element" | "Browser.document" | "Browser.application" | "worker" | "unknown"
  module?: string
  node?: string
  startedAt: number
  messageCount: number
}
```

Example:

```js
const debuggerRegistry = window.__ELM_DEV_DEBUGGER__
const apps = debuggerRegistry.listApps()
const app = debuggerRegistry.getApp(apps[0].id)
```

### App API

Each app instance exposes two methods for the initial API:

```ts
type DebuggerApp = {
  id: string
  latest(): AppSnapshot
  listen(callback: (event: DebuggerEvent) => void): () => void
}
```

`latest()` returns the most recent known state for the app.

`listen(callback)` subscribes to future events for that specific app. It returns an unsubscribe function.

```js
const stop = app.listen((event) => {
  console.log(event)
})

stop()
```

The first version intentionally has one event stream. Agents should filter that stream in user code.

### Snapshot

```ts
type AppSnapshot = {
  appId: string
  model?: DebugValue
  lastMessage?: DebugValue
  lastUpdate?: UpdateEvent
  lastFrame?: FrameEvent
  lazy: LazyStat[]
}
```

### Events

```ts
type DebuggerEvent =
  | InitEvent
  | UpdateEvent
  | FrameEvent
  | LazyEvent
  | DebugLogEvent
```

```ts
type InitEvent = {
  appId: string
  type: "init"
  id: number
  model: DebugValue
}
```

```ts
type UpdateEvent = {
  appId: string
  type: "update"
  id: number
  message: string
  rawMessage: DebugValue
  modelBefore: DebugValue
  modelAfter: DebugValue
  duration: number
}
```

`duration` is the time spent in the Elm `update` function, in milliseconds.

```ts
type FrameEvent = {
  appId: string
  type: "frame"
  id: number
  duration: number
  updateDuration: number
  renderDuration: number
}
```

`duration` and `updateDuration` are the total update time accumulated in the animation frame. `duration` is kept for compatibility with the visual debugger. `renderDuration` is the total time spent rendering, diffing, and applying patches in the same frame.

```ts
type LazyEvent = {
  appId: string
  type: "lazy"
  stats: LazyStat[]
}
```

```ts
type LazyStat = {
  id: number
  name: string
  arity: number
  calls: number
  renders: number
  avoidedRenders: number
  avgRender: number
  maxRender: number
}
```

`calls` is the number of lazy opportunities observed. `renders` is the number of times the lazy thunk actually ran. `avoidedRenders` is the number of times the previous lazy subtree was reused.

```ts
type DebugLogEvent = {
  appId: string
  type: "debugLog"
  id: number
  label: string
  value: DebugValue
  source?: SourceLocation
}
```

```ts
type SourceLocation = {
  module: string
  file?: string
  line: number
  column: number
}
```

`Debug.log` should keep its normal console behavior. The debugger can additionally emit a structured `debugLog` event with the inspected value and source location when compiler metadata is available.

### Debug values

Elm values are represented with the same JSON-friendly structure used by the visual debugger.

```ts
type DebugValue =
  | { kind: "bool"; value: boolean }
  | { kind: "number"; value: string }
  | { kind: "string"; value: string }
  | { kind: "char"; value: string }
  | { kind: "record"; fields: Field[] }
  | { kind: "constructor"; name: string | null; args: DebugValue[] }
  | { kind: "list"; items: DebugValue[] }
  | { kind: "array"; items: DebugValue[] }
  | { kind: "set"; items: DebugValue[] }
  | { kind: "dict"; entries: DictEntry[] }
  | { kind: "opaque"; label: string }
```

```ts
type Field = {
  name: string
  value: DebugValue
}
```

```ts
type DictEntry = {
  key: DebugValue
  value: DebugValue
}
```

### Filtering examples

Listen to only updates:

```js
const stop = app.listen((event) => {
  if (event.type !== "update") return

  console.log(event.message, event.modelAfter)
})
```

Listen for one specific message:

```js
const stop = app.listen((event) => {
  if (event.type !== "update") return
  if (event.message !== "SaveSucceeded") return

  console.log("saved", event.modelAfter)
  stop()
})
```

Watch for slow frames:

```js
app.listen((event) => {
  if (event.type !== "frame") return

  const total = event.updateDuration + event.renderDuration
  if (total < 16) return

  console.warn("slow Elm frame", {
    update: event.updateDuration,
    render: event.renderDuration,
    total
  })
})
```

Watch for lazy calls that almost always render:

```js
app.listen((event) => {
  if (event.type !== "lazy") return

  const suspicious = event.stats.filter((stat) => {
    if (stat.calls < 20) return false
    return stat.renders / stat.calls > 0.95
  })

  console.table(suspicious)
})
```

Listen to a specific `Debug.log` label:

```js
app.listen((event) => {
  if (event.type !== "debugLog") return
  if (event.label !== "form state") return

  console.log(event.source, event.value)
})
```

Listen to one `Debug.log` callsite:

```js
app.listen((event) => {
  if (event.type !== "debugLog") return
  if (!event.source) return
  if (event.source.module !== "Pages.Login") return
  if (event.source.line !== 88) return

  console.log(event.value)
})
```
