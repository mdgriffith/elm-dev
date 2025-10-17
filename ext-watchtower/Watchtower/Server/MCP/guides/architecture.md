# Elm Dev App Architecture

## How it works

Elm Dev has a built-in app architecture that is *optional*, but very useful for most apps.

Starting a project form scratch via `elm-dev init` will set you up with a standard project.

Key details:
- The presence of an `elm.dev.json` file tells `elm-dev` if you're using this architecture or not.
- `elm.dev.json` defines what `elm-dev` will generate for you and includes important information like *routing*.
- `elm-dev` will generate files for you behind the scenes when you `make` your project.
- Some files are *customizable*, which means you can take full ownership of that file and `elm-dev` will stop generating it's version of the file.

There are also a number of commands in `elm-dev` that you can use to manage your project.


## Concepts

Before getting started, you should be aware of the standard Elm architecture. If you're generally aware of:
- Each Elm app has a Model, an update function, and a view function
- `Cmd`, `Sub`, and flags are how you talk to the outer world.

Then you should be good to read the rest!  Otherwise checkout `https://guide.elm-lang.org/` to get caught up.

Let's go through the highlevel concepts in the Elm Dev architecture.  


### Main

Located at `src/Main.elm`, this is the main entrypoint of your app.  

You'll likely not need to modify it that often, most of the time you'll be working with Pages and Stores.


### Pages

Located in `src/Page/*`, a page is a classic Elm Model + Msg + update + view.

> `elm-dev add page url/to/my/new/page` will add a new page for you.

The actual url to get to a given page is in `elm.dev.json`.  Here's an example from `elm.dev.json`.

```
{
    "pages": {
        "Home": "/",
        "Post": "/posts/*",
        "about": "/about",
    }
}
```

Information from the URL template is passed to the `Page` on `init`. 

> **URL formatting**
> 
> The pattern language supports literals, variables, an optional path tail, and optional query fields.
> Here are some URL exmaples and what would be passed to `Page.init` as `params`:
> 
> - **"/"** → `{}`
> - **"/about"** → `{}`
> - **"/users/:id"** with `/users/42` → `{ id = "42" }`
> - **"/files/*"** with `/files/a/b/c` → `{ path_ = [ "a", "b", "c" ] }`  (captures remaining segments)
> - **"/blog/:year/:slug"** with `/blog/2025/hello-world` → `{ year = "2025", slug = "hello-world" }`
> - **"/search?{q}"**
>   - `/search?q=elm` → `{ q = Just "elm" }`
>   - `/search` → `{ q = Nothing }`
> - **"/u/:id/*?{tab}"** with `/u/7/posts?tab=favorites` → `{ id = "7", path_ = [ "posts" ], tab = Just "favorites" }`
> - **Multiple query values**: only the first value is used; e.g. `/search?q=one&q=two` → `{ q = Just "one" }`
> - **Extra query fields** not listed in `{ ... }` are ignored; e.g. pattern `"/search?{q}"` with `/search?q=elm&other=x` → `{ q = Just "elm" }`
> 
> Notes:
> - Use `:name` for path variables (each becomes a `String` field on the page params record).
> - Use `*` to include the remaining path segments as `path_ : List String`.
> - Use `?{fieldA,fieldB}` to declare optional query fields; each becomes `Maybe String` (`Nothing` when missing).
> - Query field names must be letters only (A–Z, a–z).
> - `?{**}` (query catch‑all) is reserved; it parses but is not passed to pages yet.

A `Page` is slightly different than a classic  Elm Model + Msg + update + view. Here are some differences.

- `Effect msg` is used instead of `Cmd msg` (see Effect section).
- `Listen msg` is used instead of `Sub msg` (see Listeners section).
- `init` receives
    - `App.Page.PageId` - This is a concrete identifier for this page.
    - `params` - The extracted URL params as described above.
    - `stores` - A record of all the `Stores` in the app.  (See Stores section).
    - `maybeCached` - A cached `model` is we already have one (for example, from local storage, or if the url changes, but we're on the same conceptual page).

See the `App.Page` documentation for more details.

### Stores

Located in `src/Stores/*`, each `Store` is a Model+update that are **globally readable**.

You can send messages to your `Stores` via `Broadcast`, which is covered later.

> `elm-dev add store StoreName` will setup a new store for you.

Every store is exactly one global instance that is passed to the `init`, `update`, and `view` functions of both `Main` and all `Pages` you have.

This is very useful as a data layer. A classic example of a useful store is `Store.Auth`, where you can manage your authentication setup and then use that to add headers to requests and prevent authenticated pages from being viewable.

Conceptually I think of `Stores` as mini-databases and `Pages` as stateful-views on *top* of those databases.

So, for example, in the classic todo-list app, you might consider adding a `Store.Todo` which implements the CRUD of managing the todos, and then each page can draw from that single store to view the todo list.


> **Note** - One thing to keep in mind is that stores can't read from each other, they're completely independent. This is an advantage and makes things super easy to think about.

See the `App.Store` documentation for more details.


### Effects

If you're not familair with `Cmd` or how Elm handles talking to the outside world, read this to get started: https://guide.elm-lang.org/effects/

In a standard Elm app, the update can return a `Cmd`.  In the Elm Dev architecture, we describe what we want to do as an `Effect` and only turn it into a `Cmd` at the last minute. This allows us to do powerful things with testing later.

By default you have the following `Effect` modules:
- `Effect.Clipboard`
- `Effect.Debounce`
- `Effect.File`
- `Effect.Focus`
- `Effect.Http`
- `Effect.LocalStorage`
- `Effect.Nav` - Changing the URL.
- `Effect.Page` - preloading/loading pages.
- `Effect.Random`
- `Effect.Scroll`

Check out their docs to see what they can do.

> **Talking to the JS world** - Running `elm-dev add effect EffectName` will add a new effect module that will allow you to send JSON to the outside world through a port.
>
> After generating that file, read the docs in it to get oriented on what's happening!

## Listeners

While `Effect` is *outgoing*, a `Listener` is *incoming* for when you want to listen for something form the outside.

In the same way `Effect` wraps command, `Listen` wraps `Sub`.

Check out the docs for `Listen`, but some examples of what is available by default.
- `Listen.onKeyPress` - Globally listen for keypresses
- `Listen.onResize` - Listen for window resize events.
- `Listen.LocalStorage.onUpdated` for listening to when localstorage changes for a key.

> `elm-dev add listener ListenerName` will add a new listener for you that allows you to send JSON from JS to your Elm app and listen on the Elm side using an incoming port.  Generate it and check out what is generated!


## Broadcast

The `Broadcast` module can be used to send global messages that any part of your app (`Main`, `Pages`, or `Stores`) can listen for.

First, you'll need ot run `elm-dev customize Broadcast`, which will move the `Broadcast` file into your project so you can modify it.

Then:
1. Define a new variant in the existing `Boardcast.Msg` type. For example `LoggedOut` could be a common broadcasted Msg.
2. Send as an `Effect` as `Effect.broadcast Broadcast.LoggedOut`.
You can then listen by adding the following to your listeners:

```elm
-- Listener
Listen.onBoardcast
    (\broadcastedMsg -> 
        case broadcastedMsg of
            Broadcast.LoggedOut ->
                Just StoreMsgHandlingLoggedOut
            _ ->
                -- Ignore other broadcasted messages
                Nothing
    )

```

This is the main way you can communicate to your `Stores` or `Main` from anywhere in your app.

