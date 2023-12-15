# Elm Dev Roadmap

Elm Dev is here to empower Elm tooling to allow Elm projects to be developed, and managed in a swift and delightful manner.

Elm is the armor and the blasters, Elm Dev is the jetpack.

I'm also very interested in emphasizing things that appeal to people outside of the Elm community. This means covering areas that aren't just standard stuff (though some amount of standard tooling is desired as well).

## Road map

There are generally going to be phases in Elm Dev's roadmap.

1. Elm Dev experimental CLI release.
   Initially I'm going to release a CLI tool that is focused on reporting information about a given Elm project in JSON.
   Performance is not really a consideration as there are a class of tools that would only ask questions from the CLI every now and then.

2. Elm Dev server process.
   Utilize the work that's already been done to keep compiler artifacts in memory. The server can watch multiple projects and compiler your project for you.

   This will unlock live-development tooling.

3. The server can function as a language server.

So, the general roadmap is looking something like this:

1. An Elm Dev CLI tool is released that is primarily focused on exposing info via JSON.

The main motivation here is to unblock some downstream projects that I have. The things I need are not very performance critical and aren't very sophisticated, but I need elm-dev to get them for me.

Specifically I've been working on a codegen tool that I'm currently calling `elm-press`. I'll have more to share later, but it's "plugin" based, so you can choose what you want generated based on your requirements. So far the plugins I have are:

    - An opinionated app archutecture with some superpowers around state handling.
    - Elm UI theme generator
    - Interactive code generator thing

Once that's out and I'm able to finish the above (while coordinating Elm UI 2.0 with the above generator), the focus will move to enriching the server experience.

2. Elm Dev as a development server. This unlocks a bunch of stuff performance-wise as well as some really cool features like live dev tools and code generation.

3. Elm Dev server as a language server. Once everything else it taken care of, I'd like elm-dev to power a language server.

I also want to emphasize that step 1. is about exploration
