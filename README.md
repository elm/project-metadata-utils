# Work with Elm documentation

This package provides helpers for working with the `docs.json` files produced by the Elm compiler.


## Advice for Editor Plugin Authors

Elm creates a per-user cache of package information. **This cache is read-only. Do not modify or add files to it.** Inside the cache, you have docs for every package that has been downloaded and built. This means you can just read them locally and do whatever sort of visualization or analysis you want.

If you are decoding a `docs.json` file, you probably want to say something like `Decode.list Docs.decode` to decode an *array* of module docs.

> **Note:** The per-user cache of package information lives at the directory specified in the `ELM_HOME` environment variable. If this environment variable is not set, it defaults to  `~/.elm/` on UNIX systems and `C:/Users/<user>/AppData/Roaming/elm` on Windows. Again, **do not modify or add files in `ELM_HOME` for your work** because:
>
>  1. It will be very confusing and frustrating if this cache is corrupted.
>  2. This cache is cleared very infrequently, so it should be as small as possible.
>
> If you need to cache information for your plugin, it is better to find a *separate* solution. Perhaps use the local `elm-stuff/` directory so that it is easy for users to (1) delete if there is a problem and (2) reclaim the storage.