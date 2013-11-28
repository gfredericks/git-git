# git-git

git-git is a utility for managing your git repositories across
multiple machines.

## Problem statement

How long would it take you to recover if I `rm -rf`ed your project
directory?

## Warning

This code barely works at the moment. I intend to put it to heavy use
myself and thereby iron out the rough patches that _I_ encounter, but
you may use it slightly differently and violate some of my assumptions.

I'd be happy to make minor improvements to fit others' use cases.

## Usage

Haven't thought of the best way to avoid forcing leiningen on you yet,
so let's assume you have that.

You can `lein uberjar` the project, then perhaps create a `git-git`
executable script that calls the executable jar. At _that_ point
you can:

```
git-git update-from-local my-project-directory
```

This will write to the file `my-project-directory/.git-git.clj`, which
you might choose to setup as a symlink to some sort of meta-git-repo.

To clone any repos you're missing based on the contents of a file
obtained as above, you can:

```
git-git sync-to-local my-project-directory
```

This API is terrible and hopefully I will think of a better one.

## TODO

- Better error handling
- Report what repo things are happening in
- Limit actions to a single repo

## License

Copyright © 2013 Gary Fredericks

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
