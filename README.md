# Typed-state-machine

This project is an experiment to create type-safe state machines in Haskell.

There are already projects and approaches to do this, for example:

- [motor](https://hackage.haskell.org/package/motor)
- [Modeling state machines with dependent types in Haskell](https://www.poberezkin.com/posts/2020-06-29-modeling-state-machine-dependent-types-haskell-1.html)

The aims of this projects are:

- simple UX: a user of this project should not have a good understanding of dependent types, singletons or any other complex Haskell feature or library. He should be able to describe his state machine in a very simple and straightforward way;

- composable: state machines are extremely composable. They could be sequenced and run in parallel or alternatively. This project strives to make this composability straightforward;

- information at the type level: we would like to store relevant information about our state machine at the type level, so that we could statically analyze it and render it in a user-friendly way.
