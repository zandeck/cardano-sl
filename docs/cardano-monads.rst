===============================
 Monadic effects in Cardano SL
===============================

A monad comes with two basic operations, ``return`` and ``>>=``, but this
interface is barely useful on its own. Monadic effects extend the ``Monad``
interface with additional operations (such as ``ask``, ``local``, ``get``,
``put``, ``throwError``, ``liftIO``, ``tell``, etc).

Besides general purpose effects (such as reader/writer/state) it is customary to
define our own, domain-specific effects, needed by the application (slotting,
gstate, database access, etc).

Sure enough, we don't need those effects in isolation: we want to use multiple
effects simultaneously. And that's where it gets difficult: as it turns out,
combining effects is a huge design space, and there are many conflicting
approaches (concrete transformer stacks, mtl-style classes, extensible effects,
and what not).

What further complicates matters is that the choice of the approach to monadic
effects will affect the entire codebase in drastic ways, since the effects are
so ubiqutous. This means that it's impossible to quickly try each possible
approach, one must rewrite half the application to do so.

In Cardano SL we've used multiple approaches to effects, none of them
satisfactory. The goal of this document is to provide a reference point for
discussion about further direction of travel.

The Stone Age
-------------

The first approach to effects that was used in Cardano SL was simple: for each
effect, create a class with operations and a monad transformer that implements
these operations::

    class MonadFoo m where
        ...

    data FooT m a = FooT { runFooT :: ... }

    instance MonadFoo (FooT m)
    instance MonadFoo m => MonadFoo (ReaderT r m)
    instance MonadFoo m => MonadFoo (StateT s m)
    instance MonadFoo m => MonadFoo (ExceptT e m)
    ...

    class MonadBar m where
        ...

    data BarT m a = BarT { runBarT :: ... }

    instance MonadBar (BarT m)
    instance MonadBar m => MonadBar (ReaderT r m)
    instance MonadBar m => MonadBar (StateT s m)
    instance MonadBar m => MonadBar (ExceptT e m)
    ...

This approach mirrors the one of ``mtl``. However, ``mtl`` is concerned with
general purpose effects, of which there are only a few (under 10, even if we
consider more exotic ones like reverse state or logic-t), whereas
domain-specific effects are numerous. Besides, general purpose effects don't pop
up left and right, whereas domain-specific ones might be created/deleted
depending on how the application evolves. All in all, this made mirroring
``mtl`` quite a bad idea.

Pros:

* Simple and familiar. It's easy to reason about effects when there's a 1-1
  correspondence between classes and transformers. Virtually everyone is
  familiar with ``mtl`` and can quickly learn to apply this approach.

* Flexibility. Given a monad ``m``, one can easily add an effect to it by
  putting the relevant transformer on top. It's convenient to use effects
  locally, not only application-wide.

Cons:

* Bad run-time performance. Having a transformer per effect means no flattening.
  This boils down to ``ReaderT (a, b, c, d) m`` being more performant than
  ``ReaderT a (ReaderT b (ReaderT c (ReaderT d m)))`` (same goes for ``StateT``,
  ``WriterT``, ``ExceptT``, etc). Since transformers for many domain-specific
  effects are isomorphic to one of these general purpose effects, having a dozen
  of nested transformers isn't such a good idea. (@int-index has benchmarks to back up
  this claim).

* No run-time configurability. The stack of transformers determines at
  compile-time what effect implementations are used. This means that if we
  wanted to select an implementation at run-time (via a CLI flag, for instance),
  we had to use a different stack of transformers. The amount of possible
  combinations grows rather quickly. For instance, if feature X has ``2``
  implementations and feature Y has ``3``, then we need ``2*3=6`` transformer
  stacks. (As discovered later, this can be addressed with closed effect sums,
  but then we lose extensibility, so it's still an issue).

* Cost of introducing new effects. For ``N`` transformers and ``M`` classes,
  there must be ``N * M - k`` instances (where ``k`` is the amount of impossible
  effect combinations: one does not simply use ``WriterT`` with ``ContT``). The
  more effects we have in the application, the more costly it is to add one more
  (at 10 effects we already hit 100 instances, the cost is insurmountable). Not
  only this is boilerplate-heavy, it's also antimodular, as every effect needs
  to know about each other.

Conclusion:

* In retrospect, we could probably solve the ``N*M`` issue by careful use of
  ``{-# OVERLAPPABLE #-}`` instances (although ``Mockable``-related type
  families would still lead to a fairly large amount of boilerplate).
  Nevertheless, the run-time performance of highly nested transformer stacks
  might be unsatisfactory. (Slowdown is linear in the amount of layers).


The Bronze Age
--------------

The core observation is that many domain-specific effects are isomorphic to
general purpose ones. The majority of monad transformers in Cardano SL were
basically clones of ``ReaderT`` or ``StateT``. This is why we decided to use
``ether``: unlike ``mtl``, it allows using multiple transformers of the same
sort (i.e. multiple ``ReaderT``) in the same transformer stack, so we could
simply replace our hand-crafted transformers with those from ``ether`` and put
an end to the ``N*M`` problem. Furthermore, ``ether`` offers *flattening*,
solving the run-time performance issue as well.

Typically, the code would look like this::

    type MonadFoo = Ether.MonadReader Foo
    type FooT = Ether.ReaderT Foo

    type MonadBar = Ether.MonadReader Bar
    type BarT = Ether.ReaderT Bar

Even when we needed a custom class with methods (rather than a synonym for
``Ether.MonadReader``), we could define a single instance for ``IdentityT``, no
boilerplate required.

We used flattening to achieve good run-time performance, but kept many custom
classes with effect operations (to allow for varying implementations). This led
to a peculiar situation where the transformer stack consisted mostly of
``IdentityT``::

    type M =
        TaggedTrans FooEff IdentityT $
        TaggedTrans BarEff IdentityT $
        TaggedTrans BazEff IdentityT $
        ReaderT (FooEnv, BarEnv, BazEnv) IO

Pros:

* Extensibility. Introducing a new effect is really cheap. The code is modular
  and effects don't need to know about each other.

* Flexibility. (Same as above, local use of effects).

* Good run-time performance. Since in the end the entire monad transformer stack
  was just ``ReaderT`` with a bunch of ``IdentityT`` on top (and occasional
  local ``StateT``), we enjoyed good run-time performance.

* Conciseness. No boilerplate.

Cons:

* Bad run-time configurability. (Same as above)

* Bad compile-time performance. Due to the way flattening works in Ether and due
  to a GHC bug, the compile-time performance was devastating. Turning ``-O2``
  could mean hours of compilation and required up to 65 GIGABYTES of RAM
  (ridiculous!). This was because GHC generated an exponential amount of
  coercions (as evidenced by investigating .hi-files). Basically, @int-index no longer
  can recommend Ether to people as there is no good solution to this known.

Conclusion:

* Migration to Ether allowed us to remove an immense amount of boilerplate,
  modularize the code, and get good run-time performance. However, lack of
  run-time configuability was quite inconvenient, and bad compile-time
  performance marked this approach a no-go.

The Modern Era
--------------

After we've realized what led to bad compile-time performance, @int-index came up with an
idea of ``ExecMode``. Basically, we continued to use classes from ``Ether``, but
rather than having numerous ``IdentityT`` layers there was a single ``newtype``
wrapper around ``ReaderT ModeEnv Production`` at the bottom. This solved the
compile-time performance issue completely at the cost of a moderate increase in
boilerplate.

However, FPComplete began to see ``ether`` as an Enemy of The State, and asked
us to purge its remains. Now we were supposed to remove all our custom classes,
replacing them with method records. Those records would go into a ``ReaderT``
and be passed everywhere manually (as opposed to instance search). Instead of
distinct ``Ether.MonadReader`` constraints, now we had to use ``MonadReader
ctx``, passing an annoying ``ctx`` parameter everywhere, and placing constraints
on it. The final transformer stack is just ``ReaderT ModeCtx Production``, and
not even a newtype on top.

Technically, we're still in the process of migration, as we haven't removed all
of our custom classes yet. Just to clarify: in this section we'll discuss the
current transitional state, and it's more painful than what was actually
proposed by FPComplete.

Now our code follows this pattern::

    -- effect definitions

    class MonadBaz
        baz :: ...

    defaultBaz = ...

    class MonadZaz
        zaz :: ...

    defaultZaz = ...


    -- mode definitions

    data QuuxCtx = ...

    type QuuxMode = ReaderT QuuxCtx Production

    instance HasFoo ModeCtx
    instance HasBar ModeCtx

    instance MonadBaz QuuxMode
        baz = defaultBaz

    instance MonadZaz QuuxMode
        zaz = defaultZaz


Pros:

* Good compile-time performance. There's only a single layer of transformers,
  instance search is quick, no coercions involved.

Cons:

* Bad run-time configurability. (Same as above)

* Boilerplate. Various ``HasFoo`` instances with field lenses, ``MonadBaz``
  instances to choose method implementations for the current mode. There's also
  that annoying ``ctx`` parameter.

* Cost of introducing new modes. The approach is inflexible, as introducing a
  new mode has an extremely high cost (due to boilerplate). Assuming we want to
  avoid nested ``ReaderT``, adding one more field to the context requires a new
  mode.

* Lack of inheritance. It's hard to define one mode in terms of another, with
  only minor changes. Either it becomes hard to maintain consistency, or it
  becomes hard to do overrides (as happened to ``AuxxMode``).

* Volatile ``runReaderT``. It's difficult to reason about code when
  ``runReaderT`` might imply something besides supplyng the value of the
  ``ReaderT`` environment (handling ``MonadReader``). The situation arises
  because we define instances for ``ReaderT`` without a newtype (and sometimes
  even with ``{-# OVERLAPPING #-}``).

Conclusion:

* Current solution requires huge swaths of boilerplate code, it's hard to
  reason about the code, and it's inflexible. We must seek other options.

Addendum: reflection
~~~~~~~~~~~~~~~~~~~~

To pass constant configuration to application components, instead of
``ReaderT`` we use ``reflection``. More specifically, we use the dumb and unsafe
``Given``-style reflection, avoiding the type-level complications of proper
``Reifies``-style reflection. It turned out to be a great design choice: we've
cut the potential amount of custom classes greatly, and the configs are
available even in class instances (such as ``Bi``).

Addendum: SendActions
~~~~~~~~~~~~~~~~~~~~~

`SendActions` is another effect which is implemented differently from everything
else in our system. It's an explicitly passed method dictionary that has the
monad `m` as a type parameter. Unlike classes, explicit dictionaries must be passed
manually (not by compiler via instance search), which might be both an advantage
and a disadvantage.

`SendActions` is a perfect example of a *local* effect: in general, we don't
have the ability to send requests, but we get this ability when we start a so
called conversation. Doing a local effect via an explicit dictionary is a
testament to poor support for local effects of our current approach with modes.

One of the problems with `SendActions` is its use of natural transformations. We
have a helper `hoistSendActions`, and right now we horribly misuse it. Since
``m`` in ``SendActions m`` is in an invariant position, we need
`hoistSendActions` to convert ``SendActions n`` to ``SendActions m``, and it
requires two natural transformations: ``n ~> m`` and ``m ~> n``.

First,
providing these transformations is an inconvenience (first one tends to be a
mere lift, but the second one must do certain tricks to unlift). Second, it
introduces a potential for horrible, subtle bugs. As was said, right now we misuse
this helper, in particular because we call it in a runner, `runRealMode`;
therefore, the unlift natural transformation must reconstruct the monadic
context from what it has at transformer stack initialization site, not at
conversation fork site; therefore, any local modifications to the monad
transformer stack are *not* reflected in forked conversations (such as `ReaderT`
local).

To put it into concrete terms, here is a code demonstration::

    x <- ask
    local f $ enqueueMsg msg $ \_ _ -> (:|[]) $ do
      y <- ask

What do you think, is `y` equal to `x` or `f x`? The way we currently use
`hoistSendActions`, it will be equal to `x` (completely oblivious to `f`). This
might be not at all what a programmer would expect.

The moral of this story is that, perhaps, explicit dictionaries are a bad design
because it's very easy to misuse them. A good effect system should take care of
things like this. (But perhaps it's an overegenralization and it's only bad to unlift,
whereas lifting is straightforward).

Future Plans
------------

Informed by previous failures, we are in a position to finally find a good
approach to monadic effects in our code. Ideally, with all of the pros and
none of the cons. So, to start, here's a checklist of properties we want:

* Flexibility. A flexible effect system allows to easily add an effect to a
  monadic stack locally, and to run effects partially and in arbitrary order.
  There also should be a way to have different implementations for the same
  effect.

* Extensibility. Adding a new effect must be cheap and modular. A thousand
  interconnected instances just won't cut it.

* Ease of use. We don't want to ``lift . lift . lift``.

* Compile-time performance. No type families, no instance search tricks. We've
  been bitten by this before. Keep it simple.

* Run-time performance. We want a flattened runtime representation for
  Reader-isomorphic effects.

* Run-time configurability. It's fine to keep track of effects themselves in the
  types, but the choice of an implementation must be delegated to terms. For
  instance, choosing between a real DB (RocksDB) and pure DB must be possible
  with a CLI option.

* Predictability. It must be easy manipulate effects in a predictable manner,
  without fearing that ``runReaderT`` will affect anything but ``MonadReader``.

Proposed Solutions
------------------

The ReaderT Pattern
~~~~~~~~~~~~~~~~~~~

Read the full article: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern

The general sentiment of the article is to have a single `Env` data type
(basically, what we have with our modes), and use `ReaderT Env IO` exclusively.

The general sentiment is true: `StateT`, `WriterT`, and `ExceptT` are almost
never what you actually want. `StateT` kills the story for concurrency and makes
it harder to deal with exceptions, `WriterT` is simply broken performance-wise
(and its CPS-ed version is `StateT` with the same caveats), and `ExceptT` adds
one more way to throw exceptions (so you need one more way to catch them).

For pure code (without `IO` or `MonadIO`), use `StateT`, `WriterT`, and
`ExceptT`. In other cases, avoid them.

This aspect of the `ReaderT` pattern makes a lot of sense. Monad transformers
are used to model effects in pure code, and in `IO` you don't have a choice but
to have the actual effects themselves (there's mutable state, there are
exceptions). Mixing them is unhelpful.

Now, let's consider the global `Env` type. The issue with it is associated
boilerplate of `Has` classes. In case we want to extend the environment to
introduce local effects, there's a high cost in boilerplate. This can be already
observed in our codebase.

Point-by-point rundown:

* Flexibility: LOW. Extension of environment requires declaring a new type,
  writing `Has` instances for it, and potentially monad instances (although the
  approach recommends avoiding monad classes in favor of `Has` classes, not
  everything is under our control and there are classes by external libraries).
  There's no way to partially run effects -- one needs to supply the entire
  `ReaderT` environment up front.

* Extensibility: MODERATE. Defining new effects boils down to declaring a method
  record and a `Has` class for it. Then, for each field in the method record we
  must define a function that extracts this method using the `Has` class and
  runs it. Boilerplate-heavy, yes, but at least effects don't need to know about
  each other.

* Ease of use: MODERATE. Beginners can quickly grasp the basic concepts, but
  there are pitfalls when it comes to lifting method records (see section on
  `SendActions`).

* Compile-time performance: SUPERB. There's nothing to it, GHC can easily handle
  this style of code.

* Run-time performance: MODERATE. A flat `ReaderT` layer is good, but method
  records are stored within the context (rather than passed via instance
  search). Because of this, the compiler can't assume that the method record is
  coherent (it's not, you can modify it with `local`), and it limits inlining.
  Instance specialization can't be performed because there are no instances to
  speak of.

* Run-time configurability: SUPERB. You can put just about anything into these
  manually defined method records, modify them at will, etc. However, we need to
  figure out a good story for method records that depend on other method
  records, because it's the same pitfall as with `SendActions`: how can we
  ensure that if we change the something (say, a logging method) with `local`,
  other method records that use logging will be updated accordingly?

* Predictability: LOW/HIGH. Depends on whether we have a `newtype` over
  `ReaderT`.

Verdict: the approach is viable but has its costs.
