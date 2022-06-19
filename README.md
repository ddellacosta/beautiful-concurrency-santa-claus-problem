
# Beautiful Concurrency's Santa Claus Problem Implementation, Extended


This repo contains two implementations of the Santa Claus Problem as Simon Peyton Jones implemented it in his wonderful paper, [Beautiful Concurrency](https://www.microsoft.com/en-us/research/publication/beautiful-concurrency/):

1) The first one is in the `Basic` module. It is more or less the same as Prof. Jones's implementation, but as I noticed text interleaving when I implemented this as described in the paper (and because it was fun and good practice), I ended up adding a STM-based logger. This uses a `TQueue` and a forked thread that simply checks if there is a message ready to be printed endlessly, printing when one shows up.  
  <br /><br />I'm not sure why the examples shown in the original paper don't do the same text interleaving that I saw--I can only imagine that the underlying implementation of `IO` changed from the time the paper was originally written--but after implementing it this way I read that `Data.Text.IO` does not behave this way, so I may have been able to simply swap out the `putStr` calls with the equivalent `Text` versions...but this is a thread (_cough_) I didn't pull on. If you know more about this I'd love to hear more though!

2) The other implementation was my attempt to turn this into a more sophisticated app using a reough approximation of [the `ReaderT` pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/), along with introducing `UnliftIO` so I could run `Async` actions (in particular forking the `elf` and `reindeer` actions initiated by the parent `santa` action) within the app Monad I created, which let me streamline logging, for one. At this size of an app this is certainly overkill, and I suspect there is more I should add here to ensure this is safe in terms of exception handling in particular...I may add more as I go as this is really just a way for me to learn how this kind of an implementation may work.

In any case, I'd love any feedback anyone may have, especially suggestions on how to improve the more complex app implementation.
