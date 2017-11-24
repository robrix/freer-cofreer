# freer-cofreer: freer monads and cofreer comonads.

This is an implementation of the Freer monad described by Oleg Kiselykov in _[Free and Freer Monads: Putting Monads Back in the Closet][Free and Freer]_, and the Cofreer comonad, defined by analogy with Freer.

`Freer` and `Cofreer` differentiate themselves from `Free` and `Cofree` by admitting `Functor` & `Monad`/`Comonad` instances even when the signature is not a `Functor`. This makes them particularly suitable for working with certain GADTs. For example:

```Haskell
data PromptF a where
  Write :: String -> PromptF ()
  Read :: PromptF String

type Prompt = Freer PromptF

write :: String -> Prompt ()
write s = Write s `Then` Return

read :: Prompt String
read = Read `Then` Return

greeting :: Prompt ()
greeting = do
  write "Hi! What’s your name?\n"
  name <- read
  write $ "Pleased to meet you, " ++ name ++ "!"

runPrompt :: Prompt a -> IO a
runPrompt = iterFreer (\ yield instruction -> case instruction of
  Write s -> putStrLn >>= yield
  Read -> getLine >>= yield)
```

The constraints placed on the constructors of `PromptF` mean that it doesn’t admit a `Functor` instance, and thus is not very useful with `Free`. With `Freer`, you get `Functor`, `Applicative`, and `Monad` instances with `PromptF` “for free,” complete with the majority of the API defined in `Control.Monad.Free.Freer`.

[Free and Freer]: http://okmij.org/ftp/Computation/free-monad.html
