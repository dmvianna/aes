# aes

This was a small experiment to learn how to conveniently

- encrypt a message using a random key (represented as a `ByteString`)
- create a random initialisation vector and serialise it as `ByteString`
- then use the key, the vector and the encrypted message to recreate
the original message.

At first I was a bit overwhelmed by the amazingly comprehensive
[cryptonite](https://hackage.haskell.org/package/cryptonite)
API. I did start by using one of the examples in the
[Crypto.Tutorial](https://hackage.haskell.org/package/cryptonite-0.30/docs/Crypto-Tutorial.html), but I was frustrated that I couldn't figure out how to
serialise `IV c`. Eventually I got back to the same page, read the
first paragraph, and the answer was there.
