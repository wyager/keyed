This library is simple enough: it provides overloaded functions for indexing data structures.

In Data.Keyed, we provide functions for pure data structures that return the contained values unwrapped.

For example, you can do `[1,2,3] ! 2` or `Map.fromList [(1,'a'),(2,'b')] ! 1`.

In Data.MKeyed, we provide functions for data structures that return the contained values wrapped in a Monad (like `IO`, `ST`, or `STM`) or other container. This module is highly experimental, and it may change to improve interoperability with certain libraries.