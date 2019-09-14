# Pure Art v0 (WIP)

### Idea:

This project stems from a very simple observation. Given a string of letters or numbers `s`, `h(x|s)` and `h(y|s)` for some hash function `h` can represent a coordinate on a two dimensional plane with dimensions equal to the cardinality of the range of `h`. By taking `s` to be (e.g) from the set of Bitcoin addresses and `h` to be (e.g) Sha256, we can associate a point to every Bitcoin address. Transactions between addresses can then be visualized as line segments between the points corresponding to the addresses. We can render a block (or even the entire blockchain) in this manner, leading to what mostly likely would appear as a chaotic mess of lines on a plane.

However, we can also take advantage of this correspondence between addresses + transactions and geometry "in reverse". Start with a vector image of line segments, rendered in some resolution `k` by `l`. Scaling our 2^256 sized hash image plane down to `k x l` results in huge collections of addresses all corresponding to the same point in the `k x l` plane. This makes the idea of computing vanity addresses with the property that their coordinate maps to a given coordinate in `k x l` feasible. (Early performance testing provides that _every_ point in a `1000 x 1000` plane could have a corresponding address generated at random in ~ 40 min in an unoptimized system.) Once vanity addresses have been computed for every endpoint in the image, transactions can be created between this addresses and posted to chain: your vector art will be encoded into the fabric of the chain for all time.

### Progress:

- X vanity address generation
- WIP graph conversion to transaction sequence
- WIP end wallet
- front end paint canvas
- gallery to view art
- marketplace to purchase and transfer ownership of art
