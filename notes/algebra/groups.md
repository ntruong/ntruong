---
title: Groups
ordering: a1
---

### Definition
Let $G$ be a set together with a law of composition $\cdot: G \times G \to G$,
denoted $(G, \cdot)$. We call $(G, \cdot)$ a *group* if it satisfies:

- Closure: $\forall a, b \in G, a \cdot b \in G$
- Associativity: $\forall a, b, c \in G, a \cdot (b \cdot c) = (a \cdot b) \cdot
  c$
- Identity: $\exists e \in G$ such that $e \cdot a = a \cdot e = a$ for all $a
  \in G$
- Inverse: $a \in G \Rightarrow \exists a^{-1} \in G, aa^{-1} = a^{-1}a = e$.

One example of a group is $(\mathbb{Z}, +)$. Clearly, the sum of two integers
must be an integer and we already know that addition is associative. The
identity element $e$ is simply the additive identity $0$; this is easily
verifiable. Lastly, given any integer, we can simply take its negative as the
additive inverse.

Alternatively, we can see that $(\mathbb{N}, +)$ is not a group. While closure,
associativity, and identity all hold, we do not have inverses for non-trivial
natural numbers.
