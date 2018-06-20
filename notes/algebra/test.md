---
title: Definitions
ordering: 00
tags: groups, rings
---

# Past exams
>- [Practice final](/static/practice-final.pdf)
- [Quiz 1](/static/quiz-1.pdf)
- [Quiz 2](/static/quiz-2.pdf)
- [Quiz 3](/static/quiz-3.pdf)
- [Quiz 4](/static/quiz-4.pdf)
- [Quiz 5](/static/quiz-5.pdf)
- [Quiz 6](/static/quiz-6.pdf)

# Notes
>- [Group operations](/static/group-operations.pdf)
- [Orbits and stabilizers](/static/orbits-stabilizers.pdf)
- [Rings](/static/rings.pdf)
- [Fields](/static/fields.pdf)
- [Symmetry](/static/symmetry.pdf)
- [Quotients](/static/quotients.pdf)
- [Review](/static/review.pdf)

# Group Operations
>Let $G$ be a group and $S$ a set. An *operation* of $G$ on $S$ is a map
$$
\begin{align}
  G \times S &\to S \\
  (g, s) &\mapsto g \cdot s
\end{align}
$$
such that
>
>- For all $s \in S$, $1s = s$
- For all $g, g' \in G$ and $s \in S$, $g(g')s = (gg')s$

In fact, an operation of $G$ on $S$ is equivalent to a homomorphism
$$
\begin{align}
  G &\to \mathrm{Perm}(S) \\
  g &\mapsto [m_g : s \mapsto gs ]
\end{align}
$$

# Orbits and Stabilizers
>Given a group operation $G$ on $S$, the *orbit* of some $s \in S$ is
$$
  O_s = \{gs \mid g \in G\}.
$$
Thus, the orbit is somewhat akin to the image of $s$ under $G$. Similarly, we
define the *stabilizer* of $s$ as
$$
  G_s = \{g \in G \mid gs = s\}
$$
or the "kernel" of $s$ in $G$.

The set $S$ is partitioned by the orbits of the action. The corresponding
equivalence relation is
$$
  s \sim s' \Leftrightarrow s' = gs \text{ for some } g \in G.
$$

# Transitivity
>An operation is *transitive* if there is a single orbit.

# Kernel
>Given a group operation of $G$ on $S$, its *kernel* $K$ is the kernel of the
corresponding homomorphism $\phi: G \to \mathrm{Perm}(S)$
$$
  K = \{g \in G \mid \forall s \in S, gs = s\}.
$$
An operation is *faithful* if $K = \{1\}$; that is, if $\phi$ is injective.

# Orbit-Stabilizer Counting Formula
>$$
\begin{gather}
  |O_s| |G_s| = |G| \\
  |G|/|H| \equiv O_s
\end{gather}
$$

# Rings
>A ring is a set $R$ together with two binary operations, $+, \cdot$
$$
\begin{align}
  + : R \times R &\to R \\
  (a, b) &\mapsto a + b \\
  \cdot : R \times R &\to R \\
  (a, b) &\mapsto ab
\end{align}
$$
such that
>
>- $(R, +)$ is an abelian group
- $(R, \cdot)$ is associative, commutative, and has identity $1$
- $a(b + c) = ab + ac$

# Field
>A field $F$ is a ring with $(F - \{0\}, \cdot)$ an abelian group.

# Ring Homomorphisms
>A map $\phi : R \to S$ is called a *ring homomorphism* if it satisfies
>
>- $\phi(a + b) = \phi(a) + \phi(b)$
- $\phi(ab) = \phi(a)\phi(b)$
- $\phi(1) = 1$
