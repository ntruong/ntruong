---
title: Cyclic and Generative Groups
ordering: a2
tags: groups, subgroups
---

Let $G$ be a group and $A \subseteq G$. We can successively combine elements in
$A$ to expand $A$ until $A$ is closed under the composition law. The minimum
such subgroup *generated* by $A$ is denoted $\langle A \rangle$.

# Example
Consider the group $G = (\mathbb{Z}, +)$ and the subset $A = \{2\}$ of $G$.
Since $2 \in A$, we must also have $-2 \in \langle A \rangle$. Then we must also
have $\{\pm 4\} \subseteq \langle A \rangle$. Expanding outwards, we see that
$$
  \langle A \rangle = \{\cdots, -4, -2, 0, 2, 4, \cdots\}
$$
is the minimum subgroup of $G$ that contains $A$.

# Definition
>A subgroup of a group $G$ is called *cyclic* if it is of the form $\langle x
\rangle$ for some $x \in G$.

# Proposition
>Suppose a cyclic group $G$ has $x^r = x^s$ for some $r \neq s$. Then $G =
\langle x \rangle = \{1, x, x^2, \dotsc, x^{n - 1}\}$ where $n$ is the smallest
positive integer that $x^n = 1$.

# Proof
Let $S = \{k \in \mathbb{Z} \mid x^k = 1\}$ for some $H = \langle x \rangle$ and
suppose we have $x^r = x^s$ for some $r \neq s$. Then
$$
\begin{align}
  x^r &= x^s \\
  x^r x^{-s} &= 1 \\
  x^{r - s} &= 1
\end{align}
$$
and $r - s \in S$.
