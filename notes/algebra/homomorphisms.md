---
title: Homomorphisms
ordering: a2
tags: homomorphisms
---

## Definition
>Let $\phi : G \to H$ be a mapping between groups $G$ and $H$. We call $\phi$ a
*homomorphism* if it satisfies
$$
  \phi(ab) = \phi(a) \phi(b)
$$
for all $a, b \in G$. If $\phi$ is bijective, we say it is *isomorphic*. We can
think of homomorphisms as mappings between groups that preserve composition.

## Example
Consider the two groups $G = (\mathbb{R}, +)$ and $H = (\{e^{\frac{x}{2\pi}}
\mid x \in \mathbb{R}\}, \times)$ with $\phi$ defined as
$$
\begin{align}
  \phi : \mathbb{R} &\to \mathbb{C} \\
  x &\mapsto e^{\frac{x}{2\pi}}.
\end{align}
$$
Given $x, y \in \mathbb{R}$, we have
$$
\begin{align}
  \phi(xy) &= e^{\frac{xy}{2\pi}} \\
  &= e^{\frac{x}{2\pi}} e^{\frac{y}{2\pi}} \\
  &= \phi(x) \phi(y)
\end{align}
$$
so clearly $\phi$ is a homomorphism.

## Definition
>Given a group homomorphism $\phi : G \to H$, we define the *image* and *kernel*
of the homomorphism as
$$
\begin{align}
  \mathrm{im}(\phi) &= \{\phi(x) \mid x \in G\} \\
  \ker(\phi) &= \{x \in G \mid \phi(x) = e_H \}
\end{align}
$$
where $e_H$ is the identity element in $H$.

In plain English, the image of $\phi$ is the subset of $H$ mapped to by $G$ and
the kernel of $\phi$ is the subset of $G$ that act as the identity in $H$. Note
that we must have $\phi(e_G) = e_H$ since
$$
\begin{align}
  \phi(e_G) &= \phi(e_G e_G) \\
  \phi(e_G) &= \phi(e_G) \phi(e_G) \\
  \phi(e_G) \phi(e_G)^{-1} &= \phi(e_G) \phi(e_G) \phi(e_G)^{-1} \\
  e_H &= \phi(e_G).
\end{align}
$$
