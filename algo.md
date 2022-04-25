# Pollination

## Causality between atoms

Given a pair of identical atoms $+a$ and $-a$ of opposite polarities, we compute
the causality between them by looking at the kind (flower or garden) and
polarity ($+$ or $-$) of their least common ancestor $lca(+a, -a)$. Thus there
are 4 cases:

- Negative garden: $- → +$ ($-a$ justifies $+a$)
- Positive garden: $+ → -$
- Negative flower: $+ → -$
- Positive flower: $- → +$

## Temporality/Sequentiality locks = directed information flow

If $lca(+a, -a)$ is a garden, then pollination can be done in both directions (wind-pollination = cross-pollination), that is causality between $+a$ and $-a$ can go both ways (although the above restriction based on the garden's polarity is sufficient for completeness).

If on the contrary it is a flower, then in addition to the previous restrictions, either:

- the source is in the petal
- the source is in the pistil at the top-level

## Partial vehicle

The *partial vehicle* $\mathcal{V}(Γ)$ of a garden $Γ$ is defined by:

- The vertices are the atoms of $Γ$
- There is an edge between $a$ and $b$ iff they are identical atoms of opposite polarities, and the causality and temporality constraints described above are satisfied

There should be no path of length greater than 1, since a known atom (a source) needs no justification (it is not a target).

## Algorithm

Given a garden $Γ$, the pollination $\mathsf{P}(Γ)$ of $Γ$ is computed in the
following way:

- For every edge $a → b$ in $\mathcal{V}(Γ)$
   - Let $F_b$ be the direct subflower of $lca(a, b)$ which contains $b$
   - Append a copy of $F_b$ to the garden of $a$
   - Remove $b$ from its garden in the copy