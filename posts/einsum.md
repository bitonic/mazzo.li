---
title: "`einsum` explainer"
date: 2021-05-15
published: false
---

[`numpy.einsum`](https://numpy.org/doc/stable/reference/generated/numpy.einsum.html) is a fantastically useful function to quickly define operations on matrices. It also lends itself to fairly obscure code. It seems a necessary evil much like regular expressions are -- or maybe even more necessary, but not less evil.

That said, I don't write `numpy` code very often, and when I encounter statements like

```python
equation_lhs = np.reshape(np.einsum(
  "onec,ocs,opw,onw->onesp",
  cross_corner_cams,
  corr_spinmarker_to_cams[:,:3,:3],
  world_to_spinners,
  marker_corners_world_homo
), ( -1, 3 * 4 ))
```

I need to spend 15 minutes just remembering how `einsum` works, and 15 more minutes to understand what is going on.

Wouldn't it be nice to have to have something like <https://regex101.com/>, which helps you visualize and explain regexes, for `einsum`? Why yes it would.
