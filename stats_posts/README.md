# stats_posts/

Code, data and figures for the LinkedIn Statistics Series. Kept separate from
the toolkit pipeline: nothing in `R/`, `examples/` or `tests/` reads from here.

- `scripts/` pull and visual scripts, one per post (`create_stats_postNN_*.R`,
  `stats_postNN_pull.R`). Run from the project root; paths resolve with `here()`
  or relative to the root.
- `output/plots/` rendered figures (`stats_postNN_*.png` / `.svg`), tracked.
- `output/data/` small per-post CSVs behind the figures, tracked.

Scripts read toolkit caches (for example `data/season2_cache/`) but never write
outside `stats_posts/`.
