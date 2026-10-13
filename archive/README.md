# archive/

Superseded reference copies of pipeline scripts. These files are kept for
historical comparison only and must NOT live under `R/`: they redefine the
same top-level symbols as the live scripts, so sourcing the `R/` directory
in order would silently revert the live layer to the archived version.

- `32_projection_reconciliation_v2_1_reference.R` — v2.1 snapshot of the
  projection reconciliation layer, superseded by `R/32_projection_reconciliation.R`.

Do not source anything in this directory as part of the pipeline.
