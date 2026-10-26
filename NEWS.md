## kwb.smartwater v0.5.0 (2026-10-26)

New function: plot_effect_of_trees()

## kwb.smartwater v0.4.0 (2026-10-23)

The default model parameters have been adapted:

Green roof measures
- green_roof_ext: bagrov_value is now 0.95 (was 0.65 before)
- green_roof_int: bagrov_value is now 3.5 (was 0.75 before)

Tree measures
- trees_sm: tree_volume is now 150 (was 200 before)
- trees_md: tree_volume is now 700 (was 300 before)
- trees_lg: tree_volume is now 2300 (was 400 before)

Infiltration measures
- to_swale: 
    - evaporation_factor is now 0.14 (was 0.1 before),
    - overflow_factor is now 0.0 (was 0.05 before)
- to_surf_infil:
    - evaporation_factor is now 0.33 (was 0.15 before),
    - overflow_factor is now 0.0 (was 0.15 before)
- to_swale_trench:
    - evaporation_factor is now 0.07 (was 0.08 before),
    - overflow_factor is now 0.0 (was 0.1 before)
- to_tree_pit:
    - evaporation_factor is now 0.30 (was 0.2 before),
    - overflow_factor is now 0.0 (was 0.15 before)
- to_trench:
    - evaporation_factor is now 0.0 (was 0.15 before),
    - overflow_factor is now 0.0 (was 0.15 before)

Retention measure
- retention: overflow factor is now 0.6 (was 0.5 before)

## kwb.smartwater v0.3.0 (2026-07-31)

In this version,

- tree measures are considered by increasing the vegetation class,
- default model parameters can be overridden.

## kwb.smartwater v0.2.1

Fix bug in considering existing green roofs for status quo calculation

## kwb.smartwater v0.2.0

Changed interface to the plot-related functions, allowing to specify the margins

## kwb.smartwater v0.1.0

First version to be used by the Smartwater Web App, developed by 
Technologiestiftung Berlin

## kwb.smartwater 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`
