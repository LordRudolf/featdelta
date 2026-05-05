# featdelta

`featdelta` is an R package for incremental feature engineering with database
persistence. It is designed for workflows where raw observations live in a
database, feature logic is easier to write and test in R, and the computed
features should be stored back in a database table for modelling, reporting,
monitoring, or downstream reuse.

Instead of rebuilding the same feature table from scratch whenever new rows
arrive, `featdelta` lets you define feature expressions in R, fetch only rows
that have not yet been processed, compute the features locally, and upsert the
results into a persistent feature table.

## Installation

```r
install.packages("featdelta")
```

You can install the development version from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("LordRudolf/featdelta")
```

## Core idea

The standard pipeline is:

1. define reusable feature logic with `fd_define()`;
2. fetch raw database rows that are missing from the feature table;
3. compute the requested features in R;
4. create, extend, insert into, or update the database feature table.

The main orchestration function is `fd_run()`, which combines the fetch,
compute, and upsert steps.

## Small example

This example uses an in-memory SQLite database and `mtcars`, but the same
pattern applies to database tables selected with ordinary SQL.

```r
library(DBI)
library(RSQLite)
library(featdelta)

cars <- mtcars
cars$id <- seq_len(nrow(cars))

con <- dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "raw_cars", cars[1:20, ], overwrite = TRUE)

defs <- fd_define(
  transmission = ifelse(am == 1, "automatic", "manual"),
  hp_per_cyl = hp / cyl,
  wt_per_hp = wt / hp
)

run_day_one <- fd_run(
  con = con,
  sql = "SELECT * FROM raw_cars ORDER BY id",
  defs = defs,
  key = "id",
  feat_table_name = "car_features"
)

dbGetQuery(con, "SELECT * FROM car_features ORDER BY id")
```

When more raw rows arrive, call the same pipeline again. By default,
`fd_run(fetch_mode = "new_only")` processes only keys that are not already in the
feature table.

```r
dbAppendTable(con, "raw_cars", cars[21:30, ])

run_day_two <- fd_run(
  con = con,
  sql = "SELECT * FROM raw_cars ORDER BY id",
  defs = defs,
  key = "id",
  feat_table_name = "car_features"
)

dbGetQuery(con, "SELECT * FROM car_features ORDER BY id")
```

If feature definitions change and existing rows should be recomputed, use
`fetch_mode = "all"` to refresh the rows returned by the SQL query.

```r
defs_v2 <- fd_define(
  transmission = ifelse(am == 1, "automatic", "manual"),
  hp_per_cyl = hp / cyl,
  wt_per_hp = wt / hp,
  mpg_per_cyl = mpg / cyl
)

fd_run(
  con = con,
  sql = "SELECT * FROM raw_cars ORDER BY id",
  defs = defs_v2,
  key = "id",
  feat_table_name = "car_features",
  fetch_mode = "all"
)
```

## Multi-column feature blocks

For feature logic that naturally produces several columns at once, use
`fd_block()`.

```r
defs <- fd_define(
  engine_ratios = fd_block({
    data.frame(
      hp_per_cyl = hp / cyl,
      disp_per_cyl = disp / cyl,
      wt_per_hp = wt / hp
    )
  })
)
```

## Learn more

The package includes vignettes covering the getting-started workflow, feature
definition patterns, database pipeline details, production patterns, and
scheduled runs.
