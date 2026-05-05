library(DBI)

# postgresql
con <- dbConnect(
  RPostgres::Postgres(),
  host = "localhost", port = 5433,
  dbname = "r_test_db", user = "r_test_user", password = "secret123"
)

###################################
## simulation dataset


con <- dbConnect(
  RPostgres::Postgres(),
  host = "localhost", port = 5433,
  dbname = "r_test_db", user = "r_test_user", password = "secret123"
)


DBI::dbExecute(con, "DROP TABLE \"r_variables_table\"")
DBI::dbExecute(con, "DROP TABLE \"mtcars_table\"")


data(mtcars)
data <- mtcars
n_rows <- nrow(mtcars)
data$id <- 1:n_rows
day_one <- 1:20
day_two <- 21:30
day_three <- 31:n_rows
dbWriteTable(con, "mtcars_table", data[day_one, ], overwrite = TRUE)


DBI::dbGetQuery(con, "select * from mtcars_table")

sql <- "select * from mtcars_table where id > 15"
key <- 'id'

#################
new_var_expression <- expression(ifelse(am == 1, 'automatic', 'manual'))
day_2_var_expression <- expression(hp/cyl)
# or
expr_list <- fd_define(
  transimission = new_var_expression,
  hp_per_cyl = day_2_var_expression
)
expr_list <- fd_define(
  transimission = ifelse(am == 1, 'automatic', 'manual')
)
expr_list2 <- fd_define(
  transimission = ifelse(am == 1, 'automatic', 'manual'),
  hp_per_cyl = hp/cyl
)

new_var_working_data <- DBI::dbGetQuery(con, sql)

features_df <- data.frame(
  id = new_var_working_data$id,
  transimission = eval(new_var_expression, new_var_working_data)
)

feat_table_name <- 'r_variables_table'


##### day 1
## First commit


DBI::dbGetQuery(con, "select * from r_variables_table") #shall be error as table does not exist yet


fd_upsert(con,
          features_df = features_df,
          feat_table_name = feat_table_name,
          key = key)

##### day 2

dbWriteTable(con, "mtcars_table", data[day_two, ], append = TRUE)

# what are the new rows that needs to be updated

# raw dataset where ids not been found in 'r_variables_table'
unseen_df <- fd_fetch(con, # the function that returns unseen raw data rows
         sql = sql,
         key = key,
         feat_table_name = feat_table_name)
nrow(unseen_df)


# and we introduce new variable
features_df <- data.frame(
  id = unseen_df$id,
  transimission = eval(new_var_expression, unseen_df),
  hp_per_cyl = eval(day_2_var_expression, unseen_df)
)
# or, alternative method
features_df <- fd_compute(
  unseen_df,
  defs = expr_list,
  key = key
)


fd_upsert(con,
          features_df = features_df,
          feat_table_name = feat_table_name,
          key = key) # shall be error as new column introduced

fd_upsert(con,
          features_df = features_df,
          feat_table_name = feat_table_name,
          key = key,
          alter_table = TRUE)

DBI::dbGetQuery(con, "select * from r_variables_table")


########## fd_run
DBI::dbExecute(con, "DROP TABLE \"r_variables_table\"")
DBI::dbExecute(con, "DROP TABLE \"mtcars_table\"")
dbWriteTable(con, "mtcars_table", data[day_one, ], overwrite = TRUE)

##### day 1
## First commit
fd_run(
  con,
  sql,
  expr_list,
  key,
  feat_table_name = feat_table_name
)

DBI::dbGetQuery(con, "select * from r_variables_table")

##### day 2
dbWriteTable(con, "mtcars_table", data[day_two, ], append = TRUE)
DBI::dbGetQuery(con, "select * from mtcars_table b
                left join r_variables_table v on v.id = b.id")

fd_run(
  con,
  sql,
  expr_list2,
  key,
  feat_table_name = feat_table_name
)
DBI::dbGetQuery(con, "select * from mtcars_table b
                left join r_variables_table v on v.id = b.id")
