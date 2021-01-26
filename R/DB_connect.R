library(pool)
my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "ksdatabase",
  host = "cdrlshinyapps.cdkkdi6q6ptl.us-east-2.rds.amazonaws.com",
  username = "cdrl",
  password = "cdrl_ks"
)


onStop(function() {
  poolClose(my_db)
})