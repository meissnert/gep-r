# create the initial microarray database
library(maDB)

# connect to the database
con <- dbConnect(PgSQL(), host="localhost", user="postgres", dbname="template1")
if (sum(dbListDatabases(con)[,"datname"] == "madb") > 0) {
	dbSendQuery(con, "DROP DATABASE madb")
}
# create new gepr database
dbSendQuery(con, "CREATE DATABASE madb")
dbDisconnect(con)