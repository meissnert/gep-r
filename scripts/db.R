# create the initial gep-r database
require(pgUtils)

# connect to the database
con <- dbConnect(PgSQL(), host="localhost", user="postgres", dbname="template1")
if (sum(dbListDatabases(con)[,"datname"] == "gepr") > 0) {
	dbSendQuery(con, "DROP DATABASE gepr")
}
# create new gepr database
dbSendQuery(con, "CREATE DATABASE gepr")

dbDisconnect(con)

con <- dbConnect(PgSQL(), host="localhost", user="postgres", dbname="gepr")

# create table
createDBTable(con, 
	      name="celfile", 
	      attributes=c("CEL", "NAME", "FIRST_NAME", "BIRTH", "STREET", "CITY", "ZIPCODE", 
			   "DIAGNOSIS", "IG_TYPE", "LIGHTCHAIN", "SEX",
			   "SAMPLE_DATE", "SAMPLE_VOLUME", "CD_138_PURIFICATION", "ARRAY_TYPE", "RNA_PURIFICATION_PROTCOLL", "NORMALIZATION_METHOD",
			   "QUALITYCONTROL", "IDENTITYCONTROL", "RISK_STRATIFICATION", "OVEREXPRESSED_GENES", "TARGETGENES_IMMUNOTHERAPY", "TARGETGENES_RISK_TREATMENT",
			   "REPORT"), 
	      data.types=c("VARCHAR(30)", "VARCHAR(30)", "VARCHAR(30)", "DATE", "VARCHAR(30)", "VARCHAR(30)", "REAL",
			   "VARCHAR(30)", "VARCHAR(30)", "VARCHAR(30)", "VARCHAR(30)",
			   "DATE", "REAL", "REAL", "VARCHAR(30)", "VARCHAR(30)",
			   "TEXT", "TEXT", "TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
			   "TEXT"))

dbColnames(con, "celfile")

dbDisconnect(con)
