require("RSQLite")

### helper functions to handle text
trim <- function (x) gsub("^\\s+|\\s+$", "", x) # returns string w/o leading or trailing whitespace
uppercase_first <- function(x){  # we can uppercase the first letter, it's only for aesthetics
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
remove_NA <- function(x){x[!is.na(x)]}
string_to_list <- function(x){trim(unlist(strsplit(x, ",")))}
###

originalcsv <- read.csv(file="db/data.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
colnames(originalcsv) <- c("timestamp", "firstName","lastName","email","skills","needs","needsDetail","skillsDetail","department")

sql_db <- dbConnect(SQLite(), "db/data.sqlite")
dbWriteTable(sql_db, name="skillshare", value=originalcsv, row.names=FALSE, append=TRUE)
# update full names
dbGetQuery(sql_db, "ALTER TABLE skillshare ADD fullName VARCHAR(50) NOT NULL DEFAULT(0)")
dbGetQuery(sql_db, "UPDATE skillshare SET fullName = firstName || ' ' || lastName WHERE fullName = 0")
dbDisconnect(sql_db)

# Test results/get queries
sql_db <- dbConnect(SQLite(), "db/data.sqlite")
results <- dbGetQuery(sql_db, "SELECT fullName, lastName FROM skillshare WHERE firstName = 'Xiaochen'") 
results

dbrowcount = dbGetQuery(sql_db, "SELECT COUNT(*) FROM skillshare")
dbDisconnect(sql_db)

csvrowcount <- nrow(originalcsv)

# Just to make sure that the numbers of the SQLite data and the CSV rows match
csvrowcount
dbrowcount