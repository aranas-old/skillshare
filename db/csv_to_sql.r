require("RSQLite")

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

####################################
### Clean db entries, row by row ###
###################################

# helper functions to handle text
clean_text <- function(x){ uppercase_first(trimws(remove_NA(x))) }
clean_list_to_string <- function(x){ paste(clean_text(x), collapse=", ") }
clean_text_uppercase_all <- function(x){ paste(clean_text(trimws(unlist(strsplit(x, ",")))), collapse=", ") }

uppercase_first <- function(x){  # we can uppercase the first letter, it's only for aesthetics
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
remove_NA <- function(x){x[!is.na(x)]}

# connect to db and read num of data
sql_db <- dbConnect(SQLite(), "db/data.sqlite")
all_data <- dbGetQuery(sql_db, "SELECT * FROM skillshare") 
for (id in 1:nrow(all_data)){
  firstName = clean_text(all_data[id, 2])
  lastName = clean_text(all_data[id, 3])
  email = trimws(all_data[id, 3])
  skills = clean_text_uppercase_all(all_data[id, 5])
  needs = clean_text_uppercase_all(all_data[id, 6])
  needsDetail = clean_text(all_data[id, 7])
  skillsDetail = clean_text(all_data[id, 8])
  department = clean_text(all_data[id, 9])
  values = sprintf("timestamp = CURRENT_TIMESTAMP, firstName = '%s', lastName = '%s', email = '%s', skills = '%s', needs = '%s', 
                   skillsDetail = '%s', needsDetail = '%s', department = '%s', fullName = '%s %s'", firstName, lastName, email, 
                   skills, needs, needsDetail, skillsDetail, department, firstName, lastName)
  query <- sprintf("UPDATE skillshare SET %s WHERE rowid = %s", values, id)
  dbExecute(sql_db, query)
}
dbDisconnect(sql_db)

# Edit table columns
c("timestamp", "firstName","lastName","email","skills","newskill_edit","newskill","needs","newneed","newneed_edit","needsDetail","skillsDetail",
  "cohort", "affiliation", "location")
sql_db <- dbConnect(SQLite(), "db/data.sqlite")
dbExecute(sql_db, "ALTER TABLE skillshare RENAME TO tttemp_table")
dbExecute(sql_db, "CREATE TABLE skillshare (name TEXT NOT NULL,
                                            email TEXT NOT NULL,
                                            skills TEXT NOT NULL,
                                            skillsDetail TEXT,
                                            needs TEXT,
                                            needsDetail TEXT,
                                            cohort INT DEFAULT 2015,
                                            affiliation TEXT DEFAULT 'IMPRS',
                                            location TEXT DEFAULT 'Nijmegen',
                                            timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
                                            )")

dbExecute(sql_db, "INSERT INTO skillshare (name, email, skills, skillsDetail, needs, needsDetail)
                      SELECT
                      firstName ||  ' ' || lastName, email, skills, skillsDetail, needs, needsDetail
                      FROM
                      temp_table")
dbExecute(sql_db, "DROP TABLE tttemp_table")
dbDisconnect(sql_db)


