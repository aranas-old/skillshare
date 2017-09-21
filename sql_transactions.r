### helper functions to handle text ###
clean_text <- function(x){ uppercase_first(trimws(x)) }
clean_list_to_string <- function(x){ paste(clean_text(x), collapse=", ") }
uppercase_first <- function(x){  # uppercase the first letter, it's only for aesthetics
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
remove_empty <- function(x){if (!is.null(x)) { x[x != ""] } else x }
data_to_list <- function(x){remove_empty(trimws(unlist(strsplit(x, ","))))}
string_to_list <- function(x){remove_empty(trimws(unlist(strsplit(x, ","))))}
# check the vailidity of the e-mail format:
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

### SQL Lite database transactions ###
sql_fname = "db/data.sqlite"
userExists <- function(name, email){
  con <- dbConnect(SQLite(), sql_fname)
  data <- dbGetQuery(con, sprintf("SELECT EXISTS(SELECT 1 FROM skillshare WHERE name='%s' AND email='%s');", name, email))
  dbDisconnect(con)
  as.logical(data)
}

queryUserInfo <- function(user_id) {
  sql_db <- dbConnect(SQLite(), sql_fname)
  data <- dbGetQuery(sql_db, sprintf("SELECT * FROM skillshare WHERE rowid = %s", user_id))
  dbDisconnect(sql_db)
  data
}

queryBasicData <- function() {
  sql_db <- dbConnect(SQLite(), sql_fname)
  data <- dbGetQuery(sql_db, "SELECT rowid, name, skills, needs FROM skillshare")
  dbDisconnect(sql_db)
  colnames(data) <- c("rowid","name", "skills", "needs")
  data <- data[order(data$name),]
  rownames(data) <- 1:nrow(data)
  data
}

removeUser <- function(user_id) {
  sql_db <- dbConnect(SQLite(), sql_fname)
  dbExecute(sql_db, sprintf("DELETE FROM skillshare WHERE rowid = %s", user_id))
  dbDisconnect(sql_db)
}

editData <- function(values, user_id){
  query <- sprintf("UPDATE skillshare SET %s WHERE rowid = %s", values, user_id)
  sql_db <- dbConnect(SQLite(), sql_fname)
  dbExecute(sql_db, query)
  dbDisconnect(sql_db)
}

saveData <- function(data) {
  # Clean data (uppercase etc) before saving
  name = clean_text(data$name)
  skills = clean_list_to_string(data$skills)
  needs = clean_list_to_string(data$needs)
  query <- sprintf("INSERT INTO skillshare VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', CURRENT_TIMESTAMP)", 
                   name, trimws(data$email), skills, clean_text(data$skillsDetail), needs, clean_text(data$needsDetail), data$cohort, data$affiliation, data$location
  )
  sql_db <- dbConnect(SQLite(), sql_fname)
  dbExecute(sql_db, query)
  dbDisconnect(sql_db)
}
