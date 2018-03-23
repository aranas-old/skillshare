### helper functions to handle text ###
clean_list_to_string <- function(x){ paste(clean_text(x), collapse=", ") }
clean_text <- function(x){ # don't capitalize e-mails
  if ('@' %in% x) { x = trimws(x) } 
  else { x = uppercase_first(trimws(x))}
  gsub("'", "''", x)  # escape single quotes for SQLlite
}
uppercase_first <- function(x){  # uppercase the first letter, it's only for aesthetics
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
remove_empty <- function(x){if (!is.null(x)) { x[x != ""] } else x }
data_to_list <- function(x){remove_empty(trimws(unlist(strsplit(x, ","))))}
string_to_list <- function(x){ if (length(x) > 0) { remove_empty(trimws(unlist(strsplit(x, ","))))} else { list()} }
# check the vailidity of the e-mail format:
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

### helper function that adds a red star to compulsory form fields (e.g., name, e-mail, skills)
labelMandatory <- function(label) {
  tagList(label, span("*", class = "mandatory_star"))
}

### helper function for "Detail" button ###
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(i), ...))
  }
  inputs
}

### SQL Lite database transactions ###
sql_fname = "db/phd_data.sqlite" #"db/data.sqlite"
create_db <- function() {
  con <- dbConnect(SQLite(), sql_fname)
  dbExecute(con, "CREATE TABLE skillshare (name, email PRIMARY KEY, skills, skillsDetail, needs, needsDetail, cohort, affiliation, location, timestamp TIMESTAMP
  DEFAULT CURRENT_TIMESTAMP);")
  dbDisconnect(con)
}
userExists <- function(name, email){
  con <- dbConnect(SQLite(), sql_fname)
  data <- dbGetQuery(con, sprintf("SELECT EXISTS(SELECT 1 FROM skillshare WHERE UPPER(name)=UPPER('%s') AND UPPER(email)=UPPER('%s'));", name, email))
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
  print('-----')
  sql_db <- dbConnect(SQLite(), sql_fname)
  data <- dbGetQuery(sql_db, "SELECT rowid, name, skills, needs FROM skillshare")
  print(data)
  print('-----')
  dbDisconnect(sql_db)
  colnames(data) <- c("rowid","name", "skills", "needs")
  #data <- data[order(data$name),]
  #rownames(data) <- 1:nrow(data)
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
