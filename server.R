### Libraries #####
library(RColorBrewer)
library(igraph)
library(plotly)
require(shiny)
require(visNetwork, quietly = TRUE)
library(DT)
require(shiny)
library(shinyjs)
library(shinyBS)
require("RSQLite")

### Set variables #####
fields <- c("timestamp", "firstName","lastName","email","skills","needs","needsDetail","skillsDetail","department")
sql_fname = "db/data.sqlite"
# partner institutes for Language in Interaction: https://www.languageininteraction.nl/organisation/partners.html. Maybe "departments" is too specific? Should we switch to institute/university?
# TODO: Check if the list is complete
departments <- c("Centre for Language Studies (CLS), Radboud University", "Centre for Language and Speech Technology (CLST), Radboud University", "Donders Centre for Cognition (DCC), Donders", "Institute for Logic, Language and Computation (ILLC), University of Amsterdam", "Neurobiology of Language (NB), MPI", "Language and Cognition (LC), MPI", "Language and Genetics (GEN), MPI", "Language Development, MPI", "Psychology of Language (POL), MPI", "Neurogenetics of Vocal Communication Group, MPI", "RadboudUMC", "UMC Utrecht", "Maastricht University", "Tilburg University", "Universitetit Leiden")
departments <- departments[order(departments)]  # sort alphabetically

function(input, output, session) {
  observe({
    ### Add form #####
    formData <- reactive({
      sapply(fields, function(x) input[[x]])  # Aggregate all form data
    })
    
    observe({
      # check if all mandatory fields (name, email etc) have a value
      mandatoryFilled <- vapply(c("firstName", "lastName", "skills", "email"),
                                function(x) {
                                  !is.null(input[[x]]) && input[[x]] != ""
                                },
                                logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      # enable/disable the submit button (all mandatory fields need to be filled in, and the e-mail needs to have a correct format)
      shinyjs::toggleState(id = "submit", condition = (mandatoryFilled && isValidEmail(input$email)))
    })
    
    observeEvent(input$submit, { # New data: when the Submit button is clicked, save the form data
      toggleModal(session, "modaladd", toggle = "close") # first close the pop-up window
      saveData(formData())
    })
    
    observeEvent(input$submitEdit,{
      userInfo <- queryUserInfo(value$current)
      changes = c()
      # @Sophie, I couldn't figure out quickly how to only keep track of the form edits so I followed this ugly approach: added suffix _edited to all form units and 
      # compared values to the ones stored in the db. Really ugly, but not urgent to be fixed. Do you know a better solution w/o spending more than 10' on this?
      
      # Also, I cleaned the text before saving (trimmed spaces, uppercased first letter etc)
      if (userInfo$firstName != input$firstName_edited){
        changes = c(changes, sprintf("firstName = '%s'", clean_text(input$firstName_edited)))
        fullName = input$firstName_edited
      } 
      if (userInfo$lastName != input$lastName_edited){
        changes = c(changes, sprintf("lastName = '%s'", clean_text(input$lastName_edited)))
      }
      fullName = paste(clean_text(input$firstName_edited), " ", clean_text(input$lastName_edited))
      if (userInfo$fullName != fullName){
        changes = c(changes, sprintf("fullName = '%s'", fullName))
      }
      if (userInfo$email != input$email_edited){
        changes = c(changes, sprintf("email = '%s'", trimws(input$email_edited)))
      }
      edited_skill = clean_list_to_string(input$skills_edited)
      if (clean_list_to_string(userInfo$skills) != edited_skill){
        changes = c(changes, sprintf("skills = '%s'", edited_skill))
      }
      if (userInfo$skillsDetail != input$skillsDetail_edited){
        changes = c(changes, sprintf("skillsDetail = '%s'", clean_text(input$skillsDetail_edited)))
      }
      edited_need = clean_list_to_string(input$needs_edited)
      if (clean_list_to_string(userInfo$needs) != edited_need){
        changes = c(changes, sprintf("needs = '%s'", edited_need))
      }
      if (userInfo$needsDetail != input$needsDetail_edited){
        changes = c(changes, sprintf("needsDetail = '%s'", clean_text(input$needsDetail_edited)))
      }
      if (userInfo$department != input$department_edited){  # this goes only through "selectInput", no need to clean text
        changes = c(changes, sprintf("department = '%s'", input$department_edited))
      }
      changes = c(changes, "timestamp = CURRENT_TIMESTAMP")  # Update timestamp. Otherwise only update SQL if there are changes
      changes = paste(changes, collapse=", ")  # Turn list of changes into a string
      editData(changes, value$current)
      removeModal()  # close pop-up(s) when submit button is clicked #toggleModal(session, "modaledit", toggle = "close")
    })
    
    # helper functions to handle text
    clean_text <- function(x){ uppercase_first(trimws(x)) }
    clean_list_to_string <- function(x){ paste(clean_text(x), collapse=", ") }
    uppercase_first <- function(x){  # uppercase the first letter, it's only for aesthetics
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    remove_empty <- function(x){x[x != ""]}
    data_to_list <- function(x){remove_empty(trimws(unlist(strsplit(x, ","))))}
    string_to_list <- function(x){remove_empty(trimws(unlist(strsplit(x, ","))))}
    # check the vailidity of the e-mail format:
    isValidEmail <- function(x) {
      grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
    }
    
    ### Table #####
    getBasicInfo <- reactive({
      # have database content as reactive value to be accessed later (i.e. pop-up for details)
      # update with every submit/edit/delete
      input$submit
      input$submitEdit
      input$submitDelete
      data = queryBasicData()  # query DB to get fullName, skills, needs and department
      data
    })
    
    getRowIDs <- reactive({
      data = getBasicInfo()  # query DB to get fullName, skills, needs and department
      data$rowid
    })
    
    skillsNeedsUnique <- reactive({
      data = getBasicInfo()
      all_unique_keywords = unique(data_to_list(paste(data$skills, ", ", data$needs)))
      all_unique_keywords
    })
    
    skillsKeywords <- reactive({
      data = getBasicInfo()
      skills = data_to_list(data$skills)
      skills
    })
    
    needsKeywords <- reactive({
      data = getBasicInfo()
      needs = data_to_list(data$needs)
      needs
    })
    
    # Select relevant information to visualize in table
    output$database <- DT::renderDataTable({
      #print("TABLE")
      df = getBasicInfo()
      #df <- df[order(df$fullName),] # FIXME: This also changes the order of the row numbers :/ 
      df <- df[ , !(names(df) %in% "rowid")]  # No need to show rowid, it's for internal purposes
      DT::datatable(df, filter = 'top') # TODO: put search fields on top of table  # colnames = c('First Name', 'Last Name', 'Skills', 'Needs') ?
      data=data.frame(df,
                      Details = shinyInput(actionButton, length(df$fullName), 'details', label = "Details", onclick = 'Shiny.onInputChange(\"details_button\",  this.id)'))
      data
    },escape=FALSE)
    
    #### used in the "Add data" form ####
    output$departmentSelector <- renderUI({
      selectInput("department", "Department", choices=departments)
    })
    output$skillsSelector <- renderUI({
      skills_and_needs <- skillsNeedsUnique()
      selectInput("skills", tagList("Skills", span("*", class = "mandatory_star")), choices = skills_and_needs, multiple = TRUE)
    })
    output$needsSelector <- renderUI({
      skills_and_needs <- skillsNeedsUnique()
      selectInput("needs", "Needs", choices = skills_and_needs, multiple = TRUE)
    })
    
    #########################
    ##### Network graph #####
    #########################
    node_pairs <- reactive({
      #print("GRAPH")
      # set content of graph (nodes & edges). Find pairs of people where skills match needs
      edges <- data.frame()
      nodes <- data.frame()
      data = getBasicInfo()
      skills = strsplit(data$skills, ", ")
      # Go through Needs instead of Skills, they are less
      for (row in 1:nrow(data)){
        node_connection_size = 1 # default (arbitrary) connection size. We can use it to determine the size of the node.
        current_need <- string_to_list(data$needs[row])
        for (need in current_need) {
          skilled_idx <- grep(need, skills, ignore.case = TRUE)
          if (length(skilled_idx) > 0){
            node_connection_size = node_connection_size + length(skilled_idx)  # increase by connections of current skill
            from = data$rowid[skilled_idx]  # due to deletions, rowid != the number of the row in the db, need to adjust.
            to = rep(data$rowid[row], length(from))
            title = need
            edges <- rbind(edges, as.data.frame(t(rbind(to, from, title))))
          }
        }
        nodes <- rbind(nodes, data.frame(id = data$rowid[row],
                                         fullName = data$fullName[row],
                                         skills = data$skills[row],
                                         needs = data$needs[row],
                                         department = data$department[row],
                                         connection_size = node_connection_size))
      }
      #edges <- edges[order(edges$title),]
      list(nodes = nodes, edges = edges) # returns nodes and edges (pairs)
    })
    
    # set visual parameters of graph
    networkgraph <- reactive({
      # access nodes & edges reactive values
      #print("NETGRAPH")
      graphinfo <- node_pairs()
      nodes <- graphinfo$nodes
      edges <- graphinfo$edges
      skills = sort(unique(skillsKeywords()))
      num_skills = length(skills)
      # create color palette
      palet = colorRampPalette(brewer.pal(num_skills, "Paired"))  # Pastel1 has less colors
      colors = data.frame(skills = skills, colors = c(color = palet(num_skills)))
      # set node parameters
      nodes$shape <- "dot"
      nodes$shadow <- TRUE # Nodes will drop shadow
      nodes$label <- NULL # Node label
      nodes$title <- nodes$fullName
      nodes$size <- 12 # I actually like uniformity in the nodes, but if you want to play with the size of the connections then: #nodes$connection_size * 3 (or any other number)
      nodes$font.size <- 0
      #set nodes colors
      nodes$color.background <- "#4bd8c1"
      nodes$color.border <- "#42b2a0"
      nodes$color.highlight.background <- "#4bd8c1"
      nodes$color.highlight.border <- "#006600"  #changed it to dark green instead of "red" because it looked like an error IMO. What do you think? :)
      #set edges parameters
      #edges$color <- colors$colors[match(edges$title, colors$skills)]  # line color
      edges$arrows <- "to" # arrows: 'from', 'to', or 'middle'
      edges$smooth <- FALSE    # should the edges be curved?
      edges$width <- 5 # edge shadow
      #output network   --- It would be nice if we were UPDATING here instead of calling visNetwork
      #visNetworkProxy("network") %>%
      #  visUpdateNodes(nodes) %>%
      #  visUpdateEdges(edges)
      visNetwork(nodes, edges) %>%
        visIgraphLayout(layout = "layout_in_circle") %>%
        visOptions(highlightNearest = FALSE) %>%  #nodesIdSelection = TRUE #selectedBy = list(variable = "Skills")
        visInteraction(hover = TRUE, hoverConnectedEdges = TRUE, dragNodes = FALSE, zoomView = FALSE, tooltipDelay = 150, dragView = FALSE) %>%
        visEvents(click = "function(nodes){ Shiny.onInputChange('current_node_id', nodes.nodes); }")
    })
    
    output$network <- renderVisNetwork({
      networkgraph()
    })
    # If search function in datatable is used, clear selection)
    observeEvent(input$database_rows_all, {
      proxy = dataTableProxy('database')
      selectRows(proxy, NULL)
    })
    # interaction graph & datatable (both search function updates as well as selection)
    observeEvent(c(input$database_rows_all,input$database_rows_selected), {
      indx = input$database_rows_all      # rows on all pages (after being filtered)
      if (!is.null(input$database_rows_selected)){
         indx = input$database_rows_selected
      }
      #access reactive values
      graphinfo <- node_pairs()
      nodes <- graphinfo$nodes
      edges <- graphinfo$edges
      skills = skillsKeywords()
      #create color palette
      palet = colorRampPalette(brewer.pal(length(unique(skills)), "Paired"))   # Pastel1 has less colors
      colors = data.frame(skills = sort(unique(skills)), colors = c(color = palet(length(unique(skills)))))
      #change color
      nodes$color.background <- "fff" #"#d3d3d3" #lightgray
      nodes$color.border <- "#d3d3d3"
      nodes$color.background[indx] <- "#4bd8c1"
      nodes$color.border[indx] <- "#42b2a0"
      #color all edges
      edges$color <- colors$colors[match(edges$title, colors$skills)]  # line color
      # Gray out non selected nodes
      #FIX:so ugly, there must be a more elegant way?
      rowIDs = getRowIDs()
      gray_out=intersect(which(edges$from%in%setdiff(edges$from,rowIDs[indx])),which(edges$to%in%setdiff(edges$to,rowIDs[indx])))  # FIXME
      edges$color[gray_out] <- "fff" #"#d3d3d3"
      edges$color <- as.factor(edges$color)
      edges$width <- 5    # edge shadow
      edges$arrows <- "to"
      #FIX: for some reason the tip of the arrows do not change color
      # Update network
      visNetworkProxy("network") %>%
      visUpdateNodes(nodes) %>%
      visUpdateEdges(edges)
    })
    
    ### pie plots #####
    #TODO: plots should update together with graph and table when new data is submitted
    output$piePlotSkills <- renderPlotly({
      skills = skillsKeywords()
      frequencies <-as.data.frame(table(skills))
      plot_ly(frequencies, labels = ~skills, values = ~Freq, type = 'pie',
              textposition = 'inside',
              textinfo = 'percent',
              #insidetextfont = list(color='#FFFFFF'), default color is black
              hoverinfo = 'text',
              text = ~skills,
              marker = list(colors = c(color = brewer.pal(length(skills),"Paired")), # colors do not correspond to colors in network graph
                            line = list(color = '#FFFFFF', width = 2)),
              showlegend = TRUE
      ) %>%
        layout(title = 'Skills',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    output$piePlotNeeds <- renderPlotly({
      needs = needsKeywords()
      frequencies <-as.data.frame(table(needs))
      plot_ly(frequencies, labels = ~needs, values = ~Freq, type = 'pie',
              textposition = 'inside',
              textinfo = 'percent',
              hoverinfo = 'text',
              text = ~needs,
              marker = list(colors = c(color = brewer.pal(length(needs),"Paired")),
                            line = list(color = '#FFFFFF', width = 2)),
              showlegend = TRUE
      ) %>%
        layout(title = 'Needs',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    ### Detailed view of user#####
    value <- reactiveValues()
    
    observeEvent(c(input$details_button,input$current_node_id), {
      if (!is.null(input$current_node_id)) {  # when user clicks on network nodes
        current = input$current_node_id
      } else {  # when user clicks on table "Details" button
        data = getBasicInfo()
        current = data$rowid[as.numeric(input$details_button)]  # need to update id according to data rowid. as.numeric is compulsory otherwise is returns NA
      }
      userInfo <- queryUserInfo(current)
      showModal(modalDialog(
        title= sprintf("%s (%s)", userInfo$fullName, userInfo$email),
        renderUI({  # added na.omit on values that could be non available (we don't need to show NA to the user)
          if (!is.na(userInfo$needs)){
            needs <- paste("Needs:", userInfo$needs)
          } else {
            needs <- ""
          }
          HTML(sprintf("Skills: %s </br> %s </br> %s </br> %s</br>Department: %s</br>",  #TODO: Really ugly UI, fix
                       userInfo$skills, na.omit(userInfo$skillsDetail), needs, userInfo$needsDetail, userInfo$department))
        }),
        footer = modalButton("close"),actionButton("buttonEdit","Make edits"), actionButton("buttonDelete", "Delete data")
      ))
      value$current <- current
      session$sendCustomMessage(type = "resetValue", message = "current_node_id")
      session$sendCustomMessage(type = "resetValue", message = "details_button")
    })
    
    observeEvent(input$buttonEdit, {
      skills_and_needs <- skillsNeedsUnique()
      userInfo <- queryUserInfo(value$current)
      if (!is.null(userInfo$department) &&  userInfo$department %in% departments){  # some of the inserted fields don't exist in the departments list and the app hangs
        department_value = userInfo$department
      } else { 
        department_value = "" 
      }
      showModal(modalDialog(
        title = sprintf("Edit Data for: %s", userInfo$fullName),
        textInput("firstName_edited", "First Name", value = userInfo$firstName), 
        textInput("lastName_edited", "Last Name", value = userInfo$lastName),
        textInput("email_edited", "Email", value = userInfo$email),
        selectInput("skills_edited", "Skills", choices = skills_and_needs, selected = string_to_list(userInfo$skills), multiple = TRUE,
                    selectize = TRUE, width = NULL, size = NULL),
        conditionalPanel(condition = "input.skills == 'Other'",
                         textInput("new_keyword","New Keyword", value = NULL)),
        textInput("skillsDetail_edited", "(Optional) comments on skills", value = userInfo$skillsDetail),
        selectInput("needs_edited", "Needs", choices = skills_and_needs, selected = string_to_list(userInfo$needs), multiple = TRUE,
                    selectize = TRUE, width = NULL, size = NULL),
        textInput("needsDetail_edited", "(Optional) comments on needs",value = userInfo$needsDetail),
        selectInput("department_edited", "Department", choices = departments, selected = department_value),
        footer = tagList(modalButton("Cancel"), actionButton("submitEdit", "Submit")))
      )
    })
    
    observeEvent(input$buttonDelete,{
      userInfo <- queryUserInfo(value$current)
      showModal(modalDialog(
        title = sprintf("Are you sure you want to delete all data for: %s (%s)", userInfo$fullName, userInfo$email, " ?"),
        footer = tagList(modalButton("No, cancel"), actionButton("submitDelete", "Confirm & Delete")))
      )
    })
    observeEvent(input$submitDelete,{
      removeUser(value$current)
      removeModal()
    })
    
    ### SQL Lite database
    loadData <- function() {
      con <- dbConnect(SQLite(), sql_fname)
      data <- dbGetQuery(con, "SELECT rowid, * FROM skillshare")
      dbDisconnect(con)
      data
    }
    
    queryUserInfo <- function(user_id) {
      sql_db <- dbConnect(SQLite(), sql_fname)
      data <- dbGetQuery(sql_db, sprintf("SELECT * FROM skillshare WHERE rowid = %s", user_id))
      dbDisconnect(sql_db)
      data
    }
    
    queryBasicData <- function() {
      sql_db <- dbConnect(SQLite(), sql_fname)
      data <- dbGetQuery(sql_db, "SELECT rowid, fullName, skills, needs, department FROM skillshare")
      dbDisconnect(sql_db)
      colnames(data) <- c("rowid","fullName", "skills", "needs", "department")
      data
    }
    
    removeUser <- function(user_id) {
      sql_db <- dbConnect(SQLite(), sql_fname)
      dbExecute(sql_db, sprintf("DELETE FROM skillshare WHERE rowid = %s", user_id))
      dbDisconnect(sql_db)
      removevisNetworkNode()
    }
    
    removevisNetworkNode <- function() {
      # Quite of a hack. Remove a random node so that visNetwork won't complain that the dataframe is missing a data point.
      visNetworkProxy("network") %>%   
        visRemoveNodes("network", 1)
    }
    
    editData <- function(values, user_id){
      query <- sprintf("UPDATE skillshare SET %s WHERE rowid = %s", values, user_id)
      sql_db <- dbConnect(SQLite(), sql_fname)
      dbExecute(sql_db, query)
      dbDisconnect(sql_db)
    }
    
    saveData <- function(data) {
      # Clean data (uppercase etc) before saving
      firstName = clean_text(data$firstName)
      lastName = clean_text(data$lastName)
      skills = clean_list_to_string(data$skills)
      needs = clean_list_to_string(data$needs)
      query <- sprintf("INSERT INTO skillshare VALUES (CURRENT_TIMESTAMP, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s %s')", 
                       firstName, lastName, trimws(data$email), skills, needs, clean_text(data$needsDetail), clean_text(data$skillsDetail), 
                       clean_text(data$department), firstName, lastName)
      sql_db <- dbConnect(SQLite(), sql_fname)
      dbExecute(sql_db, query)
      dbDisconnect(sql_db)
    }
    
    # "Detail" button
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(i), ...))
      }
      inputs
    }
  })
}