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
#useShinyjs()

### Set variables #####
fields <- c("timestamp", "firstName","lastName","email","skills","needs","needsDetail","skillsDetail","department")
database_fname <- "db/data"
sql_fname = "db/data.sqlite"
# partner institutes for Language in Interaction: https://www.languageininteraction.nl/organisation/partners.html. Maybe "departments" is too specific? Should we switch to institute/university?
# TODO: Check if the list is complete
departments <- c("Centre for Language Studies (CLS), Radboud University", "Centre for Language and Speech Technology (CLST), Radboud University", "Donders Centre for Cognition (DCC), Donders", "Institute for Logic, Language and Computation (ILLC), University of Amsterdam", "Neurobiology of Language (NB), MPI", "Language and Cognition (LC), MPI", "Language and Genetics (GEN), MPI", "Language Development, MPI", "Psychology of Language (POL), MPI", "Neurogenetics of Vocal Communication Group, MPI", "RadboudUMC", "UMC Utrecht", "Maastricht University", "Tilburg University", "Universitetit Leiden")
departments <- departments[order(departments)]  # sort alphabetically

function(input, output, session) {
    observe({
        ### Form #####
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
          }
          if (userInfo$lastName != input$lastName_edited){
            changes = c(changes, sprintf("lastName = '%s'", clean_text(input$lastName_edited)))
          }
          if (userInfo$email != input$email_edited){
            changes = c(changes, sprintf("email = '%s'", clean_text(input$email_edited)))
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
          removeModal()  # close pop-up when submit button is clicked #toggleModal(session, "modaledit", toggle = "close")
        })

        # helper functions to handle text
        clean_text <- function(x){ uppercase_first(trimws(x)) }
        clean_list_to_string <- function(x){ paste(clean_text(x), collapse=", ") }
        uppercase_first <- function(x){  # we can uppercase the first letter, it's only for aesthetics
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
        # have datatable content as reactive value to be accessed later (i.e. pop-up for details)
        # update with every submit/edit
        dat <- reactive({
          input$submit
          input$submitEdit
          dat = loadData()
          dat
        })
        
        getBasicInfo <- reactive({
          input$submit
          input$submitEdit
          data = queryBasicData()
          data
        })
        
        skillsNeedsUnique <- reactive({
          input$submit
          input$submitEdit
          data = querySkillsNeeds()
          all_unique_keywords = unique(data_to_list(paste(data$skills, ", ", data$needs)))
          all_unique_keywords
        })
        
        skillsKeywords <- reactive({
          input$submit
          input$submitEdit
          data = querySkillsNeeds()
          skills = data_to_list(data$skills)
          skills
        })
        
        needsKeywords <- reactive({
          input$submit
          input$submitEdit
          data = querySkillsNeeds()
          needs = data_to_list(data$needs)
          needs
        })
        
        
        # Select relevant information to visualize in table
        output$database <- DT::renderDataTable({
          df = dat()
          df <- df[,c("firstName","lastName","skills","needs")]
          ##df <- df[order(df$firstName),]
          df$skills <- as.factor(df$skills) # set columns to factor if search field should be dropdown
          datatable(df, filter = 'top') # TODO: put search fields on top of table  # colnames = c('First Name', 'Last Name', 'Skills', 'Needs') ?
          data=data.frame(df,
                          Details = shinyInput(actionButton, length(df$firstName), 'details', label = "Details", onclick = 'Shiny.onInputChange(\"details_button\",  this.id)'))
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
        
        ### Network graph #######

        # set content of graph (nodes & edges)
        node_pairs <- reactive({
          input$submit # update nodes whenever new data is submitted
          input$submitEdit  # or edited
          
          # find pairs of people where skills match needs
          df_pairs <- data.frame()
          nodes <- data.frame()
          
          data = getBasicInfo()
          skills = strsplit(data$skills, ", ")
          # Go through Needs instead of Skills, they are less
          for (row in 1:nrow(data)){
            current_need <- string_to_list(data$needs[row])
            for (need in current_need) {
              combined <- 0
              idx <- grep(need, skills, ignore.case = TRUE)
              if (length(idx)>0){
                from = idx
                to = rep(row, length(from))
                title <- need
                combined <- rbind(to, from, title)
                df_pairs <- rbind(df_pairs, as.data.frame(t(combined)))
              }
            }
            nodes <- rbind(nodes, data.frame(id = row,
                                             fullName = data$fullName[row],
                                             skills = data$skills[row],
                                             needs = data$needs[row],
                                             department = data$department[row]))
          }
          
          df_pairs <- df_pairs[order(df_pairs$title),]
          df_pairs$title <- as.factor(df_pairs$title)
          count <- data.frame()  # FIXME: we have this info already, no reason to go through data twice
          for (row in 1:nrow(data)){
            if (any(df_pairs == data$fullName[row])) {
              count = rbind(count, data.frame(Connections = length(which(df_pairs == data$fullName[row])) * 2))
            } else {
              count = rbind(count, data.frame(Connections = 4))
            }
          }
          nodes <- cbind(nodes, data.frame(Connections = count))
          graphinfo <- list(nodes = nodes, df_pairs = df_pairs) # concat all variables that are accessed elsewhere in code
          graphinfo
        })

        # set visual parameters of graph
        networkgraph <- reactive({
          # access nodes & edges reactive values
          graphinfo <- node_pairs()
          nodes <- graphinfo$nodes
          df_pairs <- graphinfo$df_pairs
          skills = skillsKeywords()
          # create color palette
          palet = colorRampPalette(brewer.pal(length(unique(skills)), "Paired"))  # Pastel1 has less colors
          colors = data.frame(skills = sort(unique(skills)), colors = c(color = palet(length(unique(skills)))))
          # set node parameters
          nodes$shape <- "dot"
          nodes$shadow <- TRUE # Nodes will drop shadow
          nodes$label <- NULL # Node label
          #nodes$title <- paste0("Name : ", nodes$id, "<br> Email : ", nodes$Email , "<br> Skill : ", nodes$Skills)
          nodes$title <- nodes$fullName #nodes$id
          nodes$size <- nodes$Connections * 3 # Increase node size
          nodes$borderWidth <- 2 # Node border width
          nodes$font.size <- 0
          #set nodes colors
          nodes$color.background <- "#4bd8c1"
          nodes$color.border <- "#42b2a0"
          nodes$color.highlight.background <- "#4bd8c1"
          nodes$color.highlight.border <- "red"
          #set edges parameters
          df_pairs$color <- colors$colors[match(df_pairs$title, colors$skills)]  # line color
          df_pairs$arrows <- "to" # arrows: 'from', 'to', or 'middle'
          df_pairs$smooth <- TRUE    # should the edges be curved?
          df_pairs$shadow <- FALSE    # edge shadow
          df_pairs$width <- 5    # edge shadow
          
          #output network
          visNetwork(nodes, df_pairs) %>%
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
        observe({
          indx = input$database_rows_all      # rows on all pages (after being filtered)
          sel = input$database_rows_selected
          if (!is.null(sel)){
            indx = sel
          }
          #access reactive values
          graphinfo <- node_pairs()
          nodes <- graphinfo$nodes
          df_pairs <- graphinfo$df_pairs
          skills = skillsKeywords()
          #create color palette
          palet = colorRampPalette(brewer.pal(length(unique(skills)), "Paired"))   # Pastel1 has less colors
          colors = data.frame(skills = sort(unique(skills)), colors = c(color = palet(length(unique(skills)))))
          #change color
          nodes$color.background <- "#d3d3d3" #lightgray
          nodes$color.border <- "#d3d3d3"
          nodes$color.background[indx] <- "#4bd8c1"
          nodes$color.border[indx] <- "#42b2a0"
          #color all edges
          df_pairs$color <- colors$colors[match(df_pairs$title, colors$skills)]  # line color
          #gray out the ones belonging to gray nodes
          #FIX:so ugly, there must be a more elegant way?
          # TODO: gray_out=intersect(which(df_pairs$from%in%setdiff(df_pairs$from,nodes$id[indx])),which(df_pairs$to%in%setdiff(df_pairs$to,nodes$id[indx])))  # FIXME
          gray_out=intersect(which(df_pairs$from%in%setdiff(df_pairs$from,nodes$id[indx])),which(df_pairs$to%in%setdiff(df_pairs$to,nodes$id[indx])))  # FIXME
          df_pairs$color <- as.character(df_pairs$color) # weird interference with the factor level
          df_pairs$color[gray_out] <- "#d3d3d3"
          df_pairs$color <- as.factor(df_pairs$color)
          df_pairs$width <- 5    # edge shadow
          df_pairs$arrows <- "to"
          #FIX: for some reason the tip of the arrows do not change color
          #update network
          visNetworkProxy("network") %>%
          visUpdateNodes(nodes) %>%
          visUpdateEdges(df_pairs)
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
                                line = list(color = '#FFFFFF', width = 3)),
                  showlegend = TRUE
          ) %>%
          layout(title = 'Skills',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })

        output$piePlotNeeds <- renderPlotly({
          needs = needsKeywords()
          frequencies <-as.data.frame(table(needs))
          #rownames(frequencies) = frequencies$needs
          plot_ly(frequencies, labels = ~needs, values = ~Freq, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'percent',
                  hoverinfo = 'text',
                  text = ~needs,
                  marker = list(colors = c(color = brewer.pal(length(needs),"Paired")),
                                line = list(color = '#FFFFFF', width = 3)),
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
                              current = input$details_button
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
                            footer = modalButton("close"),actionButton("buttonEdit","Edit data")
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
        
        ### SQL Lite database
        loadData <- function() {
          con <- dbConnect(SQLite(), sql_fname)
          data <- dbGetQuery(con, "SELECT * FROM skillshare")
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
          data <- dbGetQuery(sql_db, "SELECT fullName, skills, needs, department FROM skillshare")
          dbDisconnect(sql_db)
          colnames(data) <- c("fullName", "skills", "needs", "department")
          data
        }
        
        querySkillsNeeds <- function(){
          sql_db <- dbConnect(SQLite(), "db/data.sqlite")
          data <- dbGetQuery(sql_db, "SELECT skills, needs FROM skillshare") 
          dbDisconnect(sql_db)
          colnames(data) <- c("skills", "needs")
          data
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

        editData <- function(values, user_id){
          query <- sprintf("UPDATE skillshare SET %s WHERE rowid = %s", values, user_id)
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