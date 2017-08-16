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
          # enable/disable the submit button
          shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })

        observeEvent(input$submit, { # New data: when the Submit button is clicked, save the form data
          toggleModal(session, "modaladd", toggle = "close") # first close the pop-up window
          saveData(formData())
        })

        observeEvent(input$submitEdit,{
          userInfo <- getUserInfo(value$current)
          changes = c()
          # @Sophie, I couldn't figure out quickly how to only keep track of the form edits so I followed this ugly approach: added suffix _edited to all form units and 
          # compared values to the ones stored in the db. Really ugly, but not urgent to be fixed. Do you know a better solution w/o spending more than 10' on this?
          if (userInfo$firstName != input$firstName_edited){
            changes = c(changes, sprintf("firstName = '%s'", input$firstName_edited))
          }
          if (userInfo$lastName != input$lastName_edited){
            changes = c(changes, sprintf("lastName = '%s'", input$lastName_edited))
          }
          if (userInfo$email != input$email_edited){
            changes = c(changes, sprintf("email = '%s'", input$email_edited))
          }
          edited_skill = paste(input$skills_edited, collapse=", ")
          if (paste(userInfo$skills, collapse=", ") != edited_skill){
            changes = c(changes, sprintf("skills = '%s'", edited_skill))
          }
          if (userInfo$skillsDetail != input$skillsDetail_edited){
            changes = c(changes, sprintf("skillsDetail = '%s'", input$skillsDetail_edited))
          }
          edited_need = paste(input$needs_edited, collapse=", ")
          if (paste(userInfo$needs, collapse=", ") != edited_need){
            changes = c(changes, sprintf("needs = '%s'", edited_need))
          }
          if (userInfo$needsDetail != input$needsDetail_edited){
            changes = c(changes, sprintf("needsDetail = '%s'", input$needsDetail_edited))
          }
          if (userInfo$department != input$department_edited){
            changes = c(changes, sprintf("department = '%s'", input$department_edited))
          }
          changes = paste(changes, collapse=", ")
          if (changes != ''){  # only update SQL if there are changes
              editData(changes, value$current)
          }
          removeModal()  # close pop-up when submit button is clicked #toggleModal(session, "modaledit", toggle = "close")
        })

        # helper functions to handle text
        trim <- function (x) gsub("^\\s+|\\s+$", "", x) # returns string w/o leading or trailing whitespace
        uppercase_first <- function(x){  # we can uppercase the first letter, it's only for aesthetics
          substr(x, 1, 1) <- toupper(substr(x, 1, 1))
          x
        }
        remove_NA <- function(x){x[!is.na(x)]}
        string_to_list <- function(x){trim(unlist(strsplit(x, ",")))}

        ### Table #####
        # have datatable content as reactive value to be accessed later (i.e. pop-up for details)
        # update with every submit/edit
        dat <- reactive({
          input$submit
          input$submitEdit
          dat = loadData()
          dat
        })
        # Select relevant information to visualize in table
        output$database <- DT::renderDataTable({
          df = dat()
          df <- df[,c("firstName","lastName","skills","needs")]
          ##df <- df[order(df$firstName),]
          df$skills <- as.factor(df$skills) # set columns to factor if search field should be dropdown
          datatable(df, filter = 'top') # put search fields on top of table  # colnames = c('First Name', 'Last Name', 'Skills', 'Needs') ?
          data=data.frame(df,
                          Details = shinyInput(actionButton, length(df$firstName), 'details', label = "Details", onclick = 'Shiny.onInputChange(\"details_button\",  this.id)'))
        },escape=FALSE)

        ### Network graph #######

        # set content of graph (nodes & edges)
        node_pairs <- reactive({
          input$submit # update nodes whenever new data is submitted
          # find pairs of people where skills match needs
          df_pairs <- data.frame()
          nodes <- data.frame()
          datanet <- dat()
          skills <- uppercase_first(remove_NA(string_to_list(datanet$skills))) # reads ALL skills into a list
          needs <- c()  # reads Needs column and processes each line separately
          for (row in 1:nrow(datanet)){
            if (is.na(datanet$needs[row])){
              needs <- rbind(needs, '')  # participant has no needs but we need to keep the row, so rbind it
            } else {
              needs <- rbind(needs, paste(uppercase_first(string_to_list(datanet$needs[row])), collapse = ","))
            }
          }
          # now go through each participant row (we can force 1 line/participant, and remove "unique").
          for (row in 1:nrow(datanet)){
            currentskill <- uppercase_first(remove_NA(string_to_list(datanet$skills[row])))
            currentneed <- uppercase_first(remove_NA(string_to_list((datanet$needs[row]))))
            for (nskill in currentskill) {
              combined <- 0
              to_ind <- grep(paste("(^|,| ,)", nskill, "($|,)", sep = ""), needs, ignore.case=TRUE)
              if (length(to_ind)>0){  # first check if skill is needed
                to <- datanet$fullName[to_ind]
                from <- rep(datanet$fullName[row], length(to))
                title <- nskill
                if (length(to) != 0) {
                  combined <- rbind(from, to, title)
                  df_pairs <- rbind(df_pairs, as.data.frame(t(combined)))
                }
              }
            }
            nodes <- rbind(nodes, data.frame(id = datanet$fullName[row],
                                             skills = paste(currentskill, collapse = ","),
                                             needs = paste(currentneed, collapse = ","),
                                             department = paste(datanet$department[row], collapse = ",")))
          }
          
          df_pairs$title <- as.character(df_pairs$title)
          df_pairs <- df_pairs[order(df_pairs$title),]
          df_pairs$title <- as.factor(df_pairs$title)
          count <- data.frame()
          for (row in 1:nrow(datanet)){
            name <- datanet$fullName[row]
            if (any(df_pairs == name)) {
              count = rbind(count, data.frame(Connections = length(which(df_pairs == name)) * 2))
            } else {
              count = rbind(count, data.frame(Connections = 4))
            }
          }
          nodes <- cbind(nodes, data.frame(Connections = count))
          graphinfo <- list(nodes = nodes, df_pairs = df_pairs, skills = skills, needs = needs) # concat all variables that are accessed elsewhere in code
          graphinfo
        })

        # set visual parameters of graph
        networkgraph <- reactive({
          # access nodes & edges reactive values
          graphinfo <- node_pairs()
          nodes <- graphinfo$nodes
          df_pairs <- graphinfo$df_pairs
          skills <- graphinfo$skills
          # create color palette
          palet = colorRampPalette(brewer.pal(length(unique(skills)), "Paired"))  # Pastel1 has less colors
          colors = data.frame(skills = sort(unique(skills)), colors = c(color = palet(length(unique(skills)))))
          # set node parameters
          nodes$shape <- "dot"
          nodes$shadow <- TRUE # Nodes will drop shadow
          nodes$label <- NULL # Node label
          #nodes$title <- paste0("Name : ", nodes$id, "<br> Email : ", nodes$Email , "<br> Skill : ", nodes$Skills)
          nodes$title <- nodes$id
          nodes$size <- nodes$Connections # Node size
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
          skills <- graphinfo$skills
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
          gray_out=intersect(which(df_pairs$from%in%setdiff(df_pairs$from,nodes$id[indx])),which(df_pairs$to%in%setdiff(df_pairs$to,nodes$id[indx])))
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
          graphinfo <- node_pairs()
          frequencies <-as.data.frame(table(graphinfo$skills))
          plot_ly(frequencies, labels = ~Var1, values = ~Freq, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'percent',
                  #insidetextfont = list(color='#FFFFFF'),
                  hoverinfo = 'text',
                  text = frequencies$Var1,
                  marker = list(colors = c(color = brewer.pal(length(frequencies$Var1),"Paired")), # colors do not correspond to colors in network graph
                                line = list(color = '#FFFFFF', width = 5)),
                  showlegend = TRUE
          ) %>%
            layout(title = 'Skills',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        })

        output$piePlotNeeds <- renderPlotly({
          graphinfo <- node_pairs()
          needs <- unlist(strsplit(graphinfo$needs, ","))
          frequencies <-as.data.frame(table(needs))
          #rownames(frequencies) = frequencies$needs
          plot_ly(frequencies, labels = frequencies$needs, values = ~Freq, type = 'pie',
                  textposition = 'inside',
                  textinfo = 'percent',
                  #insidetextfont = list(color='#FFFFFF'), # leave black font color
                  hoverinfo = 'text',
                  text = frequencies$needs,
                  marker = list(colors = c(color = brewer.pal(length(frequencies$needs),"Paired")),
                                line = list(color = '#FFFFFF', width = 5)),
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
                            df = dat()  # TODO: Change current_node_id into the row id to avoid the extra search
                            current = which(df$fullName==input$current_node_id)
                          } else {  # when user clicks on table "Details" button
                            current = as.numeric(input$details_button)
                          }
                          userInfo <- getUserInfo(current)
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
            df = dat()
            current = value$current
            showModal(modalDialog(
                title = sprintf("Edit Data for: %s", df[current, 10]),
                textInput("firstName_edited", "First Name", value = as.character(df[current, 2]), placeholder = as.character(df[current, 2])), 
                textInput("lastName_edited", "Last Name", value = as.character(df[current, 3]), placeholder = as.character(df[current, 3])),
                textInput("email_edited", "Email", value = as.character(df[current, 4]), placeholder = as.character(df[current, 4])),
                selectInput("skills_edited", "Skills", choices = df$skills, selected = as.character(df[current, 5]), multiple = TRUE,
                            selectize = TRUE, width = NULL, size = NULL),
                conditionalPanel(condition = "input.skills == 'Other'",
                                 textInput("new_keyword","New Keyword", value = NULL, placeholder = NULL)),
                textInput("skillsDetail_edited", "(Optional) comments on skills", value = as.character(df[current, 8]), placeholder = as.character(df[current, 8])),
                selectInput("needs_edited", "Needs", choices = df$needs, selected = as.character(df[current, 6]), multiple = TRUE,
                            selectize = TRUE, width = NULL, size = NULL),
                textInput("needsDetail_edited", "(Optional) comments on needs",value = as.character(df[current, 7]), placeholder = as.character(df[current, 7])),
                textInput("department_edited", "Department", value = as.character(df[current, 9]), placeholder = as.character(df[current, 9])),
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
        
        getUserInfo <- function(user_id) {
          con <- dbConnect(SQLite(), sql_fname)
          data <- dbGetQuery(con, sprintf("SELECT * FROM skillshare WHERE rowid = %s", user_id))
          dbDisconnect(con)
          data
        }
        
        saveData <- function(data) {
          # TODO: Clean data (uppercase etc) before saving
          query <- sprintf("INSERT INTO skillshare VALUES (CURRENT_TIMESTAMP, '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s %s')", 
                           data$firstName, data$lastName, data$email, data$skills, data$needs, data$needsDetail, data$skillsDetail, data$department, data$firstName, data$lastName)
          
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