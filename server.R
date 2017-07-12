pdf(NULL)
# Libraries
library(googlesheets)
library(RColorBrewer)
library(igraph)
library(plotly)
#require(shiny)
require(visNetwork, quietly = TRUE) # library(visNetwork) 

# data: edge matrix
key <-extract_key_from_url('https://docs.google.com/spreadsheets/d/1zsG-2R8CMXYjUKd4Cx_EzvIelFR7nGHp4ixSuuvdy7g/pubhtml?gid=572166108&single=true')
gap <- key %>% gs_key()
data <- gap %>% gs_read(ws = "examplar")
data <- within(data,  Fullname <- paste(First_Name, Last_Name, sep=" "))  # new var "Fullname" so as to keep First+Last name separate

#find pairs of people where skills match needs and create new table with one row per pair (with repetitions)
df_pairs <- data.frame()
nodes <- data.frame()

trim <- function (x) gsub("^\\s+|\\s+$", "", x) # returns string w/o leading or trailing whitespace
uppercase_first <- function(x){
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
string_to_list <- function(x){trim(unlist(strsplit(x,",")))}

skills <- c()  # or create unified data frame that contains skills and times seen
skill_frequency <- c()
needs <- c()
needs_frequency <- c()
skills_sort <- c()
for (name in sort(unique(data$Fullname))) {
  combined <- 0
  indname <- which(data$Fullname %in% name)
  currentskill <- uppercase_first(string_to_list(data$Skills[indname]))   # we can uppercase the first letter, it's only for aesthetics 
  currentneed <- uppercase_first(string_to_list((data$Needs[indname])))  # @Sophie: if there is an issue with unique() we should resolve it here, not at the node
  for (nskill in currentskill[!is.na(currentskill)]){
    skills_sort <- rbind(skills_sort,nskill)  # only temporarily here, will remove
    if (is.null(skills) || length(grep(nskill, skills, ignore.case = TRUE))==0){
      skills <- c(skills,nskill)
      skill_frequency <- c(skill_frequency,1)
    } else { #  w/o lowercasing, the skills could contain both Yoga and yoga. We are only adding the first encounter and increasing the frequency each time.
      skill_idx = grep(nskill, skills, ignore.case = TRUE)
      skill_frequency[skill_idx] <- skill_frequency[skill_idx] + 1
    }
    to_ind <- grepl(paste("^",nskill,"$", sep=""),data$Needs, ignore.case=TRUE)  # no need to lowercase, we can have a case-insensitive match. And ^nskill$ is a regular expression that looks for word boundaries
    to <- data$Fullname[to_ind]
    from <- rep(name,length(to))
    title <- nskill
    if (length(to)!=0) {
        combined <- rbind(from,to,title)
        df_pairs <- rbind(df_pairs,as.data.frame(t(combined)))
    }
  }
  for (need in currentneed[!is.na(currentneed)]){
    if (is.null(needs) || length(grep(need, needs, ignore.case=TRUE))==0){
      needs <- c(needs,need)
      needs_frequency <- c(needs_frequency,1)
    } else {
      need_idx = grep(need, needs, ignore.case = TRUE)
      needs_frequency[need_idx] <- needs_frequency[need_idx] + 1
    }
    #needs_sort <- rbind(needs_sort,need)
  }
  nodes <- rbind(nodes,data.frame(id = name,
                                  Skills = paste(currentskill, collapse = ", ") ,
                                  Needs = paste(data$Needs[indname], collapse = ", "),
                                  Department = paste(data$Department[indname], collapse = ", ")))
}

df_pairs$title <- as.character(df_pairs$title)
df_pairs      <-  df_pairs[order(df_pairs$title),]
df_pairs$title <- as.factor(df_pairs$title)
info <- nodes
count = data.frame()
for (name in sort(unique(data$Fullname))) {
  if (any(df_pairs == name)){
    count = rbind(count,data.frame(Connections = length(which(df_pairs == name))*2))
  }else {count = rbind(count,data.frame(Connections = 4))}
}
nodes <- cbind(nodes,data.frame(Connections = count))

nodes$shape <- "dot"  
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$label <- NULL # Node label
#nodes$title <- paste0("Name : ", nodes$id, "<br> Email : ", nodes$Email , "<br> Skill : ", nodes$Skills)
nodes$title <- nodes$id
nodes$size <- nodes$Connections # Node size
nodes$borderWidth <- 2 # Node border width
nodes$font.size <- 0

palet = colorRampPalette(brewer.pal(length(skills),"Pastel1"))
colors = data.frame(skills = sort(skills), colors = c(color = palet(length(skills))))

nodes$color.background <- "#4bd8c1"
nodes$color.border <- "#42b2a0"
nodes$color.highlight.background <- "#4bd8c1"
nodes$color.highlight.border <- "red"

df_pairs$color <-colors$colors[match(df_pairs$title,colors$skills)]  # line color  
df_pairs$arrows <- "to" # arrows: 'from', 'to', or 'middle'
df_pairs$smooth <- TRUE    # should the edges be curved?
df_pairs$shadow <- FALSE    # edge shadow
df_pairs$width <- 5    # edge shadow
# server.R
Logged = TRUE;
PASSWORD <- data.frame(Brukernavn = "imprs", Passord = "6289384392e39fe85938d7bd7b43ff48")

function(input, output, session) {
   source("www/Login.R",  local = TRUE)
  
   observe({
    if (USER$Logged == TRUE) {
  
  output$network <- renderVisNetwork({
    visNetwork(nodes,df_pairs) %>% 
      visIgraphLayout(layout = "layout_in_circle") %>%
      visOptions(highlightNearest = FALSE
                 #nodesIdSelection = TRUE
                 #selectedBy = list(variable = "Skills")
      ) %>%
      visInteraction(hover = T, hoverConnectedEdges = T, dragNodes = FALSE) %>%
      visEvents(click = "function(nodes){
                Shiny.onInputChange('current_node_id', nodes.nodes);}"
      )
  })
  
  #TODO: Need to plot frequencies according to skill_frequency now
  output$piePlot1 <- renderPlotly({
    skills_sort <- sort(skills_sort)
    tmpdata <-as.data.frame(table(skills_sort))
    rownames(tmpdata) = tmpdata$skills_sort
    plot_ly(tmpdata, labels = ~skills_sort, values = ~Freq, type = 'pie',
            textposition = 'inside',
            textinfo = 'percent',
            #insidetextfont = list(color='#FFFFFF'),
            hoverinfo = 'text',
            mode = 'text',
            text = tmpdata$skills_sort,
            marker = list(colors = colors$colors,
            line = list(color = '#FFFFFF', width = 5)),
            showlegend = TRUE
    ) %>%
      layout(title = 'Skills',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # TODO: Need to plot needs according to needs_frequency
  output$piePlot2 <- renderPlotly({
    tmpdata <-as.data.frame(table(needs_sort))
    rownames(tmpdata) = tmpdata$needs_sort
    colors2 = c(color = brewer.pal(length(unique(skills_sort)),"YlGnBu"))
   plot_ly(tmpdata, labels = ~needs_sort, values = ~Freq, type = 'pie',
            textposition = 'inside',
            textinfo = 'percent',
            #insidetextfont = list(color='#FFFFFF'),
            hoverinfo = 'text',
            text = tmpdata$needs_sort,
            marker = list(colors = colors2,
                          line = list(color = '#FFFFFF', width = 5)),
            showlegend = TRUE
    ) %>%
      layout(title = 'Needs',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$nodes_data_from_shiny <- renderDataTable( {
    if((!is.null(input$selskill) && !input$selskill == "") || (!is.null(input$selneed) && !input$selneed == "")){
      outtable = data.frame()
      for (skill in input$selskill){
        outtable <- rbind(outtable,info[grepl(skill,nodes$Skills),])
      }
      for (need in input$selneed){
        outtable <- rbind(outtable,info[grepl(need,nodes$Needs),])
      }
      outtable = unique(outtable)
    }
    else {info}
  }, options = list(lengthMenu = c(5,30,50), pageLength = 5))
  
  output$data_individual <- renderUI({  # added na.omit on values that could be non available (we don't need to show NA to the user)
    if (!is.null(input$current_node_id)) {
      str1 <- paste(input$current_node_id,", ",unique(info$Department[info$id == input$current_node_id]))
      str2 <- paste(na.omit(unique(info$Email[info$id == input$current_node_id])))  # @Sophie: why unique here?
      str3 <- paste("My Skills:   ",unique(info$Skills[info$id == input$current_node_id]))
      str4 <- paste(na.omit(unique(data$Skills_details[data$Fullname == input$current_node_id])))
      str5 <- paste("My Needs:    ",unique(info$Needs[info$id == input$current_node_id]))
      str6 <- paste(na.omit(unique(data$Needs_details[data$Fullname == input$current_node_id])))
      HTML(paste(str1,str2," ",str3,str4," ",str5,str6,sep = '<br/>'))
    }
  })
  
   output$distPlot <- renderPlot({
    dist <- NULL
   dist <- rnorm(input$obs)
  hist(dist, breaks = 100, main = paste("Your password:", input$passwd))
  })
   
  observe({
    if((!is.null(input$selskill) && !input$selskill == "") || (!is.null(input$selneed) && !input$selneed == "")){
      outtable = data.frame()
      for (skill in input$selskill){
        outtable <- rbind(outtable,info[grepl(skill,nodes$Skills),])
      }
      for (need in input$selneed){
        outtable <- rbind(outtable,info[grepl(need,nodes$Needs),])
      }
      all_id = unique(outtable$id)
      
      #skill_id <- nodes$Skills %in% input$selskill
      #need_id <- nodes$Needs %in% input$selneed
      #all_id <- skill_id | need_id
      nodes_selection <- nodes$id[all_id]
      nodes$color.background <- "gray"
      nodes$color.border <- "gray"
      nodes$color.background[all_id] <- "red"
      nodes$color.border[all_id] <- "red"
    }
    else {nodes$color.background <- "#4bd8c1"
    nodes$color.border <- "42b2a0"}  
    visNetworkProxy("network") %>%
      visUpdateNodes(nodes)
  })
  
    }
   })
}