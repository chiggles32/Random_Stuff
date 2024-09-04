## update rlang to above version 1.1.0
## depending on version of rlang "DiagrammeR might not load properly at first, there are work arounds however.
## most likely need to download "RTools" from CRAN to compile libraries
## remove rlang, igraph, DiagrammeR in user library if present prior to 
## https://cran.r-project.org/bin/windows/Rtools/rtools40.html
## After RTools is installed, when prompted if you want to install from source which needs compilation click "yes"

# install.packages("rlang")
# # check version with packageVersion("rlang") 
# install.packages("igraph")
# install.packages("DiagrammeR")
# install.packages("rsconnect")

# under the Packages gui make sure to 

library(DiagrammeR)
library(tidyverse)
library(readxl)
library(shiny)
library(rsconnect)

# get all successive nodes recursive function

get_direct_impact = function(graph, nodes_vec) {
  x = c()
  for (i in 1:length(nodes_vec)){
    x = union(x, get_successors(graph, nodes_vec[i]))
  }
  as.numeric(na.omit(x))
}

get_all_successors = function(graph, nodes_vec){
  
  visited = c()
  
  while (length(nodes_vec) > 0) {
    
    if (!(nodes_vec[1] %in% visited)){
      
      nodes_vec = union(nodes_vec, get_successors(graph, nodes_vec[1]))
      
      visited = union(visited, nodes_vec[1])
      
      nodes_vec = nodes_vec[-1]
      
    } else {nodes_vec = nodes_vec[-1]}
    
  }
  
  return(as.numeric(na.omit(visited)[-1]))
  
}

# all downstream - affects one node, affects the whole file
get_all_successors_by_file = function(graph, nodes_vec){
  
  visited = c()
  
  while (length(nodes_vec) > 0) {
    
    if (!(nodes_vec[1] %in% visited)){
      
      nodes_vec = union(nodes_vec, get_successors(graph, nodes_vec[1])) %>%
        union(nodes_by_file(graph, file_by_node(graph, nodes_vec[1])))
      
      visited = union(visited, nodes_vec[1])
      
      nodes_vec = nodes_vec[-1]
      
    } else {nodes_vec = nodes_vec[-1]}
    
  }
  
  return(na.omit(visited)[-1])
  
}

# given a node, get its file, then get all nodes

file_by_node = function(graph, node_vec) {
  get_node_attrs(graph, node_attr = File, nodes = node_vec)
}
nodes_by_file = function(graph, files) {
  graph$nodes_df %>%
    filter(File %in% files) %>%
    pull(ID)
}

get_my_nodes = function(graph, nodes_vec) {
  graph %>%
    select_nodes_by_id(nodes_vec) %>%
    get_node_df_ws() %>%
    select(ID, label, File, Team, Contact) %>%
    arrange(File, label)
}

get_input_nodes = function(graph, nodes_vec) {
  
  tryCatch({
  graph %>%
    select_nodes(conditions = id %in% nodes_vec) %>%
    trav_in_edge() %>%
    trav_out_node() %>%
    get_node_attrs_ws(node_attr = ID) %>%
    unname()},
  error = function(e){
    NA
  }
  )
}

get_all_upstream = function(graph, nodes_vec) {
  
  visited = c()
  
  while (length(nodes_vec) > 0) {
    
    if (!(nodes_vec[1] %in% visited)){
      
      nodes_vec = union(nodes_vec, get_input_nodes(graph, nodes_vec[1]))
      
      visited = union(visited, nodes_vec[1])
      
      nodes_vec = nodes_vec[-1]
      
    } else {nodes_vec = nodes_vec[-1]}
    
  }
  
  return(as.numeric(na.omit(visited)))
}




# Predetermining shapes/arrows based on criteria

shape_df = data.frame("file_type" = c("xl", "Database", "SAS", "Email"),
                      "n_shape" = c("tab", "cylinder", "rectangle", "folder"))
arrow_df = data.frame("load_type" = c("Query", "VBA", "manual", "Query.Manual", "Script"),
                      "a_shape" = c("diamond", "box", "icurve", "crow", "odiamond"))

# reading excel file
#setwd("C:/Users/TAG2054/OneDrive - The Toronto-Dominion Bank/Desktop/Shiny Apps")

## how to deal with same name tabs?

n_data = readxl::read_excel("Network File.xlsx", sheet = "Sheet1")

macro_data = n_data


# renaming

n_data = n_data %>%
  mutate(End_File = `End File or Database`,
         Input_File = `Input File or Database`,
         Team = `End File Team`,
         Contact = `End File Contact`,
         End_Tab = `End Tab Name or Script name`,
         Input_Tab = `Input Tab Name or Script name`,
         end_node = paste0(End_File, "~", End_Tab),
         input_node = paste0(Input_File, "~", Input_Tab))

nodes_end = n_data %>%
  select(End_Tab, End_File, Team, Contact, `End File Type`, end_node) %>%
  rename("node" = end_node,
         "node_label" = End_Tab,
         "File" = End_File,
         "file_type" = `End File Type`) %>%
  group_by(node) %>%
  slice_head(n=1) %>%
  ungroup()

nodes_start = n_data %>%
  select(Input_Tab, Input_File, `Input File Type`, input_node) %>%
  rename("node" = input_node,
         "node_label" = Input_Tab,
         "File" = Input_File,
         "file_type" = `Input File Type`) %>%
  mutate(Team = NA,
         Contact = NA) %>%
  group_by(node) %>%
  slice_head(n=1) %>%
  ungroup()

# Taking all distinct files based on connections
# Team/Contact info is only recorded for the destination file for accuracy purposes (can change later)

nodes = rbind(nodes_end, nodes_start) %>%
  group_by(node) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  mutate(ID = row_number(),
         info = paste0("Team: ", Team, "\n", "Contact: ", Contact, "\n"), # tooltip info
         Team = if_else(is.na(Team), "Not Identified", Team),
         cluster = File) %>% # groupings via cluster
  left_join(shape_df, by = "file_type") %>% # adding shape 
  mutate(n_shape = if_else(is.na(n_shape), "circle", n_shape))

join_table = nodes %>%
  select(node, ID)

# Setting random color for cluster fill

c_options = colors() 
files = nodes %>%
  pull(File) %>%
  unique()

files = nodes %>%
  select(File) %>%
  distinct() %>%
  mutate(file_color = sample(c_options, length(files)))

nodes = nodes %>%
  left_join(files, by = "File") # adding color

# getting connections 

links = n_data %>%
  left_join(join_table, by = c("end_node" = "node")) %>% # getting info for description
  left_join(join_table, by = c("input_node" = "node")) %>% # getting info for description
  mutate(end_tab = ID.x,
         input_tab = ID.y,
         load_type = `Load Type`,
         description = paste("VIA", load_type)) %>% # tabs for excel or file name otherwise
  #select(end_file, input_file, load_type, description, Notes, Team , `Input Tab Name or Script name`, `End Tab Name or Script name`, `Source File Team`, `Source File Contact`) %>%
  as.data.frame() %>%
  left_join(arrow_df, by = "load_type") %>% # arrow shape
  mutate(a_shape = if_else(is.na(a_shape), "vee", a_shape)) %>%
  left_join(files, by = c("Input_File" = "File"))


flow = create_graph() %>%
  add_nodes_from_table(nodes, label_col = node_label) %>% # nodes based on file name
  add_edges_from_table(links, 
                       from_col = input_tab,
                       to_col = end_tab,
                       from_to_map = ID) %>% # connections from link df
  set_node_attrs(node_attr = shape, values = nodes$n_shape) %>%
  set_node_attrs(node_attr = tooltip, values = nodes$info) %>%
  set_node_attrs(node_attr = fillcolor, values = nodes$file_color) %>%
  set_node_attrs(node_attr = fontcolor, values = "black") %>%
  set_node_attrs(node_attr = fixedsize, values = FALSE) %>%
  set_edge_attrs(edge_attr = tooltip, values = links$description) %>%
  set_edge_attrs(edge_attr = arrowhead, values = links$a_shape) %>%
  set_edge_attrs(edge_attr = penwidth, values = 2) %>%
  set_edge_attrs(edge_attr = color, values = links$file_color) %>%
  add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") # allows for clustering

# render_graph(flow)

all_reports = nodes %>%
  pull(File) %>%
  unique()

all_teams = nodes %>%
  pull(Team) %>%
  unique()

tab_given_report = function(graph, file) {
  x = graph$nodes_df %>%
    filter(File %in% file) %>%
    select(label, ID)
  out = x %>%
    pull(ID)
  names(out) = x %>% 
    pull(label)
  out
}
  
# render_graph(flow)
 
###################### macro view ####################
 
 
 macro_data = macro_data %>%
   mutate(End_File = `End File or Database`,
          Input_File = `Input File or Database`,
          Team = `End File Team`,
          Contact = `End File Contact`)
 
 macro_nodes_end = macro_data %>%
   select(End_File, `End File Type`, Team, Contact) %>%
   rename("file_type" = `End File Type`,
          "File" = End_File) %>%
   group_by(File) %>%
   slice_head(n=1) %>%
   ungroup()
 
 macro_nodes_start = macro_data %>%
   select(Input_File, `Input File Type`) %>%
   rename("file_type" = `Input File Type`,
          "File" = Input_File) %>%
   mutate(Team = NA,
          Contact = NA) %>%
   group_by(File) %>%
   slice_head(n=1) %>%
   ungroup()
 
 # Taking all distinct files based on connections
 # Team/Contact info is only recorded for the destination file for accuracy purposes (can change later)
 
 macro_nodes = rbind(macro_nodes_end, macro_nodes_start) %>%
   group_by(File) %>%
   slice_head(n=1) %>%
   ungroup() %>%
   mutate(ID = row_number(),
          info = paste0("Team: ", Team, "\n", "Contact: ", Contact, "\n"), # tooltip info
          Team = if_else(is.na(Team), "Not Identified", Team),
          cluster = Team) %>% # groupings via cluster
   left_join(shape_df, by = "file_type") %>% # adding shape 
   mutate(n_shape = if_else(is.na(n_shape), "circle", n_shape))
 
 # Setting random color for cluster fill
 
 macro_c_options = colors() 
 macro_teams = macro_nodes %>%
   pull(Team) %>%
   unique()
 
 macro_teams = macro_nodes %>%
   select(Team) %>%
   distinct() %>%
   mutate(team_color = sample(macro_c_options, length(macro_teams)))
 
 macro_nodes = macro_nodes %>%
   left_join(macro_teams, by = "Team") # adding color
 
 # getting connections 
 
 macro_links = macro_data %>%
   left_join(macro_nodes, by = c("End_File" = "File")) %>% # getting info for description
   left_join(macro_nodes, by = c("Input_File" = "File")) %>% # getting info for description
   mutate(end_file = ID.x, 
          input_file = ID.y,
          load_type = `Load Type`,
          description = paste(`Input Tab Name or Script name`, "TO", `End Tab Name or Script name`, "VIA", load_type)) %>% # tabs for excel or file name otherwise
   select(end_file, input_file, load_type, description, Notes, Team , `Input Tab Name or Script name`, `End Tab Name or Script name`, `Source File Team`, `Source File Contact`) %>%
   as.data.frame() %>%
   left_join(arrow_df, by = "load_type") %>% # arrow shape
   mutate(a_shape = if_else(is.na(a_shape), "vee", a_shape)) %>%
   left_join(macro_teams, by = "Team")
 
 
 macro_flow = create_graph() %>%
   add_nodes_from_table(macro_nodes, label_col = File, type_col = Team) %>% # nodes based on file name
   add_edges_from_table(macro_links, 
                        from_col = input_file,
                        to_col = end_file,
                        from_to_map = ID) %>% # connections from link df
   set_node_attrs(node_attr = shape, values = macro_nodes$n_shape) %>%
   set_node_attrs(node_attr = tooltip, values = macro_nodes$info) %>%
   set_node_attrs(node_attr = fillcolor, values = macro_nodes$team_color) %>%
   set_node_attrs(node_attr = fontcolor, values = "black") %>%
   set_node_attrs(node_attr = fixedsize, values = FALSE) %>%
   set_edge_attrs(edge_attr = tooltip, values = macro_links$description) %>%
   set_edge_attrs(edge_attr = arrowhead, values = macro_links$a_shape) %>%
   set_edge_attrs(edge_attr = penwidth, values = 2) %>%
   set_edge_attrs(edge_attr = color, values = macro_links$team_color) %>%
   add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") # allows for clustering
 

 
# render_graph(macro_flow)

# # all inputs for node 6 (LTCFF)
# flow %>%
#   select_nodes(conditions = id == 6) %>%
#   trav_in_edge() %>%
#   trav_out_node() %>%
#   get_node_attrs_ws(node_attr = label)
# 
# 
# # all potential impacts from changing Retail Level
# flow %>%
#   get_all_successors(nodes_vec = 3)
# 
# # connections into node 2 (CLM)
# flow %>%
#   select_nodes(conditions = id == 2) %>%
#   trav_in_until(max_steps = 1) %>%
#   get_node_attrs_ws(node_attr = label)
# 
# # all connections where manual data transfer occurs
# flow %>%
#   select_edges(conditions = grepl("manual", load_type)) %>%
#   get_edge_attrs_ws(edge_attr = description)
# 
# # count the feedback loops
# 
# flow %>%
#   count_mutual_node_pairs()
# 
# # graph of all 1 off connections to CLM
# 
# flow %>%
#   DiagrammeR::select_nodes_in_neighborhood(node = 2, distance = 1) %>%
#   #set_edge_attrs_ws(edge_attr = decorate, value = TRUE) %>%
#   transform_to_subgraph_ws() %>%
#   render_graph()

# want all direct impacts from node
#given a node: 12 RRDW
# direct_impact = flow %>%
#   get_all_successors(17) %>%
#   append(17)
# flow %>%
#   select_nodes_by_id(direct_impact) %>%
#   transform_to_subgraph_ws() %>%
#   render_graph()

# results impact given a file
# results_impact = get_all_successors_by_file(flow, 1)
# 
# results_impact = flow %>%
#   select_nodes_by_id(results_impact) %>%
#   transform_to_subgraph_ws()
# 
# results_impact %>%
#   render_graph()
# 
# results_impact %>%
#   get_node_df() %>%
#   select(ID, File, label, Contact, Team) %>%
#   arrange(File)
#     
  


ui = navbarPage("TBSM Data Flow Network",
                tabPanel("User Selections",
                         selectInput("report", label = "What Process would you like to look at?", choices = all_reports, multiple = TRUE),
                         selectInput("tab", label = "What part of the process would you like to look at?", choices = NULL , multiple = TRUE)),
                tabPanel("Process Inputs",
                         tableOutput("inputs_table"),
                         grVizOutput("network", width = "100%")),
                tabPanel("Part of Process Direct Downstream Impacts",
                         tableOutput("direct_impact_table"),
                         grVizOutput("network_di", width = "100%")),
                tabPanel("Process All Possible Downstream Impacts from Changes",
                         tableOutput("all_downstream_table"),
                         grVizOutput("network_ad", width = "100%")),
                tabPanel("Entire Network",
                         grVizOutput("network2", width = "100%")))

server = function(input, output, session) {
  
  observeEvent(input$report, {
    x = 0
    names(x) = "Entire Process"
    tab_choices = c(tab_given_report(flow, input$report), x)
    updateSelectInput(session, "tab", choices = tab_choices)
  })
  
# Process Inputs
  output$inputs_table = renderTable({
    
    if (0 %in% input$tab){
      
      all_nodes = nodes_by_file(flow, input$report)
      
      all_nodes = union(all_nodes, get_input_nodes(flow, all_nodes))
      
      get_my_nodes(flow, all_nodes)} else {
        
          all_nodes = union(input$tab, get_input_nodes(flow, input$tab))
          
          get_my_nodes(flow, all_nodes)
        }
    }) 
  
  output$network = renderGrViz({
    
    if (0 %in% input$tab){
      
      all_nodes = nodes_by_file(flow, input$report)
      
      all_nodes = get_input_nodes(flow, all_nodes) %>%
        union(all_nodes) 
      
      flow %>%
        select_nodes_by_id(all_nodes) %>%
        transform_to_subgraph_ws() %>%
        render_graph()} else {
          
          all_nodes = input$tab
          
          all_nodes = union(all_nodes, get_input_nodes(flow, all_nodes)) 
          
          flow %>%
            select_nodes_by_id(all_nodes) %>%
            transform_to_subgraph_ws() %>%
            render_graph()
        }
  })
  
# Part of Process Direct Down Stream Impacts
  output$direct_impact_table = renderTable({
    
    if (0 %in% input$tab){
      
      all_nodes = nodes_by_file(flow, input$report)
      
      all_nodes = union(all_nodes, get_direct_impact(flow, all_nodes))
      
      get_my_nodes(flow, all_nodes)} else {
        
          all_nodes = union(input$tab, get_direct_impact(flow, input$tab))
          
          get_my_nodes(flow, all_nodes)
        }
  }) 
  
  output$network_di = renderGrViz({
    
    if (0 %in% input$tab){
      
      all_nodes = nodes_by_file(flow, input$report)
      
      all_nodes = union(all_nodes, get_direct_impact(flow, all_nodes))
      
      flow %>%
        select_nodes_by_id(all_nodes) %>%
        transform_to_subgraph_ws() %>%
        render_graph()} else {
          
          all_nodes = input$tab
          all_nodes = union(all_nodes, get_direct_impact(flow, all_nodes))
          
          flow %>%
            select_nodes_by_id(all_nodes) %>%
            transform_to_subgraph_ws() %>%
            render_graph()
        }
  })
  
# Process All Possible Downstream Impacts from Changes
  output$all_downstream_table = renderTable({
    
    if (0 %in% input$tab){
      
      all_nodes = nodes_by_file(flow, input$report)
      
      all_nodes = union(all_nodes, get_all_successors_by_file(flow, all_nodes))
      
      get_my_nodes(flow, all_nodes)} else {
        
      all_nodes = union(input$tab, get_all_successors_by_file(flow, input$tab))
      
      get_my_nodes(flow, all_nodes)
    }
  }) 
  
  output$network_ad = renderGrViz({
    
    if (0 %in% input$tab){
      
      all_nodes = nodes_by_file(flow, input$report)
      
      all_nodes = get_all_successors_by_file(flow, all_nodes) %>%
        union(all_nodes) 
      
      flow %>%
        select_nodes_by_id(all_nodes) %>%
        transform_to_subgraph_ws() %>%
        render_graph()} else {
          
          all_nodes = input$tab
          all_nodes = union(all_nodes, get_all_successors_by_file(flow, all_nodes))
          
          flow %>%
            select_nodes_by_id(all_nodes) %>%
            transform_to_subgraph_ws() %>%
            render_graph()
        }
  })
  
# whole network
  output$network2 = renderGrViz({
    
    macro_flow %>%
      render_graph()
    
  })
  
}

shinyApp(ui,server)

