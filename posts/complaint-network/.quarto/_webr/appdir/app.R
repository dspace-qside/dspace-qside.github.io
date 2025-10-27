library(shiny)

#completely clean workspace for independent run
rm(list = ls())
#load necessary file for network analysis
load(file = "data/network_files.Rdata")

###
### Code for Shiny widget
###
# Define UI
ui <- fluidPage(
  tags$head(tags$style(HTML(".shiny-input-container{margin-bottom:6px;} hr{margin-top:6px;margin-bottom:6px;}"))),
  titlePanel("Interactive Network Visualization of MPD Co-Complaint Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_node", 
                  "Choose or type the name of an officer to highlight in the network:",
                  choices = c("No officer selected" = "none"),  # Start with None option
                  selected = "none",
                  selectize = TRUE),
      
      hr(),
      
      selectInput("color_option", 
                  "Color officers/nodes by:",
                  choices = c("Default" = "none",
                              "Rank" = "rank", 
                              "Betweenness Centrality" = "betw_central"),
                  selected = "none"),
      
      hr(),
      
      conditionalPanel(
        condition = "input.color_option != 'none'",
        h5("Legend"),
        plotOutput("legend_plot", width = "100%", height = "300px")
      )
    ),
    
    mainPanel(
      plotlyOutput("network_plot", width = "100%", height = "550px")
    )
  ),
  
  # Density plot panel below everything
  conditionalPanel(
    condition = "input.selected_node != 'none' && input.selected_node != null",
    fluidRow(
      column(12,
             plotOutput("density_plot", width = "100%", height = "300px")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Default values for colors and sizes
  default_color <- "#ADD8E6"
  highlight_size <- 20
  default_size <- 10
  
  # Build network with name and rank attributes
  shiny_network <- reactive({
    g <- current_network
    V(g)$name <- nodes$names
    V(g)$rank <- nodes$JOBTITLE
    V(g)$betw_central <- nodes$betw_vals
    return(g)
  })
  
  # Set fixed layout
  network_layout <- reactive({
    req(shiny_network())
    layout_with_fr(shiny_network())
  })
  
  display_name_map <- reactive({
    req(shiny_network())
    internal <- V(shiny_network())$name
    display <- str_to_title(internal)
    names(internal) <- display
    internal
  })
  
  # Update dropdown list of node names (capitalized & sorted)
  observe({
    req(shiny_network())
    
    node_names <- V(shiny_network())$name
    node_names_display <- node_names %>%
      tolower() %>%
      str_to_title() %>%
      sort()
    
    # Create choices with None option always at top
    choices_list <- c("No officer selected" = "none")
    for(name in node_names_display) {
      choices_list[name] <- name
    }
    
    updateSelectInput(session, "selected_node",
                      choices = choices_list,
                      selected = input$selected_node)
  })
  
  # Main network plot
  output$network_plot <- renderPlotly({
    req(shiny_network(), display_name_map())
    
    g <- shiny_network()
    layout <- network_layout()
    
    vertex.size <- rep(default_size, vcount(g))
    vertex.col <- rep(default_color, vcount(g))
    
    main_title <- "Co-Complaint Network of the Minneapolis Police Department"
    legend_data <- NULL
    
    # Color by selected option
    if (input$color_option == "rank") {
      ranks <- V(g)$rank
      rank_levels <- unique(ranks)
      # Use Set1 palette for distinct, nominal colors
      pal <- RColorBrewer::brewer.pal(min(length(rank_levels), 9), "Set1")
      # If more than 9 ranks, supplement with additional colors
      if (length(rank_levels) > 9) {
        additional_colors <- rainbow(length(rank_levels) - 9)
        pal <- c(pal, additional_colors)
      }
      names(pal) <- rank_levels
      
      vertex.col <- pal[as.character(ranks)]
      main_title <- "Network Colored by Officer Rank"
      legend_data <- list(title = "Officer Rank", levels = rank_levels, 
                          colors = pal[rank_levels])
      
    } else if (input$color_option == "betw_central") {
      betw_vals <- V(g)$betw_central
      
      # Create truly continuous color scale
      color_palette <- colorRampPalette(c("lightblue", "yellow", "red"))(1000)
      
      # Map each betweenness value to a color
      min_val <- min(betw_vals, na.rm = TRUE)
      max_val <- max(betw_vals, na.rm = TRUE)
      
      if (max_val > min_val) {
        # Scale values to 1-1000 range for color mapping
        scaled_vals <- round((betw_vals - min_val) / (max_val - min_val) * 999) + 1
        vertex.col <- color_palette[scaled_vals]
      } else {
        # All values are the same
        vertex.col <- rep("lightblue", vcount(g))
      }
      
      main_title <- "Network Colored by Betweenness Centrality"
      
      # Continuous legend with actual values
      if (max_val > min_val) {
        legend_vals <- round(seq(min_val, max_val, length.out = 10), 3)
        legend_colors <- colorRampPalette(c("lightblue", "yellow", "red"))(10)
        legend_data <- list(title = "Betweenness\nCentrality", 
                            levels = legend_vals, 
                            colors = legend_colors)
      } else {
        legend_data <- list(title = "Betweenness\nCentrality", 
                            levels = min_val, 
                            colors = "lightblue")
      }
    }
    
    # If a node is selected, override its color/size
    if (!is.null(input$selected_node) && input$selected_node != "none" && 
        input$selected_node %in% names(display_name_map())) {
      selected_internal_name <- display_name_map()[[input$selected_node]]
      selected_index <- which(V(g)$name == selected_internal_name)
      
      vertex.size[selected_index] <- highlight_size
      
      selected_rank <- V(g)$rank[selected_index]
      main_title <- paste0("Highlighting: ", input$selected_node,
                           " (Rank: ", selected_rank, ")")
    }
    
    # Set node shapes and outline width
    vertex.symbol <- rep("circle", vcount(g))
    marker.line.width <- rep(1, vcount(g))
    if (!is.null(input$selected_node) && input$selected_node != "none" &&
        input$selected_node %in% names(display_name_map())) {
      selected_internal_name <- display_name_map()[[input$selected_node]]
      selected_index <- which(V(g)$name == selected_internal_name)
      vertex.symbol[selected_index] <- "star"
      marker.line.width[selected_index] <- 3
    }
    
    # Create the plot
    edge_list <- igraph::as_edgelist(g, names = FALSE)
    p <- plot_ly(source = "net")
    if (nrow(edge_list) > 0) {
      p <- add_segments(p,
                        x = layout[edge_list[,1],1], 
                        y = layout[edge_list[,1],2],
                        xend = layout[edge_list[,2],1], 
                        yend = layout[edge_list[,2],2],
                        line = list(color = "gray", width = 0.5), 
                        hoverinfo = "none", showlegend = FALSE)
    }
    display_names <- str_to_title(V(g)$name)
    hover_text <- paste(display_names, "\nRank:", V(g)$rank)
    sel_idx <- which(vertex.symbol == "star")
    unsel_idx <- setdiff(seq_len(vcount(g)), sel_idx)
    
    # Unselected nodes
    p <- add_markers(p,
                     x = layout[unsel_idx,1], y = layout[unsel_idx,2],
                     marker = list(color = vertex.col[unsel_idx], 
                                   size = vertex.size[unsel_idx], 
                                   opacity = 1, line = 
                                     list(color = "black", 
                                          width = marker.line.width[unsel_idx]),
                                   symbol = vertex.symbol[unsel_idx]),
                     text = hover_text[unsel_idx], hoverinfo = "text",
                     showlegend = FALSE,
                     customdata = display_names[unsel_idx])
    
    # Selected node on top
    if (length(sel_idx) == 1) {
      p <- add_markers(p,
                       x = layout[sel_idx,1], y = layout[sel_idx,2],
                       marker = list(color = vertex.col[sel_idx], 
                                     size = vertex.size[sel_idx], 
                                     opacity = 1, 
                                     line = 
                                       list(color = "black", 
                                            width = marker.line.width[sel_idx]), 
                                     symbol = vertex.symbol[sel_idx]),
                       text = hover_text[sel_idx], hoverinfo = "text", 
                       showlegend = FALSE,
                       customdata = display_names[sel_idx])
    }
              p <- plotly::layout(p,
                        title = main_title,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                     showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                     showticklabels = FALSE),
                        margin = list(t = 60))
      p <- plotly::config(p, displayModeBar = FALSE)
      p
    })
    
    # Click-to-select
    observeEvent(event_data("plotly_click", source = "net"), {
      d <- event_data("plotly_click", source = "net")
      if (!is.null(d) && NROW(d) > 0 && !is.null(d$customdata)) {
        clicked_name <- as.character(d$customdata[[1]])
        updateSelectInput(session, "selected_node", selected = clicked_name)
      }
    })
    
    # Separate legend plot
  output$legend_plot <- renderPlot({
    req(shiny_network(), display_name_map())
    
    g <- shiny_network()
    legend_data <- NULL
    
    # Determine what legend to show
    if (input$color_option == "rank") {
      ranks <- V(g)$rank
      rank_levels <- unique(ranks)
      pal <- RColorBrewer::brewer.pal(min(length(rank_levels), 9), "Set1")
      if (length(rank_levels) > 9) {
        additional_colors <- rainbow(length(rank_levels) - 9)
        pal <- c(pal, additional_colors)
      }
      names(pal) <- rank_levels
      legend_data <- list(title = "Officer Rank", levels = rank_levels, 
                          colors = pal[rank_levels])
      
    } else if (input$color_option == "betw_central") {
      betw_vals <- V(g)$betw_central
      min_val <- min(betw_vals, na.rm = TRUE)
      max_val <- max(betw_vals, na.rm = TRUE)
      
      if (max_val > min_val) {
        legend_vals <- round(seq(min_val, max_val, length.out = 10), 3)
        legend_colors <- colorRampPalette(c("lightblue", "yellow", "red"))(10)
        legend_data <- list(title = "Betweenness\nCentrality", 
                            levels = legend_vals, 
                            colors = legend_colors)
      }
    }
    
    # Create legend plot
    if (!is.null(legend_data)) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      legend("center",
             legend = legend_data$levels,
             fill = legend_data$colors,
             border = NA,
             bty = "n",
             title = legend_data$title,
             cex = 1.2,
             title.cex = 1.3)
    }
  })
  
  # Density plot for betweenness centrality
  output$density_plot <- renderPlot({
    req(shiny_network(), display_name_map(), input$selected_node, 
        input$selected_node != "")
    
    g <- shiny_network()
    betw_vals <- V(g)$betw_central
    
    # Get selected node's betweenness centrality
    selected_internal_name <- display_name_map()[[input$selected_node]]
    selected_index <- which(V(g)$name == selected_internal_name)
    selected_betw <- betw_vals[selected_index]
    
    # Create data frame for ggplot
    df <- data.frame(betweenness = betw_vals)
    
    # Create density plot
    p <- ggplot(df, aes(x = betweenness)) +
      geom_density(fill = "lightblue", alpha = 0.7, color = "blue", trim = TRUE) +
      geom_vline(xintercept = selected_betw, color = "red", 
                 linetype = "dashed", linewidth = 1) +
      annotate("segment", 
               x = selected_betw, 
               xend = selected_betw, 
               y = 0, 
               yend = 0.1,
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "red", 
               linewidth = 1.2) +
      annotate("text", 
               x = selected_betw, 
               y = 62.5, 
               label = paste(input$selected_node, "\n(", 
                             round(selected_betw, 3), ")"), 
               color = "red", 
               hjust = 0.5,
               vjust = 0,
               fontface = "bold",
               size = 5) +
      labs(title = "Distribution of Betweenness Centrality",
           x = "Betweenness Centrality",
           y = "Density") +
      xlim(c(-0.005, max(nodes$betw_vals))) +
      geom_vline(xintercept=0) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
    
    print(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server, options = list(height = "950px"))
