
library(shiny)
library(tidyverse)
library(igraph)
library(ggraph)
library(here)
library(dplyr)
library(RColorBrewer)
library(readxl)
library(stringdist)
library(viridis)
library(shiny)
library(stringr)
library(ggplot2)
library(plotly)

server <- function(input, output, session) {
  
  load(file = "network_files.Rdata")
  
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
    set.seed(4)
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
  
  # Create sub-network for selected officer
  subnetwork_data <- reactive({
    req(shiny_network(), display_name_map(), input$selected_node,
        input$selected_node != "none")
    
    g <- shiny_network()
    selected_internal_name <- display_name_map()[[input$selected_node]]
    selected_index <- which(V(g)$name == selected_internal_name)
    
    # Get neighbors of selected node
    neighbors <- neighbors(g, selected_index)
    
    if (length(neighbors) == 0) {
      return(NULL)  # No connections
    }
    
    # Include selected node and its neighbors
    subgraph_nodes <- c(selected_index, neighbors)
    subgraph <- induced_subgraph(g, subgraph_nodes)
    
    return(list(graph = subgraph, 
                selected_in_sub = which(V(subgraph)$name ==
                                          selected_internal_name)))
  })
  
  # Main network plot
  output$network_plot <- renderPlotly({
    req(shiny_network(), display_name_map())
    
    g <- shiny_network()
    layout <- network_layout()
    
    vertex.size <- rep(default_size, vcount(g))
    vertex.col <- rep(default_color, vcount(g))
    
    main_title <- "Co-Complaint Network"
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
        scaled_vals <- round((betw_vals - min_val) / 
                               (max_val - min_val) * 999) + 1
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
      # main_title <- paste0(input$selected_node, " (Rank: ", selected_rank, ")")
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
                        # title = main_title,
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
  
  # Sub-network title
  output$subnetwork_title <- renderText({
    req(input$selected_node, input$selected_node != "none")
    paste("Highlighted Officer: ", input$selected_node)
  })
  
  # Officer summary statistics
  output$officer_summary <- renderTable({
    req(shiny_network(), display_name_map(), input$selected_node,
        input$selected_node != "none")
    
    g <- shiny_network()
    selected_internal_name <- display_name_map()[[input$selected_node]]
    selected_index <- which(V(g)$name == selected_internal_name)
    
    # Get connected officers
    neighbors_idx <- neighbors(g, selected_index)
    connected_officers <- V(g)$name[neighbors_idx]
    connected_officers_display <- str_to_title(connected_officers)
    
    # Calculate complaint statistics
    selected_complaints <- final_complaints[final_complaints$format_name ==
                                              selected_internal_name, ]
    selected_complaint_ids <- unique(selected_complaints$complaint_id)
    
    # Create comma-separated list of connected officers
    connected_list <- paste(connected_officers_display, collapse = ", ")
    
    # Summary statistics for selected officer
    summary_stats <- data.frame(
      Statistic = c("<b>Total Number of Complaints</b>",
                    paste0("<b>Connected to ", length(connected_officers), " Officers</b>")),
      Value = c(length(selected_complaint_ids),
                connected_list),
      stringsAsFactors = FALSE
    )
    
    return(summary_stats)
  }, 
  colnames = FALSE, 
  rownames = FALSE,
  spacing = "xs",
  sanitize.text.function = function(x) x)
  
  # Sub-network plot
  output$subnetwork_plot <- renderPlotly({
    req(subnetwork_data())
    
    subdata <- subnetwork_data()
    if (is.null(subdata)) {
      # Create empty plotly plot with message
      p <- plot_ly() %>%
        add_annotations(
          text = "Selected officer has no connections in the network",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16, color = "gray")
        ) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                       showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                       showticklabels = FALSE)
        )
      return(p)
    }
    
    sub_g <- subdata$graph
    selected_in_sub <- subdata$selected_in_sub
    
    # Create layout for sub-network (more compact)
    sub_layout <- layout_with_fr(sub_g)
    
    # Scale down the layout to make it more compact
    layout_range <- apply(sub_layout, 2, range)
    layout_center <- apply(sub_layout, 2, mean)
    
    # Compress layout more (25% of original size) to leave room for text labels
    sub_layout[,1] <- (sub_layout[,1] - layout_center[1]) * 0.25 +
      layout_center[1]
    sub_layout[,2] <- (sub_layout[,2] - layout_center[2]) * 0.25 +
      layout_center[2]
    
    # Apply the same coloring logic as main network
    vertex.col <- rep(default_color, vcount(sub_g))
    
    # Color by selected option (same as main network)
    if (input$color_option == "rank") {
      ranks <- V(sub_g)$rank
      rank_levels <- 
        unique(V(shiny_network())$rank)  # Use full network rank levels
      # Use Set1 palette for distinct, nominal colors
      pal <- RColorBrewer::brewer.pal(min(length(rank_levels), 9), "Set1")
      # If more than 9 ranks, supplement with additional colors
      if (length(rank_levels) > 9) {
        additional_colors <- rainbow(length(rank_levels) - 9)
        pal <- c(pal, additional_colors)
      }
      names(pal) <- rank_levels
      
      vertex.col <- pal[as.character(ranks)]
      
    } else if (input$color_option == "betw_central") {
      betw_vals <- V(sub_g)$betw_central
      all_betw_vals <- 
        V(shiny_network())$betw_central  # Use full network for scale
      
      # Create truly continuous color scale (same as main network)
      color_palette <- colorRampPalette(c("lightblue", "yellow", "red"))(1000)
      
      # Map each betweenness value to a color using full network scale
      min_val <- min(all_betw_vals, na.rm = TRUE)
      max_val <- max(all_betw_vals, na.rm = TRUE)
      
      if (max_val > min_val) {
        # Scale values to 1-1000 range for color mapping
        scaled_vals <- round((betw_vals - min_val) / 
                               (max_val - min_val) * 999) + 1
        vertex.col <- color_palette[scaled_vals]
      } else {
        # All values are the same
        vertex.col <- rep("lightblue", vcount(sub_g))
      }
    }
    
    vertex.size <- rep(12, vcount(sub_g))
    vertex.size[selected_in_sub] <- 20           # Larger for selected officer
    
    # Create hover text with names, ranks, and complaint info
    display_names <- str_to_title(V(sub_g)$name)
    hover_text <- character(vcount(sub_g))
    
    for (i in 1:vcount(sub_g)) {
      officer_name <- V(sub_g)$name[i]
      officer_display <- display_names[i]
      officer_rank <- V(sub_g)$rank[i]
      
      if (i == selected_in_sub) {
        # Selected officer - just show name and rank
        hover_text[i] <- paste(officer_display, "\nRank:", officer_rank)
      } else {
        # Connected officers - show name, rank, and complaint info
        officer_complaints <- final_complaints[final_complaints$format_name ==
                                                 officer_name, ]
        officer_complaint_ids <- unique(officer_complaints$complaint_id)
        
        selected_internal_name <- display_name_map()[[input$selected_node]]
        selected_complaints <- final_complaints[final_complaints$format_name ==
                                                  selected_internal_name, ]
        selected_complaint_ids <- unique(selected_complaints$complaint_id)
        
        shared_complaints <- length(intersect(selected_complaint_ids,
                                              officer_complaint_ids))
        total_complaints <- length(officer_complaint_ids)
        
        hover_text[i] <- paste0(officer_display, 
                                "\nRank: ", officer_rank,
                                "\n", shared_complaints, " shared complaint", 
                                ifelse(shared_complaints != 1, "s", ""),
                                " (", total_complaints, " total)")
      }
    }
    
    # Set node shapes
    vertex.symbol <- rep("circle", vcount(sub_g))
    vertex.symbol[selected_in_sub] <- "star"
    marker.line.width <- rep(1, vcount(sub_g))
    marker.line.width[selected_in_sub] <- 3
    
    # Create the plotly plot
    edge_list <- igraph::as_edgelist(sub_g, names = FALSE)
    p <- plot_ly(source = "subnet")
    
    # Add edges if they exist
    if (nrow(edge_list) > 0) {
      p <- add_segments(p,
                        x = sub_layout[edge_list[,1],1], 
                        y = sub_layout[edge_list[,1],2],
                        xend = sub_layout[edge_list[,2],1], 
                        yend = sub_layout[edge_list[,2],2],
                        line = list(color = "gray", width = 2), 
                        hoverinfo = "none", showlegend = FALSE)
    }
    
    # Add all nodes with hover info
    p <- add_markers(p,
                     x = sub_layout[,1], y = sub_layout[,2],
                     marker = list(color = vertex.col, 
                                   size = vertex.size, 
                                   opacity = 1, 
                                   line = list(color = "black", 
                                               width = marker.line.width),
                                   symbol = vertex.symbol),
                     text = hover_text, 
                     hoverinfo = "text",
                     showlegend = FALSE)
    
    # Add text labels for all nodes (positioned more carefully)
    # Create labels with complaint info
    label_text <- character(vcount(sub_g))
    for (i in 1:vcount(sub_g)) {
      officer_name <- V(sub_g)$name[i]
      officer_display <- display_names[i]
      
      if (i == selected_in_sub) {
        # Selected officer - just show name
        label_text[i] <- officer_display
      } else {
        # Connected officers - show name and complaint info
        officer_complaints <- final_complaints[final_complaints$format_name ==
                                                 officer_name, ]
        officer_complaint_ids <- unique(officer_complaints$complaint_id)
        
        selected_internal_name <- display_name_map()[[input$selected_node]]
        selected_complaints <- final_complaints[final_complaints$format_name ==
                                                  selected_internal_name, ]
        selected_complaint_ids <- unique(selected_complaints$complaint_id)
        
        shared_complaints <- length(intersect(selected_complaint_ids,
                                              officer_complaint_ids))
        total_complaints <- length(officer_complaint_ids)
        
        label_text[i] <- paste0(officer_display, "\n")
      }
    }
    
    p <- add_text(p,
                  x = sub_layout[,1] + 0.05, 
                  y = sub_layout[,2],
                  text = label_text,
                  textposition = "middle right",
                  textfont = list(size = 9, color = "black"),
                  showlegend = FALSE,
                  hoverinfo = "none")
    
    # Calculate layout boundaries for the smaller network
    x_range <- range(sub_layout[,1])
    y_range <- range(sub_layout[,2])
    
    # Add generous padding to ensure text is visible
    x_padding <- diff(x_range) * 2  # Large padding on right for text
    y_padding <- diff(y_range) * 0.5
    
    p <- layout(p,
                title = "",
                xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                             showticklabels = FALSE,
                             range = c(x_range[1] - x_padding * 0.2, 
                                       x_range[2] + x_padding)),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, 
                             showticklabels = FALSE,
                             range = c(y_range[1] - y_padding,
                                       y_range[2] + y_padding)),
                # margin = list(t = 40, l = 20, r = 10, b = 20))
                margin = list(t = 0, l = 0, r = 0, b = 0))
    
    p <- config(p, displayModeBar = FALSE)
    p
  })
  
  # Horizontal legend plot
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
      
      # Create horizontal legend for rank - wrap to multiple rows if needed
      par(mar = c(0.5, 0.5, 1.5, 0.5))
      plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
           axes = FALSE, xlab = "", ylab = "")
      
      # Calculate number of columns to fit within plot area
      n_items <- length(rank_levels)
      ncol_legend <- ceiling(n_items / 2)  # Maximum 2 rows
      
      legend("center",
             legend = rank_levels,
             fill = pal[rank_levels],
             border = NA,
             bty = "n",
             title = "Officer Rank",
             horiz = FALSE,
             cex = 0.75,
             title.cex = 0.9,
             ncol = ncol_legend,
             x.intersp = 0.3,
             text.width = max(strwidth(rank_levels, cex = 0.75)) * 1.1,
             xpd = FALSE)
      
    } else if (input$color_option == "betw_central") {
      betw_vals <- V(g)$betw_central
      min_val <- min(betw_vals, na.rm = TRUE)
      max_val <- max(betw_vals, na.rm = TRUE)
      
      if (max_val > min_val) {
        # Create horizontal color bar for continuous scale (left to right)
        par(mar = c(3, 6, 2, 6))
        
        # Create color gradient
        legend_colors <- colorRampPalette(c("lightblue", "yellow", "red"))(100)
        
        # Create horizontal bar using image (transpose for left-to-right)
        image(x = seq(min_val, max_val, length.out = 100),
              y = 0:1,
              z = matrix(1:100, nrow = 100, ncol = 1),
              col = legend_colors,
              axes = FALSE,
              xlab = "Betweenness Centrality",
              ylab = "",
              xlim = c(min_val, max_val),
              ylim = c(0, 1))
        
        # Add axis and title
        axis(1, at = seq(min_val, max_val, length.out = 5),
             labels = round(seq(min_val, max_val, length.out = 5), 3),
             cex.axis = 1.0)
        title(main = "Betweenness Centrality", cex.main = 1.1, line = 0.5)
        box()
      }
    }
  })
  
  # Histogram for betweenness centrality
  output$hist_betw_plot <- renderPlot({
    req(shiny_network(), display_name_map(), input$selected_node, 
        input$selected_node != "none")
    
    g <- shiny_network()
    betw_vals <- V(g)$betw_central
    
    # Get selected node's betweenness centrality
    selected_internal_name <- display_name_map()[[input$selected_node]]
    selected_index <- which(V(g)$name == selected_internal_name)
    selected_betw <- betw_vals[selected_index]
    
    # Create data frame for ggplot
    df <- data.frame(betweenness = betw_vals)
    
    # Create histogram of betweenness plot
    p <- ggplot(df, aes(x = betweenness)) +
      geom_histogram(fill = "lightblue", alpha = 0.7, color = "blue", 
                     trim = TRUE,
                     bins=100) +
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
      annotate("label", 
               x = selected_betw, 
               y = 62.5, 
               label = paste0(gsub(" ", "\n", input$selected_node), "\n(", round(selected_betw, 3), ")"),
               hjust = 0.5,
               vjust = 0,
               fontface = "bold",
               size = 4,
               label.size = 0.3) +
      labs(title = "Distribution of Betweenness Centrality",
           x = "Betweenness Centrality",
           y = "Count of Officers") +
      xlim(c(-0.005, max(nodes$betw_vals))) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12))
    
    print(p)
  })
}