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

ui <- fluidPage(
  tags$head(tags$style(HTML(".shiny-input-container{margin-bottom:6px;} hr{margin-top:6px;margin-bottom:6px;}"))),
  titlePanel("Interactive Network Visualization of MPD Co-Complaint Data"),
  
  # Dropdowns at the top
  fluidRow(
    column(6,
           selectInput("selected_node", 
                       "Choose or type the name of an officer:",
                       choices = c("No officer selected" = "none"),
                       selected = "none",
                       selectize = TRUE)
    ),
    column(6,
           selectInput("color_option", 
                       "Color officers/nodes by:",
                       choices = c("Default" = "none",
                                   "Rank" = "rank", 
                                   "Betweenness Centrality" = "betw_central",
                                   "Number of Complaints" = "num_complaints"),
                       selected = "none")
    )
  ),
  
  hr(),
  
  # Top row: Full network (left) and sub-network (right)
  fluidRow(
    column(6,
           h4("Full Co-Complaint Network"),
           plotlyOutput("network_plot", width = "100%", height = "500px")
    ),
    column(6,
           conditionalPanel(
             condition = "input.selected_node != 'none' && 
             input.selected_node != null",
             h4(textOutput("subnetwork_title")),
             plotlyOutput("subnetwork_plot", width = "100%", height = "500px")
           )
    )
  ),
  
  # Legend as horizontal color bar below networks
  conditionalPanel(
    condition = "input.color_option != 'none'",
    fluidRow(
      column(12,
             plotOutput("legend_plot", width = "100%", height = "80px")
      )
    )
  ),
  
  hr(),
  
  # Bottom row: Officer summary (left) and histogram (right)
  conditionalPanel(
    condition = "input.selected_node != 'none' && input.selected_node != null",
    fluidRow(
      column(6,
             # h4("Officer Summary"),
             tableOutput("officer_summary")
      ),
      column(6,
             plotOutput("hist_betw_plot", width = "100%", height = "190px")
      )
    )
  )
)
