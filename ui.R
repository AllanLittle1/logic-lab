# LIBRARIES --------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(bslib)
library(shinydashboard)
library(shinyalert)
library(magrittr)

# UI SET UP -------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  # Include JavaScript and CSS
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"),
    useShinyjs(),
    tags$style("
      .navbar {
        background: linear-gradient(-45deg, #07a48e, #009cd6, #8b559d, #009cd6) !important;
        background-size: 400% 400% !important;
        animation: gradient 15s ease infinite !important;
      }
      
      .game-container {
        padding: 20px;
        background-color: rgba(255, 255, 255, 0.9);
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        margin: 20px 0;
      }
      
      .dropzone {
        min-height: 200px;
        border: 2px dashed #ddd;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
        background-color: rgba(255, 255, 255, 0.8);
        transition: all 0.3s ease;
      }
      
      .draggable-item {
        padding: 12px;
        margin: 8px;
        background-color: white;
        border: 1px solid #ddd;
        border-radius: 6px;
        cursor: move;
        transition: all 0.2s ease;
        font-size: 0.9em;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      
      .score-panel {
        background: linear-gradient(135deg, #07a48e, #009cd6);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin: 20px 0;
      }
      
      .btn-unite {
        background: linear-gradient(45deg, #07a48e, #009cd6) !important;
        border: none !important;
        color: white !important;
        padding: 10px 20px !important;
        border-radius: 5px !important;
      }
    ")
  ),
  
  # Header
  div(class = "text-center mt-4 mb-4",
      h1("LogicLab", style = "color: #009cd6; font-size: 3em; font-weight: bold;"),
      div(class = "container", style = "max-width: 800px; margin: auto;",
          p(class = "lead", "Build a theory of change for an employment intervention",
            style = "font-size: 1.4em; color: #444;"),
          p("Drag and drop the components into their correct positions to create a logic model. 
            Based on research by Frijters & Krekel (2019), this game helps you understand how 
            employment support programs create wellbeing impacts.",
            style = "font-size: 1.1em; color: #666; margin-bottom: 1em;"),
          p("Match inputs with activities, outputs, intermediate outcomes, and final impacts to see 
            how interventions create change.",
            style = "font-size: 1.1em; color: #666;")
      )
  ),
  
  # Score Panel with improved styling
  div(class = "score-panel",
      style = "background: linear-gradient(135deg, #07a48e, #009cd6); border-radius: 15px; padding: 20px; margin: 20px auto; max-width: 800px;",
      fluidRow(
        column(3, 
               div(class = "text-center",
                   div(icon("trophy"), style = "font-size: 24px; color: #ffd700;"),
                   div(class = "mt-2", style = "font-size: 0.9em; color: rgba(255,255,255,0.9);", "SCORE"),
                   div(textOutput("score"), 
                       style = "font-size: 28px; font-weight: bold; text-shadow: 1px 1px 2px rgba(0,0,0,0.1);")
               )
        ),
        column(3,
               div(class = "text-center",
                   div(icon("check-circle"), style = "font-size: 24px; color: #98FB98;"),
                   div(class = "mt-2", style = "font-size: 0.9em; color: rgba(255,255,255,0.9);", "CORRECT"),
                   div(textOutput("correct_attempts"), 
                       style = "font-size: 28px; font-weight: bold; text-shadow: 1px 1px 2px rgba(0,0,0,0.1);")
               )
        ),
        column(3,
               div(class = "text-center",
                   div(icon("times-circle"), style = "font-size: 24px; color: #FFB6C1;"),
                   div(class = "mt-2", style = "font-size: 0.9em; color: rgba(255,255,255,0.9);", "INCORRECT"),
                   div(textOutput("incorrect_attempts"), 
                       style = "font-size: 28px; font-weight: bold; text-shadow: 1px 1px 2px rgba(0,0,0,0.1);")
               )
        ),
        column(3,
               div(class = "text-center",
                   div(icon("clock"), style = "font-size: 24px; color: #F0E68C;"),
                   div(class = "mt-2", style = "font-size: 0.9em; color: rgba(255,255,255,0.9);", "TIME"),
                   div(textOutput("timer"), 
                       style = "font-size: 28px; font-weight: bold; text-shadow: 1px 1px 2px rgba(0,0,0,0.1);")
               )
        )
      )
  ),
  
  # Game Grid
  div(class = "game-container",
      fluidRow(
        column(2,
               h4("Inputs", class = "text-center"),
               div(class = "dropzone", id = "drop_inputs")
        ),
        column(2,
               h4("Activities", class = "text-center"),
               div(class = "dropzone", id = "drop_activities")
        ),
        column(2,
               h4("Direct Output", class = "text-center"),
               div(class = "dropzone", id = "drop_output")
        ),
        column(3,
               h4("Intermediate Outcomes", class = "text-center"),
               div(class = "dropzone", id = "drop_intermediate")
        ),
        column(3,
               h4("Final Outcomes", class = "text-center"),
               div(class = "dropzone", id = "drop_final")
        )
      )
  ),
  
  # Available Items
  div(class = "game-container",
      h4("Available Components", class = "mb-3"),
      div(id = "available_items", 
          class = "d-flex flex-wrap justify-content-center",
          style = "gap: 10px; min-height: 100px; padding: 10px;")
  ),
  
  # Button Row
  div(class = "text-center mt-4 mb-4",
      actionButton("reset_game", "Reset Game", 
                   class = "btn-unite", 
                   style = "margin-right: 10px"),
      downloadButton("download_report", "Download Report", 
                     class = "btn-unite")
  )
)