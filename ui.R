# LIBRARIES --------------------------------------------------------------------
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(bslib)
library(bsicons)
library(shinydashboard)
library(shinyalert)
library(magrittr)

# UI SET UP -------------------------------------------------------------------------------------------------------------------------
ui <- navbarPage(
  title = div(
    class = "navbar-brand",
    img(src = "main-logo-white.png", height = "100px", style = "margin-right: 15px;"),
    span("Mission Maps", style = "color: #ffffff;")
  ),
  id = "navbar",
  theme = bs_theme(version = 5, bootswatch = "flatly") %>%
    bs_theme_update(
      bg = "#ffffff",
      fg = "#333333",
      primary = "#009cd6"
    ),
  
  # Include necessary scripts and styles
  header = tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.15.0/Sortable.min.js"),
    useShinyjs(),
    tags$style(HTML("
      .navbar {
        background: linear-gradient(-45deg, #07a48e, #009cd6, #8b559d, #009cd6) !important;
        background-size: 400% 400% !important;
        animation: gradient 15s ease infinite !important;
      }
      
      @keyframes gradient {
        0% { background-position: 0% 50%; }
        50% { background-position: 100% 50%; }
        100% { background-position: 0% 50%; }
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
      
      .dropzone.correct {
        border-color: #4CAF50;
        background-color: rgba(76, 175, 80, 0.1);
        box-shadow: 0 0 10px rgba(76, 175, 80, 0.3);
      }
      
      .dropzone.incorrect {
        border-color: #f44336;
        background-color: rgba(244, 67, 54, 0.1);
        box-shadow: 0 0 10px rgba(244, 67, 54, 0.3);
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
        display: flex;
        align-items: center;
        width: calc(33.333% - 16px);
        float: left;
      }
      
      .draggable-item i {
        margin-right: 10px;
        color: #009cd6;
      }
      
      .draggable-item:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        border-color: #009cd6;
      }
      
      #available_items {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        justify-content: flex-start;
      }
      
      .level-card {
        background: white;
        border-radius: 15px;
        padding: 20px;
        margin: 15px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        transition: all 0.3s ease;
        height: 300px;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        text-align: center;
      }
      
      .level-card:hover {
        transform: translateY(-5px);
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15);
      }
      
      .score-panel {
        background: linear-gradient(135deg, #07a48e, #009cd6);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin: 20px 0;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      .score-item {
        text-align: center;
        padding: 10px;
        background: rgba(255,255,255,0.1);
        border-radius: 10px;
        backdrop-filter: blur(5px);
      }
      
      .score-value {
        font-size: 28px;
        font-weight: bold;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
      }
      
      .score-label {
        font-size: 14px;
        opacity: 0.9;
        letter-spacing: 1px;
        text-transform: uppercase;
      }
      
      .gradient-text {
        background: linear-gradient(45deg, #07a48e, #009cd6, #8b559d);
        background-size: 800%;
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }
      
      .btn-unite {
        background: linear-gradient(45deg, #07a48e, #009cd6) !important;
        border: none !important;
        color: white !important;
        padding: 10px 20px !important;
        border-radius: 5px !important;
      }
    "))
  ),
  
  # HOME TAB --------------------------------------------------
  tabPanel("Home",
    fluidPage(
      # Header content
      div(class = "text-center mt-4 mb-4",
        h1("Mission Maps", class = "gradient-text", 
           style = "font-size: 3.5em; font-weight: bold;"),
        p("Learn to map impact pathways with interactive logic models", 
          class = "lead", style = "font-size: 1.4em; color: #666;")
      ),
      
      # Video Section
      div(class = "container mb-5",
        div(class = "row justify-content-center",
          div(class = "col-md-8",
            div(style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden;",
              tags$iframe(
                src = "https://www.powtoon.com/embed/ePwqY92sgIB/",
                style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                frameborder = "0",
                allowfullscreen = TRUE
              )
            )
          )
        )
      ),
      
      # Level selection cards
      fluidRow(
        column(6,
          div(class = "level-card",
            h3("Level 1: Lifecourse Model"),
            p("Map impacts across the life course"),
            actionButton("start_lifecourse", "Start", class = "btn-unite")
          )
        ),
        column(6,
          div(class = "level-card",
            h3("Level 2: Skills Intervention"),
            p("Employment programme logic model"),
            actionButton("start_skills", "Start", class = "btn-unite")
          )
        )
      ),
      fluidRow(
        column(6,
          div(class = "level-card",
            h3("Level 3: Community Cooking"),
            p("Food programme impact pathways"),
            actionButton("start_cooking", "Coming Soon", class = "btn-unite", disabled = TRUE)
          )
        ),
        column(6,
          div(class = "level-card",
            h3("Level 4: Build Your Own"),
            p("Create custom logic models"),
            actionButton("start_build", "Join Beta", class = "btn-unite")
          )
        )
      )
    )
  ),
  
  # SKILLS INTERVENTION TAB ------------------------------------
  tabPanel("Skills Intervention",
    fluidPage(
      # Score panel with new styling
      div(class = "score-panel",
          style = "background: linear-gradient(135deg, #07a48e, #009cd6); border-radius: 15px; padding: 20px; margin: 20px auto; max-width: 800px;",
          fluidRow(
            column(3, 
                   div(class = "score-item",
                       div(icon("trophy"), style = "font-size: 24px; color: #ffd700;"),
                       div(class = "mt-2", style = "font-size: 0.9em; color: rgba(255,255,255,0.9);", "SCORE"),
                       div(textOutput("score"), 
                           style = "font-size: 28px; font-weight: bold; text-shadow: 1px 1px 2px rgba(0,0,0,0.1);")
                   )
            ),
            column(3,
                   div(class = "score-item",
                       div(icon("check-circle"), style = "font-size: 24px; color: #98FB98;"),
                       div(class = "mt-2", style = "font-size: 0.9em; color: rgba(255,255,255,0.9);", "CORRECT"),
                       div(textOutput("correct_attempts"), 
                           style = "font-size: 28px; font-weight: bold; text-shadow: 1px 1px 2px rgba(0,0,0,0.1);")
                   )
            ),
            column(3,
                   div(class = "score-item",
                       div(icon("times-circle"), style = "font-size: 24px; color: #FFB6C1;"),
                       div(class = "mt-2", style = "font-size: 0.9em; color: rgba(255,255,255,0.9);", "INCORRECT"),
                       div(textOutput("incorrect_attempts"), 
                           style = "font-size: 28px; font-weight: bold; text-shadow: 1px 1px 2px rgba(0,0,0,0.1);")
                   )
            ),
            column(3,
                   div(class = "score-item",
                       div(icon("clock"), style = "font-size: 24px; color: #F0E68C;"),
                       div(class = "mt-2", style = "font-size: 0.9em; color: rgba(255,255,255,0.9);", "TIME"),
                       div(textOutput("timer"), 
                           style = "font-size: 28px; font-weight: bold; text-shadow: 1px 1px 2px rgba(0,0,0,0.1);")
                   )
            )
          )
      ),
      
      # Game grid with original structure
      div(class = "game-container",
        fluidRow(
          column(2,
            h4("Inputs", class = "text-center"),
            div(class = "dropzone", id = "drop_inputs",
                div(class = "dropzone-header", "Resources required")
            )
          ),
          column(2,
            h4("Activities", class = "text-center"),
            div(class = "dropzone", id = "drop_activities",
                div(class = "dropzone-header", "Programme interventions")
            )
          ),
          column(2,
            h4("Direct Output", class = "text-center"),
            div(class = "dropzone", id = "drop_output",
                div(class = "dropzone-header", "Primary result")
            )
          ),
          column(3,
            h4("Intermediate Outcomes", class = "text-center"),
            div(class = "dropzone", id = "drop_intermediate",
                div(class = "dropzone-header", "Medium-term changes")
            )
          ),
          column(3,
            h4("Final Outcomes", class = "text-center"),
            div(class = "dropzone", id = "drop_final",
                div(class = "dropzone-header", "Wellbeing impacts")
            )
          )
        )
      ),
      
      # Available items with original structure
      div(class = "game-container",
        h4("Available Components", class = "mb-3"),
        div(id = "available_items", 
            class = "d-flex flex-wrap justify-content-center",
            style = "gap: 10px; min-height: 100px; padding: 10px;")
      )
    )
  ),
  
  # OTHER LEVEL TABS -----------------------------------------
  tabPanel("Lifecourse Model",
    fluidPage(
      h2("Coming Soon"),
      p("This level is under development.")
    )
  ),
  
  tabPanel("Community Cooking",
    fluidPage(
      h2("Coming Soon"),
      p("This level is under development.")
    )
  ),
  
  tabPanel("Build Your Own",
    fluidPage(
      h2("Beta Sign-up"),
      textInput("beta_email", "Email"),
      actionButton("beta_signup", "Join Beta", class = "btn-unite")
    )
  ),
  
  # FOOTER ------------------------------------------------------------------------------------------------------------------
  footer = div(
    style = "width: 100%; padding: 10px 0; display: flex; justify-content: space-between; align-items: center;",
    div(style = "margin-left: 20px;", 
      tags$a(href = "https://missioneconomics.org", target = "_blank",
        img(src = "main-logo.png", style = "height: 200px; width: auto;")
      )
    ),
    div(style = "margin-right: 20px;", 
      tags$a(href = "https://missioneconomics.org", target = "_blank",
        img(src = "main-logo.png", style = "height: 200px; width: auto;")
      )
    )
  )
)