server <- function(input, output, session) {
  
  # Initialize reactive values
  game_state <- reactiveValues(
    score = 0,
    correct_attempts = 0,
    incorrect_attempts = 0,
    timer = 0,
    is_running = FALSE,
    game_complete = FALSE,
    placed_items = list()
  )
  
  # Game items with their correct categories and icons
  items <- list(
    list(id = "1", text = "Veterans' Time Investment", category = "inputs", icon = "clock"),
    list(id = "2", text = "Programme Delivery Costs", category = "inputs", icon = "pound-sign"),
    list(id = "3", text = "Skills Workshop", category = "activities", icon = "chalkboard-teacher"),
    list(id = "4", text = "One-to-one Careers Coaching", category = "activities", icon = "user-friends"),
    list(id = "5", text = "Employer Partnership Network", category = "activities", icon = "handshake"),
    list(id = "6", text = "Additional Employment", category = "output", icon = "briefcase"),
    list(id = "7", text = "Higher Income", category = "intermediate", icon = "chart-line"),
    list(id = "8", text = "Lower Crime Rate", category = "intermediate", icon = "shield-alt"),
    list(id = "9", text = "Better Physical Health", category = "intermediate", icon = "heartbeat"),
    list(id = "10", text = "Better Mental Health", category = "intermediate", icon = "brain"),
    list(id = "11", text = "Direct Life Satisfaction Improvement", category = "final", icon = "smile"),
    list(id = "12", text = "Indirect Family Life Satisfaction Improvement", category = "final", icon = "users"),
    list(id = "13", text = "Health System Cost Savings", category = "final", icon = "pound-sign"),
    list(id = "14", text = "Benefit System Savings", category = "final", icon = "pound-sign")
  )
  
  # Initialize game items
  observeEvent(c(1), {
    # Shuffle items
    shuffled_items <- sample(items)
    
    # Clear existing items
    removeUI(selector = "#available_items > *", immediate = TRUE)
    
    # Add shuffled items
    for(item in shuffled_items) {
      insertUI(
        selector = "#available_items",
        where = "beforeEnd",
        ui = div(
          id = paste0("item_", item$id),
          class = "draggable-item",
          `data-category` = item$category,
          tags$i(class = paste0("fas fa-", item$icon)),
          span(item$text)
        )
      )
    }
    
    # Initialize Sortable
    runjs("
      new Sortable(document.getElementById('available_items'), {
        group: 'logic_items',
        animation: 150,
        sort: false
      });
      
      document.querySelectorAll('.dropzone').forEach(function(dropzone) {
        new Sortable(dropzone, {
          group: 'logic_items',
          animation: 150,
          onAdd: function(evt) {
            Shiny.setInputValue('item_dropped', {
              item: evt.item.id,
              target: evt.to.id
            });
          }
        });
      });
    ")
  })
  
  # Timer observer
  observe({
    if (game_state$is_running) {
      invalidateLater(1000)
      isolate({
        game_state$timer <- game_state$timer + 1
      })
    }
  })
  
  # Format timer output
  output$timer <- renderText({
    mins <- floor(game_state$timer / 60)
    secs <- game_state$timer %% 60
    sprintf("%02d:%02d", mins, secs)
  })
  
  # Score outputs
  output$score <- renderText({
    sprintf("%d/%d", game_state$score, length(items))
  })
  
  output$correct_attempts <- renderText({
    game_state$correct_attempts
  })
  
  output$incorrect_attempts <- renderText({
    game_state$incorrect_attempts
  })
  
  # Handle item drops
  observeEvent(input$item_dropped, {
    if (!game_state$is_running) {
      game_state$is_running <- TRUE
    }
    
    item_id <- gsub("item_", "", input$item_dropped$item)
    target_zone <- gsub("drop_", "", input$item_dropped$target)
    
    # Get item details
    item <- items[[as.numeric(item_id)]]
    
    # Check if correct
    is_correct <- item$category == target_zone
    
    if (is_correct) {
      game_state$score <- game_state$score + 1
      game_state$correct_attempts <- game_state$correct_attempts + 1
      game_state$placed_items[[item_id]] <- item
      
      # Add correct feedback
      runjs(sprintf("
        document.getElementById('%s').classList.add('correct');
        setTimeout(function() {
          document.getElementById('%s').classList.remove('correct');
        }, 2000);
      ", input$item_dropped$target, input$item_dropped$target))
      
      showNotification(
        ui = div(
          style = "display: flex; align-items: center;",
          tags$i(class = "fas fa-check-circle", style = "color: #4CAF50; margin-right: 8px;"),
          "Correct placement!"
        ),
        type = "message",
        duration = 2
      )
    } else {
      game_state$score <- max(0, game_state$score - 1)
      game_state$incorrect_attempts <- game_state$incorrect_attempts + 1
      
      # Add incorrect feedback
      runjs(sprintf("
        document.getElementById('%s').classList.add('incorrect');
        setTimeout(function() {
          document.getElementById('%s').classList.remove('incorrect');
        }, 2000);
      ", input$item_dropped$target, input$item_dropped$target))
      
      showNotification(
        ui = div(
          style = "display: flex; align-items: center;",
          tags$i(class = "fas fa-times-circle", style = "color: #f44336; margin-right: 8px;"),
          "Try again!"
        ),
        type = "warning",
        duration = 2
      )
      
      # Return item to available items
      runjs(sprintf("
        document.getElementById('available_items').appendChild(
          document.getElementById('item_%s')
        );
      ", item_id))
    }
    
    # Check for game completion
    if (length(game_state$placed_items) == length(items)) {
      game_state$is_running <- FALSE
      game_state$game_complete <- TRUE
      
      showModal(modalDialog(
        title = NULL,
        div(
          class = "text-center",
          style = "padding: 20px;",
          tags$i(class = "fas fa-trophy", 
                 style = "font-size: 48px; color: #ffd700; margin-bottom: 20px;"),
          h2("Congratulations! 🎉", 
             style = "color: #009cd6; margin-bottom: 20px;"),
          div(
            style = "background: rgba(0,156,214,0.1); padding: 20px; border-radius: 10px; margin-bottom: 20px;",
            h4(sprintf("Final Score: %d/%d", game_state$score, length(items)),
               style = "color: #009cd6; margin-bottom: 10px;"),
            h4(sprintf("Time: %02d:%02d", floor(game_state$timer / 60), game_state$timer %% 60),
               style = "margin-bottom: 10px;"),
            h4(sprintf("Correct Attempts: %d", game_state$correct_attempts),
               style = "color: #4CAF50; margin-bottom: 10px;"),
            h4(sprintf("Incorrect Attempts: %d", game_state$incorrect_attempts),
               style = "color: #f44336;")
          )
        ),
        footer = div(
          class = "text-center",
          actionButton("play_again", "Play Again", 
                       class = "btn-unite",
                       style = "margin-right: 10px;"),
          tags$button(
            "Close",
            type = "button",
            class = "btn btn-unite",
            `data-bs-dismiss` = "modal"
          )
        ),
        size = "m",
        easyClose = TRUE
      ))
    }
  })
  
  # Reset game
  observeEvent(input$reset_game, {
    game_state$score <- 0
    game_state$correct_attempts <- 0
    game_state$incorrect_attempts <- 0
    game_state$timer <- 0
    game_state$is_running <- FALSE
    game_state$game_complete <- FALSE
    game_state$placed_items <- list()
    
    # Clear all dropzones
    runjs("
      document.querySelectorAll('.dropzone').forEach(function(dropzone) {
        while (dropzone.firstChild) {
          dropzone.removeChild(dropzone.firstChild);
        }
      });
    ")
    
    # Reinitialize items
    shuffled_items <- sample(items)
    removeUI(selector = "#available_items > *", immediate = TRUE)
    
    for(item in shuffled_items) {
      insertUI(
        selector = "#available_items",
        where = "beforeEnd",
        ui = div(
          id = paste0("item_", item$id),
          class = "draggable-item",
          `data-category` = item$category,
          tags$i(class = paste0("fas fa-", item$icon)),
          span(item$text)
        )
      )
    }
    
    # Reinitialize sortable
    runjs("
      new Sortable(document.getElementById('available_items'), {
        group: 'logic_items',
        animation: 150,
        sort: false
      });
      
      document.querySelectorAll('.dropzone').forEach(function(dropzone) {
        new Sortable(dropzone, {
          group: 'logic_items',
          animation: 150,
          onAdd: function(evt) {
            Shiny.setInputValue('item_dropped', {
              item: evt.item.id,
              target: evt.to.id
            });
          }
        });
      });
    ")
  })
  
  # Play again handler
  observeEvent(input$play_again, {
    removeModal()
    game_state$score <- 0
    game_state$correct_attempts <- 0
    game_state$incorrect_attempts <- 0
    game_state$timer <- 0
    game_state$is_running <- FALSE
    game_state$game_complete <- FALSE
    game_state$placed_items <- list()
    
    # Clear all dropzones
    runjs("
      document.querySelectorAll('.dropzone').forEach(function(dropzone) {
        while (dropzone.firstChild) {
          dropzone.removeChild(dropzone.firstChild);
        }
      });
    ")
    
    # Reinitialize items
    shuffled_items <- sample(items)
    removeUI(selector = "#available_items > *", immediate = TRUE)
    
    for(item in shuffled_items) {
      insertUI(
        selector = "#available_items",
        where = "beforeEnd",
        ui = div(
          id = paste0("item_", item$id),
          class = "draggable-item",
          `data-category` = item$category,
          tags$i(class = paste0("fas fa-", item$icon)),
          span(item$text)
        )
      )
    }
    
    # Reinitialize sortable
    runjs("
      new Sortable(document.getElementById('available_items'), {
        group: 'logic_items',
        animation: 150,
        sort: false
      });
      
      document.querySelectorAll('.dropzone').forEach(function(dropzone) {
        new Sortable(dropzone, {
          group: 'logic_items',
          animation: 150,
          onAdd: function(evt) {
            Shiny.setInputValue('item_dropped', {
              item: evt.item.id,
              target: evt.to.id
            });
          }
        });
      });
    ")
  })
  
  # Download report handler
  output$download_report <- downloadHandler(
    filename = function() {
      paste("LogicLab_Report_", format(Sys.time(), "%Y%m%d_%H%M"), ".pdf", sep = "")
    },
    content = function(file) {
      # TODO: Implement report generation
    }
  )
}