server <- function(input, output, session) {
  
  # LEVEL NAVIGATION ---------------------------------------------
  observeEvent(input$start_skills, {
    updateTabsetPanel(session, "navbar", selected = "Skills Intervention")
  })
  
  observeEvent(input$start_lifecourse, {
    updateTabsetPanel(session, "navbar", selected = "Lifecourse Model")
  })
  
  # SKILLS GAME STATE -------------------------------------------
  game_state <- reactiveValues(
    score = 0,
    correct_attempts = 0,
    incorrect_attempts = 0,
    timer = 0,
    is_running = FALSE,
    game_complete = FALSE,
    placed_items = list(),
    consecutive_correct = 0
  )
  
  # Game items
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
  
  # Timer observer
  observe({
    if (game_state$is_running) {
      invalidateLater(1000)
      isolate({
        game_state$timer <- game_state$timer + 1
      })
    }
  })
  
  # Initialize game on Skills tab load
  observeEvent(input$navbar, {
    if(input$navbar == "Skills Intervention") {
      # Initialize game state
      game_state$score <- 0
      game_state$correct_attempts <- 0
      game_state$incorrect_attempts <- 0
      game_state$timer <- 0
      game_state$is_running <- FALSE
      game_state$game_complete <- FALSE
      game_state$placed_items <- list()
      game_state$consecutive_correct <- 0
      
      # Shuffle and initialize items
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
    }
  })
  
  # Handle item drops
  observeEvent(input$item_dropped, {
    req(input$item_dropped$item, input$item_dropped$target)
    
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
      game_state$consecutive_correct <- game_state$consecutive_correct + 1
      game_state$placed_items[[item_id]] <- item
      
      # Add visual feedback
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
      game_state$consecutive_correct <- 0
      
      # Add visual feedback
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
      
      # Calculate final score
      final_score <- game_state$score
      time_bonus <- if(game_state$timer < 180) 5 else 0
      accuracy_bonus <- if(game_state$incorrect_attempts == 0) 3 else 0
      total_score <- final_score + time_bonus + accuracy_bonus
      
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
            h4(sprintf("Final Score: %d", total_score),
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
          modalButton("Close")
        ),
        size = "m",
        easyClose = TRUE
      ))
    }
  })
  
  # OUTPUTS ----------------------------------------------------
  output$timer <- renderText({
    sprintf("%02d:%02d", floor(game_state$timer / 60), game_state$timer %% 60)
  })
  
  output$score <- renderText({
    sprintf("%d/%d", game_state$score, length(items))
  })
  
  output$correct_attempts <- renderText({
    game_state$correct_attempts
  })
  
  output$incorrect_attempts <- renderText({
    game_state$incorrect_attempts
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
    
    # Clear dropzones
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
  })
}