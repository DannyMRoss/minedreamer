
server <- function(input, output, session) {

  rv <- reactiveValues(
    DIM = NULL,
    N_MINES = NULL,
    BASE = NULL,
    BOARD = NULL,
    FIRST = TRUE,
    OPEN = NULL,
    CELLS = NULL,
    MINES = NULL,
    iMINES = NULL,
    iCELLS = NULL,
    HOLES = NULL,
    FLAGS = NULL,
    FLAG = FALSE,
    CHORD = FALSE,
    GAMEOVER = FALSE,
    WINNER = FALSE,
    MOVE = NULL,
    i = NULL
  )

  initialize_game <- function(dim, mines, base, rv) {
    rv$DIM <- as.numeric(dim)
    rv$N_MINES <- as.numeric(mines)
    rv$BASE <- as.numeric(base)
    rv$BOARD <- matrix("", rv$DIM, rv$DIM)
    rv$FIRST <- TRUE
    rv$OPEN <- NULL
    rv$GAMEOVER <- FALSE
    rv$WINNER <- FALSE
    rv$CELLS <- NULL
    rv$MINES <- NULL
    rv$iMINES <- NULL
    rv$iCELLS <- NULL
    rv$HOLES <- NULL
    rv$FLAGS <- matrix(FALSE, rv$DIM, rv$DIM)
    rv$FLAG <- FALSE
    rv$CHORD <- FALSE
    rv$GAMEOVER <- FALSE
    rv$WINNER <- FALSE
    rv$MOVE <- NULL
    rv$i <- NULL
  }


  game_board_data <- reactive({
    req(input$dim, input$mines, input$base)
    if(!is.null(rv$BOARD)){
      DT <- dtboard(rv$BOARD)
      vals <- sort(unique(as.vector(rv$CELLS)))
      colors <- c("grey50", "black", "black", hcl.colors(length(vals) - 1, "Batlow"))
      DT[, color := factor(L, levels = c("", "X", "F", vals))]
      DT[, fill := 1]
      DT[L == "", fill := 2]
      DT[L == "F", fill := 3]
      DT[L == "X", fill := 4]
      DT[, fill := factor(fill, levels = 1:4)]
      DT[, LAB := L]
      DT[L == "0", LAB := ""]
      list(DT = DT, colors = colors)
    }
  })

  output$game_board <- renderPlot({
    board_data <- game_board_data()
    if (!is.null(board_data)){
      DT <- board_data$DT
      colors <- board_data$colors

      p <- ggplot(DT, aes(x = X, y = Y, label = LAB, color = color, fill = fill)) +
        geom_tile(color = "grey10", linewidth = 0.5) +
        coord_fixed(ratio = 1) +
        geom_fit_text(grow = TRUE) +
        scale_y_reverse() +
        scale_color_manual(breaks = DT[, levels(color)], values = colors) +
        scale_fill_manual(breaks = DT[, levels(fill)], values = c("grey70", "grey50", "grey50", "red")) +
        theme_void() +
        theme(legend.position = "none",
              plot.background = element_rect(fill = "black"),
              panel.background = element_rect(fill = "black"))
      p
    }
  }, bg = "black")


  modal_start <- function(title, message) {
    showModal(modalDialog(
      title = title,
      tagList(
        tags$p(message),
        textInput("dim", "Dimension:", ""),
        textInput("mines", "Number of Mines:", ""),
        textInput("base", "Base:", value = "")
      ),
      easyClose = FALSE,
      footer = tagList(
        actionButton("start_game", "Start Game"),
        modalButton("Close")
      )
    ))
  }

  wm <- c("Welcome...","Happy Sweeping","Sweep Wisely")
  modal_start(wm[sample(length(wm),1)], " ")


  update_board <- function(i, rv) {
    if (rv$FIRST) {
      if (rv$BASE == 1) {
        rv$MINES <- minesum(openboard(rv$DIM, rv$N_MINES, rv$MOVE))
      } else {
        rv$MINES <- power(minesum(openboard(rv$DIM, rv$N_MINES, rv$MOVE)), rv$BASE)
      }
      rv$CELLS <- cells(rv$MINES)
      rv$iMINES <- which(rv$MINES != 0)
      rv$iCELLS <- which(rv$CELLS != 0)
      rv$HOLES <- rv$CELLS == 0 & rv$MINES == 0
      rv$FIRST <- FALSE
    }

    if (input$flag_checkbox) {
      rv$FLAGS[i] <- TRUE
      rv$BOARD[i] <- "F"
    } else if (input$chord_checkbox) {
      ichord <- c(i, setdiff(adjacentA(i, rv$MINES == 0), rv$OPEN))
      ichordmines <- adjacentA(i, rv$MINES != 0)
      rv$BOARD[ichord] <- rv$CELLS[ichord]
      if (length(ichordmines) > 0) {
        rv$BOARD[ichordmines] <- "X"
        rv$GAMEOVER <- TRUE
        rv$WINNER <- FALSE
      }
    } else if (i %in% rv$iMINES) {
      rv$BOARD[i] <- "X"
      rv$GAMEOVER <- TRUE
      rv$WINNER <- FALSE
    } else {
      if (rv$HOLES[i]) {
        ipit <- pit(i, rv$HOLES, rv$CELLS)
        rv$OPEN <- c(rv$OPEN, ipit)
        rv$BOARD[ipit] <- rv$CELLS[ipit]
      } else {
        rv$BOARD[i] <- rv$CELLS[i]
      }
      rv$OPEN <- c(rv$OPEN, i)
      if (all(rv$iCELLS %in% rv$OPEN)) {
        rv$GAMEOVER <- TRUE
        rv$WINNER <- TRUE
      }
    }

    if (rv$GAMEOVER) {
      if (rv$WINNER) {
        output$messages <- renderText("You win. Good for you.")
      } else {
        rv$BOARD[rv$iMINES] <- "X"
        output$messages <- renderText("KABOOM!")
      }

      insertUI(
        selector = "#game_board",
        where = "afterEnd",
        ui = actionButton("play_again","Play Again")
      )
    }
  }

  observeEvent(input$start_game, {
    req(input$dim, input$mines, input$base)
    initialize_game(input$dim, input$mines, input$base, rv)
    removeModal()
  })

  observeEvent(input$play_again, {
    session$reload()
  })


  observeEvent(input$move, {
    req(input$dim, input$mines, input$base)
    rv$MOVE <- c(round(input$move$y), round(input$move$x))
    rv$i <- index(rv$MOVE[1], rv$MOVE[2], rv$DIM)
    if(!rv$GAMEOVER){
      update_board(rv$i, rv)
    }
  })

  observe({
    rv$BOARD
  })

}
