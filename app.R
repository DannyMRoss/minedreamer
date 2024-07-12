library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Consolas', monospace;
        background-color: black;
        color: white;
      }
      .container-fluid {
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
        height: 100vh;
        padding: 0;
      }
      .well {
        background-color: black;
        border-color: black;
      }
      #plot-container {
        display: flex;
        align-items: center;
        justify-content: center;
        width: 100%;
        height: 100%;
      }
      #game_board {
        width: auto !important;
        height: auto !important;
        max-width: 100%;
        max-height: 100%;
      }
    "))
  ),
  titlePanel("Minedreamer"),
  mainPanel(
    div(
      id = "plot-container",
      plotOutput("game_board", click = "move")
    ),
    checkboxInput("flag_checkbox", "Flag", value = FALSE),
    checkboxInput("chord_checkbox", "Chord", value = FALSE),
    verbatimTextOutput("info")
  )
)
source("mines.R")

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
    rv$FLAGS <- matrix(FALSE, rv$DIM, rv$DIM)
    rv$GAMEOVER <- FALSE
    rv$WINNER <- FALSE
  }

  output$game_board <- renderPlot({
    req(input$dim, input$mines, input$base)
    DT <- dtboard(rv$BOARD)
    vals <- sort(unique(as.vector(rv$CELLS)))
    colors <- c("grey50", "black", "black",hcl.colors(length(vals) - 1, "Batlow"))
    DT[, color := factor(L, levels = c("", "X", "F", vals))]
    DT[, fill := 1]
    DT[L == "", fill := 2]
    DT[L == "F", fill := 3]
    DT[L == "X", fill := 4]
    DT[, fill := factor(fill, levels = 1:4)]
    DT[, LAB := L]
    DT[L == "0", LAB := ""]

    p <- ggplot(DT, aes(x = X, y = Y, label = LAB, color = color, fill = fill)) +
      geom_tile(color = "grey10", linewidth = 0.5) +
      coord_fixed(ratio = 1) +
      geom_text(size = 10) +
      scale_y_reverse() +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = c("grey50", "grey70", "grey70","red")) +
      theme_void() +
      theme(legend.position = "none",
            plot.background = element_rect(fill = "black"),
            panel.background = element_rect(fill = "black")
            )
    p

  }, bg="black")

  show_modal <- function(title, message) {
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
        actionButton("play_again", "Play Again"),
        modalButton("Close")
      )
    ))
  }

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

  modal_start("Welcome to Minesweeper", " ")


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
        show_modal("Congratulations!", "You won! Do you want to play again?")
      } else {
        show_modal("Game Over", "You lost! Do you want to play again?")
      }
    }
  }

  observeEvent(input$start_game, {
    req(input$dim, input$mines, input$base)
    initialize_game(input$dim, input$mines, input$base, rv)
    removeModal()
  })

  observeEvent(input$play_again, {
    req(input$dim, input$mines, input$base)
    removeModal()
    initialize_game(input$dim, input$mines, input$base, rv)
  })

  observeEvent(input$move, {
    req(input$dim, input$mines, input$base, input$start_game)
    rv$MOVE <- c(round(input$move$y), round(input$move$x))
    rv$i <- index(rv$MOVE[1], rv$MOVE[2], rv$DIM)
    update_board(rv$i, rv)
    print(rv$CELLS)
    print(rv$MINES)
  })

}

shinyApp(ui, server)
