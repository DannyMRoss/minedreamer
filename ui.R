ui <- fluidPage(
  theme = shinytheme("darkly"),
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Consolas', monospace;
        background-color: black;
        color: white;
        margin: 0;
        padding: 20px; /* Add padding to the body */
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        height: 100vh;
      }
      .container-fluid {
        width: 100%;
        max-width: 800px; /* Adjust max-width as needed */
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        height: 100%;
      }
      .well {
        background-color: black;
        border-color: black;
      }
      #plot-container {
        width: 100%;
        height: 80vh; /* Adjust height as needed */
        display: flex;
        align-items: center;
        justify-content: center;
      }
      #message-container {
        width: 100%;
        height: 20vh; /* Adjust height as needed */
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 24px;
      }
      .checkbox-container {
        display: flex;
        flex-direction: row;
        justify-content: center;
        align-items: center;
        width: 100%;
        height: 10%;
      }
    "))
  ),
  titlePanel("Minedreamer", windowTitle = "Minesweeper"),
  div(
    class = "container-fluid",
    div(
      id = "plot-container",
      plotOutput("game_board", click = "move", width = "100%", height = "100%")
    ),
    div(
      id = "message-container",
      uiOutput("messages")
    ),
    div(
      class = "checkbox-container",
      checkboxInput("flag_checkbox", "Flag", value = FALSE),
      checkboxInput("chord_checkbox", "Chord", value = FALSE)
    )
  )
)
