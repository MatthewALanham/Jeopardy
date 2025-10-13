# Install if necessary
# install.packages("shiny")

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: black; }
      .board {
        display: grid;
        grid-template-columns: repeat(6, 1fr);
        gap: 5px;
        width: 90%;
        margin: 20px auto;
      }
      .category {
        background-color: #081A6B;
        color: white;
        font-weight: bold;
        font-size: 22px;
        text-align: center;
        padding: 10px;
        border: 2px solid black;
      }
      .tile {
        background-color: #081A6B;
        color: #FFCC00;
        font-weight: bold;
        font-size: 26px;
        text-align: center;
        padding: 25px;
        border: 2px solid black;
        cursor: pointer;
      }
      .tile.black {
        background-color: black;
        color: black;
      }
    "))
  ),
  
  h2("Jeopardy Board", align = "center", style = "color: white;"),
  
  uiOutput("boardUI")
)

server <- function(input, output, session) {
  categories <- c("Python", "Python Exam Qs", "Linear Regression",
                  "Regression Diagnostics", "Cross-validation", "Random")
  values <- c("$200", "$400", "$600", "$800", "$1000")
  
  # Store tile states
  tile_states <- reactiveValues()
  for (cat in categories) {
    for (val in values) {
      tile_states[[paste(cat, val, sep = "_")]] <- FALSE
    }
  }
  
  observeEvent(input$tile_click, {
    id <- input$tile_click
    tile_states[[id]] <- !tile_states[[id]]
  })
  
  output$boardUI <- renderUI({
    tiles <- list()
    for (cat in categories) {
      tiles <- append(tiles, list(div(class = "category", cat)))
    }
    for (val in values) {
      for (cat in categories) {
        id <- paste(cat, val, sep = "_")
        is_black <- tile_states[[id]]
        tile_class <- if (is_black) "tile black" else "tile"
        label <- if (is_black) "" else val
        tiles <- append(tiles, list(
          div(class = tile_class,
              id = id,
              label,
              onclick = sprintf("Shiny.setInputValue('tile_click', '%s')", id))
        ))
      }
    }
    div(class = "board", tiles)
  })
}

shinyApp(ui, server)