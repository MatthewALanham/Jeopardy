# install.packages(c("shiny","shinyjs"))
library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    body { background-color: black; }
    .board { display: grid; grid-template-columns: repeat(6, 1fr); gap: 5px; width: 90%; margin: 20px auto; }
    .category { background-color: #081A6B; color: white; font-weight: bold; font-size: 22px; text-align: center; padding: 10px; border: 2px solid black; }
    .tile { background-color: #081A6B; color: #FFCC00; font-weight: bold; font-size: 26px; text-align: center; padding: 25px; border: 2px solid black; cursor: pointer; }
    .tile.black { background-color: black; color: black; }
  "))),
  # preload the Daily Double sound
  tags$audio(id = "ddsound", src = "daily-double.mp3", type = "audio/mpeg", preload = "auto"),
  h2("Jeopardy Board", align = "center", style = "color: white;"),
  uiOutput("boardUI")
)

server <- function(input, output, session) {
  categories <- c("Python","Python Exam Qs","Linear Regression","Regression Diagnostics","Cross-validation","Random")
  values <- c("$200","$400","$600","$800","$1000")
  
  # track tile state
  tile_states <- reactiveValues()
  for (cat in categories) for (val in values) tile_states[[paste(cat, val, sep = "_")]] <- FALSE
  
  observeEvent(input$tile_click, {
    id <- input$tile_click
    
    # DAILY DOUBLE trigger: $800 in the 5th column (Cross-validation)
    if (identical(id, "Cross-validation_$800")) {
      showModal(modalDialog(
        easyClose = TRUE, footer = NULL,
        tags$div(style = "background:#000; padding:0; text-align:center;",
                 tags$img(src = "daily-double.jpg", style = "width:100%; height:auto;"),
                 tags$h2("DAILY DOUBLE!", style = "color:#FFCC00; font-weight:bold; margin:12px 0;")
        )
      ))
      runjs("document.getElementById('ddsound').currentTime = 0; document.getElementById('ddsound').play();")
    }
    
    # toggle the clicked tile (including the Daily Double tile)
    tile_states[[id]] <- !isTRUE(tile_states[[id]])
  })
  
  output$boardUI <- renderUI({
    tiles <- list()
    for (cat in categories) tiles <- append(tiles, list(div(class = "category", cat)))
    for (val in values) for (cat in categories) {
      id <- paste(cat, val, sep = "_")
      is_black <- isTRUE(tile_states[[id]])
      tiles <- append(tiles, list(
        div(class = if (is_black) "tile black" else "tile",
            id = id, if (is_black) "" else val,
            onclick = sprintf("Shiny.setInputValue('tile_click','%s',{priority:'event'})", id))
      ))
    }
    div(class = "board", tiles)
  })
}

shinyApp(ui, server)