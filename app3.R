# install.packages(c("shiny","shinyjs"))
library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    body { background-color: black; }
    .board { display:grid; grid-template-columns:repeat(6,1fr); gap:5px; width:90%; margin:20px auto; }
    .category { background:#081A6B; color:white; font-weight:bold; font-size:22px; text-align:center; padding:10px; border:2px solid black; }
    .tile { background:#081A6B; color:#FFCC00; font-weight:bold; font-size:26px; text-align:center; padding:25px; border:2px solid black; cursor:pointer; }
    .tile.black { background:black; color:black; }
    .theme-controls { width:90%; margin:12px auto 24px auto; text-align:center; }
    .theme-btn { background:#1e3a8a; color:white; border:none; font-weight:600; }
  "))),
  # preload sounds
  tags$audio(id = "ddsound", src = "daily-double.mp3", type = "audio/mpeg", preload = "auto"),
  tags$audio(id = "theme",  src = "jeopardy-theme.mp3", type = "audio/mpeg", preload = "auto"),
  h2("Jeopardy Board", align = "center", style = "color: white;"),
  uiOutput("boardUI"),
  div(class = "theme-controls",
      actionButton("theme_toggle", label = " Play Theme", icon = icon("play"), class = "theme-btn")
  )
)

server <- function(input, output, session) {
  categories <- c("Python","Python Exam Qs","Linear Regression","Regression Diagnostics","Cross-validation","Random")
  values <- c("$200","$400","$600","$800","$1000")
  
  # track tile state
  tile_states <- reactiveValues()
  for (cat in categories) for (val in values) tile_states[[paste(cat, val, sep = "_")]] <- FALSE
  
  # DAILY DOUBLE + toggle behavior
  observeEvent(input$tile_click, {
    id <- input$tile_click
    if (identical(id, "Cross-validation_$800")) {
      showModal(modalDialog(
        easyClose = TRUE, footer = NULL,
        tags$div(style="background:#000;text-align:center;",
                 tags$img(src="daily-double.jpg", style="width:100%;height:auto;"),
                 tags$h2("DAILY DOUBLE!", style="color:#FFCC00;font-weight:bold;margin:12px 0;")
        )
      ))
      runjs("const a=document.getElementById('ddsound'); a.currentTime=0; a.play();")
    }
    tile_states[[id]] <- !isTRUE(tile_states[[id]])
  })
  
  # THEME play/stop toggle (independent of other sounds)
  themePlaying <- reactiveVal(FALSE)
  observeEvent(input$theme_toggle, {
    if (!themePlaying()) {
      runjs("const t=document.getElementById('theme'); t.currentTime ||= 0; t.play();")
      themePlaying(TRUE)
      updateActionButton(session, "theme_toggle", label = " Stop Theme", icon = icon("stop"))
    } else {
      runjs("const t=document.getElementById('theme'); t.pause(); t.currentTime=0;")
      themePlaying(FALSE)
      updateActionButton(session, "theme_toggle", label = " Play Theme", icon = icon("play"))
    }
  })
  
  # build board
  output$boardUI <- renderUI({
    tiles <- list()
    for (cat in categories) tiles <- append(tiles, list(div(class="category", cat)))
    for (val in values) for (cat in categories) {
      id <- paste(cat, val, sep = "_")
      is_black <- isTRUE(tile_states[[id]])
      tiles <- append(tiles, list(
        div(class = if (is_black) "tile black" else "tile",
            id = id, if (is_black) "" else val,
            onclick = sprintf("Shiny.setInputValue('tile_click','%s',{priority:'event'})", id))
      ))
    }
    div(class="board", tiles)
  })
}

shinyApp(ui, server)