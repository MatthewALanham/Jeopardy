# app.R
# install.packages(c("shiny","shinyjs","readxl"))
library(shiny)
library(shinyjs)
library(readxl)

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    body { background:black; }
    .board { display:grid; grid-template-columns:repeat(6,1fr); gap:5px; width:90%; margin:20px auto; }
    .category { background:#081A6B; color:white; font-weight:bold; font-size:22px; text-align:center; padding:10px; border:2px solid black; }
    .tile { background:#081A6B; color:#FFCC00; font-weight:bold; font-size:26px; text-align:center; padding:25px; border:2px solid black; cursor:pointer; }
    .tile.black { background:black; color:black; }
    .theme-controls { width:90%; margin:12px auto 24px auto; text-align:center; }
    .theme-btn { background:#1e3a8a; color:white; border:none; font-weight:600; }
    /* Jeopardy clue styling */
    .clue-wrap {
      background:#0b2d84;
      background:radial-gradient(ellipse at center,#1546b4 0%,#0b2d84 45%,#071a5e 100%);
      padding:48px 36px; text-align:center;
    }
    .clue-text {
      color:#ffffff; text-shadow:2px 2px 0 #000000;
      font-family: Georgia, 'Times New Roman', serif;
      font-weight:bold; letter-spacing:1px; font-size:38px; line-height:1.25; text-transform:uppercase;
    }
  "))),
  # preload audio
  tags$audio(id = "ddsound", src = "daily-double.mp3", type = "audio/mpeg", preload = "auto"),
  tags$audio(id = "theme",  src = "Jeopardy-theme-song.mp3", type = "audio/mpeg", preload = "auto"),
  
  h2("Jeopardy Board", align = "center", style = "color:white;"),
  uiOutput("boardUI"),
  div(class = "theme-controls",
      actionButton("theme_toggle", label = " Play Theme", icon = icon("play"), class = "theme-btn")
  )
)

server <- function(input, output, session) {
  
  # Board shape
  categories <- c("Python","Python Exam Qs","Linear Regression",
                  "Regression Diagnostics","Cross-validation","Random")
  values <- c("$200","$400","$600","$800","$1000")
  
  # ---- Read questions from spreadsheet (auto-refresh) ----
  read_questions <- reactiveFileReader(
    intervalMillis = 2000, session = session, filePath = "questions.xlsx",
    readFunc = function(path){
      df <- read_excel(path)
      names(df) <- tolower(names(df))
      stopifnot(all(c("category","value","question") %in% names(df)))
      df$category <- as.character(df$category)
      df$value    <- gsub("\\$","", as.character(df$value))
      df$question <- as.character(df$question)
      df$key      <- paste0(df$category, "_$", df$value)
      df
    }
  )
  
  # ---- Tile state (clicked->black) ----
  tile_states <- reactiveValues()
  for (cat in categories) for (val in values) tile_states[[paste(cat, val, sep = "_")]] <- FALSE
  
  # ---- Helpers to fetch and display a clue (text or image) ----
  get_clue <- function(tile_id){
    df <- read_questions()
    row <- df[df$key == tile_id, , drop = FALSE]
    if (nrow(row) == 0) return(list(type="text", payload="(No question found in spreadsheet for this tile.)"))
    q <- trimws(as.character(row$question[1]))
    is_img <- grepl("\\.(jpg|jpeg|png|gif)$", q, ignore.case = TRUE)
    if (is_img && file.exists(file.path("www", q))) {
      return(list(type="image", payload=q))
    }
    list(type="text", payload=q)
  }
  
  show_clue_modal <- function(clue){
    if (clue$type == "image") {
      showModal(modalDialog(easyClose=TRUE, footer=NULL, size="l",
                            tags$div(class="clue-wrap",
                                     tags$img(src = clue$payload,
                                              style="max-width:100%; height:auto; border:6px solid #ffcc00; box-shadow:0 0 20px #000;")
                            )
      ))
    } else {
      showModal(modalDialog(easyClose=TRUE, footer=NULL, size="l",
                            tags$div(class="clue-wrap",
                                     tags$div(class="clue-text", clue$payload)
                            )
      ))
    }
  }
  
  # ---- Theme Play/Stop (independent) ----
  themePlaying <- reactiveVal(FALSE)
  observeEvent(input$theme_toggle, {
    if (!themePlaying()) {
      runjs("const t=document.getElementById('theme'); t.currentTime ||= 0; t.play();")
      themePlaying(TRUE); updateActionButton(session, "theme_toggle", label=" Stop Theme", icon=icon("stop"))
    } else {
      runjs("const t=document.getElementById('theme'); t.pause(); t.currentTime=0;")
      themePlaying(FALSE); updateActionButton(session, "theme_toggle", label=" Play Theme", icon=icon("play"))
    }
  })
  
  # ---- Click handler: Daily Double + clue + toggle ----
  observeEvent(input$tile_click, {
    id <- input$tile_click
    
    # Daily Double: $800 in 5th column (Cross-validation)
    if (identical(id, "Cross-validation_$800")) {
      showModal(modalDialog(
        easyClose = TRUE, footer = NULL,
        tags$div(style="background:#000;text-align:center;",
                 tags$img(src="daily-double.jpg", style="width:100%;height:auto;"),
                 tags$h2("DAILY DOUBLE!", style="color:#FFCC00;font-weight:bold;margin:12px 0;")
        )
      ))
      runjs("
        const a=document.getElementById('ddsound'); a.currentTime=0; a.play();
        setTimeout(function(){
          $('#shiny-modal').one('hidden.bs.modal', function(){
            Shiny.setInputValue('dd_closed', '1', {priority:'event'});
          });
        }, 0);
      ")
    } else {
      show_clue_modal(get_clue(id))
    }
    
    # toggle tile to black (used) / restore on second click
    tile_states[[id]] <- !isTRUE(tile_states[[id]])
  })
  
  # After the DD splash closes, show its clue from the sheet
  observeEvent(input$dd_closed, {
    show_clue_modal(get_clue("Cross-validation_$800"))
  })
  
  # ---- Render board ----
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
