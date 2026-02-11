################################################################################
# Developed by Dr. Matthew A. Lanham
# Email: mlanham1@butler.edu
# Date: 12/17/2025
################################################################################
# install.packages(c("shiny","shinyjs","readxl"))
library(shiny)
library(shinyjs)
library(readxl)

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    body { background:black; }
    .topbar { width:90%; margin:18px auto 0 auto; display:flex; gap:10px; align-items:center; }
    .board { display:grid; grid-template-columns:repeat(6,1fr); gap:5px; width:90%; margin:12px auto 0 auto; }
    .category { background:#081A6B; color:white; font-weight:bold; font-size:22px; text-align:center; padding:10px; border:2px solid black; }
    .tile { background:#081A6B; color:#FFCC00; font-weight:bold; font-size:26px; text-align:center; padding:25px; border:2px solid black; cursor:pointer; }
    .tile.black { background:black; color:black; }
    .tile.disabled { background:#222; color:#555; cursor:not-allowed; }
    .theme-btn { background:#1e3a8a; color:white; border:none; font-weight:600; }
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
  
  h2("Jeopardy Review", align = "center", style = "color:white;"),
  
  # Sheet selector + theme control
  div(class="topbar",
      selectInput("sheet", "Question set (sheet):", choices = character(0), width = "300px"),
      div(style="flex:1 1 auto;"),
      actionButton("theme_toggle", label = " Play Theme", icon = icon("play"), class = "theme-btn")
  ),
  
  uiOutput("boardUI")
)

server <- function(input, output, session) {
  
  # ---------- Sheet discovery ----------
  sheet_names <- reactiveFileReader(2000, session, "questions.xlsx",
                                    function(path) excel_sheets(path))
  observe({
    sheets <- sheet_names()
    req(length(sheets) > 0)
    sel <- if (!is.null(input$sheet) && input$sheet %in% sheets) input$sheet else sheets[1]
    updateSelectInput(session, "sheet", choices = sheets, selected = sel)
  })
  
  # ---------- Read selected sheet ----------
  questions_df <- reactive({
    req(input$sheet)
    df <- read_excel("questions.xlsx", sheet = input$sheet)
    
    # normalize header names: lower + underscores (so "Daily Double" -> "daily_double")
    nms <- tolower(trimws(names(df)))
    nms <- gsub("[^a-z0-9]+","_", nms)
    names(df) <- nms
    
    req(all(c("category","value","question") %in% names(df)))
    
    df$category <- as.character(df$category)
    df$value    <- gsub("\\$","", as.character(df$value))
    df$question <- as.character(df$question)
    df$value_num <- suppressWarnings(as.numeric(df$value))
    df$key      <- paste0(df$category, "_$", df$value)   # lookup key used throughout
    
    # ensure daily_double exists (0/1); tolerate missing column
    if (!"daily_double" %in% names(df)) {
      df$daily_double <- NA_real_
    }
    df$daily_double <- suppressWarnings(as.numeric(df$daily_double))
    df$daily_double[is.na(df$daily_double)] <- 0
    
    df
  })
  
  # Board shape derived from data
  categories <- reactive({
    unique(questions_df()$category)  # keep order of appearance
  })
  values <- reactive({
    v <- unique(questions_df()[, c("value","value_num")])
    v <- v[order(v$value_num), , drop = FALSE]
    paste0("$", v$value)
  })
  
  # ---------- Tile state with safe reset ----------
  tile_states <- reactiveValues()
  
  reset_states <- function() {
    isolate({
      existing <- names(reactiveValuesToList(tile_states))
      for (k in existing) tile_states[[k]] <- NULL
      if (length(categories()) && length(values())) {
        for (cat in categories()) for (val in values()) {
          tile_states[[paste(cat, val, sep = "_")]] <- FALSE
        }
      }
    })
  }
  observeEvent(list(categories(), values()), { reset_states() })
  
  # ---------- Helpers ----------
  id_to_key <- function(id) {
    parts <- strsplit(id, "_")[[1]]
    paste0(parts[1], "_$", gsub("\\$","", parts[2]))
  }
  
  get_clue <- function(tile_id){
    df <- questions_df()
    key <- id_to_key(tile_id)
    row <- df[df$key == key, , drop = FALSE]
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
                            )))
    } else {
      showModal(modalDialog(easyClose=TRUE, footer=NULL, size="l",
                            tags$div(class="clue-wrap",
                                     tags$div(class="clue-text", clue$payload)
                            )))
    }
  }
  
  # ---------- Theme play/stop (independent) ----------
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
  
  # ---------- Daily Double from spreadsheet ----------
  # Uses df$daily_double == 1 to identify the tile (by its key).
  dd_id <- reactive({
    df <- questions_df()
    dd_keys <- df$key[df$daily_double == 1]
    if (length(dd_keys) == 0) {
      return(NA_character_)
    }
    if (length(dd_keys) > 1) {
      showNotification("Multiple Daily Double cells flagged; using the first one found.", type = "warning", duration = 5)
    }
    # keys have the same pattern as our tile ids (e.g., 'Category_$800'), so we can return directly
    dd_keys[1]
  })
  
  # ---------- Tile click handler ----------
  observeEvent(input$tile_click, {
    id <- input$tile_click
    df <- questions_df()
    present <- any(df$key == id_to_key(id))
    
    if (!is.na(dd_id()) && identical(id, dd_id())) {
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
    } else if (present) {
      show_clue_modal(get_clue(id))
    }
    
    if (present) tile_states[[id]] <- !isTRUE(tile_states[[id]])
  })
  
  observeEvent(input$dd_closed, {
    if (!is.na(dd_id())) show_clue_modal(get_clue(dd_id()))
  })
  
  # ---------- Render dynamic board ----------
  output$boardUI <- renderUI({
    req(categories(), values())
    df <- questions_df()
    tiles <- list()
    
    # headers
    for (cat in categories()) tiles <- append(tiles, list(div(class="category", cat)))
    
    # rows
    for (val in values()) {
      for (cat in categories()) {
        id <- paste(cat, val, sep = "_")
        present <- any(df$key == id_to_key(id))
        classes <- if (!present) "tile disabled" else if (isTRUE(tile_states[[id]])) "tile black" else "tile"
        onclick_js <- if (present) sprintf("Shiny.setInputValue('tile_click','%s',{priority:'event'})", id) else ""
        label <- if (present && !isTRUE(tile_states[[id]])) val else ""
        tiles <- append(tiles, list(
          div(class = classes, id = id, label, onclick = onclick_js)
        ))
      }
    }
    div(class="board", tiles)
  })
}

shinyApp(ui, server)
