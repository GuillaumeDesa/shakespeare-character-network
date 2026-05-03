# Shakespeare Character Networks
# Single-file Shiny app — run with: shiny::runApp("app.R")

# ── Auto-install missing packages ─────────────────────────────────────────────
required_packages <- c(
  "shiny", "bslib", "dplyr", "tidyr", "stringr",
  "ggplot2", "reshape2", "scales", "visNetwork",
  "gutenbergr", "igraph"
)
missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(visNetwork)
library(gutenbergr)
library(igraph)

# ── Play catalogue ────────────────────────────────────────────────────────────
SHAKESPEARE_PLAYS <- c(
  "Macbeth"                     = 1533,
  "Hamlet"                      = 1524,
  "Othello"                     = 1531,
  "King Lear"                   = 1532,
  "A Midsummer Night's Dream"   = 1514,
  "Romeo and Juliet"            = 1513,
  "The Merchant of Venice"      = 2243,
  "Much Ado About Nothing"      = 2269,
  "Twelfth Night"               = 1526,
  "The Tempest"                 = 23042,
  "Julius Caesar"               = 1522,
  "Antony and Cleopatra"        = 1534,
  "Richard III"                 = 1542,
  "Henry V"                     = 2253,
  "The Taming of the Shrew"     = 1508
)

# ── Helpers ───────────────────────────────────────────────────────────────────

parse_play <- function(raw_lines) {
  # Scene headers vary across Gutenberg editions:
  #   "SCENE I."        (all-caps, own line)       — e.g. Macbeth 1533
  #   "Scene I."        (title-case, own line)      — e.g. Hamlet 1524
  #   "  Scene I. ..."  (indented, location follows on same line)
  #   "ACT I"           (act-only lines — NOT a scene boundary on their own)
  # Strategy: treat any line whose first non-space token is SCENE/Scene
  # as a scene boundary.  ACT-only lines are not counted as scenes
  # (they sit between scenes and would inflate the count to 5 for a 5-act play).
  tibble(raw = raw_lines) %>%
    mutate(
      is_scene = str_detect(raw, regex(
        "^\\s*scene\\s+[ivxlc0-9]+", ignore_case = TRUE
      )),
      scene = cumsum(is_scene)
    ) %>%
    filter(!is_scene, scene > 0, raw != "", !str_detect(raw, "^\\s*\\[")) %>%
    mutate(
      speaker_raw = str_match(raw, "^([A-Z][A-Z ]{1,30}[A-Z])[.:]")[, 2],
      dialogue    = str_replace(raw, "^[A-Z][A-Z ]{1,30}[A-Z][.:]\\s*", "")
    ) %>%
    fill(speaker_raw, .direction = "down") %>%
    filter(!is.na(speaker_raw)) %>%
    mutate(
      speaker = str_squish(speaker_raw),
      speaker = if_else(
        str_detect(speaker, regex(
          "^(ALL|BOTH|CHORUS|PROLOGUE|EPILOGUE|SCENE|EXEUNT|EXIT|ENTER)$|^ACT\\s+[IVXLC0-9]|\\bS$|ORCHARD|HOUSE|STREET|ROOM|GARDEN|COURT|FOREST|CAMP|FIELD|PALACE|CHURCH|PRISON|HALL|TOWER|CASTLE|TENT|ANTE"
        )),
        NA_character_, speaker
      )
    ) %>%
    filter(!is.na(speaker)) %>%
    group_by(scene, line = cumsum(!is.na(speaker_raw))) %>%
    summarise(
      speaker  = first(speaker),
      dialogue = str_c(dialogue, collapse = " "),
      .groups  = "drop"
    ) %>%
    filter(!is.na(speaker), speaker != "")
}

build_cooccurrences <- function(lines) {
  by_speaker_scene <- lines %>% count(scene, speaker)
  mat <- by_speaker_scene %>%
    acast(speaker ~ scene, fun.aggregate = length, fill = 0)
  mat <- mat[, colSums(mat) > 1, drop = FALSE]
  mat <- mat[rowSums(mat) > 0, , drop = FALSE]
  n_scenes <- ncol(mat)
  if (n_scenes == 0) return(list())
  lapply(seq_len(n_scenes), function(i) {
    sub <- as.matrix(mat[, seq_len(i), drop = FALSE])
    sub <- sub[rowSums(sub) > 0, , drop = FALSE]
    co  <- sub %*% t(sub)
    melt(co, varnames = c("Source", "Target"), value.name = "scenes") %>%
      filter(Source != Target, scenes > 0) %>%
      as_tibble()
  })
}

build_timeline <- function(lines) {
  by_speaker_scene <- lines %>% count(scene, speaker)
  mat <- by_speaker_scene %>%
    acast(speaker ~ scene, fun.aggregate = length, fill = 0)
  mat <- mat[rowSums(mat) > 0, , drop = FALSE]
  ordering <- if (nrow(mat) < 2) {
    rownames(mat)
  } else {
    norm <- mat / pmax(rowSums(mat), 1)
    h    <- hclust(dist(norm, method = "canberra"))
    h$labels[h$order]
  }
  by_speaker_scene %>%
    filter(n() > 1) %>%
    ungroup() %>%
    mutate(
      scene   = as.numeric(factor(scene)),
      speaker = factor(speaker, levels = ordering)
    )
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = tags$span(
    style = "font-family: 'Inter', sans-serif; font-weight: 700;
             letter-spacing: -0.03em; font-size: 1.25rem;",
    "Shakespeare Character Networks"
  ),
  theme = bs_theme(
    bootswatch   = "flatly",
    primary      = "#4e79a7",
    base_font    = font_google("Lato"),
    heading_font = font_google("Inter")
  ),
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap"
    ),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('downloadNetwork', function(filename) {
        var canvas = document.querySelector('#networkPlot canvas');
        if (!canvas) { alert('Network not ready yet.'); return; }
        var link = document.createElement('a');
        link.download = filename;
        link.href = canvas.toDataURL('image/png');
        link.click();
      });
    "))
  ),

  sidebar = sidebar(
    width = 290,

    selectInput(
      "play_id",
      label    = "Choose a play",
      choices  = SHAKESPEARE_PLAYS,
      selected = 1533
    ),

    hr(),

    sliderInput("scene",
      label = "Scene (cumulative network up to…)",
      min = 1, max = 28, value = 10, step = 1, ticks = FALSE
    ),

    sliderInput("min_cooc",
      label = "Minimum co-appearances (edge filter)",
      min = 1, max = 10, value = 1, step = 1, ticks = FALSE
    ),

    helpText(
      "Increase to reduce hairball density: only character pairs",
      "sharing at least this many scenes will be shown."
    ),

    hr(),

    tags$small(
      tags$b("How to read the network:"),
      tags$ul(
        tags$li("Node size = degree (number of connections)"),
        tags$li("Node colour = betweenness centrality (orange → high)"),
        tags$li("Edge thickness = number of shared scenes"),
        tags$li("Hover a node or edge for details")
      )
    ),

    hr(),

    tags$small(
      "Texts from ",
      tags$a("Project Gutenberg", href = "https://www.gutenberg.org",
             target = "_blank"),
      " via ",
      tags$a("gutenbergr", href = "https://docs.ropensci.org/gutenbergr/",
             target = "_blank"), "."
    )
  ),

  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Character co-occurrence network",
        actionButton(
          "exportNetwork", label = "PNG", icon = icon("download"),
          class = "btn btn-sm btn-outline-secondary py-0"
        )
      ),
      card_body(padding = 0,
        visNetworkOutput("networkPlot", width = "100%", height = "72vh")
      )
    ),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Character timeline",
        downloadButton(
          "exportTimeline", label = "PNG",
          class = "btn btn-sm btn-outline-secondary py-0"
        )
      ),
      card_body(padding = "0.5rem",
        plotOutput("timelinePlot", width = "100%", height = "72vh")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  play_data <- eventReactive(input$play_id, {
    req(input$play_id)
    withProgress(message = "Fetching play from Project Gutenberg…", {
      gid <- as.integer(input$play_id)
      raw <- tryCatch(
        gutenberg_download(gid, mirror = "http://aleph.gutenberg.org")$text,
        error = function(e) NULL
      )
      if (is.null(raw) || length(raw) == 0) {
        showNotification(
          "Could not download play. Check your internet connection.",
          type = "error"
        )
        return(NULL)
      }
      setProgress(0.4, detail = "Parsing…")
      lines <- parse_play(raw)
      setProgress(0.7, detail = "Building network…")
      cooc <- build_cooccurrences(lines)
      tl   <- build_timeline(lines)
      setProgress(1)
      list(lines = lines, cooc = cooc, timeline = tl, n_scenes = length(cooc))
    })
  }, ignoreNULL = FALSE)

  observe({
    pd <- play_data()
    req(!is.null(pd))
    updateSliderInput(session, "scene",
      max   = pd$n_scenes,
      value = min(input$scene, pd$n_scenes)
    )
  })

  output$networkPlot <- renderVisNetwork({
    pd <- play_data()
    req(!is.null(pd), input$scene <= pd$n_scenes)

    edges_raw <- pd$cooc[[input$scene]] %>% filter(scenes >= input$min_cooc)

    if (nrow(edges_raw) == 0) {
      return(visNetwork(
        nodes = data.frame(id = 1, label = "(no connections yet)"),
        edges = data.frame()
      ))
    }

    g   <- graph_from_data_frame(
      edges_raw %>% rename(weight = scenes),
      directed = FALSE,
      vertices = unique(c(as.character(edges_raw$Source),
                          as.character(edges_raw$Target)))
    )
    deg <- degree(g)
    btw <- betweenness(g, normalized = TRUE)

    nodes <- data.frame(
      id    = V(g)$name,
      label = V(g)$name,
      value = deg,
      title = paste0("<b>", V(g)$name, "</b><br>",
                     "Degree: ", deg, "<br>",
                     "Betweenness: ", round(btw, 3)),
      color = colorRampPalette(c("#4e79a7", "#f28e2b", "#e15759"))(100)[
        pmax(1, ceiling(btw * 99)) + 1
      ],
      stringsAsFactors = FALSE
    )

    edges <- edges_raw %>%
      transmute(
        from  = as.character(Source),
        to    = as.character(Target),
        value = scenes,
        title = paste0("Co-appearances: ", scenes)
      )

    visNetwork(nodes, edges, width = "100%", height = "420px") %>%
      visNodes(shape = "dot", font = list(size = 14, color = "#222"),
               borderWidth = 1.5) %>%
      visEdges(smooth = list(type = "continuous"),
               color  = list(color = "#aaaaaa", highlight = "#e15759")) %>%
      visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE),
                 nodesIdSelection = TRUE) %>%
      visPhysics(
        solver    = "barnesHut",
        barnesHut = list(gravitationalConstant = -8000,
                         springConstant = 0.04, springLength = 120),
        stabilization = list(iterations = 200)
      ) %>%
      visLayout(randomSeed = 42) %>%
      visInteraction(navigationButtons = TRUE, tooltipDelay = 100)
  })

  output$timelinePlot <- renderPlot({
    pd <- play_data()
    req(!is.null(pd))

    cur_sc   <- min(input$scene, pd$n_scenes)
    appeared <- pd$lines %>%
      filter(scene <= cur_sc) %>%
      distinct(speaker) %>%
      pull(speaker)

    pd$timeline %>%
      mutate(appeared = speaker %in% appeared) %>%
      ggplot(aes(scene, speaker)) +
      geom_path(aes(group = scene), colour = "grey70", linewidth = 0.4) +
      geom_point(aes(colour = appeared, size = n), alpha = 0.85) +
      geom_vline(xintercept = cur_sc, colour = "#e15759",
                 linetype = "dashed", linewidth = 0.8) +
      scale_colour_manual(
        values = c("TRUE" = "#4e79a7", "FALSE" = "#cccccc"),
        name   = NULL
      ) +
      scale_size_continuous(range = c(1, 5), guide = "none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
      labs(x = "Scene", y = NULL,
           title   = "Appearances by scene",
           caption = "Point size = number of lines · red line = current scene") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            legend.position  = "none",
            plot.title       = element_text(face = "bold"),
            axis.text.y      = element_text(size = 10))
  })

  # ── PNG export: network (JS canvas grab) ─────────────────────────────────
  observeEvent(input$exportNetwork, {
    session$sendCustomMessage(
      "downloadNetwork",
      paste0("character_network_scene", input$scene, ".png")
    )
  })

  # ── PNG export: timeline (server-side ggsave) ─────────────────────────────
  output$exportTimeline <- downloadHandler(
    filename = function() {
      paste0("character_timeline_scene", input$scene, ".png")
    },
    content = function(file) {
      pd <- play_data()
      if (is.null(pd)) return(invisible(NULL))
      cur_sc   <- min(input$scene, pd$n_scenes)
      appeared <- pd$lines %>%
        filter(scene <= cur_sc) %>%
        distinct(speaker) %>%
        pull(speaker)
      p <- pd$timeline %>%
        mutate(appeared = speaker %in% appeared) %>%
        ggplot(aes(scene, speaker)) +
        geom_path(aes(group = scene), colour = "grey70", linewidth = 0.4) +
        geom_point(aes(colour = appeared, size = n), alpha = 0.85) +
        geom_vline(xintercept = cur_sc, colour = "#e15759",
                   linetype = "dashed", linewidth = 0.8) +
        scale_colour_manual(
          values = c("TRUE" = "#4e79a7", "FALSE" = "#cccccc"),
          name   = NULL
        ) +
        scale_size_continuous(range = c(1, 5), guide = "none") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
        labs(x = "Scene", y = NULL,
             title   = "Appearances by scene",
             caption = "Point size = number of lines · red line = current scene") +
        theme_minimal(base_size = 13) +
        theme(panel.grid.minor = element_blank(),
              legend.position  = "none",
              plot.title       = element_text(face = "bold"),
              axis.text.y      = element_text(size = 10))
      ggsave(file, plot = p, device = "png",
             width = 10, height = 8, dpi = 150, bg = "white")
    }
  )
}

# ── Launch ────────────────────────────────────────────────────────────────────

shinyApp(ui = ui, server = server)
