# src/app.R
# Keep simulate.R and plot_clusters.R beside app.R.
# Launch from the parent folder with shiny::runApp("src").
# Install dependencies, if not present
pkgs <- c("shiny", "ggplot2", "bslib")

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

# Load package dependencies
invisible(lapply(pkgs, library, character.only = TRUE))

# src/app.R
# Keep simulate.R and plot_clusters.R beside app.R.
# Launch from the parent folder: shiny::runApp("src")
library(shiny)
library(ggplot2)
library(bslib)

dirs <- unique(c(getOption("cluster_app_dir", NULL), getwd(), file.path(getwd(), "src")))
found <- dirs[vapply(dirs, function(d) all(file.exists(file.path(d, c("simulate.R", "plot_clusters.R")))), logical(1))]
if (!length(found)) stop(paste("Cannot find simulate.R and plot_clusters.R together. Checked:", paste(dirs, collapse = "; ")), call. = FALSE)
src_dir <- normalizePath(found[1], mustWork = TRUE)
source(file.path(src_dir, "simulate.R"), local = TRUE, chdir = TRUE)
source(file.path(src_dir, "plot_clusters.R"), local = TRUE, chdir = TRUE)
if (!exists("simulate_clusters", mode = "function", inherits = FALSE)) stop("simulate.R must define simulate_clusters().")
if (!exists("plot_clusters", mode = "function", inherits = FALSE)) stop("plot_clusters.R must define plot_clusters().")

snake_case_names <- function(labels) {
  labels <- as.character(labels)
  originals <- unique(labels[!is.na(labels)])
  normalized <- gsub("([\\p{Lu}]+)([\\p{Lu}][\\p{Ll}])", "\\1_\\2", originals, perl = TRUE)
  normalized <- gsub("([\\p{Ll}\\p{N}])([\\p{Lu}])", "\\1_\\2", normalized, perl = TRUE)
  normalized <- tolower(trimws(normalized))
  normalized <- gsub("[^\\p{L}\\p{N}]+", "_", normalized, perl = TRUE)
  normalized <- gsub("^_+|_+$", "", normalized)
  normalized[!nzchar(normalized)] <- "cluster"
  normalized <- make.unique(normalized, sep = "_")
  normalized[match(labels, originals)]
}

# Seed is still a simulation input, but is omitted from the overview.
# Adjust aliases if simulate.R uses different attribute names.
metadata_keys <- list(
  names = c("group_names", "cluster_names"),
  centroids = c("centroids", "cluster_centroids"),
  separation = "separation", closeness = "closeness"
)
get_metadata <- function(df, keys) {
  attrs <- attributes(df)
  for (key in keys) if (key %in% names(attrs)) return(attrs[[key]])
  NULL
}
pretty_value <- function(v) {
  if (is.null(v) || !length(v)) return("Not recorded")
  if (!is.atomic(v)) return("Unrecognized attribute format")
  if (is.factor(v)) v <- as.character(v)
  text <- if (is.numeric(v)) format(v, digits = 6, trim = TRUE) else as.character(v)
  text[is.na(v)] <- "Not available"
  paste(text, collapse = ", ")
}
cluster_value <- function(value, label, index, n) {
  if (is.null(value)) return(NULL)
  if (!is.null(names(value)) && label %in% names(value)) return(value[[label]])
  if (length(value) == 1L) return(value[[1]])
  if (length(value) == n && !is.na(index)) return(value[[index]])
  NULL
}
centroid_matrix <- function(value) {
  if (is.list(value) && !is.data.frame(value)) {
    if (all(c("x", "y") %in% names(value))) {
      value <- tryCatch(as.data.frame(value[c("x", "y")]), error = function(e) NULL)
    } else if (length(value) && all(vapply(value, function(v) is.atomic(v) && length(v) == 2L, logical(1)))) {
      value <- do.call(rbind, value)
    }
  }
  if (!(is.matrix(value) || is.data.frame(value)) || ncol(value) < 2L) return(NULL)
  if (all(c("x", "y") %in% colnames(value))) value <- value[, c("x", "y"), drop = FALSE]
  else value <- value[, 1:2, drop = FALSE]
  as.matrix(value)
}

ui <- fluidPage(
  theme = bs_theme(version = 3, bootswatch = "sandstone"),
  tags$head(tags$style(HTML("
    .compact{font-size:13px;line-height:1.4;padding:10px}
    .compact h4{font-size:15px;margin:0 0 8px}
    .compact .form-group{margin-bottom:9px}
    .compact .control-label{font-size:13px;margin-bottom:4px;white-space:normal}
    .compact .form-control{height:32px;padding:4px 8px;font-size:13px}
    .compact .shiny-input-container{width:100%;max-width:100%}
    .compact .help-block{font-size:12px;margin:5px 0 8px}
    .compact .checkbox{margin:4px 0 6px}
    .compact .btn{min-height:32px;padding:5px 9px;font-size:12px;white-space:normal}
    .compact hr{margin:10px 0}
    .compact .selectize-control{margin-bottom:0}
    .compact .selectize-input{min-height:32px;padding:5px 28px 5px 8px;font-size:13px;line-height:20px}
    .compact .selectize-input input,.compact .selectize-dropdown{font-size:13px}
    .pair{display:flex;gap:8px;align-items:flex-end}.pair>div{flex:1;min-width:0}
    .compact input[type=number]{max-width:112px}
    .compact .cluster-name input{max-width:145px}
    .cluster-editor{max-height:340px;overflow:auto;padding-right:3px}
    .cluster-card{border:1px solid #ddd;border-radius:4px;padding:7px;margin-bottom:7px}
    .cluster-card-top{display:flex;align-items:center;justify-content:space-between;margin-bottom:5px}
    .cluster-remove{width:32px;height:32px;padding:4px!important}
    .cluster-card .form-group{margin-bottom:6px}
    .metadata{margin-top:16px}
    .overall-details{padding-bottom:12px;border-bottom:1px solid #ddd;margin-bottom:16px}
    .cluster-description{border:1px solid #ddd;border-radius:4px;padding:12px;margin-bottom:12px}
    .cluster-description h4{margin:0 0 10px;font-size:16px;overflow-wrap:anywhere}
    .detail-line{margin:4px 0;overflow-wrap:anywhere}
    @media(min-width:768px) and (max-width:1199px){
      .compact .pair{flex-direction:column;align-items:stretch;gap:0}
      .compact .pair>div{width:100%}
      .compact input[type=number],.compact .cluster-name input{max-width:100%}
    }
    @media(max-width:380px){.pair{flex-direction:column;gap:0;align-items:stretch}}
  "))),
  titlePanel("Clustered bivariate data simulator"),
  sidebarLayout(
    sidebarPanel(width = 3, class = "compact",
                 h4("Simulation inputs"),
                 div(class = "pair",
                     div(numericInput("n_total", "Observations", 300, min = 2, max = 100000, step = 1)),
                     div(numericInput("n_clusters", "Clusters", 3, min = 1, max = 100, step = 1))),
                 checkboxInput("specify_centroids", "Specify centroids", FALSE),
                 h4("Cluster settings"), div(class = "cluster-editor", uiOutput("cluster_rows")),
                 actionButton("add_cluster", "Add cluster", icon = icon("plus")),
                 helpText("Size 2 has twice the relative weight of size 1. Check Specify centroids for x/y. Minus removes one cluster; minimum 1."),
                 hr(), selectInput("layout", "Layout", c("circle", "line", "random"), "circle"),
                 div(class = "pair", div(numericInput("separation", "Separation", 3, step = 0.1)), div(numericInput("closeness", "Closeness", 1, step = 0.1))),
                 h4("Coordinate ranges"),
                 div(class = "pair", div(numericInput("x_min", "x minimum", -5, step = 0.1)), div(numericInput("x_max", "x maximum", 5, step = 0.1))),
                 div(class = "pair", div(numericInput("y_min", "y minimum", -5, step = 0.1)), div(numericInput("y_max", "y maximum", 5, step = 0.1))),
                 checkboxInput("use_seed", "Specify random seed", FALSE),
                 conditionalPanel("input.use_seed", numericInput("seed", "Random seed", 123, min = 0, max = .Machine$integer.max, step = 1)),
                 helpText("Unchecked passes seed = NULL."), actionButton("simulate", "Simulate data", class = "btn-primary"),
                 hr(), h4("CSV export"),
                 conditionalPanel("output.hasData", downloadButton("download_csv", "Download CSV")),
                 helpText("CSV class names use snake_case. Plot labels are unchanged. CSV excludes attributes.")),
    mainPanel(width = 9,
              h4("Simulated dataset"), plotOutput("scatter_plot", height = "500px"),
              div(class = "panel panel-default metadata",
                  div(class = "panel-heading", "Simulation details"),
                  div(class = "panel-body", uiOutput("metadata"))))
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  simulation_settings <- reactiveVal(NULL)
  status <- reactiveVal("Click Simulate data to generate a dataset.")
  finite <- function(z) is.numeric(z) && length(z) == 1L && is.finite(z)
  integer_value <- function(z) finite(z) && z == floor(z)
  rows_state <- reactiveVal(list(
    list(id = 1L, name = "Cluster 1", weight = 1, x = 0, y = 0),
    list(id = 2L, name = "Cluster 2", weight = 1, x = 0, y = 0),
    list(id = 3L, name = "Cluster 3", weight = 1, x = 0, y = 0)))
  next_id <- 3L
  delete_observers <- list()
  snapshot <- function() isolate(lapply(rows_state(), function(row) {
    for (field in c("name", "weight", "x", "y")) {
      value <- input[[paste0("cluster_", field, "_", row$id)]]
      if (!is.null(value)) row[[field]] <- value
    }
    row
  }))
  set_rows <- function(rows) { rows_state(rows); updateNumericInput(session, "n_clusters", value = length(rows)) }
  register_delete <- function(id) {
    force(id)
    delete_observers[[as.character(id)]] <<- observeEvent(input[[paste0("delete_", id)]], {
      rows <- snapshot()
      if (length(rows) <= 1L) return(invisible(NULL))
      keep <- vapply(rows, function(row) row$id != id, logical(1))
      if (!all(keep)) set_rows(rows[keep])
    }, ignoreInit = TRUE)
  }
  new_row <- function() {
    next_id <<- next_id + 1L
    register_delete(next_id)
    list(id = next_id, name = paste("Cluster", next_id), weight = 1, x = 0, y = 0)
  }
  for (id in 1:3) register_delete(id)
  session$onSessionEnded(function() { for (o in delete_observers) o$destroy() })
  output$cluster_rows <- renderUI({
    rows <- rows_state()
    tagList(lapply(seq_along(rows), function(i) {
      row <- rows[[i]]
      button <- actionButton(paste0("delete_", row$id), NULL, icon = icon("minus"), class = "cluster-remove", title = paste("Remove cluster", i), `aria-label` = paste("Remove cluster", i))
      if (length(rows) == 1L) button <- tagAppendAttributes(button, disabled = "disabled")
      div(class = "cluster-card", div(class = "cluster-card-top", strong(paste("Cluster", i)), button),
          div(class = "pair",
              div(class = "cluster-name", textInput(paste0("cluster_name_", row$id), "Name", row$name)),
              div(numericInput(paste0("cluster_weight_", row$id), "Relative size", row$weight, min = 0.01, step = 0.1))),
          conditionalPanel("input.specify_centroids", div(class = "pair",
                                                          div(numericInput(paste0("cluster_x_", row$id), "Centroid x", row$x, step = 0.1)),
                                                          div(numericInput(paste0("cluster_y_", row$id), "Centroid y", row$y, step = 0.1)))))
    }))
  })
  outputOptions(output, "cluster_rows", suspendWhenHidden = FALSE)
  observeEvent(input$n_clusters, {
    target <- input$n_clusters
    if (!integer_value(target) || target < 1 || target > 100) {
      showNotification("Clusters must be an integer from 1 to 100.", type = "warning")
      updateNumericInput(session, "n_clusters", value = length(rows_state()))
      return(invisible(NULL))
    }
    rows <- snapshot()
    if (target == length(rows)) return(invisible(NULL))
    if (target < length(rows)) rows <- rows[seq_len(target)]
    while (length(rows) < target) rows[[length(rows) + 1L]] <- new_row()
    set_rows(rows)
  }, ignoreInit = TRUE)
  observeEvent(input$add_cluster, {
    rows <- snapshot()
    if (length(rows) >= 100) { showNotification("Maximum 100 clusters.", type = "warning"); return(invisible(NULL)) }
    rows[[length(rows) + 1L]] <- new_row()
    set_rows(rows)
  }, ignoreInit = TRUE)
  
  observeEvent(input$simulate, {
    dataset(NULL)
    simulation_settings(NULL)
    tryCatch({
      if (!integer_value(input$n_total) || input$n_total < 2 || input$n_total > 100000) stop("Total observations must be an integer from 2 to 100000.")
      if (!integer_value(input$n_clusters) || input$n_clusters < 1 || input$n_clusters > 100 || input$n_clusters > input$n_total) stop("Cluster count must be between 1 and min(100, observations).")
      if (!finite(input$separation) || !finite(input$closeness)) stop("Separation and closeness must be finite numbers.")
      if (!(input$layout %in% c("circle", "line", "random"))) stop("Select a valid layout.")
      if (!all(vapply(list(input$x_min, input$x_max, input$y_min, input$y_max), finite, logical(1)))) stop("Range endpoints must be finite.")
      if (input$x_min >= input$x_max || input$y_min >= input$y_max) stop("Each range minimum must be below its maximum.")
      rows <- snapshot()
      if (length(rows) != input$n_clusters) stop("Wait for cluster settings to update, then retry.")
      if (!all(vapply(rows, function(row) finite(row$weight) && row$weight > 0, logical(1)))) stop("Relative sizes must be positive and finite.")
      group_sizes <- vapply(rows, function(row) as.numeric(row$weight), numeric(1))
      group_names <- vapply(rows, function(row) trimws(row$name), character(1))
      if (any(!nzchar(group_names)) || anyDuplicated(group_names)) stop("Use a non-empty, unique name for every cluster.")
      # simulate_clusters must interpret group_sizes as relative weights.
      seed <- NULL
      if (isTRUE(input$use_seed)) {
        if (!integer_value(input$seed) || input$seed < 0 || input$seed > .Machine$integer.max) stop("Supply a valid non-negative integer seed.")
        seed <- as.integer(input$seed)
      }
      centroids <- NULL
      if (isTRUE(input$specify_centroids)) {
        if (!all(vapply(rows, function(row) finite(row$x) && finite(row$y), logical(1)))) stop("Every centroid requires finite x/y.")
        centroids <- data.frame(x = vapply(rows, function(row) as.numeric(row$x), numeric(1)), y = vapply(rows, function(row) as.numeric(row$y), numeric(1)))
      }
      df <- simulate_clusters(n_clusters = as.integer(input$n_clusters), n_total = as.integer(input$n_total),
                              group_sizes = group_sizes, separation = input$separation, layout = input$layout,
                              closeness = input$closeness, x_range = c(input$x_min, input$x_max), y_range = c(input$y_min, input$y_max),
                              centroids = centroids, group_names = group_names, seed = seed)
      if (!is.data.frame(df) || !all(c("x", "y", "class") %in% names(df))) stop("simulate_clusters must return a dataframe with x, y, class.")
      if (!nrow(df) || !is.numeric(df$x) || !is.numeric(df$y)) stop("Data must be non-empty with numeric x/y.")
      if (!is.atomic(df$class) || !is.null(dim(df$class))) stop("class must be a vector.")
      if (any(!is.finite(df$x)) || any(!is.finite(df$y))) stop("x/y must be finite.")
      simulation_settings(list(names = group_names, n_clusters = length(rows)))
      dataset(df)
      status("Dataset generated successfully.")
    }, error = function(e) status(paste("Unable to generate data:", conditionMessage(e))))
  }, ignoreInit = TRUE)
  
  output$scatter_plot <- renderPlot({
    df <- dataset()
    validate(need(!is.null(df), status()))
    # No title inside the ggplot; the separate UI heading is unchanged.
    print(plot_clusters(data = df, x_label = "x", y_label = "y", title = NULL, show_centroids = TRUE))
  }, res = 110)
  
  output$metadata <- renderUI({
    df <- dataset()
    if (is.null(df)) return(p(status()))
    settings <- simulation_settings()
    values <- lapply(metadata_keys, function(keys) get_metadata(df, keys))
    group_order <- if (is.atomic(values$names) && length(values$names)) as.character(values$names) else settings$names
    observed <- unique(as.character(df$class[!is.na(df$class)]))
    labels <- if (all(observed %in% group_order)) unique(c(group_order, observed)) else observed
    centers <- centroid_matrix(values$centroids)
    line <- function(label, text) p(class = "detail-line", strong(paste0(label, ": ")), text)
    tagList(
      div(class = "overall-details", h4("Overall dataset"),
          line("Total size", paste(format(nrow(df), big.mark = ","), "observations")),
          line("Number of clusters", pretty_value(settings$n_clusters)),
          line("Separation", pretty_value(values$separation))),
      h4("Cluster descriptions"),
      p(class = "text-muted", "Sizes are actual observation counts. Centroids and closeness are read from dataset attributes; shared closeness applies to every cluster."),
      lapply(labels, function(label) {
        index <- match(label, group_order)
        n <- length(group_order)
        centroid <- NULL
        if (!is.null(centers)) {
          row_index <- if (!is.null(rownames(centers)) && label %in% rownames(centers)) match(label, rownames(centers)) else if (nrow(centers) == n) index else NA_integer_
          if (!is.na(row_index)) centroid <- centers[row_index, ]
        }
        centroid_text <- if (is.null(centroid)) "Not recorded or cannot be matched to this cluster" else paste0("x = ", pretty_value(centroid[1]), "; y = ", pretty_value(centroid[2]))
        div(class = "cluster-description", h4(label),
            line("Name", label),
            line("Size", paste(sum(as.character(df$class) == label, na.rm = TRUE), "observations")),
            line("Centroid", centroid_text),
            line("Closeness", pretty_value(cluster_value(values$closeness, label, index, n))))
      }),
      if (anyNA(df$class)) p(class = "text-muted", paste(sum(is.na(df$class)), "observations have no class label."))
    )
  })
  output$hasData <- reactive(!is.null(dataset()))
  outputOptions(output, "hasData", suspendWhenHidden = FALSE)
  output$download_csv <- downloadHandler(
    filename = function() paste0("clustered_bivariate_data_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- dataset()
      req(!is.null(df))
      # CSV-only conversion; assumes group names are returned in class.
      df$class <- snake_case_names(df$class)
      write.csv(df, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }, contentType = "text/csv")
}
shinyApp(ui = ui, server = server)

