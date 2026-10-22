# src/app.R
# Keep simulate.R and plot_clusters.R beside this file.
# Launch from parent: shiny::runApp("src")
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

# Algorithm placeholders: insert implementations returning
# list(classification = labels_in_original_row_order, fit_statistics = dataframe).
# Clustering normally uses x/y only; supervised functions need evaluation logic.
run_kmeans <- function(data, n_clusters, seed) { NULL }
run_hierarchical <- function(data, n_clusters, seed) { NULL }
run_dbscan <- function(data, n_clusters, seed) { NULL }
run_lda <- function(data, n_clusters, seed) { NULL }
run_random_forest <- function(data, n_clusters, seed) { NULL }
algorithms <- list(
  kmeans = list(label = "K-means", run = run_kmeans),
  hierarchical = list(label = "Hierarchical clustering", run = run_hierarchical),
  dbscan = list(label = "DBSCAN", run = run_dbscan),
  lda = list(label = "Linear discriminant analysis", run = run_lda),
  random_forest = list(label = "Random forest", run = run_random_forest))

# CSV-only name conversion. Distinct names that normalize identically receive
# suffixes (_2, _3, ...), so different clusters are never merged in the export.
# Missing labels remain missing. Unicode letters and numbers are retained.
snake_case_names <- function(labels) {
  labels <- as.character(labels)
  originals <- unique(labels[!is.na(labels)])
  normalized <- tolower(trimws(originals))
  normalized <- gsub("([\\p{Lu}]+)([\\p{Lu}][\\p{Ll}])", "\\1_\\2", originals, perl = TRUE)
  normalized <- gsub("([\\p{Ll}\\p{N}])([\\p{Lu}])", "\\1_\\2", normalized, perl = TRUE)
  normalized <- tolower(trimws(normalized))
  normalized <- gsub("[^\\p{L}\\p{N}]+", "_", normalized, perl = TRUE)
  normalized <- gsub("^_+|_+$", "", normalized)
  normalized[!nzchar(normalized)] <- "cluster"
  normalized <- make.unique(normalized, sep = "_")
  normalized[match(labels, originals)]
}

# Adjust aliases to actual attribute names in simulate.R.
metadata_keys <- list(count = c("n_clusters", "number_of_clusters"),
                      names = c("group_names", "cluster_names"), sizes = c("cluster_sizes", "sizes", "group_sizes"),
                      centroids = c("centroids", "cluster_centroids"), separation = "separation", closeness = "closeness", seed = "seed")
get_metadata <- function(df, keys) {
  attrs <- attributes(df)
  for (key in keys) if (key %in% names(attrs)) return(attrs[[key]])
  NULL
}
readable_value <- function(value) {
  if (is.null(value)) return("Not supplied in the dataset attributes")
  if (is.factor(value)) value <- as.character(value)
  if (is.atomic(value)) {
    text <- if (is.numeric(value)) format(value, digits = 6, trim = TRUE) else as.character(value)
    text[is.na(value)] <- "Not available"
    if (!length(text)) return("No values supplied")
    nm <- names(value)
    if (!is.null(nm)) text <- ifelse(nzchar(nm), paste0(nm, ": ", text), text)
    return(paste(text, collapse = ", "))
  }
  if (is.list(value)) {
    parts <- vapply(value, readable_value, character(1))
    nm <- names(value)
    if (!is.null(nm)) parts <- ifelse(nzchar(nm), paste0(nm, ": ", parts), parts)
    return(paste(parts, collapse = "; "))
  }
  "Unsupported attribute format"
}
centroid_display <- function(value, cluster_names) {
  if (is.null(value)) return(p("Not supplied in the dataset attributes"))
  if (is.list(value) && !is.data.frame(value)) {
    if (all(c("x", "y") %in% names(value))) value <- tryCatch(as.data.frame(value[c("x", "y")]), error = function(e) value)
    else if (length(value) && all(vapply(value, function(v) is.atomic(v) && length(v) == 2L, logical(1)))) value <- do.call(rbind, value)
  }
  if ((is.matrix(value) || is.data.frame(value)) && ncol(value) >= 2L) {
    coords <- if (all(c("x", "y") %in% colnames(value))) value[, c("x", "y"), drop = FALSE] else value[, 1:2, drop = FALSE]
    labels <- if (is.atomic(cluster_names) && length(cluster_names) == nrow(coords)) as.character(cluster_names) else paste("Cluster", seq_len(nrow(coords)))
    return(tags$table(class = "table table-condensed table-striped",
                      tags$thead(tags$tr(tags$th("Cluster"), tags$th("x coordinate"), tags$th("y coordinate"))),
                      tags$tbody(lapply(seq_len(nrow(coords)), function(i) tags$tr(tags$td(labels[i]),
                                                                                   tags$td(readable_value(coords[i, 1])), tags$td(readable_value(coords[i, 2])))))))
  }
  p(readable_value(value))
}

ui <- fluidPage(
  theme = bs_theme(version = 3, bootswatch = "sandstone"),
  tags$head(tags$style(HTML("
    .compact{font-size:13px;line-height:1.4;padding:12px 14px}
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
    .compact .selectize-dropdown .option{padding:5px 8px}
    .pair{display:flex;gap:8px}.pair>div{flex:1;min-width:0}
    .simulation-sidebar{padding:10px}
    .simulation-sidebar input[type=number]{max-width:112px}
    .simulation-sidebar .cluster-name input{max-width:145px}
    .simulation-sidebar .pair{align-items:flex-end}
    .cluster-editor{max-height:340px;overflow:auto;padding-right:3px}
    .cluster-card{border:1px solid #ddd;border-radius:4px;padding:7px;margin-bottom:7px}
    .cluster-card-top{display:flex;align-items:center;justify-content:space-between;margin-bottom:5px}
    .cluster-remove{width:32px;height:32px;padding:4px!important}
    .cluster-card .form-group{margin-bottom:6px}
    .fit-controls{margin-top:12px}
    .fit-grid{display:grid;grid-template-columns:minmax(0,2fr) repeat(2,minmax(0,1fr));gap:10px;align-items:end}
    .fit-grid>div{min-width:0}
    .comparison{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:12px}
    .comparison>div{min-width:0}.comparison .panel-body{padding:6px}
    .comparison .panel-heading{font-size:13px}
    .metadata{margin-top:16px}.metadata-content{max-height:380px;overflow:auto}
    .metadata-field{margin-bottom:12px}.metadata-field h5{font-weight:bold;margin:0 0 4px}
    .metadata-field p{margin-bottom:0;overflow-wrap:anywhere}
    .results{max-height:260px;overflow:auto}
    @media(min-width:768px) and (max-width:1199px){
      .simulation-sidebar .pair{flex-direction:column;align-items:stretch;gap:0}
      .simulation-sidebar .pair>div{width:100%}
      .simulation-sidebar input[type=number],.simulation-sidebar .cluster-name input{max-width:100%}
    }
    @media(max-width:1100px){.comparison{grid-template-columns:1fr}.fit-grid{grid-template-columns:repeat(2,minmax(0,1fr))}.algorithm-choice{grid-column:1/-1}}
    @media(max-width:380px){.pair{flex-direction:column;gap:0}.fit-grid{grid-template-columns:1fr}}
  "))),
  titlePanel("Clustered bivariate data simulator"),
  sidebarLayout(
    sidebarPanel(width = 3, class = "compact simulation-sidebar",
                 h4("Simulation inputs"),
                 div(class = "pair", div(numericInput("n_total", "Observations", 300, min = 2, max = 100000, step = 1)), div(numericInput("n_clusters", "Clusters", 3, min = 1, max = 100, step = 1))),
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
                 conditionalPanel("output.hasResult", checkboxInput("include_classification", "Include resulting classification", FALSE)),
                 conditionalPanel("output.hasData", downloadButton("download_csv", "Download CSV")),
                 helpText("CSV class names use snake_case, e.g. control_group. Plot labels are unchanged. CSV excludes attributes.")),
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel("Simulated data", h4("Bivariate plot"), plotOutput("scatter_plot", height = "500px"),
                         div(class = "panel panel-default metadata", div(class = "panel-heading", "Simulation details"), div(class = "panel-body metadata-content", uiOutput("metadata")))),
                tabPanel("Clustering & classification",
                         div(class = "well compact fit-controls", h4("Apply an algorithm"),
                             div(class = "fit-grid",
                                 div(class = "algorithm-choice", selectInput("algorithm", "Algorithm", list(
                                   "Clustering" = c("K-means" = "kmeans", "Hierarchical clustering" = "hierarchical", "DBSCAN" = "dbscan"),
                                   "Classification (supervised)" = c("Linear discriminant analysis" = "lda", "Random forest" = "random_forest")))),
                                 div(numericInput("analysis_clusters", "Requested clusters", 3, min = 1, max = 100, step = 1)), div(numericInput("analysis_seed", "Algorithm seed", 123, min = 0, max = .Machine$integer.max, step = 1))),
                             actionButton("run_algorithm", "Run algorithm", class = "btn-primary"), helpText("Uses the latest simulated dataset. Requested clusters apply where relevant. Implement evaluation and extra parameters in your functions."), textOutput("analysis_status")),
                         h4("Compare original and assigned labels"),
                         div(class = "comparison",
                             div(class = "panel panel-default", div(class = "panel-heading", "Currently simulated dataset — true classes"), div(class = "panel-body", plotOutput("comparison_original_plot", height = "380px"))),
                             div(class = "panel panel-default", div(class = "panel-heading", "Algorithm result — assigned classes / clusters"), div(class = "panel-body", plotOutput("classification_plot", height = "380px")))),
                         helpText("Both plots share observations and axis limits. Cluster numbers and colours need not match true classes."),
                         h4("Resulting classification / cluster assignment"), div(class = "results", tableOutput("classification_table")), helpText("First 100 rows shown; CSV includes all rows."),
                         h4("Fit statistics"), div(class = "results", tableOutput("fit_statistics")), helpText("Statistics come from your function. Use the CSV checkbox to append the latest labels."))
              )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactiveVal(NULL)
  result <- reactiveVal(NULL)
  simulation_status <- reactiveVal("Click Simulate data to generate a dataset.")
  analysis_status <- reactiveVal("Generate data, choose an algorithm, then click Run algorithm.")
  finite <- function(z) is.numeric(z) && length(z) == 1L && is.finite(z)
  integer_value <- function(z) finite(z) && z == floor(z)
  rows_state <- reactiveVal(list(list(id = 1L, name = "Cluster 1", weight = 1, x = 0, y = 0), list(id = 2L, name = "Cluster 2", weight = 1, x = 0, y = 0), list(id = 3L, name = "Cluster 3", weight = 1, x = 0, y = 0)))
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
  new_row <- function() { next_id <<- next_id + 1L; register_delete(next_id); list(id = next_id, name = paste("Cluster", next_id), weight = 1, x = 0, y = 0) }
  for (id in 1:3) register_delete(id)
  session$onSessionEnded(function() { for (o in delete_observers) o$destroy() })
  output$cluster_rows <- renderUI({
    rows <- rows_state()
    tagList(lapply(seq_along(rows), function(i) {
      row <- rows[[i]]
      button <- actionButton(paste0("delete_", row$id), NULL, icon = icon("minus"), class = "cluster-remove", title = paste("Remove cluster", i), `aria-label` = paste("Remove cluster", i))
      if (length(rows) == 1L) button <- tagAppendAttributes(button, disabled = "disabled")
      div(class = "cluster-card", div(class = "cluster-card-top", strong(paste("Cluster", i)), button),
          div(class = "pair", div(class = "cluster-name", textInput(paste0("cluster_name_", row$id), "Name", row$name)), div(numericInput(paste0("cluster_weight_", row$id), "Relative size", row$weight, min = 0.01, step = 0.1))),
          conditionalPanel("input.specify_centroids", div(class = "pair", div(numericInput(paste0("cluster_x_", row$id), "Centroid x", row$x, step = 0.1)), div(numericInput(paste0("cluster_y_", row$id), "Centroid y", row$y, step = 0.1)))))
    }))
  })
  outputOptions(output, "cluster_rows", suspendWhenHidden = FALSE)
  observeEvent(input$n_clusters, {
    target <- input$n_clusters
    if (!integer_value(target) || target < 1 || target > 100) {
      showNotification("Clusters must be an integer from 1 to 100.", type = "warning")
      updateNumericInput(session, "n_clusters", value = length(rows_state())); return(invisible(NULL))
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
    rows[[length(rows) + 1L]] <- new_row(); set_rows(rows)
  }, ignoreInit = TRUE)
  observeEvent(input$simulate, {
    dataset(NULL); result(NULL); updateCheckboxInput(session, "include_classification", value = FALSE)
    analysis_status("Generate data, choose an algorithm, then click Run algorithm.")
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
      df <- simulate_clusters(n_clusters = as.integer(input$n_clusters), n_total = as.integer(input$n_total), group_sizes = group_sizes,
                              separation = input$separation, layout = input$layout, closeness = input$closeness,
                              x_range = c(input$x_min, input$x_max), y_range = c(input$y_min, input$y_max), centroids = centroids, group_names = group_names, seed = seed)
      if (!is.data.frame(df) || !all(c("x", "y", "class") %in% names(df))) stop("simulate_clusters must return a dataframe with x, y, class.")
      if (!nrow(df) || !is.numeric(df$x) || !is.numeric(df$y)) stop("Data must be non-empty with numeric x/y.")
      if (!is.atomic(df$class) || !is.null(dim(df$class))) stop("class must be a vector.")
      if (any(!is.finite(df$x)) || any(!is.finite(df$y))) stop("x/y must be finite.")
      dataset(df); simulation_status("Dataset generated successfully."); analysis_status("Choose an algorithm and click Run algorithm.")
      updateNumericInput(session, "analysis_clusters", value = input$n_clusters)
    }, error = function(e) simulation_status(paste("Unable to generate data:", conditionMessage(e))))
  }, ignoreInit = TRUE)
  observeEvent(input$run_algorithm, {
    result(NULL); updateCheckboxInput(session, "include_classification", value = FALSE)
    tryCatch({
      df <- dataset()
      if (is.null(df)) stop("Generate data first.")
      method <- algorithms[[input$algorithm]]
      if (is.null(method)) stop("Select a valid algorithm.")
      if (!integer_value(input$analysis_clusters) || input$analysis_clusters < 1 || input$analysis_clusters > min(100, nrow(df))) stop("Requested clusters must be between 1 and min(100, observations).")
      if (!integer_value(input$analysis_seed) || input$analysis_seed < 0 || input$analysis_seed > .Machine$integer.max) stop("Supply a valid algorithm seed.")
      fitted <- method$run(data = df, n_clusters = as.integer(input$analysis_clusters), seed = as.integer(input$analysis_seed))
      if (is.null(fitted)) { analysis_status(paste(method$label, "is a placeholder. Insert your implementation first.")); return(invisible(NULL)) }
      if (!is.list(fitted) || !all(c("classification", "fit_statistics") %in% names(fitted))) stop("Return a list with classification and fit_statistics.")
      labels <- fitted$classification
      if (is.null(labels) || !is.atomic(labels) || !is.null(dim(labels)) || length(labels) != nrow(df)) stop("Return one label per original row, in original order.")
      if (!is.data.frame(fitted$fit_statistics)) stop("fit_statistics must be a dataframe.")
      result(list(labels = labels, statistics = fitted$fit_statistics, method = method$label))
      analysis_status(paste("Completed:", method$label, "— results reflect the last run."))
    }, error = function(e) analysis_status(paste("Unable to fit:", conditionMessage(e))))
  }, ignoreInit = TRUE)
  limits <- reactive({
    df <- dataset(); req(!is.null(df))
    padded <- function(values) { r <- range(values); padding <- if (diff(r) == 0) max(1, abs(r[1]) * 0.05) else diff(r) * 0.05; r + c(-padding, padding) }
    list(x = padded(df$x), y = padded(df$y))
  })
  output$scatter_plot <- renderPlot({
    df <- dataset(); validate(need(!is.null(df), simulation_status()))
    print(plot_clusters(data = df, x_label = "x", y_label = "y", title = "Simulated data", show_centroids = TRUE))
  }, res = 110)
  output$comparison_original_plot <- renderPlot({
    df <- dataset(); validate(need(!is.null(df), simulation_status()))
    p <- plot_clusters(data = df, x_label = "x", y_label = "y", title = "Original classes", show_centroids = TRUE)
    if (!inherits(p, "ggplot")) stop("plot_clusters must return a ggplot for shared comparison axes.")
    lim <- limits(); print(p + coord_cartesian(xlim = lim$x, ylim = lim$y, expand = FALSE) + theme(legend.position = "bottom"))
  }, res = 110)
  output$classification_plot <- renderPlot({
    df <- dataset(); validate(need(!is.null(df), simulation_status())); fitted <- result()
    validate(need(!is.null(fitted), "Run an implemented algorithm to display assigned labels."))
    classified_data <- data.frame(x = df$x, y = df$y, class = factor(fitted$labels))
    p <- plot_clusters(data = classified_data, x_label = "x", y_label = "y", title = fitted$method, show_centroids = TRUE)
    if (!inherits(p, "ggplot")) stop("plot_clusters must return a ggplot for shared comparison axes.")
    lim <- limits(); print(p + coord_cartesian(xlim = lim$x, ylim = lim$y, expand = FALSE) + theme(legend.position = "bottom"))
  }, res = 110)
  output$metadata <- renderUI({
    df <- dataset(); if (is.null(df)) return(p(simulation_status()))
    values <- lapply(metadata_keys, function(keys) get_metadata(df, keys))
    field <- function(label, content) div(class = "metadata-field", h5(label), content)
    sizes <- values$sizes
    if (is.atomic(sizes) && is.null(names(sizes)) && is.atomic(values$names) && length(sizes) == length(values$names)) names(sizes) <- as.character(values$names)
    tagList(field("Number of clusters", p(readable_value(values$count))), field("Name of clusters", p(readable_value(values$names))),
            field("Size of clusters", p(readable_value(sizes))), field("Cluster centroids", centroid_display(values$centroids, values$names)),
            field("Separation", p(readable_value(values$separation))), field("Closeness", p(readable_value(values$closeness))),
            field("Seed", p(if (is.null(values$seed)) "Not recorded (or no fixed seed supplied)" else readable_value(values$seed))))
  })
  output$analysis_status <- renderText(analysis_status())
  output$classification_table <- renderTable({
    fitted <- result(); req(!is.null(fitted)); df <- dataset()
    head(data.frame(row = seq_len(nrow(df)), x = df$x, y = df$y, true_class = df$class, resulting_classification = fitted$labels), 100)
  }, striped = TRUE, bordered = TRUE, spacing = "xs", digits = 4)
  output$fit_statistics <- renderTable({
    fitted <- result(); validate(need(!is.null(fitted), "Fit statistics appear after an implemented algorithm runs."))
    validate(need(nrow(fitted$statistics) > 0, "No fit statistics returned.")); fitted$statistics
  }, striped = TRUE, bordered = TRUE, spacing = "xs", digits = 4)
  output$hasData <- reactive(!is.null(dataset())); output$hasResult <- reactive(!is.null(result()))
  outputOptions(output, "hasData", suspendWhenHidden = FALSE); outputOptions(output, "hasResult", suspendWhenHidden = FALSE)
  output$download_csv <- downloadHandler(filename = function() paste0("clustered_bivariate_data_", Sys.Date(), ".csv"),
                                         content = function(file) {
                                           df <- dataset(); req(!is.null(df)); fitted <- result()
                                           # Convert only the export copy, using actual class names, not factor IDs.
                                           # Assumes simulate_clusters stores the supplied group names in class.
                                           df$class <- snake_case_names(df$class)
                                           if (isTRUE(input$include_classification) && !is.null(fitted)) {
                                             columns <- tail(make.unique(c(names(df), "resulting_classification", "classification_method")), 2)
                                             df[[columns[1]]] <- fitted$labels; df[[columns[2]]] <- rep(fitted$method, nrow(df))
                                           }
                                           write.csv(df, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
                                         }, contentType = "text/csv")
}
shinyApp(ui = ui, server = server)
