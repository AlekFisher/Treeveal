# app.R

library(shiny)
library(bslib)
library(DT)
library(rpart)
library(rpart.plot)
library(withr)
library(jsonlite)

library(ellmer)
library(shinychat)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

ui <- page_sidebar(
  title = "AI Decision Tree Explorer",
  sidebar = sidebar(
    width = 360,

    # ---- Data ----
    card(
      card_header("1) Data"),
      selectInput("example", "Use example data", choices = c("None", "iris", "mtcars"), selected = "iris"),
      fileInput("file", "…or upload a CSV", accept = c(".csv")),
      checkboxInput("chars_to_factor", "Convert character columns to factors", TRUE),
      helpText("Tip: if your CSV has lots of text columns, converting to factors helps rpart.")
    ),

    # ---- Model ----
    card(
      card_header("2) Tree settings"),
      uiOutput("outcome_ui"),
      uiOutput("predictors_ui"),
      selectInput("task", "Task", choices = c("Auto", "Classification", "Regression"), selected = "Auto"),
      sliderInput("train_prop", "Train share", min = 0.5, max = 0.9, value = 0.8, step = 0.05),
      numericInput("seed", "Random seed", value = 1, min = 1),
      sliderInput("cp", "Complexity parameter (cp)", min = 0, max = 0.1, value = 0.01, step = 0.001),
      numericInput("minbucket", "minbucket", value = 10, min = 1),
      actionButton("fit", "Fit / Refit tree", class = "btn-primary")
    ),

    # ---- AI ----
    card(
      card_header("3) AI assistant (ellmer tools)"),
      textInput("llm_model", "OpenAI model", value = "gpt-4.1"),
      textAreaInput(
        "domain_context",
        "Context for the AI (optional)",
        placeholder = "Example: 'This is churn; outcome=1 means churned. False negatives are costly.'",
        rows = 3
      ),
      checkboxInput("allow_data_sample", "Allow AI tool to return a small data sample (first 100 rows)", FALSE),
      actionButton("reset_ai", "Reset AI chat with latest model/tree", class = "btn-secondary"),
      helpText("Reset chat after refitting so the assistant gets fresh context.")
    )
  ),

  # Main area
  layout_columns(
    col_widths = c(8, 4),

    # Left: results
    card(
      full_screen = TRUE,
      navset_card_tab(
        nav_panel(
          "Tree",
          plotOutput("tree_plot", height = "650px"),
          accordion(
            accordion_panel("Tree (print())", verbatimTextOutput("tree_text")),
            accordion_panel("Rules", verbatimTextOutput("tree_rules")),
            accordion_panel("Metrics (simple holdout)", verbatimTextOutput("metrics_text"))
          )
        ),
        nav_panel(
          "Data preview",
          DTOutput("data_tbl")
        )
      )
    ),

    # Right: chat
    card(
      full_screen = TRUE,
      card_header("Ask the AI about this tree"),
      chat_ui(
        "chat",
        height = "820px",
        messages =
          paste0(
            "**Try these:**\n\n",
            "* <span class='suggestion submit'>Summarize the tree in plain language.</span>\n",
            "* <span class='suggestion submit'>Which splits define the highest-risk group?</span>\n",
            "* <span class='suggestion submit'>What variables matter most, and why?</span>\n",
            "* <span class='suggestion submit'>Does this look overfit? Suggest cp/minbucket changes.</span>\n",
            "* <span class='suggestion submit'>Use predict_case with: {\"Sepal.Length\":5.1,\"Sepal.Width\":3.5,\"Petal.Length\":1.4,\"Petal.Width\":0.2}</span>\n"
          )
      )
    )
  ),

  fillable = TRUE
)

server <- function(input, output, session) {

  # Per-session state (NOT reactive objects so tools can read it safely)
  state <- new.env(parent = emptyenv())
  state$df_raw <- NULL
  state$df_model <- NULL
  state$outcome <- NULL
  state$predictors <- NULL
  state$task <- NULL
  state$tree <- NULL
  state$train_idx <- NULL
  state$test_idx <- NULL
  state$metrics <- NULL

  state_version <- reactiveVal(0)
  chat_client <- reactiveVal(NULL)

  # -------- Data loading --------
  df_raw <- reactive({
    if (input$example != "None") {
      return(switch(input$example, iris = iris, mtcars = mtcars))
    }
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })

  df_prepped <- reactive({
    df <- df_raw()
    if (isTRUE(input$chars_to_factor)) {
      for (nm in names(df)) {
        if (is.character(df[[nm]])) df[[nm]] <- as.factor(df[[nm]])
      }
    }
    df
  })

  observeEvent(df_prepped(), {
    state$df_raw <- df_prepped()
    state_version(state_version() + 1)
  }, ignoreInit = TRUE)

  # -------- Variable selection UI --------
  output$outcome_ui <- renderUI({
    df <- df_prepped()
    cols <- names(df)
    validate(need(length(cols) >= 2, "Dataset must have at least 2 columns."))
    selected <- input$outcome %||% cols[[length(cols)]]
    if (!selected %in% cols) selected <- cols[[length(cols)]]
    selectInput("outcome", "Outcome (Y)", choices = cols, selected = selected)
  })

  output$predictors_ui <- renderUI({
    df <- df_prepped()
    cols <- names(df)
    y <- input$outcome %||% cols[[length(cols)]]
    choices <- setdiff(cols, y)
    selected <- input$predictors %||% choices
    selected <- intersect(selected, choices)

    selectizeInput(
      "predictors",
      "Predictors (X) — remove variables here",
      choices = choices,
      selected = selected,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  })

  # Keep predictors choices consistent when outcome changes
  observeEvent(list(df_prepped(), input$outcome), {
    df <- df_prepped()
    cols <- names(df)
    y <- input$outcome %||% cols[[length(cols)]]
    choices <- setdiff(cols, y)

    current <- input$predictors %||% choices
    selected <- intersect(current, choices)

    updateSelectizeInput(session, "predictors", choices = choices, selected = selected, server = TRUE)
  }, ignoreInit = TRUE)

  # -------- Fit tree --------
  observeEvent(input$fit, {
    df <- df_prepped()
    req(input$outcome, input$predictors)
    validate(need(length(input$predictors) >= 1, "Pick at least 1 predictor."))

    outcome <- input$outcome
    preds <- input$predictors
    df_model <- df[, c(outcome, preds), drop = FALSE]

    # Decide task
    y <- df_model[[outcome]]
    task <- input$task
    if (task == "Auto") {
      # Heuristic: factor/character => classification; numeric with <=10 unique => classification
      uniq <- length(unique(y[!is.na(y)]))
      if (is.factor(y) || is.character(y) || (is.numeric(y) && uniq <= 10)) {
        task_use <- "Classification"
      } else {
        task_use <- "Regression"
      }
    } else {
      task_use <- task
    }

    if (task_use == "Classification") df_model[[outcome]] <- as.factor(df_model[[outcome]])
    if (task_use == "Regression") df_model[[outcome]] <- as.numeric(df_model[[outcome]])

    set.seed(input$seed)
    n <- nrow(df_model)
    train_n <- max(1, floor(input$train_prop * n))
    train_idx <- sample.int(n, train_n)
    test_idx <- setdiff(seq_len(n), train_idx)

    fml <- as.formula(paste(outcome, "~", paste(preds, collapse = " + ")))
    fit <- rpart::rpart(
      fml,
      data = df_model[train_idx, , drop = FALSE],
      method = if (task_use == "Classification") "class" else "anova",
      control = rpart::rpart.control(cp = input$cp, minbucket = input$minbucket),
      na.action = rpart::na.rpart
    )

    # Simple holdout metrics
    metrics <- list(task = task_use)
    if (length(test_idx) >= 1) {
      if (task_use == "Classification") {
        pred <- predict(fit, newdata = df_model[test_idx, , drop = FALSE], type = "class")
        actual <- df_model[[outcome]][test_idx]
        cm <- table(actual, pred, useNA = "ifany")
        acc <- if (sum(cm) > 0) sum(diag(cm)) / sum(cm) else NA_real_
        metrics$confusion_matrix <- cm
        metrics$accuracy <- acc
      } else {
        pred <- predict(fit, newdata = df_model[test_idx, , drop = FALSE])
        actual <- df_model[[outcome]][test_idx]
        rmse <- sqrt(mean((pred - actual)^2, na.rm = TRUE))
        mae <- mean(abs(pred - actual), na.rm = TRUE)
        metrics$rmse <- rmse
        metrics$mae <- mae
      }
    }

    # Store
    state$df_model <- df_model
    state$outcome <- outcome
    state$predictors <- preds
    state$task <- task_use
    state$tree <- fit
    state$train_idx <- train_idx
    state$test_idx <- test_idx
    state$metrics <- metrics

    state_version(state_version() + 1)
  })

  # Reactive accessors for outputs
  tree_obj <- reactive({ state_version(); state$tree })
  task_use <- reactive({ state_version(); state$task })

  # -------- Outputs --------
  output$tree_plot <- renderPlot({
    req(tree_obj())
    fit <- tree_obj()
    extra <- if (task_use() == "Classification") 104 else 101
    rpart.plot::rpart.plot(
      fit,
      type = 2,
      extra = extra,
      fallen.leaves = TRUE,
      under = TRUE,
      tweak = 1.1
    )
  })

  output$tree_text <- renderText({
    req(tree_obj())
    paste(capture.output(print(tree_obj())), collapse = "\n")
  })

  output$tree_rules <- renderText({
    req(tree_obj())
    txt <- capture.output(rpart.plot::rpart.rules(tree_obj()))
    paste(txt, collapse = "\n")
  })

  output$metrics_text <- renderText({
    state_version()
    m <- state$metrics
    if (is.null(m)) return("No metrics yet. Fit a tree.")
    if (m$task == "Classification") {
      paste0(
        "Task: Classification\n",
        "Accuracy (holdout): ", sprintf("%.3f", m$accuracy), "\n\n",
        "Confusion matrix:\n",
        paste(capture.output(m$confusion_matrix), collapse = "\n")
      )
    } else {
      paste0(
        "Task: Regression\n",
        "RMSE (holdout): ", sprintf("%.4f", m$rmse), "\n",
        "MAE (holdout):  ", sprintf("%.4f", m$mae)
      )
    }
  })

  output$data_tbl <- renderDT({
    df <- df_prepped()
    DT::datatable(head(df, 200), options = list(pageLength = 10, scrollX = TRUE))
  })

  # --------- Tool functions for ellmer ---------
  make_tools <- function() {
    tool_get_data_schema <- ellmer::tool(
      function(max_cols = 50L) {
        df <- state$df_model %||% state$df_raw
        if (is.null(df)) ellmer::tool_reject("No data loaded yet.")
        ellmer::df_schema(df, max_cols = max_cols)
      },
      name = "get_data_schema",
      description = "Return a compact schema/summary of the dataset (column names, types, missingness, ranges, levels).",
      arguments = list(
        max_cols = ellmer::type_integer("Maximum columns to include (default 50).", required = FALSE)
      )
    )

    tool_get_data_sample <- ellmer::tool(
      function(n = 100L) {
        if (!isTRUE(input$allow_data_sample)) {
          ellmer::tool_reject("Data sample sharing is disabled in the UI.")
        }
        df <- state$df_model %||% state$df_raw
        if (is.null(df)) ellmer::tool_reject("No data loaded yet.")
        head(df, n)
      },
      name = "get_data_sample",
      description = "Return the first n rows of the dataset (ONLY if the user enabled it).",
      arguments = list(
        n = ellmer::type_integer("Number of rows (max 200 recommended).", required = FALSE)
      )
    )

    tool_get_tree_text <- ellmer::tool(
      function() {
        if (is.null(state$tree)) ellmer::tool_reject("No tree has been fit yet.")
        paste(capture.output(print(state$tree)), collapse = "\n")
      },
      name = "get_tree_text",
      description = "Return the printed rpart tree output including splits and node summaries.",
      arguments = list()
    )

    tool_get_tree_rules <- ellmer::tool(
      function() {
        if (is.null(state$tree)) ellmer::tool_reject("No tree has been fit yet.")
        paste(capture.output(rpart.plot::rpart.rules(state$tree)), collapse = "\n")
      },
      name = "get_tree_rules",
      description = "Return the tree as readable if/then rules (from rpart.plot::rpart.rules).",
      arguments = list()
    )

    tool_get_tree_stats <- ellmer::tool(
      function() {
        if (is.null(state$tree)) ellmer::tool_reject("No tree has been fit yet.")
        fit <- state$tree
        frame <- fit$frame
        n_leaves <- sum(frame$var == "<leaf>")
        n_splits <- sum(frame$var != "<leaf>")
        vi <- fit$variable.importance
        if (!is.null(vi)) vi <- sort(vi, decreasing = TRUE)

        list(
          task = state$task,
          outcome = state$outcome,
          predictors = state$predictors,
          n_train = length(state$train_idx %||% integer()),
          n_test = length(state$test_idx %||% integer()),
          n_splits = n_splits,
          n_leaves = n_leaves,
          cp = fit$control$cp,
          minbucket = fit$control$minbucket,
          variable_importance = vi
        )
      },
      name = "get_tree_stats",
      description = "Return high-level tree stats including number of splits/leaves and variable importance.",
      arguments = list()
    )

    tool_get_tree_plot <- ellmer::tool(
      function(width = 1200L, height = 800L) {
        if (is.null(state$tree)) ellmer::tool_reject("No tree has been fit yet.")
        tmp <- withr::local_tempfile(fileext = ".png")
        grDevices::png(tmp, width = width, height = height, res = 150)
        on.exit(grDevices::dev.off(), add = TRUE)

        extra <- if (state$task == "Classification") 104 else 101
        rpart.plot::rpart.plot(state$tree, type = 2, extra = extra, fallen.leaves = TRUE, under = TRUE)

        ellmer::content_image_file(tmp)
      },
      name = "get_tree_plot",
      description = "Render the current decision tree plot as a PNG image and return it (for vision-capable models).",
      arguments = list(
        width  = ellmer::type_integer("Image width in pixels (default 1200).", required = FALSE),
        height = ellmer::type_integer("Image height in pixels (default 800).", required = FALSE)
      )
    )

    tool_predict_case <- ellmer::tool(
      function(case_json) {
        if (is.null(state$tree) || is.null(state$df_model)) {
          ellmer::tool_reject("No fitted tree/data available yet.")
        }
        fit <- state$tree
        df_train <- state$df_model
        preds <- state$predictors

        x <- jsonlite::fromJSON(case_json)
        if (!is.list(x)) ellmer::tool_reject("case_json must decode to a JSON object (named fields).")

        # Build 1-row newdata with all predictors present
        newdata <- as.data.frame(as.list(rep(NA, length(preds))))
        names(newdata) <- preds

        for (nm in intersect(names(x), preds)) newdata[[nm]] <- x[[nm]]

        # Coerce types to match training data
        for (nm in preds) {
          if (is.factor(df_train[[nm]])) {
            newdata[[nm]] <- as.factor(newdata[[nm]])
            levels(newdata[[nm]]) <- levels(df_train[[nm]])
          } else if (is.numeric(df_train[[nm]])) {
            newdata[[nm]] <- suppressWarnings(as.numeric(newdata[[nm]]))
          }
        }

        node <- predict(fit, newdata = newdata, type = "where")
        path <- rpart::path.rpart(fit, nodes = node, print.it = FALSE)[[1]]

        if (state$task == "Classification") {
          prob <- predict(fit, newdata = newdata, type = "prob")
          cls <- predict(fit, newdata = newdata, type = "class")
          list(
            prediction_class = as.character(cls),
            prediction_prob = prob[1, , drop = TRUE],
            terminal_node = as.integer(node),
            decision_path = path
          )
        } else {
          val <- predict(fit, newdata = newdata)
          list(
            prediction_value = as.numeric(val),
            terminal_node = as.integer(node),
            decision_path = path
          )
        }
      },
      name = "predict_case",
      description = "Predict for one hypothetical case. Input is a JSON object of predictor values. Returns prediction + the decision path.",
      arguments = list(
        case_json = ellmer::type_string("JSON object mapping predictor names to values.")
      )
    )

    list(
      tool_get_data_schema,
      tool_get_data_sample,
      tool_get_tree_text,
      tool_get_tree_rules,
      tool_get_tree_stats,
      tool_get_tree_plot,
      tool_predict_case
    )
  }

  build_system_prompt <- function() {
    # Keep the initial prompt lightweight; encourage tool use for details
    df <- state$df_model %||% state$df_raw
    schema <- if (!is.null(df)) {
      paste(ellmer::df_schema(df), collapse = "\n")
    } else {
      "No dataset loaded yet."
    }

    model_ctx <- c(
      "You are a careful data science assistant helping a user interpret a decision tree trained with rpart in R.",
      "",
      "Ground rules:",
      "- Do NOT invent splits, thresholds, or metrics. If you need specifics, call the tools.",
      "- Prefer calling get_tree_text / get_tree_rules / get_tree_stats before explaining.",
      "- If the user asks about the visual layout (depth, balance, leaf purity), call get_tree_plot too (vision-capable models only).",
      "- Do not claim causality; this is a predictive model.",
      "- If information is missing, ask a focused follow-up question.",
      "",
      "User-provided context:",
      if (nzchar(input$domain_context)) input$domain_context else "(none)",
      "",
      "Dataset schema:",
      schema,
      "",
      "Available tools:",
      "- get_data_schema(max_cols)",
      "- get_data_sample(n)  (may be disabled)",
      "- get_tree_text()",
      "- get_tree_rules()",
      "- get_tree_stats()",
      "- get_tree_plot(width,height)",
      "- predict_case(case_json)",
      "",
      "When summarizing the tree, include:",
      "1) the top splits / segments, 2) which subgroup has highest predicted outcome, 3) practical cautions (overfit, leakage, class imbalance)."
    )

    paste(model_ctx, collapse = "\n")
  }

  # -------- AI chat wiring --------
  observeEvent(input$reset_ai, {
    # Reset UI
    shinychat::chat_clear("chat")

    # Create a fresh chat client using latest state
    # (Explicit model selection is recommended by ellmer docs.)
    # chat_openai defaults may change over time.
    client <- NULL
    err <- NULL

    tryCatch({
      client <- ellmer::chat_openai(
        model = input$llm_model,
        system_prompt = build_system_prompt(),
        echo = "none"
      )

      # Register tools
      tools <- make_tools()
      for (t in tools) client$register_tool(t)

    }, error = function(e) {
      err <<- conditionMessage(e)
    })

    if (!is.null(err)) {
      shinychat::chat_append("chat", paste0(
        "⚠️ Could not initialize the AI client.\n\n",
        "Error: ", err, "\n\n",
        "Common causes:\n",
        "- Missing OPENAI_API_KEY in your environment\n",
        "- Invalid model name\n",
        "- Network/firewall restrictions"
      ))
      chat_client(NULL)
      return()
    }

    chat_client(client)
    shinychat::chat_append("chat", "✅ AI initialized. Ask me about the tree (I can call tools to inspect it).")
  })

  observeEvent(input$chat_user_input, {
    user_msg <- input$chat_user_input
    if (!nzchar(user_msg)) return()

    if (is.null(chat_client())) {
      shinychat::chat_append("chat", "Please click **Reset AI chat** first (so I can register tools + context).")
      return()
    }

    client <- chat_client()

    # Stream responses into shinychat UI
    tryCatch({
      stream <- client$stream_async(user_msg)
      shinychat::chat_append("chat", stream)
    }, error = function(e) {
      shinychat::chat_append("chat", paste0("⚠️ Chat error: ", conditionMessage(e)))
    })
  })
}

shinyApp(ui, server)
