# chat_widget.R — AI Data Assistant
#
# Requires: ellmer, shinychat (both in Suggests)
# Install:  install.packages(c("ellmer", "shinychat"))
#
# Supports Anthropic (Claude) and OpenAI (GPT) via ellmer.
# API keys must be set as environment variables before launching the app:
#   ANTHROPIC_API_KEY=sk-ant-...
#   OPENAI_API_KEY=sk-...
# Add to ~/.Renviron (run usethis::edit_r_environ()) so they load automatically.
#
# Only element names and aggregate summaries are sent to the AI —
# individual facility records are never transmitted.

.PROVIDER_MODELS <- list(
  anthropic = list(label = "Claude (Anthropic)", default = "claude-sonnet-4-6"),
  openai    = list(label = "GPT (OpenAI)",        default = "gpt-4o")
)

chat_widget_ui <- function(id) {
  ns <- NS(id)

  has_deps <- requireNamespace("shinychat", quietly = TRUE) &&
              requireNamespace("ellmer",    quietly = TRUE)

  if (!has_deps) {
    return(fluidPage(
      br(),
      div(
        style = "max-width:640px; margin:auto;",
        div(
          style = paste0("background:#fff3cd; padding:16px 20px;",
                         " border-left:4px solid #ffc107; border-radius:3px;"),
          tags$strong("The AI Assistant requires two additional packages:"),
          tags$pre(
            style = "margin-top:10px; margin-bottom:0;",
            'install.packages(c("ellmer", "shinychat"))'
          ),
          tags$p(
            style = "margin-top:8px; margin-bottom:0; font-size:0.9em;",
            "After installing, restart the app."
          )
        )
      )
    ))
  }

  tagList(
    # Override shinychat's internal max-width so chat fills the main panel
    tags$head(tags$style(
      ".shiny-chat-container, .shiny-chat-messages { max-width: 100% !important; width: 100% !important; }
       .shiny-chat-input-container { max-width: 100% !important; }"
    )),

    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 2,

          h5("Dataset Context"),
          uiOutput(ns("context_summary")),

          hr(),

          h5("AI Provider"),
          uiOutput(ns("provider_ui")),

          div(
            style = "font-size:0.78em; color:#777; margin-top:4px;",
            actionLink(ns("go_about"), "API key setup instructions"),
            " — About tab"
          ),

          hr(),

          actionButton(
            ns("reset_chat"),
            label = "New Conversation",
            icon  = icon("rotate"),
            class = "btn-sm btn-outline-secondary w-100"
          ),

          br(), br(),

          div(
            style = "font-size:0.78em; color:#777; line-height:1.5;",
            tags$strong("Privacy note:"),
            " Only element names and aggregate summaries are sent to the AI.",
            " Individual facility records stay on your machine."
          )
        ),

        mainPanel(
          width = 10,
          uiOutput(ns("chat_or_instructions"))
        )
      )
    )
  )
}

chat_widget_server <- function(id,
                                data_widget_output,
                                reporting_widget_output,
                                cleaning_widget_output,
                                metadata_widget_output) {
  moduleServer(id, function(input, output, session) {

    if (!requireNamespace("shinychat", quietly = TRUE) ||
        !requireNamespace("ellmer",    quietly = TRUE)) return()

    # Navigate to About tab when the setup link is clicked
    observeEvent(input$go_about, {
      shinyjs::runjs('document.querySelector(\'[data-value="About"]\').click()')
    })

    # --- Check available keys at startup ----------------------------------
    env_keys <- list(
      anthropic = Sys.getenv("ANTHROPIC_API_KEY"),
      openai    = Sys.getenv("OPENAI_API_KEY")
    )
    available <- Filter(nzchar, env_keys)

    # --- Provider selector (inline, only shows available providers) -------
    output$provider_ui <- renderUI({
      if (length(available) == 0) return(NULL)

      choices <- c(
        if (nzchar(env_keys$anthropic)) c("Claude (Anthropic)" = "anthropic"),
        if (nzchar(env_keys$openai))    c("GPT (OpenAI)"       = "openai")
      )

      tagList(
        radioButtons(
          session$ns("provider"),
          label    = NULL,
          choices  = choices,
          selected = names(available)[1]
        ),
        if (length(available) > 1)
          helpText(style = "font-size:0.78em;",
                   "Provider switch takes effect after restarting the app.")
      )
    })

    # --- No key: show instructions instead of chat UI ---------------------
    if (length(available) == 0) {
      output$chat_or_instructions <- renderUI({
        div(
          style = paste0("max-width:560px; margin:40px auto; padding:20px;",
                         " background:#fff3cd; border-left:4px solid #ffc107;",
                         " border-radius:3px;"),
          tags$strong("No API key found."),
          tags$p(
            "Add one or both of the following to your ",
            tags$code("~/.Renviron"),
            " file (run ",
            tags$code("usethis::edit_r_environ()"),
            " to open it), then restart the app:"
          ),
          tags$pre(
            "ANTHROPIC_API_KEY=sk-ant-...\nOPENAI_API_KEY=sk-..."
          ),
          tags$p(
            style = "margin-bottom:0;",
            actionLink(session$ns("go_about"), "Full setup instructions"),
            " are in the About tab."
          )
        )
      })
      return()
    }

    # --- Shared prompt builder (used at init and on reset) ----------------
    build_prompt <- function() {
      data1      <- tryCatch(isolate(data_widget_output$data1()),           error = function(e) NULL)
      data_total <- tryCatch(isolate(reporting_widget_output$data.total()), error = function(e) NULL)
      data2      <- tryCatch(isolate(cleaning_widget_output$data2()),       error = function(e) NULL)
      period     <- tryCatch(isolate(reporting_widget_output$period()),     error = function(e) "Month")

      n_facilities <- if (!is.null(data1) && "orgUnit" %in% names(data1))
        length(unique(data1$orgUnit)) else NULL

      build_mg2_system_prompt(
        formula_elements     = tryCatch(isolate(data_widget_output$formula_elements()),   error = function(e) NULL),
        num_facilities       = n_facilities,
        starting_month       = tryCatch(isolate(reporting_widget_output$startingMonth()), error = function(e) NULL),
        ending_month         = tryCatch(isolate(reporting_widget_output$endingMonth()),   error = function(e) NULL),
        validation_rules     = tryCatch(isolate(metadata_widget_output$validationRules()), error = function(e) NULL),
        completeness_summary = summarize_completeness_for_prompt(data1),
        raw_totals_summary   = if (is.null(data_total)) summarize_completeness_for_prompt(data1) else NULL,
        reporting_summary    = summarize_reporting_for_prompt(data_total, period_col = period %||% "Month"),
        outlier_summary      = summarize_outliers_for_prompt(data2)
      )
    }

    # --- Create the initial client ----------------------------------------
    initial_provider <- names(available)[1]
    model            <- .PROVIDER_MODELS[[initial_provider]]$default

    system_prompt <- build_prompt()

    client <- tryCatch(
      if (initial_provider == "anthropic")
        ellmer::chat_anthropic(model = model, system_prompt = system_prompt)
      else
        ellmer::chat_openai(model = model, system_prompt = system_prompt),
      error = function(e) {
        showNotification(paste("Failed to connect:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )

    if (is.null(client)) return()

    # Render the chat UI now that we have a valid client
    output$chat_or_instructions <- renderUI({
      shinychat::chat_mod_ui(session$ns("chat"), height = "calc(100vh - 160px)")
    })

    chat_mod_out <- shinychat::chat_mod_server("chat", client = client)

    # --- Context summary (sidebar) ----------------------------------------
    output$context_summary <- renderUI({
      fe <- tryCatch(data_widget_output$formula_elements(), error = function(e) NULL)
      d1 <- tryCatch(data_widget_output$data1(),            error = function(e) NULL)
      sm <- tryCatch(reporting_widget_output$startingMonth(), error = function(e) NULL)
      em <- tryCatch(reporting_widget_output$endingMonth(),   error = function(e) NULL)

      n_fac <- if (!is.null(d1) && "orgUnit" %in% names(d1))
        length(unique(d1$orgUnit)) else NULL

      items <- list()
      if (!is.null(fe) && nrow(fe) > 0)
        items <- c(items, list(tags$li(paste(nrow(fe), "formula elements"))))
      else
        items <- c(items, list(tags$li(tags$em("No dataset loaded"))))
      if (!is.null(n_fac))
        items <- c(items, list(tags$li(paste(n_fac, "facilities"))))
      if (!is.null(sm) && !is.null(em))
        items <- c(items, list(tags$li(paste(as.character(sm), "–", as.character(em)))))

      tagList(
        tags$ul(style = "padding-left:16px; font-size:0.88em; margin:4px 0 8px 0;", items),
        tags$p(style = "font-size:0.78em; color:#777; margin:0;",
               "Context updates automatically as you visit other tabs.")
      )
    })

    # --- Auto-update system prompt when data from other tabs arrives -------
    # Only updates the context (set_system_prompt) — never clears the
    # conversation history. The user keeps their chat; the AI just gets
    # richer context for subsequent questions.

    observeEvent(data_widget_output$data1(), {
      client$set_system_prompt(build_prompt())
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(reporting_widget_output$data.total(), {
      client$set_system_prompt(build_prompt())
      showNotification("Assistant context updated with Reporting data.",
                       type = "message", duration = 3)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(cleaning_widget_output$data2(), {
      client$set_system_prompt(build_prompt())
      showNotification("Assistant context updated with Outlier data.",
                       type = "message", duration = 3)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # --- Reset: clears conversation history and rebuilds prompt -----------
    observeEvent(input$reset_chat, {
      client$set_system_prompt(build_prompt())
      chat_mod_out$clear(client_history = "clear")
    })
  })
}
