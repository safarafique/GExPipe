## Workspace save / load observers for the GExPipe Shiny app
##
## Extracted from inst/shinyapp/server.R.

gexp_get_saved_workspaces_dir <- function() {
  d <- file.path(getwd(), "saved_workspaces")
  if (dir.exists(d)) {
    return(d)
  }
  d <- file.path(".", "saved_workspaces")
  if (dir.exists(d)) {
    return(normalizePath(d, winslash = "/"))
  }
  file.path(getwd(), "saved_workspaces")
}

gexp_make_workspace_state <- function(input, rv) {
  current_step <- input$sidebar_menu
  if (is.null(current_step) || !nzchar(current_step)) current_step <- "download"
  tryCatch(
    {
      raw_list <- shiny::reactiveValuesToList(rv)
      raw_list$saved_step <- current_step
      drop_names <- c("download_start", "normalize_start", "batch_start", "de_start", "wgcna_start")
      for (d in drop_names) raw_list[[d]] <- NULL
      state <- list()
      for (nm in names(raw_list)) {
        tryCatch(
          {
            serialize(raw_list[[nm]], NULL)
            state[[nm]] <- raw_list[[nm]]
          },
          error = function(e) NULL
        )
      }
      if (!"saved_step" %in% names(state)) state$saved_step <- current_step
      state
    },
    error = function(e) {
      list(saved_step = current_step, saved_note = "Minimal save; full state could not be read.")
    }
  )
}

gexp_restore_workspace_from_state <- function(state, session, rv) {
  if (!is.list(state) || is.null(state$saved_step)) {
    shiny::showNotification("Invalid saved file (missing step). Use a file saved by this app.", type = "error", duration = 6)
    return(invisible(NULL))
  }
  step <- state$saved_step
  if (!is.character(step) || !nzchar(step)) step <- "download"
  for (nm in setdiff(names(state), "saved_step")) {
    tryCatch(
      {
        rv[[nm]] <- state[[nm]]
      },
      error = function(e) NULL
    )
  }
  # Deferred analysis UI: legacy .rds may omit show_analysis; infer from pipeline progress.
  if (!("show_analysis" %in% names(state)) && !isTRUE(rv$show_analysis)) {
    if (isTRUE(rv$download_complete) || isTRUE(rv$normalization_complete) || isTRUE(rv$batch_complete)) {
      rv$show_analysis <- TRUE
    }
  }
  if (!is.null(rv$all_genes_list) && is.list(rv$all_genes_list)) {
    rv$dataset_count <- length(rv$all_genes_list)
    rv$single_dataset <- isTRUE(rv$dataset_count == 1)
  }
  if (!is.null(rv$combined_expr) && (is.matrix(rv$combined_expr) || is.data.frame(rv$combined_expr)) && nrow(rv$combined_expr) > 0) {
    rv$download_complete <- TRUE
  }
  if (!is.null(rv$batch_corrected) && (is.matrix(rv$batch_corrected) || is.data.frame(rv$batch_corrected)) && nrow(rv$batch_corrected) > 0) {
    rv$batch_complete <- TRUE
    rv$groups_applied <- TRUE
  }
  if (!is.null(rv$expr_filtered) && (is.matrix(rv$expr_filtered) || is.data.frame(rv$expr_filtered)) && nrow(rv$expr_filtered) > 0) {
    rv$normalization_complete <- TRUE
  }
  if (!is.null(rv$moduleColors) && length(rv$moduleColors) > 0 && !is.null(rv$gene_metrics) && !is.null(rv$significant_modules)) {
    rv$wgcna_complete <- TRUE
    rv$wgcna_prepared <- TRUE
  }
  if (!is.null(rv$common_genes_de_wgcna) && length(rv$common_genes_de_wgcna) > 0) {
    if (is.null(rv$common_genes_df)) rv$common_genes_df <- data.frame(Gene = rv$common_genes_de_wgcna, stringsAsFactors = FALSE)
  }
  if ((!is.null(rv$ppi_graph) || isTRUE(rv$ppi_complete)) && length(rv$ppi_interactive_genes) > 0) {
    rv$ppi_complete <- TRUE
  }
  if (!is.null(rv$gsea_results_by_gene) && length(rv$gsea_results_by_gene) > 0) {
    rv$gsea_complete <- TRUE
  } else if (!is.null(rv$gsea_result) && inherits(rv$gsea_result, "enrichResult") && nrow(rv$gsea_result@result) > 0) {
    rv$gsea_complete <- TRUE
  }
  if (isTRUE(rv$ml_complete) || (!is.null(rv$ml_common_genes) && length(rv$ml_common_genes) > 0)) {
    rv$ml_complete <- TRUE
  }
  if (isTRUE(rv$nomogram_complete) || (!is.null(rv$nomogram_model) && inherits(rv$nomogram_model, "rms"))) {
    rv$nomogram_complete <- TRUE
  }
  valid_tabs <- c(
    "download", "qc", "normalize", "groups", "batch", "results", "wgcna",
    "common_genes", "ppi", "ml", "validation", "roc", "nomogram", "gsea",
    "results_summary"
  )
  if (!step %in% valid_tabs) step <- "download"
  if (!is.null(state$disease_name) && nzchar(state$disease_name)) {
    shiny::updateTextInput(session, "disease_name", value = as.character(state$disease_name))
  }
  shinydashboard::updateTabItems(session, "sidebar_menu", step)
  shiny::showNotification("Workspace loaded. Continue from the step shown.", type = "message", duration = 6)
  invisible(NULL)
}

gexp_register_workspace_observers <- function(input, output, session, rv) {
  # nocov start
  workspace_download_handler <- function() {
    shiny::downloadHandler(
      filename = function() {
        tryCatch(
          {
            custom <- trimws(input$workspace_save_filename)
            if (is.null(custom) || !nzchar(custom)) {
              step <- input$sidebar_menu
              if (is.null(step) || !nzchar(step)) step <- "workspace"
              return(paste0("app_saved_state_", step, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
            }
            base <- gsub("[^A-Za-z0-9_.-]+", "_", custom)
            base <- sub("_+$", "", base)
            if (!nzchar(base)) base <- "workspace"
            paste0(base, ".rds")
          },
          error = function(e) paste0("workspace_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
        )
      },
      content = function(file) {
        save_state <- new.env(parent = emptyenv())
        save_state$err_msg <- NULL
        tryCatch(
          {
            state <- gexp_make_workspace_state(input, rv)
            current_step <- state$saved_step
            save_subdir <- "saved_workspaces"
            save_dir <- file.path(getwd(), save_subdir)
            if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
            custom <- trimws(input$workspace_save_filename)
            if (nzchar(custom)) {
              base <- gsub("[^A-Za-z0-9_.-]+", "_", custom)
              base <- sub("_+$", "", base)
              if (!nzchar(base)) base <- "workspace"
              fname <- paste0(base, ".rds")
            } else {
              fname <- paste0("app_saved_state_", current_step, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
            }
            app_path <- file.path(save_dir, fname)
            saved_to_disk <- FALSE
            if (dir.exists(save_dir)) {
              tryCatch(
                {
                  saveRDS(state, app_path)
                  saved_to_disk <- TRUE
                },
                error = function(e2) NULL
              )
            }
            saveRDS(state, file)
            if (saved_to_disk) {
              shiny::showNotification(
                paste0("Saved! File is in folder '", save_subdir, "'. Load it at Step 1 to return to this step."),
                type = "message", duration = 8
              )
            } else {
              shiny::showNotification("Saved! Use Step 1 -> Load and choose this file to return to this step.", type = "message", duration = 8)
            }
          },
          error = function(e) {
            save_state$err_msg <- conditionMessage(e)
          }
        )
        if (!is.null(save_state$err_msg)) {
          minimal <- list(
            saved_step = if (is.null(input$sidebar_menu) || !nzchar(input$sidebar_menu)) "download" else input$sidebar_menu,
            saved_note = "Minimal save; some data could not be serialized."
          )
          tryCatch(
            {
              saveRDS(minimal, file)
              shiny::showNotification("Saved minimal state (some data was skipped). Load at Step 1 to return to your step.", type = "warning", duration = 8)
            },
            error = function(e2) {
              shiny::showNotification(paste("Save failed:", save_state$err_msg), type = "error", duration = 10)
            }
          )
        }
      }
    )
  }
  output$download_workspace <- workspace_download_handler()
  output$download_workspace_results <- workspace_download_handler()

  shiny::observe({
    if (isTRUE(getOption("shiny.testmode"))) {
      return()
    }
    if (!isTRUE(rv$download_complete)) {
      return()
    }
    if (is.null(rv$download_complete_at)) rv$download_complete_at <- Sys.time()
    if (isTRUE(rv$auto_save_after_download_done)) {
      return()
    }
    elapsed_sec <- as.numeric(difftime(Sys.time(), rv$download_complete_at, units = "secs"))
    if (elapsed_sec < 300) {
      shiny::invalidateLater(60000, session)
      return()
    }
    rv$auto_save_after_download_done <- TRUE
    tryCatch(
      {
        state <- gexp_make_workspace_state(input, rv)
        save_dir <- file.path(getwd(), "saved_workspaces")
        if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
        fname <- paste0("auto_save_after_download_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
        app_path <- file.path(save_dir, fname)
        saveRDS(state, app_path)
        shiny::showNotification(
          paste0("Auto-save: data saved to saved_workspaces/", fname, ". Load at Step 1 to return to this state."),
          type = "message",
          duration = 8
        )
      },
      error = function(e) {
        tryCatch(
          {
            minimal <- list(
              saved_step = if (is.null(input$sidebar_menu) || !nzchar(input$sidebar_menu)) "download" else input$sidebar_menu,
              saved_note = "Auto-save minimal state."
            )
            save_dir <- file.path(getwd(), "saved_workspaces")
            if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
            fname <- paste0("auto_save_after_download_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
            saveRDS(minimal, file.path(save_dir, fname))
            shiny::showNotification(paste0("Auto-save (minimal) to saved_workspaces/", fname, "."), type = "warning", duration = 8)
          },
          error = function(e2) NULL
        )
      }
    )
  })

  shiny::observeEvent(input$save_workspace_to_folder, {
    tryCatch(
      {
        state <- gexp_make_workspace_state(input, rv)
        current_step <- state$saved_step
        save_subdir <- "saved_workspaces"
        save_dir <- file.path(getwd(), save_subdir)
        if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
        custom <- trimws(input$workspace_save_filename)
        if (is.null(custom)) custom <- ""
        if (nzchar(custom)) {
          base <- gsub("[^A-Za-z0-9_.-]+", "_", custom)
          base <- sub("_+$", "", base)
          if (!nzchar(base)) base <- "workspace"
          fname <- paste0(base, ".rds")
        } else {
          fname <- paste0("app_saved_state_", current_step, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
        }
        app_path <- file.path(save_dir, fname)
        saveRDS(state, app_path)
        shiny::showNotification(
          paste0("Saved to '", save_subdir, "/", fname, "'. Load it at Step 1 (Download) to return to this step."),
          type = "message", duration = 8
        )
      },
      error = function(e) {
        tryCatch(
          {
            minimal <- list(
              saved_step = if (is.null(input$sidebar_menu) || !nzchar(input$sidebar_menu)) "download" else input$sidebar_menu,
              saved_note = "Minimal save."
            )
            save_dir <- file.path(getwd(), "saved_workspaces")
            if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
            fname <- paste0("workspace_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
            saveRDS(minimal, file.path(save_dir, fname))
            shiny::showNotification(paste0("Saved minimal state to saved_workspaces/", fname, ". Load at Step 1."), type = "warning", duration = 8)
          },
          error = function(e2) {
            shiny::showNotification(paste("Save failed:", conditionMessage(e)), type = "error", duration = 10)
          }
        )
      }
    )
  })

  output$load_from_folder_ui <- shiny::renderUI({
    save_dir <- gexp_get_saved_workspaces_dir()
    if (!dir.exists(save_dir)) {
      return(shiny::selectInput("load_from_folder_file", NULL, choices = c("(No saved workspaces)" = ""), width = "100%"))
    }
    f <- list.files(save_dir, pattern = "\\.rds$", full.names = FALSE)
    if (length(f) == 0) {
      return(shiny::selectInput("load_from_folder_file", NULL, choices = c("(No .rds files)" = ""), width = "100%"))
    }
    choices <- stats::setNames(f, f)
    shiny::selectInput("load_from_folder_file", NULL, choices = c("\u2014 Select file \u2014" = "", choices), width = "100%")
  })

  shiny::observeEvent(input$skip_load_btn, {
    shiny::showNotification("You can start a new analysis: choose platform above and click 'Start Processing' below.", type = "message", duration = 5)
  })

  shiny::observeEvent(input$load_from_folder_btn, {
    shiny::req(input$load_from_folder_file)
    if (!nzchar(trimws(input$load_from_folder_file))) {
      shiny::showNotification("Select a file from the dropdown first.", type = "warning", duration = 4)
      return()
    }
    save_dir <- gexp_get_saved_workspaces_dir()
    fname <- trimws(input$load_from_folder_file)
    path <- file.path(save_dir, fname)
    if (!file.exists(path)) {
      shiny::showNotification("File not found in saved_workspaces.", type = "error", duration = 5)
      return()
    }
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    tryCatch(
      {
        con <- file(path, "rb")
        on.exit(close(con), add = TRUE)
        state <- readRDS(con, refhook = NULL)
        if (is.null(state)) stop("File is empty or invalid.")
        gexp_restore_workspace_from_state(state, session, rv)
      },
      error = function(e) {
        msg <- conditionMessage(e)
        f <- list.files(gexp_get_saved_workspaces_dir(), pattern = "\\.rds$", full.names = FALSE)
        choices <- if (length(f) > 0) c("\u2014 Select file \u2014" = "", stats::setNames(f, f)) else c("(No .rds files)" = "")
        shiny::updateSelectInput(session, "load_from_folder_file", choices = choices, selected = "")
        if (grepl("unknown input format|invalid.*format|not a serialized|error in read", msg, ignore.case = TRUE)) {
          shiny::showNotification(
            "Load failed: file is not a valid workspace save. Use a .rds saved by this app, select another file, or click \"Continue without loading\" to start a new analysis.",
            type = "error",
            duration = 12
          )
        } else if (grepl("connection|reading from", msg, ignore.case = TRUE)) {
          shiny::showNotification(
            "Load failed: file may be corrupted or in use. Select another file or click \"Continue without loading\" to proceed.",
            type = "error",
            duration = 12
          )
        } else {
          shiny::showNotification(
            paste0("Load failed: ", msg, " Select another file or click \"Continue without loading\" to start a new analysis."),
            type = "error",
            duration = 10
          )
        }
      }
    )
  })

  shiny::observeEvent(input$load_uploaded_btn, {
    shiny::req(input$upload_workspace_file)
    up <- input$upload_workspace_file
    path <- if (is.data.frame(up)) as.character(up$datapath)[1] else as.character(up$datapath)
    path <- path[!is.na(path) & nzchar(path)][1]
    if (is.null(path) || is.na(path) || !nzchar(path)) {
      shiny::showNotification("No file selected. Please choose a .rds workspace file first.", type = "warning", duration = 5)
      return()
    }
    if (!file.exists(path)) {
      shiny::showNotification("Uploaded file not found. The file may be too large (max 500 MB) or the upload was interrupted. Try again or use \"Load from folder\" if the file is in saved_workspaces.", type = "error", duration = 8)
      shinyjs::reset("upload_workspace_file")
      return()
    }
    tmpfile <- tempfile(fileext = ".rds")
    on.exit(unlink(tmpfile, force = TRUE), add = TRUE)
    if (!file.copy(path, tmpfile, overwrite = TRUE)) {
      shiny::showNotification("Could not read the uploaded file. Try again or use \"Load from folder\".", type = "error", duration = 6)
      shinyjs::reset("upload_workspace_file")
      return()
    }
    tryCatch(
      {
        state <- readRDS(tmpfile)
        if (is.null(state)) stop("File is empty or invalid.")
        gexp_restore_workspace_from_state(state, session, rv)
        shinyjs::reset("upload_workspace_file")
      },
      error = function(e) {
        msg <- conditionMessage(e)
        shinyjs::reset("upload_workspace_file")
        if (grepl("unknown input format|invalid.*format|not a serialized|error in read", msg, ignore.case = TRUE)) {
          shiny::showNotification(
            "Load failed: file is not a valid workspace save. Use a .rds saved by this app, upload another file, or click \"Continue without loading\" to start a new analysis.",
            type = "error",
            duration = 12
          )
        } else if (grepl("connection|reading from", msg, ignore.case = TRUE)) {
          shiny::showNotification(
            "Load failed: file may be corrupted or in use. Upload another file or click \"Continue without loading\" to proceed.",
            type = "error",
            duration = 12
          )
        } else {
          shiny::showNotification(
            paste0("Load failed: ", msg, " Upload another file or click \"Continue without loading\" to start a new analysis."),
            type = "error",
            duration = 10
          )
        }
      }
    )
  })
  # nocov end
}
