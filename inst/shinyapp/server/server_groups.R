# ==============================================================================
# SERVER_GROUPS.R - Group Selection Module (Enhanced with Better Alignment)
# ==============================================================================
# Integrated group selection for the modular RNA analysis app
# Includes improved GSM ID alignment and sample matching

server_groups <- function(input, output, session, rv) {
  
  # Reactive to store extracted groups
  extracted_groups <- reactiveVal(list())
  extracted_groups_per_gse <- reactiveVal(list())  # Store per-sample alignment data
  selected_columns <- reactiveVal(list())  # Store selected columns per GSE
  
  # ==============================================================================
  # DE METHOD BANNER — shows which pipeline is active on this step
  # ==============================================================================
  output$groups_de_method_banner <- renderUI({
    method <- if (!is.null(input$de_method)) input$de_method else "limma"
    if (method == "deseq2") {
      fluidRow(
        box(width = 12, status = "success", solidHeader = TRUE,
            title = tags$span(icon("dna"), " DESeq2 Mode Active"),
            tags$div(
              style = "padding: 10px; background: linear-gradient(135deg, #d5f5e3 0%, #abebc6 100%); border-radius: 8px;",
              tags$p(icon("check-circle", style = "color: #1e8449; margin-right: 5px;"),
                     tags$strong("DE Method: DESeq2"),
                     " — Normalization was handled automatically. Select phenotype columns below to define your groups.",
                     style = "font-size: 14px; margin: 0; color: #1e8449;")
            ))
      )
    } else if (method == "edger") {
      fluidRow(
        box(width = 12, status = "warning", solidHeader = TRUE,
            title = tags$span(icon("chart-bar"), " edgeR Mode Active"),
            tags$div(
              style = "padding: 10px; background: linear-gradient(135deg, #fdebd0 0%, #f9e79f 100%); border-radius: 8px;",
              tags$p(icon("check-circle", style = "color: #b7950b; margin-right: 5px;"),
                     tags$strong("DE Method: edgeR"),
                     " — Normalization was handled automatically (TMM). Select phenotype columns below to define your groups.",
                     style = "font-size: 14px; margin: 0; color: #7d6608;")
            ))
      )
    } else {
      fluidRow(
        box(width = 12, status = "primary", solidHeader = TRUE,
            title = tags$span(icon("chart-line"), " limma Mode Active"),
            tags$div(
              style = "padding: 10px; background: linear-gradient(135deg, #d6eaf8 0%, #aed6f1 100%); border-radius: 8px;",
              tags$p(icon("check-circle", style = "color: #2471a3; margin-right: 5px;"),
                     tags$strong("DE Method: limma"),
                     " — Select phenotype columns below to define your groups.",
                     style = "font-size: 14px; margin: 0; color: #2471a3;")
            ))
      )
    }
  })
  
  # ==============================================================================
  # PHENODATA BROWSER — Full interactive table per dataset
  # ==============================================================================
  output$phenodata_browser_ui <- renderUI({
    req(rv$download_complete)
    
    # Get all dataset names from whichever metadata source is available
    all_gses <- c(names(rv$micro_metadata_list), names(rv$rna_metadata_list))
    all_gses <- unique(all_gses)
    
    if (length(all_gses) == 0) {
      return(tags$div(class = "alert alert-warning",
                      icon("exclamation-triangle"),
                      " No phenodata available. Complete data download first."))
    }
    
    # Create a tabsetPanel with one tab per dataset
    tabs <- lapply(all_gses, function(gse) {
      pdata <- if (gse %in% names(rv$rna_metadata_list)) {
        rv$rna_metadata_list[[gse]]
      } else if (gse %in% names(rv$micro_metadata_list)) {
        rv$micro_metadata_list[[gse]]
      } else {
        NULL
      }
      
      if (is.null(pdata) || ncol(pdata) == 0) return(NULL)
      
      platform_label <- if (gse %in% names(rv$rna_metadata_list)) "RNA-seq" else "Microarray"
      
      tabPanel(
        title = tags$span(icon("database"), paste0(" ", gse, " (", platform_label, ")")),
        tags$div(
          style = "padding: 15px 0;",
          tags$div(
            style = "display: flex; gap: 15px; flex-wrap: wrap; margin-bottom: 12px;",
            tags$span(
              class = "badge", style = "background: #3498db; font-size: 13px; padding: 6px 12px;",
              icon("vial"), paste0(" Samples: ", nrow(pdata))
            ),
            tags$span(
              class = "badge", style = "background: #2ecc71; font-size: 13px; padding: 6px 12px;",
              icon("columns"), paste0(" Columns: ", ncol(pdata))
            ),
            tags$span(
              class = "badge", style = paste0("background: ", if (platform_label == "RNA-seq") "#e74c3c" else "#9b59b6", "; font-size: 13px; padding: 6px 12px;"),
              icon("microchip"), paste0(" ", platform_label)
            )
          ),
          DT::DTOutput(paste0("phenodata_table_", gse)),
          tags$p(
            icon("info-circle", style = "color: #17a2b8; margin-right: 5px;"),
            tags$em("Tip: Look for columns like 'characteristics_ch1', 'source_name_ch1', 'tissue', 'disease state' etc. that contain group/condition information."),
            style = "color: #6c757d; font-size: 12px; margin-top: 10px;"
          )
        )
      )
    })
    
    # Remove NULL tabs
    tabs <- tabs[!sapply(tabs, is.null)]
    
    if (length(tabs) == 0) {
      return(tags$div(class = "alert alert-warning",
                      "No phenodata tables available."))
    }
    
    do.call(tabsetPanel, c(list(id = "phenodata_tabs", type = "pills"), tabs))
  })
  
  # Render DT tables for each dataset
  observe({
    req(rv$download_complete)
    
    all_gses <- c(names(rv$micro_metadata_list), names(rv$rna_metadata_list))
    all_gses <- unique(all_gses)
    
    for (gse in all_gses) {
      local({
        gse_local <- gse
        
        output[[paste0("phenodata_table_", gse_local)]] <- DT::renderDT({
          pdata <- if (gse_local %in% names(rv$rna_metadata_list)) {
            rv$rna_metadata_list[[gse_local]]
          } else if (gse_local %in% names(rv$micro_metadata_list)) {
            rv$micro_metadata_list[[gse_local]]
          } else {
            return(NULL)
          }
          
          if (is.null(pdata) || ncol(pdata) == 0) return(NULL)
          
          # Add SampleID column from rownames for clarity
          display_df <- data.frame(SampleID = rownames(pdata), pdata,
                                   check.names = FALSE, stringsAsFactors = FALSE)
          
          DT::datatable(
            display_df,
            options = list(
              scrollX = TRUE,
              scrollY = "350px",
              pageLength = 10,
              lengthMenu = c(5, 10, 25, 50),
              dom = "lfrtip",
              autoWidth = FALSE,
              columnDefs = list(
                list(width = "120px", targets = 0),
                list(className = "dt-left", targets = "_all")
              )
            ),
            class = "display compact stripe hover",
            rownames = FALSE,
            filter = "top",
            selection = "none"
          )
        })
      })
    }
  })
  
  # ==============================================================================
  # HELPER FUNCTIONS
  # ==============================================================================
  
  # Safe trim function for cleaning group values
  safe_trim <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x[x == ""] <- NA_character_
    x
  }
  
  # Get expression column names and GSM IDs for a GSE (for alignment)
  get_expr_and_gsm_for_gse <- function(gse, pdata = NULL) {
    tryCatch({
      # Get expression data for this GSE
      expr_cols <- NULL
      
      if (!is.null(rv$rna_counts_list) && gse %in% names(rv$rna_counts_list)) {
        expr_data <- rv$rna_counts_list[[gse]]
        if (!is.null(expr_data) && ncol(expr_data) > 0) {
          expr_cols <- colnames(expr_data)
        }
      } else if (!is.null(rv$micro_expr_list) && gse %in% names(rv$micro_expr_list)) {
        expr_data <- rv$micro_expr_list[[gse]]
        if (!is.null(expr_data) && ncol(expr_data) > 0) {
          expr_cols <- colnames(expr_data)
        }
      }
      
      if (is.null(expr_cols) || length(expr_cols) == 0) {
        return(list(expr_cols = character(0), gsm_ids = character(0)))
      }
      
      # Try to get GSM IDs from sample_id_map if available
      gsm_ids <- expr_cols  # Default: use expression column names
      
      # If we have phenotype data, try to match
      if (!is.null(pdata) && is.data.frame(pdata) && nrow(pdata) > 0) {
        pdata_rownames <- rownames(pdata)
        if (!is.null(pdata_rownames) && length(pdata_rownames) > 0) {
          # Try matching expression column names with phenotype rownames
          match_count <- sum(expr_cols %in% pdata_rownames)
          
          # If fewer than 20% match by ID, use positional alignment
          if (match_count < max(2, floor(0.2 * length(expr_cols)))) {
            n <- min(length(expr_cols), nrow(pdata))
            if (n > 0) {
              gsm_ids <- c(pdata_rownames[seq_len(n)], rep(NA_character_, max(0, length(expr_cols) - n)))
            }
          } else {
            # Use direct matching
            gsm_ids <- ifelse(expr_cols %in% pdata_rownames, expr_cols, NA_character_)
          }
        }
      }
      
      list(expr_cols = as.character(expr_cols), gsm_ids = as.character(gsm_ids))
    }, error = function(e) {
      # Return empty lists on error
      list(expr_cols = character(0), gsm_ids = character(0))
    })
  }
  
  # ==============================================================================
  # GROUP SELECTOR UI - Per-GSE Column Selection
  # ==============================================================================
  
  output$group_selector_ui <- renderUI({
    req(rv$download_complete)
    
    # Get dataset names: prefer unified_metadata, fallback to metadata lists
    if (!is.null(rv$unified_metadata) && nrow(rv$unified_metadata) > 0) {
      all_gses <- unique(rv$unified_metadata$Dataset)
    } else {
      # Fallback: derive from the raw metadata lists (before normalization finishes)
      all_gses <- unique(c(names(rv$micro_metadata_list), names(rv$rna_metadata_list)))
    }
    
    if (length(all_gses) == 0) {
      return(tags$div(class = "alert alert-warning",
                      icon("exclamation-triangle"),
                      " No datasets available. Please complete data download and normalization first."))
    }
    
    # If unified_metadata not ready yet (DESeq2 auto-normalize still running), show a message
    if (is.null(rv$unified_metadata) || nrow(rv$unified_metadata) == 0) {
      return(tags$div(
        class = "alert alert-info",
        style = "padding: 15px; border-radius: 8px;",
        icon("spinner", class = "fa-spin", style = "margin-right: 8px;"),
        tags$strong("Normalization is still processing..."),
        tags$br(),
        tags$span("Please wait a moment. The phenotype column selector will appear once normalization completes.",
                  style = "font-size: 13px; color: #495057;")
      ))
    }
    
    # Create UI for each GSE
    selector_boxes <- lapply(all_gses, function(gse) {
      # Get phenotype data
      pdata <- if (gse %in% names(rv$rna_metadata_list)) {
        rv$rna_metadata_list[[gse]]
      } else if (gse %in% names(rv$micro_metadata_list)) {
        rv$micro_metadata_list[[gse]]
      } else {
        NULL
      }
      
      if (is.null(pdata) || ncol(pdata) == 0) {
        return(NULL)
      }
      
      # Collect ALL phenotype column names (use both colnames and names to avoid missing any)
      col_names <- unique(c(colnames(pdata), names(pdata)))
      col_names <- as.character(col_names[!is.na(col_names) & nzchar(trimws(col_names))])
      if (length(col_names) == 0) return(NULL)
      # Put likely group-like columns first for convenience; list still contains ALL columns
      likely <- col_names[grepl("characteristics|tissue|group|condition|disease|type|source|title|description|sample", 
                                col_names, ignore.case = TRUE)]
      other <- setdiff(col_names, likely)
      group_cols <- c(likely, other)
      # Build choices as explicit list so every column appears in the dropdown (no truncation)
      choices_list <- list("-- Select Column --" = "")
      for (col in group_cols) choices_list[[col]] <- col
      choices_vec <- unlist(choices_list, use.names = TRUE)
      
      box(
        title = tags$span(
          icon("database", style = "margin-right: 8px; color: #3498db;"),
          tags$strong(gse, style = "font-size: 16px; color: #2c3e50;")
        ),
        width = 6, status = "primary", solidHeader = TRUE, collapsible = TRUE,
        collapsed = FALSE,
        tags$div(
          style = "padding: 10px 0;",
          selectInput(
            inputId = paste0("group_col_", gse),
            label = tags$span(icon("list"), " Select phenotype column:"),
            choices = choices_vec,
            selected = "",
            width = "100%"
          ),
          tags$div(
            style = "margin-top: 10px; padding: 8px; background: #f8f9fa; border-radius: 5px;",
            tags$span(icon("vial"), style = "margin-right: 5px; color: #17a2b8;"),
            tags$strong("Samples: ", style = "color: #495057;"),
            tags$span(sum(rv$unified_metadata$Dataset == gse), 
                     style = "color: #3498db; font-weight: bold; font-size: 16px;")
          ),
          tags$div(
            style = "margin-top: 15px;",
            uiOutput(paste0("group_preview_", gse))
          )
        )
      )
    })
    
    # Remove NULL boxes
    selector_boxes <- selector_boxes[!sapply(selector_boxes, is.null)]
    
    if (length(selector_boxes) == 0) {
      return(tags$div(class = "alert alert-warning",
                      "No phenotype data available for group selection."))
    }
    
    # Display in rows of 2
    fluidRow(selector_boxes)
  })
  
  # Store selected columns when they change and show preview
  observe({
    req(rv$download_complete, rv$unified_metadata)
    all_gses <- unique(rv$unified_metadata$Dataset)
    
    sel_cols <- list()
    for (gse in all_gses) {
      col_input <- input[[paste0("group_col_", gse)]]
      if (!is.null(col_input) && col_input != "") {
        sel_cols[[gse]] <- col_input
      }
    }
    selected_columns(sel_cols)
  })
  
  # Show preview for each GSE when column is selected (using reactive pattern)
  observe({
    req(rv$download_complete, rv$unified_metadata)
    all_gses <- unique(rv$unified_metadata$Dataset)
    
    # Create observers for each GSE only once
    for (gse in all_gses) {
      local({
        gse_local <- gse  # Capture the value
        observeEvent(input[[paste0("group_col_", gse_local)]], {
          col_input <- input[[paste0("group_col_", gse_local)]]
          
          # Get phenotype data
          pdata <- if (gse_local %in% names(rv$rna_metadata_list)) {
            rv$rna_metadata_list[[gse_local]]
          } else if (gse_local %in% names(rv$micro_metadata_list)) {
            rv$micro_metadata_list[[gse_local]]
          } else {
            NULL
          }
          
          if (is.null(col_input) || col_input == "" || is.null(pdata) || !(col_input %in% colnames(pdata))) {
            output[[paste0("group_preview_", gse_local)]] <- renderUI({
              NULL
            })
            return()
          }
          
          # Get unique values from the selected column
          vals <- safe_trim(pdata[[col_input]])
          vals <- vals[!is.na(vals) & vals != ""]
          
          if (length(vals) == 0) {
            output[[paste0("group_preview_", gse_local)]] <- renderUI({
              tags$div(
                class = "alert alert-warning",
                style = "margin-top: 10px; padding: 10px; border-radius: 5px;",
                icon("exclamation-triangle"),
                " No valid values found in this column."
              )
            })
            return()
          }
          
          # Count occurrences of each unique value
          val_counts <- table(vals)
          unique_vals <- names(val_counts)
          counts <- as.integer(val_counts)
          
          # Sort by count (descending) for better visibility
          ord <- order(counts, decreasing = TRUE)
          unique_vals <- unique_vals[ord]
          counts <- counts[ord]
          
          # Show preview
          output[[paste0("group_preview_", gse_local)]] <- renderUI({
            tags$div(
              style = "margin-top: 15px; padding: 15px; background: #ffffff; border: 2px solid #3498db; border-radius: 8px;",
              tags$div(
                style = "display: flex; align-items: center; margin-bottom: 12px;",
                icon("eye", style = "margin-right: 8px; color: #3498db; font-size: 18px;"),
                tags$strong("Column Preview:", style = "color: #2c3e50; font-size: 16px;")
              ),
              tags$div(
                style = "max-height: 200px; overflow-y: auto;",
                tags$table(
                  class = "table table-sm table-striped",
                  style = "margin: 0;",
                  tags$thead(
                    tags$tr(
                      tags$th("Value", style = "font-weight: bold; color: #495057;"),
                      tags$th("Count", style = "font-weight: bold; color: #495057; text-align: center;")
                    )
                  ),
                  tags$tbody(
                    lapply(seq_along(unique_vals), function(i) {
                      tags$tr(
                        tags$td(
                          tags$code(unique_vals[i], style = "color: #2c3e50; font-size: 13px;")
                        ),
                        tags$td(
                          tags$span(
                            counts[i],
                            class = "badge",
                            style = "background-color: #3498db; color: white; font-size: 12px; padding: 4px 8px;"
                          ),
                          style = "text-align: center;"
                        )
                      )
                    })
                  )
                )
              ),
              tags$div(
                style = "margin-top: 10px; padding-top: 10px; border-top: 1px solid #dee2e6;",
                tags$small(
                  tags$strong("Total unique values: ", style = "color: #495057;"),
                  tags$span(length(unique_vals), style = "color: #3498db; font-weight: bold;"),
                  tags$span(" | Total samples: ", style = "color: #495057; margin-left: 10px;"),
                  tags$span(sum(counts), style = "color: #3498db; font-weight: bold;")
                )
              )
            )
          })
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # ==============================================================================
  # EXTRACT GROUPS (Enhanced with Better Alignment)
  # ==============================================================================
  
  observeEvent(input$extract_groups_btn, {
    tryCatch({
      # Check if metadata exists
      if (is.null(rv$unified_metadata) || nrow(rv$unified_metadata) == 0) {
        showNotification("No metadata available. Please normalize data first.", 
                         type = "error", duration = 5)
        return()
      }
      
      all_gses <- unique(rv$unified_metadata$Dataset)
      if (length(all_gses) == 0) {
        showNotification("No datasets found in metadata.", 
                         type = "error", duration = 5)
        return()
      }
      
      sel_cols <- selected_columns()
      
      if (length(sel_cols) == 0) {
        showNotification("Please select phenotype columns for at least one dataset first.", 
                         type = "warning", duration = 5)
        return()
      }
      
      all_groups <- list()
      extracted_per_gse <- list()
      
      for (gse in names(sel_cols)) {
        if (!gse %in% all_gses) next
        
        col_input <- sel_cols[[gse]]
        
        # Get phenotype data
        pdata <- if (gse %in% names(rv$rna_metadata_list)) {
          rv$rna_metadata_list[[gse]]
        } else if (gse %in% names(rv$micro_metadata_list)) {
          rv$micro_metadata_list[[gse]]
        } else {
          NULL
        }
        
        if (is.null(pdata) || !(col_input %in% colnames(pdata))) {
          next
        }
        
        # Get expression columns and GSM IDs for alignment
        ids <- tryCatch({
          get_expr_and_gsm_for_gse(gse, pdata = pdata)
        }, error = function(e) {
          showNotification(paste("Warning: Could not get expression columns for", gse, ":", e$message), 
                           type = "warning", duration = 4)
          list(expr_cols = character(0), gsm_ids = character(0))
        })
        
        expr_cols <- ids$expr_cols
        gsm_ids <- ids$gsm_ids
        
        if (is.null(expr_cols) || length(expr_cols) == 0) {
          showNotification(paste("Warning: No expression columns found for", gse), 
                           type = "warning", duration = 4)
          next
        }
        
        # Build group per expression sample (aligned)
        group_raw <- rep(NA_character_, length(expr_cols))
        names(group_raw) <- expr_cols
        
        # Try to match by GSM IDs first
        if (!is.null(gsm_ids) && length(gsm_ids) > 0 && !is.null(pdata) && nrow(pdata) > 0) {
          valid <- !is.na(gsm_ids) & gsm_ids %in% rownames(pdata)
          if (any(valid)) {
            tryCatch({
              matched_ids <- gsm_ids[valid]
              if (length(matched_ids) > 0) {
                vals <- safe_trim(pdata[matched_ids, col_input, drop = TRUE])
                if (length(vals) == length(matched_ids)) {
                  group_raw[valid] <- vals
                }
              }
            }, error = function(e) {
              # If indexing fails, continue to next method
            })
          }
        }
        
        # If no matches by ID, try positional alignment
        if (sum(!is.na(group_raw)) == 0 && !is.null(pdata) && nrow(pdata) > 0) {
          n <- min(length(expr_cols), nrow(pdata))
          if (n > 0) {
            tryCatch({
              vals <- safe_trim(pdata[seq_len(n), col_input, drop = TRUE])
              group_raw[seq_len(n)] <- vals
            }, error = function(e) {
              # If indexing fails, continue to next method
            })
          }
        }
        
        # If still no matches, try matching expression column names directly
        if (sum(!is.na(group_raw)) == 0 && !is.null(pdata) && nrow(pdata) > 0) {
          pdata_rownames <- rownames(pdata)
          for (i in seq_along(expr_cols)) {
            tryCatch({
              if (expr_cols[i] %in% pdata_rownames) {
                group_raw[i] <- safe_trim(pdata[expr_cols[i], col_input, drop = TRUE])[1]
              } else {
                # Try partial match (remove _Sample suffix)
                expr_base <- gsub("_Sample[0-9]+$", "", expr_cols[i])
                matches <- grep(paste0("^", expr_base), pdata_rownames, value = TRUE)
                if (length(matches) > 0) {
                  group_raw[i] <- safe_trim(pdata[matches[1], col_input, drop = TRUE])[1]
                }
              }
            }, error = function(e) {
              # Skip this sample if there's an error
            })
          }
        }
        
        # Extract unique groups
        gr_vals <- group_raw[!is.na(group_raw) & group_raw != "" & !is.null(group_raw)]
        if (length(gr_vals) > 0) {
          unique_groups <- unique(gr_vals)
          
          if (length(unique_groups) > 0) {
            all_groups[[gse]] <- unique_groups
            extracted_per_gse[[gse]] <- list(
              expr_cols = expr_cols,
              gsm_ids = gsm_ids,
              group_raw = group_raw
            )
          }
        }
      }
      
      if (length(all_groups) == 0) {
        showNotification("No groups extracted. Please verify selected columns and datasets.", 
                         type = "warning", duration = 5)
        return()
      }
      
      extracted_groups(all_groups)
      extracted_groups_per_gse(extracted_per_gse)
      
      showNotification(paste("✓ Extracted groups from", length(all_groups), "datasets. Please categorize them below."), 
                       type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Error extracting groups:", e$message), 
                       type = "error", duration = 8)
    })
  })
  
  # ==============================================================================
  # EXTRACTED GROUPS UI - Categorization (Aesthetic)
  # ==============================================================================
  
  output$extracted_groups_ui <- renderUI({
    tryCatch({
      groups <- extracted_groups()
      
      if (is.null(groups) || length(groups) == 0) {
        return(tags$div(
          class = "alert alert-info",
          icon("info-circle"),
          " Click 'Extract Groups' to see available groups."
        ))
      }
      
      # Create categorization UI for each dataset
      group_uis <- lapply(names(groups), function(gse) {
        tryCatch({
          gse_groups <- groups[[gse]]
          
          if (is.null(gse_groups) || length(gse_groups) == 0) {
            return(NULL)
          }
          
          # Auto-detect categories
          auto_cats <- sapply(gse_groups, function(g) {
            if (is.null(g) || is.na(g) || g == "") {
              return("None")
            }
            g_lower <- tolower(as.character(g))
            if (grepl("normal|control|healthy|wild", g_lower)) {
              return("Normal")
            } else if (grepl("disease|tumor|cancer|metastatic|patient", g_lower)) {
              return("Disease")
            } else {
              return("None")
            }
          })
          
          # Count samples per group
          per_gse_data <- extracted_groups_per_gse()[[gse]]
          group_counts <- tryCatch({
            if (!is.null(per_gse_data) && !is.null(per_gse_data$group_raw)) {
              valid_groups <- per_gse_data$group_raw[!is.na(per_gse_data$group_raw) & 
                                                      per_gse_data$group_raw != "" & 
                                                      !is.null(per_gse_data$group_raw)]
              if (length(valid_groups) > 0) {
                table(valid_groups)
              } else {
                table()
              }
            } else {
              table()
            }
          }, error = function(e) {
            table()
          })
          
          tags$div(
            style = "margin-bottom: 25px;",
            box(
              title = tags$span(
                icon("database", style = "margin-right: 8px; color: #3498db;"),
                tags$strong(gse, style = "font-size: 18px; color: #2c3e50;")
              ),
              width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
              collapsed = FALSE,
              tags$div(
                style = "padding: 10px 0;",
                lapply(seq_along(gse_groups), function(i) {
                  grp <- gse_groups[i]
                  input_id <- paste0("cat_", gse, "_", i)
                  
                  # Get sample count - handle potential issues with group name matching
                  n_samples <- tryCatch({
                    if (length(group_counts) > 0 && grp %in% names(group_counts)) {
                      as.integer(group_counts[[grp]])
                    } else {
                      0
                    }
                  }, error = function(e) {
                    0
                  })
                  
                  # Color based on auto-detected category
                  cat_color <- if (auto_cats[i] == "Normal") {
                    "#2ecc71"
                  } else if (auto_cats[i] == "Disease") {
                    "#e74c3c"
                  } else {
                    "#95a5a6"
                  }
                  
                  # Category class for styling
                  cat_class <- paste0("category-", tolower(auto_cats[i]))
                  
                  tags$div(
                    class = paste("group-category-card", cat_class),
                    style = paste0("margin-bottom: 15px; padding: 15px; background: #ffffff; border-left: 4px solid ", cat_color, "; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);"),
                    tags$div(
                      style = "display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 15px;",
                      tags$div(
                        style = "flex: 1; min-width: 200px;",
                        tags$h5(
                          style = "margin: 0; display: flex; align-items: center;",
                          tags$span(icon("tag"), style = "margin-right: 10px; color: #495057; font-size: 18px;"),
                          tags$strong(grp, class = "group-name")
                        )
                      ),
                      tags$div(
                        style = "display: flex; align-items: center; gap: 15px; flex-wrap: wrap;",
                        tags$div(
                          style = "min-width: 180px;",
                          selectInput(
                            inputId = input_id,
                            label = NULL,
                            choices = list(
                              "Normal" = "Normal",
                              "Disease" = "Disease",
                              "None" = "None"
                            ),
                            selected = auto_cats[i],
                            width = "100%"
                          )
                        ),
                        tags$div(
                          class = "sample-count-badge",
                          tags$span(icon("vial"), style = "margin-right: 5px;"),
                          tags$strong(n_samples)
                        )
                      )
                    )
                  )
                })
              )
            )
          )
        }, error = function(e) {
          tags$div(
            class = "alert alert-danger",
            paste("Error rendering groups for", gse, ":", e$message)
          )
        })
      })
      
      # Remove NULL elements
      group_uis <- group_uis[!sapply(group_uis, is.null)]
      
      if (length(group_uis) == 0) {
        return(tags$div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " No groups to display."
        ))
      }
      
      tagList(group_uis)
    }, error = function(e) {
      tags$div(
        class = "alert alert-danger",
        icon("exclamation-triangle"),
        tags$strong("Error displaying groups: "),
        e$message
      )
    })
  })
  
  # ==============================================================================
  # APPLY GROUPS (Enhanced with Better Alignment)
  # ==============================================================================
  
  observeEvent(input$apply_groups_btn, {
    groups <- extracted_groups()
    per_gse_data <- extracted_groups_per_gse()
    
    if (length(groups) == 0) {
      showNotification("Please extract groups first.", type = "warning", duration = 5)
      return()
    }
    
    # Collect categorizations
    all_cats <- list()
    
    for (gse in names(groups)) {
      gse_groups <- groups[[gse]]
      gse_cats <- sapply(seq_along(gse_groups), function(i) {
        input[[paste0("cat_", gse, "_", i)]]
      })
      names(gse_cats) <- gse_groups
      all_cats[[gse]] <- gse_cats
    }
    
    # Build per-sample labels using aligned data
    all_samples <- character(0)
    all_labels <- character(0)
    all_batches <- character(0)
    
    for (gse in names(per_gse_data)) {
      per <- per_gse_data[[gse]]
      expr_cols <- per$expr_cols
      gr <- per$group_raw
      
      if (length(expr_cols) == 0 || length(gr) == 0) next
      
      # Map groups to categories
      labels <- rep(NA_character_, length(expr_cols))
      cats <- all_cats[[gse]]
      
      for (i in seq_along(expr_cols)) {
        gval <- gr[[expr_cols[i]]]
        if (!is.null(gval) && !is.na(gval) && gval != "" && gval %in% names(cats)) {
          cat_val <- cats[[as.character(gval)]]
          if (!is.null(cat_val) && !is.na(cat_val) && cat_val != "None") {
            labels[i] <- cat_val
          }
        }
      }
      
      all_samples <- c(all_samples, expr_cols)
      all_labels <- c(all_labels, labels)
      all_batches <- c(all_batches, rep(gse, length(expr_cols)))
    }
    
    # Keep only samples labeled Normal/Disease
    keep <- !is.na(all_labels)
    if (sum(keep) == 0) {
      showNotification("All samples are excluded (None/empty). Please assign at least one group to Normal or Disease.", 
                       type = "error", duration = 6)
      return()
    }
    
    samples_kept <- all_samples[keep]
    labels_kept <- all_labels[keep]
    batch_kept <- all_batches[keep]
    
    # Align to expression matrix
    if (is.null(rv$combined_expr)) {
      showNotification("No expression matrix found. Please normalize data first.", 
                       type = "error", duration = 6)
      return()
    }
    
    # Find samples that exist in both group selection and expression matrix
    matched <- intersect(samples_kept, colnames(rv$combined_expr))
    if (length(matched) == 0) {
      showNotification("No matching samples between group selection and expression matrix.", 
                       type = "error", duration = 6)
      return()
    }
    
    # Also ensure matched samples exist in metadata
    matched <- intersect(matched, rownames(rv$unified_metadata))
    if (length(matched) == 0) {
      showNotification("No matching samples between group selection and metadata.", 
                       type = "error", duration = 6)
      return()
    }
    
    # Filter to matched samples, preserving order
    idx <- match(matched, samples_kept)
    labels_final <- labels_kept[idx]
    batch_final <- batch_kept[idx]
    
    # Store gene count before filtering
    genes_before <- nrow(rv$combined_expr)
    
    # Filter expression matrix to matched samples first
    rv$combined_expr <- rv$combined_expr[, matched, drop = FALSE]
    
    # Filter metadata to matched samples
    rv$unified_metadata <- rv$unified_metadata[matched, , drop = FALSE]
    
    # Now update Condition and Batch columns (they should match exactly now)
    rv$unified_metadata$Condition <- labels_final
    rv$unified_metadata$Batch <- factor(batch_final)
    
    # Ensure Condition is a factor with correct levels
    rv$unified_metadata$Condition <- factor(rv$unified_metadata$Condition, 
                                            levels = c("Normal", "Disease"))
    
    # Store gene count after filtering (should be same, but track it)
    genes_after <- nrow(rv$combined_expr)
    
    # ----- DESeq2: align raw counts and metadata to matched samples -----
    if (!is.null(rv$raw_counts_for_deseq2)) {
      common_samp <- intersect(matched, colnames(rv$raw_counts_for_deseq2))
      if (length(common_samp) > 0) {
        rv$raw_counts_for_deseq2 <- rv$raw_counts_for_deseq2[, common_samp, drop = FALSE]
        # Build matching metadata for DESeq2 with Condition + Batch
        rv$raw_counts_metadata <- rv$unified_metadata[common_samp, , drop = FALSE]
      }
    }
    
    rv$groups_applied <- TRUE
    
    counts <- table(rv$unified_metadata$Condition)
    de_method <- if (!is.null(input$de_method)) input$de_method else "limma"
    showNotification(
      tags$div(
        tags$strong("✓ Groups applied!"),
        tags$br(),
        tags$span("Normal: ", ifelse("Normal" %in% names(counts), counts[["Normal"]], 0),
                  " | Disease: ", ifelse("Disease" %in% names(counts), counts[["Disease"]], 0)),
        tags$br(),
        tags$span("Genes: ", format(genes_after, big.mark = ","), 
                  " | Samples: ", sum(counts)),
        if (de_method == "deseq2" && !is.null(rv$raw_counts_for_deseq2)) {
          tags$span(tags$br(), icon("dna"), " DESeq2 raw counts aligned: ",
                    ncol(rv$raw_counts_for_deseq2), " samples",
                    style = "color: #1e8449;")
        },
        style = "font-size: 13px;"
      ),
      type = "message", duration = 8
    )
  })
  
  # ==============================================================================
  # GROUP SUMMARY
  # ==============================================================================
  
  output$group_summary_ui <- renderUI({
    if (is.null(rv$groups_applied) || !rv$groups_applied) {
      return(tags$div(
        class = "alert alert-info",
        style = "text-align: center; padding: 20px; border-radius: 10px;",
        icon("info-circle", style = "font-size: 24px; margin-right: 10px;"),
        tags$strong("Apply groups to see summary", style = "font-size: 16px;")
      ))
    }
    
    counts <- table(rv$unified_metadata$Condition)
    total_samples <- sum(counts)
    total_genes <- if (!is.null(rv$combined_expr)) nrow(rv$combined_expr) else 0
    
    tags$div(
      style = "padding: 20px;",
      tags$div(
        style = "text-align: center; margin-bottom: 25px; padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 10px; color: white;",
        tags$h4(
          icon("check-circle", style = "margin-right: 10px;"),
          "Groups Successfully Applied!",
          style = "margin: 0; font-weight: bold;"
        ),
        tags$div(
          style = "margin-top: 15px; display: flex; justify-content: center; gap: 30px; flex-wrap: wrap;",
          tags$div(
            tags$p(
              tags$strong("Total Samples:", style = "font-size: 16px;"),
              tags$br(),
              tags$span(format(total_samples, big.mark = ","), style = "font-size: 20px; font-weight: bold;")
            )
          ),
          tags$div(
            tags$p(
              tags$strong("Total Genes:", style = "font-size: 16px;"),
              tags$br(),
              tags$span(format(total_genes, big.mark = ","), style = "font-size: 20px; font-weight: bold;")
            )
          )
        )
      ),
      tags$div(
        style = "display: flex; justify-content: center; gap: 20px; flex-wrap: wrap;",
        lapply(names(counts), function(g) {
          bg_color <- if (g == "Normal") {
            "linear-gradient(135deg, #2ecc71 0%, #27ae60 100%)"
          } else if (g == "Disease") {
            "linear-gradient(135deg, #e74c3c 0%, #c0392b 100%)"
          } else {
            "linear-gradient(135deg, #95a5a6 0%, #7f8c8d 100%)"
          }
          
          icon_name <- if (g == "Normal") "check-circle" else if (g == "Disease") "exclamation-triangle" else "minus-circle"
          
          tags$div(
            style = paste0("min-width: 200px; padding: 25px; background: ", bg_color, "; border-radius: 15px; color: white; text-align: center; box-shadow: 0 6px 20px rgba(0,0,0,0.2);"),
            tags$div(
              icon(icon_name, class = "fa-3x"),
              style = "margin-bottom: 15px;"
            ),
            tags$h3(
              tags$strong(g),
              style = "margin: 0 0 10px 0; font-size: 24px; font-weight: bold;"
            ),
            tags$div(
              style = "font-size: 36px; font-weight: bold;",
              counts[[g]]
            ),
            tags$div(
              style = "margin-top: 10px; font-size: 14px; opacity: 0.9;",
              "samples"
            )
          )
        })
      )
    )
  })
  
  # ==============================================================================
  # NEXT BUTTON
  # ==============================================================================
  
  output$next_to_batch_btn <- renderUI({
    if (is.null(rv$groups_applied) || !rv$groups_applied) {
      return(NULL)
    }
    
    actionButton("next_to_batch_btn",
                 tagList(icon("arrow-right"), " Next: Batch Correction"),
                 class = "btn-success btn-lg")
  })
  
  observeEvent(input$next_to_batch_btn, {
    if (is.null(rv$groups_applied) || !rv$groups_applied) {
      showNotification("Please apply groups first.", type = "warning", duration = 5)
      return()
    }
    showNotification("Navigating to Batch Correction...", type = "message", duration = 3)
  })
}
