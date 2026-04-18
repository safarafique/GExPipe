interface_lines <- readLines("e:/GExPipe/R/interface_app.R")

head_start <- grep("shiny::tags\\$head\\(", interface_lines)[1]

# Find the end of tags$head. It should be just before shiny::uiOutput("pipeline_progress")
# Let's search for pipeline_progress
progress_idx <- grep("shiny::uiOutput\\(\"pipeline_progress\"\\)", interface_lines)[1]
head_end <- progress_idx - 1
if (trimws(interface_lines[head_end]) == ",") {
    head_end <- head_end - 1
}

head_block <- interface_lines[head_start:head_end]

# Remove the head_block from its current location
# Need to remove the comma before or after if there is one
new_lines <- c()
if (head_start > 1) new_lines <- c(new_lines, interface_lines[1:(head_start - 1)])
# Check if the line before head_start ends with a comma, or if we need to remove a comma after
new_lines <- c(new_lines, interface_lines[progress_idx:length(interface_lines)])

# Now we construct the gexp_app_head function
gexp_app_head_func <- c(
    "",
    "gexp_app_head <- function() {",
    head_block,
    "}"
)

# Insert gexp_app_head_func right before gexp_app_ui
ui_func_start <- grep("gexp_app_ui <- function", new_lines)[1]
new_lines <- c(new_lines[1:(ui_func_start-1)], gexp_app_head_func, new_lines[ui_func_start:length(new_lines)])

# Now modify gexp_app_ui to call gexp_app_head() inside shiny::fluidPage(
fluid_start <- grep("shiny::fluidPage\\(", new_lines)[1]
new_lines <- c(
    new_lines[1:fluid_start],
    "    gexp_app_head(),",
    new_lines[(fluid_start+1):length(new_lines)]
)

writeLines(new_lines, "e:/GExPipe/R/interface_app.R")
