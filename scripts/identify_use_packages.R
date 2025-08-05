# File: detect_used_packages.R

library(stringr)
library(tools)
library(tibble)
library(dplyr)

detect_used_packages <- function(rmd_file, loaded_packages) {
        lines <- readLines(rmd_file, warn = FALSE)
        
        # Extract R chunks
        in_chunk <- FALSE
        code_lines <- c()
        
        for (line in lines) {
                if (grepl("^```\\{r.*\\}", line)) {
                        in_chunk <- TRUE
                } else if (grepl("^```\\s*$", line)) {
                        in_chunk <- FALSE
                } else if (in_chunk) {
                        code_lines <- c(code_lines, line)
                }
        }
        
        full_code <- paste(code_lines, collapse = "\n")
        
        # Detect explicit pkg::func usage
        pkg_refs <- str_extract_all(full_code, "\\b([a-zA-Z0-9.]+)::[a-zA-Z0-9._]+") |> unlist()
        used_pkgs_explicit <- unique(str_extract(pkg_refs, "^[a-zA-Z0-9.]+"))
        
        # Detect function names used
        func_calls <- str_extract_all(full_code, "\\b([a-zA-Z0-9._]+)\\s*\\(") |> unlist()
        func_calls <- str_replace(func_calls, "\\($", "")
        func_calls <- setdiff(unique(func_calls), c("if", "for", "while", "function", "return"))
        
        # Build map of package exports
        pkg_func_map <- purrr::map_df(loaded_packages, function(pkg) {
                tryCatch({
                        fns <- getNamespaceExports(pkg)
                        tibble(pkg = pkg, fn = fns)
                }, error = function(e) NULL)
        })
        
        # Find which packages' functions were called
        used_pkgs_by_fn <- pkg_func_map |> filter(fn %in% func_calls) |> distinct(pkg) |> pull(pkg)
        
        all_used <- union(used_pkgs_explicit, used_pkgs_by_fn)
        
        # Return status table
        tibble(
                package = loaded_packages,
                used = package %in% all_used
        ) |> arrange(desc(used))
}

# Example usage:
loaded <- c("survey", "haven", "dplyr", "labelled", "officer", "tidyr", "DT", "skimr", 
             "ggplot2", "shiny", "knitr", "kableExtra", "svyweight", "scales", 
             "purrr", "srvyr", "tibble", "stringr")
 result <- detect_used_packages("CSECT_2018_VACS_results.Rmd", loaded)
 print(result)
 
 
 # File: find_package_usage.R
 
 library(stringr)
 library(purrr)
 library(tibble)
 library(dplyr)
 
 find_package_usage_locations <- function(rmd_file, package_name) {
         # Get exported functions
         exported <- tryCatch(getNamespaceExports(package_name), error = function(e) character(0))
         if (length(exported) == 0) stop("Package not found or has no exports.")
         
         lines <- readLines(rmd_file, warn = FALSE)
         in_chunk <- FALSE
         results <- list()
         
         for (i in seq_along(lines)) {
                 line <- lines[[i]]
                 
                 if (grepl("^```\\{r.*\\}", line)) {
                         in_chunk <- TRUE
                 } else if (grepl("^```\\s*$", line)) {
                         in_chunk <- FALSE
                 } else if (in_chunk) {
                         if (grepl(paste0("\\b", package_name, "::"), line)) {
                                 results[[length(results)+1]] <- list(line_num = i, type = "explicit", line = line)
                         } else {
                                 for (fn in exported) {
                                         pattern <- paste0("\\b", fn, "\\s*\\(")
                                         if (grepl(pattern, line)) {
                                                 results[[length(results)+1]] <- list(line_num = i, type = "implicit", line = line)
                                                 break
                                         }
                                 }
                         }
                 }
         }
         
         bind_rows(results)
 }
 
 # Example usage:
result <- find_package_usage_locations("CSECT_2018_VACS_results.Rmd", "svyweight")
print(result)
 
