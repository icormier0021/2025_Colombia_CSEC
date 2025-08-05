# install and load writexl for Excel output
if (!requireNamespace("writexl", quietly = TRUE)) {
        install.packages("writexl")
}
library(writexl)

get_variable_types <- function(df) {
        max_show   <- 20      # how many distinct values to list
        max_length <- 30000   # truncate any very long string
        
        vars <- names(df)
        dtypes <- sapply(df, function(x) paste(class(x), collapse = ", "))
        vlabels <- sapply(df, function(x) {
                lbl <- attr(x, "label", exact = TRUE)
                if (is.null(lbl)) NA_character_ else as.character(lbl)
        })
        nuni <- sapply(df, function(x) length(unique(x[!is.na(x)])))
        uv <- sapply(df, function(x) {
                if (is.factor(x) || is.character(x)) {
                        vals <- unique(x[!is.na(x)])
                        n    <- length(vals)
                        shown <- if (n > max_show) head(vals, max_show) else vals
                        s <- paste(shown, collapse = "; ")
                        if (n > max_show) s <- paste0(s, "; … [", n, " total]")
                        if (nchar(s) > max_length) {
                                s <- paste0(substr(s, 1, max_length - 3), "…")
                        }
                        s
                } else {
                        NA_character_
                }
        })
        
        data.frame(
                Variable     = vars,
                DataType     = dtypes,
                Label        = vlabels,
                UniqueCount  = nuni,
                UniqueValues = uv,
                stringsAsFactors = FALSE
        )
}

save_variable_types <- function(df, file_path = "variable_types.xlsx") {
        vt <- get_variable_types(df)
        writexl::write_xlsx(vt, path = file_path)
        message("✅ Variable types written to: ", normalizePath(file_path))
}

# Example:
# save_variable_types(my_df, "my_df_variable_types.xlsx")
