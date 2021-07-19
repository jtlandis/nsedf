#' @importFrom tidyselect eval_select
#' @importFrom rlang eval_tidy enquo

#' @title Non-Standard Evaulation Data Frame
#' @rdname nse-data.frame
#' @description An additional class to extend `data.frame` object to allow
#' for NSE filtering on `i`, and tidyselect functionality on `j`.
#' This will help condense some code statements.
#' @return a `data.frame` object with `nse_df` class
NULL


#' @rdname nse-data.frame
#' @inheritParams tibble::tibble
#' @export
nse_tbl <- function(..., .rows = NULL, .name_repair = c("check_unique",
                                                     "unique", "universal", "minimal")) {
  `_call` <- match.call()
  `_call`[[1]] <- quote(tibble)
  tbl <- eval.parent(`_call`)
  as_nse_df(tbl)
}

#' @rdname nse-data.frame
#' @param ... these arguments are of either the form `value` or `tag = value`.
#'  Component names are created based on the tag (if present) or the deparsed
#'  argument itself.
#' @param row.names NULL or a single integer or character
#'  string specifying a column to be used as row names,
#'  or a character or integer vector giving the row names
#'  for the data frame.
#' @param check.rows if TRUE then the rows are checked for
#'  consistency of length and names.
#' @param check.names logical. If TRUE then the names of the
#'  variables in the data frame are checked to ensure that
#'  they are syntactically valid variable names and are not
#'  duplicated. If necessary they are adjusted (by \link{make.names})
#'  so that they are.
#' @param fix.empty.names logical indicating if arguments which are
#'  "unnamed" (in the sense of not being formally called as `someName = arg`)
#'  get an automatically constructed name or rather name "". Needs
#'  to be set to FALSE even when check.names is false if "" names
#'  should be kept.
#' @param stringsAsFactors logical: should character vectors be
#'  converted to factors?
#' @export
nse_df <- function(...,
                   row.names = NULL,
                   check.rows = FALSE,
                   check.names = TRUE,
                   fix.empty.names = TRUE,
                   stringsAsFactors = FALSE) {
  `_call` <- match.call()
  `_call`[[1]] <- quote(data.frame)
  df <- eval.parent(`_call`)
  as_nse_df(df)
}

