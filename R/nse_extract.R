#' @name nse-extract
#' @title Extract or Replace Parts of a Data Frame
#' @description Extract or replace subsets of a nse data frame.
#' @param x a nse data frame.
#' @param i,j,... elements to extract or replace. `i` is typically
#'  evaluated with \link[rlang]{eval_tidy} and `j` is
#'  evaluated with \link[tidyselect]{eval_select} prior to calling
#'  \link[base]{NextMethod}. See details for more information.
#' @param drop logical. If TRUE the result is coerced to the lowest
#'  possible dimension. The default is to drop if only one column is
#'  left, but *not* to drop if only one row is left. Default is
#'  always set to FALSE.
#' @param value A suitable replacement value: it will be repeated a whole number
#'  of times if necessary and it will be coerced. if NULL, deletes the column if a
#'  single column is selected.
#' @details
#'  For `[`, arguments `i` and `j` will be evaluated in the context of `x`.
#'  If `i` is provided alone, it will act as if using `j`, i.e. evaluated
#'  with \link[tidyselect]{eval_select}. When `j` is missing, `i` is
#'  evaluated with \link[rlang]{eval_tidy} with `x` as the mask.
#'  the evaluations of `i` and `j` are then passed to \link[base]{NextMethod}.
#'  For `[<-`, `j` will first attempt to evaluate as \link[tidyselect]{eval_select}, and if
#'  an error is encountered, will evaluate with \link[rlang]{eval_tidy} with `x`
#'  as the mask.
#'  @return a modifed `data.frame` object
NULL

#' @rdname nse-extract
#' @export
`[.nse_df` <- function(x, i, j, drop = FALSE) {
  i_quo <- enquo(i)
  j_quo <- enquo(j)
  n_real_args <- nargs() - !missing(drop)
  if (n_real_args <= 2L) {
    if(!missing(drop)){
      warning("`drop` argument ignored for subsetting a tibble with `x[j]`, it has an effect only for `x[i, j]`.", call. = F)
      drop <- FALSE
    }
    #attempt to eval as if j
    i <- eval_select(i_quo, x)
  } else if (!missing(i)){
    i <- eval_tidy(i_quo, x)
  }

  if(!missing(j)){
    j <- eval_select(j_quo, x)
  }

  NextMethod("[")
}

#' @rdname nse-extract
#' @export
`[<-.nse_df` <- function(x, i, j, value) {
  i_quo <- enquo(i)
  j_quo <- enquo(j)
  n_real_args <- nargs()
  if (n_real_args <= 3L) {
    #attempt to eval as if j
    i <- try_nse_select_tidy(x, i_quo)
  } else if (!missing(i)){
    i <- eval_tidy(i_quo, x)
  }

  if(!missing(j)){
    j <- eval_select(j_quo, x)
  }
  NextMethod("[<-")
}

#' @rdname nse-extract
#' @export
`[[.nse_df` <- function(x, i){
  i_quo <- enquo(i)
  i <- try_nse_select_tidy(x, i_quo)
  NextMethod("[[")
}

#' @rdname nse-extract
#' @export
`[[<-.nse_df` <- function(x, i, value){
  i_quo <- enquo(i)
  i <- try_nse_select_tidy(x, i_quo)
  NextMethod("[[<-")
}

try_nse_select_tidy <- function(data, quo){
  tryCatch(
    eval_select(quo, data),
    error = function(e){
      eval_tidy(quo, data)
    }
  )
}
