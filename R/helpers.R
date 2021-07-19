#' @name as_nse_df
#' @title Convert `data.frame` to `nse_df`
#' @description Simple method to add `nse_df` class
#' to a `data.frame` object. The method only modifies the
#' class field of the object, and will fail by default.
#' @export
#' @return a `data.frame` object with the class `nse_df`
as_nse_df <- function(x) UseMethod('as_nse_df')
#' @export
as_nse_df.data.frame <- function(x) return(add_nse_df_class(x))
#' @export
as_nse_df.default <- function(x) nse_df_method_fail(x, method = "as_nse_df")



add_nse_df_class <- function(x) UseMethod('add_nse_df_class')
add_nse_df_class.default <- function(x) nse_df_method_fail(x, method = "add_nse_df_class")
add_nse_df_class.data.frame <- function(x) {
  class(x) <- c('nse_df', class(x))
  return(x)
}
add_nse_df_class.nse_df <- function(x) return(x)


nse_df_method_fail <- function(x, method) {
  stop(
    paste0("There is no `",
           method,
           "()` method for object of class <",
           paste0(class(x), collapse = "/"),
           ">."),
    call. = F
    )
}
