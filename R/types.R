#' Type predicates
#'
#' These type predicates aim to make type testing in R more
#' consistent. They are wrappers around \code{\link{typeof}}, so
#' operate at a level beneath S3/S4 etc.
#'
#' Compare to base R functions:
#' \itemize{
#'   \item Unlike \code{is.atomic()}, \code{is_atomic()} does not
#'      return \code{TRUE} for \code{NULL}.
#'   \item Unlike \code{is.vector()}, \code{is_vector()} test if an
#'         object is an atomic vector or a list. \code{is.vector}
#'         checks for the presence of attributes (other than name).
#'   \item \code{is_numeric()} is not generic so, (e.g.) dates and date times
#'     are \code{TRUE}, not \code{FALSE}.
#'   \item \code{is_function()} returns \code{TRUE} only for regular
#'     functions, not special or primitive functions.
#' }
#'
#' @param x object to be tested.
#' @return \code{TRUE} or \code{FALSE}.
#' @seealso \link{bare-type-predicates} \link{scalar-type-predicates}
#' @examples
#' is_atomic(NULL)
#' is_vector(1:3)
#' is_vector(list(1:3))
#' is_numeric(1)
#' is_numeric(1:3)
#' is_integer(1:3)
#' is_double(1:3)
#' is_double(c(1, 2, 3))
#' is_atomic(as.Date("2013-01-01"))
#' is_numeric(as.Date("2013-01-01"))
#' is_numeric(as.POSIXct("2013-01-01"))
#' is_character(letters)
#' is_character(factor(letters))
#' is_integer(factor(letters))
#' is_logical(NA)
#' @name type-predicates
NULL

#' @export
#' @rdname type-predicates
is_list <- function(x) {
  typeof(x) == "list"
}

#' @export
#' @rdname type-predicates
is_atomic <- function(x) {
  typeof(x) %in% c("logical", "integer", "double", "complex", "character", "raw")
}

#' @export
#' @rdname type-predicates
is_vector <- function(x) {
  is_atomic(x) || is.list(x)
}

#' @export
#' @rdname type-predicates
is_numeric <- function(x) {
  typeof(x) %in% c("integer", "double")
}

#' @export
#' @rdname type-predicates
is_integer <- function(x) {
  typeof(x) == "integer"
}

#' @export
#' @rdname type-predicates
is_double <- function(x) {
  typeof(x) == "double"
}

#' @export
#' @rdname type-predicates
is_character <- function(x) {
  typeof(x) == "character"
}

#' @export
#' @rdname type-predicates
is_logical <- function(x) {
  typeof(x) == "logical"
}

#' @export
#' @rdname type-predicates
is_null <- function(x) {
  typeof(x) == "NULL"
}

#' @export
#' @rdname type-predicates
is_function <- function(x) {
  typeof(x) == "closure"
}

is_name <- function(x) {
  typeof(x) == "name"
}

is_call <- function(x) {
  typeof(x) == "language"
}

is_language <- function(x) {
  is_call(x) || is_name(x) || is_atomic(x)
}
