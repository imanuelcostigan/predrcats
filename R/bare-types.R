#' Bare type predicates
#'
#' These predicates check for a given type but only return \code{TRUE}
#' for bare R objects. Bare objects have no class attributes. For
#' example, a data frame is a list, but not a bare list.
#'
#' \itemize{
#'   \item Like \code{\link{is_atomic}()} and unlike base R
#'         \code{is.atomic()}, \code{is_bare_atomic()} does not return
#'         \code{TRUE} for \code{NULL}.
#'   \item Unlike base R \code{is.numeric()}, \code{is_bare_double()}
#'         only returns \code{TRUE} for floating point numbers.
#' }
#' @param x object to be tested.
#' @return \code{TRUE} or \code{FALSE}
#' @family type predicates
#' @name bare-type-predicates
#' @examples
#' is_list(data.frame(a = 1))
#' is_bare_list(data.frame(a = 1))
#' is_double(Sys.Date())
#' is_bare_double(Sys.Date())
NULL

#' @export
#' @rdname bare-type-predicates
is_bare_list <- function(x) {
  !is.object(x) && is_list(x)
}

#' @export
#' @rdname bare-type-predicates
is_bare_atomic <- function(x) {
  !is.object(x) && is_atomic(x)
}

#' @export
#' @rdname bare-type-predicates
is_bare_vector <- function(x) {
  is_bare_atomic(x) || is_bare_list(x)
}

#' @export
#' @rdname bare-type-predicates
is_bare_double <- function(x) {
  !is.object(x) && is_double(x)
}

#' @export
#' @rdname bare-type-predicates
is_bare_integer <- function(x) {
  !is.object(x) && is_integer(x)
}

#' @export
#' @rdname bare-type-predicates
is_bare_numeric <- function(x) {
  !is.object(x) && is_numeric(x)
}

#' @export
#' @rdname bare-type-predicates
is_bare_character <- function(x) {
  !is.object(x) && is_character(x)
}

#' @export
#' @rdname bare-type-predicates
is_bare_logical <- function(x) {
  !is.object(x) && is_logical(x)
}

