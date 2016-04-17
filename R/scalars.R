#' Scalar type predicates
#'
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#'
#' @param x object to be tested.
#' @return \code{TRUE} or \code{FALSE}
#' @family type predicates
#' @name scalar-type-predicates
#' @examples
#' is_scalar_list(list(1))
#' is_scalar_list(list(1, 2))
#' is_integer(1:2)
#' is_scalar_integer(1:2)
#' is_scalar_integer(1L)
NULL

#' @export
#' @rdname scalar-type-predicates
is_scalar_list <- function(x) {
  length(x) == 1 && is_list(x)
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_atomic <- function(x) {
  length(x) == 1 && is_atomic(x)
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_vector <- function(x) {
  length(x) == 1 && is_vector(x)
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_numeric <- function(x) {
  length(x) == 1 && is_numeric(x)
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_integer <- function(x) {
  length(x) == 1 && is_integer(x)
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_double <- function(x) {
  length(x) == 1 && is_double(x)
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_character <- function(x) {
  length(x) == 1 && is_character(x)
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_logical <- function(x) {
  length(x) == 1 && is_logical(x)
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_complex <- function(x) {
  length(x) == 1 && is_complex(x)
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_raw <- function(x) {
  length(x) == 1 && is_raw(x)
}

