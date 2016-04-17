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
  is_list(x) && length(x) == 1
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_atomic <- function(x) {
  is_atomic(x) && length(x) == 1
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_vector <- function(x) {
  is_vector(x) && length(x) == 1
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_numeric <- function(x) {
  is_numeric(x) && length(x) == 1
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_integer <- function(x) {
  is_integer(x) && length(x) == 1
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_double <- function(x) {
  is_double(x) && length(x) == 1
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_character <- function(x) {
  is_character(x) && length(x) == 1
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_logical <- function(x) {
  is_logical(x) && length(x) == 1
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_complex <- function(x) {
  is_complex(x) && length(x) == 1
}

#' @export
#' @rdname scalar-type-predicates
is_scalar_raw <- function(x) {
  is_raw(x) && length(x) == 1
}

