#' Scalar type predicates
#'
#' These predicates check for a given type and whether the vector is
#' "scalar", that is, of length 1.
#' @param x object to be tested.
#' @seealso \link{type-predicates} \link{bare-type-predicates}
#' @name scalar-type-predicates
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

#' Is a vector/list empty?
#'
#' @param x object to test
#' @export
#' @examples
#' is_empty(NULL)
#' is_empty(list())
#' is_empty(list(NULL))
is_empty <- function(x) length(x) == 0

#' Is a formula?
#'
#' @inheritParams is_empty
#' @export
#' @examples
#' x <- disp ~ am
#' is_formula(x)
is_formula <- function(x) inherits(x, "formula")
