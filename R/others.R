#' Is a vector/list empty?
#'
#' @param x object to test
#' @return \code{TRUE} if length of \code{x} is zero. Otherwise \code{FALSE}
#' @export
#' @examples
#' is_empty(NULL)
#' is_empty(list())
#' is_empty(list(NULL))
is_empty <- function(x) length(x) == 0

#' Is of length one?
#'
#' @inheritParams is_empty
#' @return \code{TRUE} if length of \code{x} is one. Otherwise \code{FALSE}
#' @export
#' @examples
#' is_scalar(c(1, 2))
#' is_scalar(1)
is_scalar <- function(x) length(x) == 1

#' Is a formula?
#'
#' @inheritParams is_empty
#' @return \code{TRUE} if \code{x} inherits from \code{formula}. Otherwise
#'   \code{FALSE}
#' @export
#' @examples
#' x <- disp ~ am
#' is_formula(x)
is_formula <- function(x) inherits(x, "formula")

#' Is a Date?
#'
#' @inheritParams is_empty
#' @return \code{TRUE} if \code{x} is a \code{Date}. Otherwise \code{FALSE}
#' @export
#' @examples
#' is_date(Sys.Date())
#' is_date(Sys.time())
is_date <- function(x) methods::is(x, "Date")

#' Is a time (Date-Time)?
#'
#' @inheritParams is_empty
#' @return \code{TRUE} if \code{x} inherits from \code{\link{POSIXt}}.
#'   Otherwise \code{FALSE}
#' @export
#' @examples
#' is_time(Sys.Date())
#' is_time(Sys.time())
is_time <- function(x) inherits(x, "POSIXt")

#' @export
#' @rdname is_time
is_POSIXct <- function(x) methods::is(x, "POSIXct")

#' @export
#' @rdname is_time
is_POSIXlt <- function(x) methods::is(x, "POSIXlt")

#' NA predicates
#'
#' \code{is_na} returns \code{TRUE} if \code{x} are all \code{NA} and otherwise
#' \code{FALSE}. Meanwhile \code{any_na} returns \code{TRUE} if \code{x}
#' contains any NAs (but not recursively to lists and pairlists), otherwise
#' \code{FALSE}, while \code{is_scalar_na} returns \code{TRUE} if \code{x} is
#' singular \code{NA}.
#'
#' @inheritParams is_empty
#' @export
#' @examples
#' is_na(c(1, NA))
#' is_na(NA)
#' any_na(c(1, NA))
#' any_na(list(1, list(1, NA)))
#' @name nas
is_na <- function(x) all(is.na(x))

#' @export
#' @rdname nas
is_scalar_na <- function (x) length(x) == 1 && is.na(x)

#' @export
#' @rdname nas
any_na <- function(x) anyNA(x)
