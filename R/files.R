#' File system predicates
#'
#' @param x file system object to test
#' @param ext file extension as a string
#' @return logical value
#' @export
#' @examples
#' does_file_exist(system.file("DESCRIPTION")
#' is_directory(system.file())
#' has_extension(system.file("DESCRIPTION"), "")
#' has_extension(system.file("R/base.rdb"), "rdb")
#' @name filepreds

does_file_exist <- function(x) {
  is_scalar_character(x) && file.exists(x)
}

#' @export
#' @rdname filepreds
is_directory <- function(x) {
  does_file_exist(x) && file.info(x)$isdir
}

#' @export
#' @rdname filepreds
is_writeable <- function(x) {
  does_file_exist(x) && file.access(x, mode = 2)[[1]] == 0
}

#' @export
#' @rdname filepreds
is_readable <- function(x) {
  does_file_exist(x) && file.access(x, mode = 4)[[1]] == 0
}

#' @export
#' @rdname filepreds
has_extension <- function (x, ext) {
  is_scalar_character(x) && is_scalar_character(ext) &&
    tools::file_ext(x) == ext
}

