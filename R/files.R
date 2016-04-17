does_file_exist <- function(x) {
  is_scalar_character(x) && file.exists(x)
}

is_directory <- function(x) {
  does_file_exist(x) && file.info(x)$isdir
}

is_writeable <- function(x) {
  does_file_exist(x) && file.access(x, mode = 2)[[1]] == 0
}

is_readable <- function(x) {
  does_file_exist(x) && file.access(x, mode = 4)[[1]] == 0
}

has_extension <- function (x, ext) {
  is_scalar_character(x) && is_scalar_character(ext) &&
    tools::file_ext(x) == ext
}

