has_attribute <- function(x, which) {
  !is_null(attr(x, which, TRUE))
}

has_name <- function(x, which) {
  length(which) == 1 && which %in% names(x)
}