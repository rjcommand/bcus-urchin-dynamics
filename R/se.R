se <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x))
}