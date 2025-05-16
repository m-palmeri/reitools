toStringWithAnd <- function(x, quote = FALSE) {
  x <- as.character(x)
  if (quote) {
    x <- sQuote(x, FALSE)
  }
  n <- length(x)
  if (n == 0) {
    return("")
  } else if (n == 1) {
    return(x)
  } else if (n == 2) {
    return(paste(x, collapse = " and "))
  } else {
    return(paste(paste(x[1:(n-1)], collapse = ", "), "and", x[n]))
  }
}
