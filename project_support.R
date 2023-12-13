
library(tictoc)
library(dplyr)

options(warnPartialMatchDollar = TRUE)


str_to_mat <- function(x) {
  out <- matrix(NA, nrow = length(x), ncol = nchar(x[1]))
  for (i in 1:length(x)) {
    out[i,] <- strsplit(x[i], "")[[1]]
  }
  return(out)
} # generally useful for advent!