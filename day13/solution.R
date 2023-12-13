
count_reflections <- function(path, part1 = TRUE) {
  x <- readLines(path)
  if (part1) {
    tol <- 0
  } else {
    tol <- 1
  }
  breaks <- c(which(x == ""), length(x)+1)
  dat <- list()
  dat[[1]] <- x[1:(breaks[1]-1)]
  for (i in 2:length(breaks)) dat[[i]] <- x[(breaks[i-1]+1):(breaks[i]-1)]
  dat <- lapply(dat, str_to_mat)
  refls <- data.frame(
    n = rep(NA, length(dat)),
    d = rep(NA, length(dat))
  )
  # first, do row reflections
  for(i in 1:length(dat)) {
    mat <- dat[[i]]
    for (j in 1:(nrow(mat)-1)) {
      # the reflection window goes down from j and up from j+1 to the same max
      if ((2*j) <= nrow(mat)) {
        refl_rows <- j:1
        comp_rows <- (j+1):(2*j)
      } else {
        comp_rows <- (j+1):nrow(mat)
        refl_rows <- j:(j - length(comp_rows) + 1)
      }
      refl <- mat[refl_rows, , drop = FALSE]
      comp <- mat[comp_rows, , drop = FALSE]
      if (sum(refl != comp) == tol) {
        refls$n[i] <- j
        refls$d[i] <- "h"
        break()
      }
    }
  }
  # now do the columns reflections!
  for(i in 1:length(dat)) {
    mat <- dat[[i]]
    for (j in 1:(ncol(mat)-1)) {
      # the reflection window goes down from j and up from j+1 to the same max
      if ((2*j) <= ncol(mat)) {
        refl_cols <- j:1
        comp_cols <- (j+1):(2*j)
      } else {
        comp_cols <- (j+1):ncol(mat)
        refl_cols <- j:(j - length(comp_cols) + 1)
      }
      refl <- mat[ , refl_cols, drop = FALSE]
      comp <- mat[ , comp_cols, drop = FALSE]
      if (sum(refl != comp) == tol) {
        if (!is.na(refls$n[i])) stop()
        refls$n[i] <- j
        refls$d[i] <- "v"
        break()
      }
    }
  }
  out <- sum(refls$n[which(refls$d == "h")]) * 100 + sum(refls$n[which(refls$d == "v")])
  return(out)
}

stopifnot(count_reflections("day13/test_input.txt") == 405)
tic("day 13, part 1")
stopifnot(count_reflections("day13/input.txt") == 35538)
toc(log = TRUE)

stopifnot(count_reflections("day13/test_input.txt", part1 = FALSE) == 400)
tic("day 13, part 2")
stopifnot(count_reflections("day13/input.txt", part1 = FALSE) == 30442)
toc(log = TRUE)

