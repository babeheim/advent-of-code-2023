
differ <- function(x, n_times = 1) {
  if (all(diff(x) == 0)) {
    out <- n_times
  } else {
    out <- differ(diff(x), n_times = n_times + 1)
  }
  return(out)
}

extrapolate <- function(path, part1 = TRUE) {
  x <- readLines(path)
  dat <- strsplit(x, split = "\\s+")
  dat <- lapply(dat, as.numeric)
  n_step <- length(dat[[1]])
  pred <- rep(NA, length(dat))
  for (i in 1:length(dat)) {
    y <- dat[[i]]
    order <- differ(y)
    mat <- list()
    mat[[1]] <- y
    for (j in 2:order) mat[[j]] <- diff(mat[[j-1]])
    coefs <- rep(NA, order)
    for (j in order:1) {
      if (part1) 
        coefs[order - j + 1] <- mat[[j]][length(mat[[j]])]
      else 
        coefs[order - j + 1] <- mat[[j]][1]
    }
    if (part1) {
      output <- cumsum(coefs)
    } else {
      output <- cumsum(coefs * (-1)^c(1:order - order %% 2))
    }
    pred[i] <- output[length(output)]
  }
  out <- sum(pred)
  return(out)
}

stopifnot(extrapolate("day09/test_input.txt") == 114)

tic("day 09, part 1")
stopifnot(extrapolate("day09/input.txt") == 1798691765)
toc(log = TRUE)

stopifnot(extrapolate("day09/test_input.txt", part1 = FALSE) == 2)

tic("day 09, part 2")
stopifnot(extrapolate("day09/input.txt", part1 = FALSE) == 1104)
toc(log = TRUE)

