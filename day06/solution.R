
calc_win_ways <- function(path, part1 = TRUE) {
  x <- readLines(path)
  times <- gsub("Time:\\s+", "", x[1])
  records <- gsub("Distance:\\s+", "", x[2])
  if (part1) {
    times <- as.numeric(unlist(strsplit(times, split = "\\s+")))
    records <- as.numeric(unlist(strsplit(records, split = "\\s+")))
  } else {
    times <- as.numeric(gsub("\\s+", "", times))
    records <- as.numeric(gsub("\\s+", "", records))
  }
  n_ways <- rep(NA, length(times))
  for (i in 1:length(n_ways)) {
    s1 <- (times[i] + sqrt(times[i]^2 - 4 * records[i])) / 2
    s2 <- (times[i] - sqrt(times[i]^2 - 4 * records[i])) / 2
    n_ways[i] <- floor(s1 - 1e-10) - ceiling(s2 + 1e-10) + 1
  }
  out <- prod(n_ways)
  return(out)
}

stopifnot(calc_win_ways("day06/test_input.txt") == 288)
tic("day 06, part 1")
stopifnot(calc_win_ways("day06/input.txt") == 1710720)
toc(log = TRUE)

stopifnot(calc_win_ways("day06/test_input.txt", part1 = FALSE) == 71503)
tic("day 06, part 2")
stopifnot(calc_win_ways("day06/input.txt", part1 = FALSE) == 35349468)
toc(log = TRUE)
