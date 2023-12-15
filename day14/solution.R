
line_shift <- function(x, left = TRUE) {
  if (!left) x <- rev(x)
  stops <- c(0, which(x == "#"), (length(x) + 1))
  rocks <- which(x == "O")
  for (i in 1:(length(stops)-1)) {
    hits <- rocks[which(rocks > stops[i] & rocks < stops[i+1])]
    if (length(hits) > 0) {
      x[hits] <- "."
      x[stops[i] + 1:length(hits)] <- "O"
    }
  }
  if (!left) x <- rev(x)
  return(x)
}

shift_rocks <- function(path, part1 = TRUE, target_cycle = 1e9) {
  x <- readLines(path)
  dat <- str_to_mat(x)
  if (part1) {
    dat <- apply(dat, 2, line_shift) # roll north
    out <- sum(rowSums(dat == "O") * nrow(dat):1)
  } else if (target_cycle < 10) {
    for (i in 1:target_cycle) {
      dat <- apply(dat, 2, line_shift) # roll north
      dat <- t(apply(dat, 1, line_shift)) # roll west
      dat <- apply(dat, 2, line_shift, left = FALSE) # roll south
      dat <- t(apply(dat, 1, line_shift, left = FALSE)) # roll east
      if (i %% 100 == 0) print(i)
    }
    out <- sum(rowSums(dat == "O") * nrow(dat):1)
  } else {
    anchor_cycle <- 200 # seems safe
    n_cycles_sim <- 400
    scores <- rep(NA, length.out = n_cycles_sim)
    for (i in 1:n_cycles_sim) {
      dat <- apply(dat, 2, line_shift) # roll north
      dat <- t(apply(dat, 1, line_shift)) # roll west
      dat <- apply(dat, 2, line_shift, left = FALSE) # roll south
      dat <- t(apply(dat, 1, line_shift, left = FALSE)) # roll east
      scores[i] <- sum(rowSums(dat == "O") * nrow(dat):1)
    }
    hits <- which(scores == scores[anchor_cycle])
    hits <- hits[hits > anchor_cycle]
    for (i in 1:length(hits)) {
      candidate_period <- (hits[i] - anchor_cycle)
      n_test <- (n_cycles_sim - anchor_cycle) %/% candidate_period
      test_points <- anchor_cycle + candidate_period * 1:n_test
      if (all(scores[test_points] == scores[anchor_cycle])) {
        period <- candidate_period
        break()
      }
      if (i == length(hits)) {
        stop("no stable period found!")
      }
    }
    n_periods <- (target_cycle - anchor_cycle) %/% period
    remaining_cycles <- (target_cycle - anchor_cycle) %% period
    stopifnot(all.equal(anchor_cycle + n_periods * period + remaining_cycles, target_cycle))
    out <- scores[anchor_cycle + remaining_cycles] 
  }
  return(out)
}

stopifnot(shift_rocks("day14/test_input.txt") == 136)

tic("day 14, part 1")
stopifnot(shift_rocks("day14/input.txt") == 108826)
toc(log = TRUE)

stopifnot(shift_rocks("day14/test_input.txt", part1 = FALSE, target_cycle = 1) == 87)
stopifnot(shift_rocks("day14/test_input.txt", part1 = FALSE, target_cycle = 2) == 69)
stopifnot(shift_rocks("day14/test_input.txt", part1 = FALSE, target_cycle = 3) == 69)

stopifnot(shift_rocks("day14/test_input.txt", part1 = FALSE, target_cycle = 1e9) == 64)

tic("day 14, part 2")
stopifnot(shift_rocks("day14/input.txt", part1 = FALSE) == 99291)
toc(log = TRUE)
