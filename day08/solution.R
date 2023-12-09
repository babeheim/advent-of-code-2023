
options(scipen = 999)
library(numbers) # for LCM function

lcmer <- function(x) {
  out <- LCM(x[1], x[2])
  if (length(x) > 2) {
    for (i in 3:length(x)) {
      out <- LCM(out, x[i])
    }
  }
  return(out)
}

find_cycle_length <- function(path, part1 = TRUE) {

  x <- readLines(path)
  moves <- strsplit(x[1], split = "")[[1]]
  n_moves <- length(moves)

  nodes <- substr(x[3:length(x)], 1, 3)
  stopifnot(!any(duplicated(nodes)))
  left_nodes <- substr(x[3:length(x)], 8, 10)
  right_nodes <- substr(x[3:length(x)], 13, 15)
  left <- match(left_nodes, nodes)
  right <- match(right_nodes, nodes)

  if (part1) {
    start <- which(nodes == "AAA")
    dest <- which(nodes == "ZZZ")
  } else {
    start <- grep("A$", nodes)
    dest <- grep("Z$", nodes)
  }

  out <- rep(NA, length(start))

  for (i in 1:length(start)) {
    counter <- 0L
    pos <- start[i]
    while (!(pos %in% dest)) {
      counter <- counter + 1L
      counter_i <- (counter - 1L) %% n_moves + 1L
      if (moves[counter_i] == "L") 
        pos <- left[pos]
      else 
        pos <- right[pos]
    }
    out[i] <- counter
  }

  if (!part1) out <- lcmer(out)
  return(out)
}

stopifnot(find_cycle_length("day08/test_input.txt") == 2)
stopifnot(find_cycle_length("day08/test_input_ii.txt") == 6)

tic("day 08, part 1")
stopifnot(find_cycle_length("day08/input.txt") == 13207)
toc(log = TRUE)

stopifnot(find_cycle_length("day08/test_input_iii.txt", part1 = FALSE) == 6)

tic("day 08, part 2")
stopifnot(find_cycle_length("day08/input.txt", part1 = FALSE) == 12324145107121)
toc(log = TRUE)