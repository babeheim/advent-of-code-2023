
check_bags <- function(path, part1 = TRUE) {

  x <- readLines(path)

  valid_game <- rep(TRUE, length(x))

  # in part 1, we must spot bags which have less than this number of cubes of each color:
  red_max <- 12
  green_max <- 13
  blue_max <- 14

  # in part 2, we must calculate the minimum number of cubes of each color that could have been in the bag
  # aka the maximum number of cubes observed
  red_mins <- rep(NA, length(x))
  green_mins <- rep(NA, length(x))
  blue_mins <- rep(NA, length(x))

  for (i in 1:length(x)) {

    pattern <- "\\d+ red"
    all_matches <- gregexpr(pattern, x[i])
    counts <- regmatches(x[i], all_matches)[[1]]
    red_counts <- as.numeric(gsub("red", "", counts))

    red_mins[i] <- max(red_counts) 
    if (any(red_counts > red_max)) valid_game[i] <- FALSE

    pattern <- "\\d+ green"
    all_matches <- gregexpr(pattern, x[i])
    counts <- regmatches(x[i], all_matches)[[1]]
    green_counts <- as.numeric(gsub("green", "", counts))

    green_mins[i] <- max(green_counts) 
    if (any(green_counts > green_max)) valid_game[i] <- FALSE

    pattern <- "\\d+ blue"
    all_matches <- gregexpr(pattern, x[i])
    counts <- regmatches(x[i], all_matches)[[1]]
    blue_counts <- as.numeric(gsub("blue", "", counts))

    blue_mins[i] <- max(blue_counts) 
    if (any(blue_counts > blue_max)) valid_game[i] <- FALSE

  }

  if (part1) {
    out <- sum(which(valid_game))
  } else {
    out <- sum(red_mins * green_mins * blue_mins)
  }
  return(out)

}

stopifnot(check_bags("day02/test_input.txt") == 8)

tic("day 02, part 1")
stopifnot(check_bags("day02/input.txt") == 2810)
toc()

stopifnot(check_bags("day02/test_input.txt", part1 = FALSE) == 2286)

tic("day 02, part 1")
stopifnot(check_bags("day02/input.txt", part1 = FALSE) == 69110)
toc()
