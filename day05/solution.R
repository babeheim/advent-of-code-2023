

library(parallel)

extract_map <- function(flag, x) {

  flag_line <- grep(flag, x)

  breaks <- c(which(x == ""), length(x) + 1)

  start <- flag_line + 1
  stop <- min(breaks[which(breaks > flag_line)]) - 1

  map_length <- stop - start + 1
  mat <- matrix(NA, ncol = 3, nrow = map_length)
  for (i in 1:nrow(mat)) mat[i,] <- as.numeric(unlist(strsplit(x[start - 1 + i], split = "\\s+")))

  mat <- mat[order(mat[,2]),]

  # consistency check: the range never overlaps with the next range
  stopifnot(all(mat[1:(nrow(mat)-1),2] + (mat[1:(nrow(mat)-1),3] - 1) < mat[2:nrow(mat),2]))

  return(mat)

}

mapper <- function(x, map) {
  # find the range you are within (if any)
  if (any(map[, 2] <= x)) {
    check_row <- max(which(map[, 2] <= x))
    range_start <- map[check_row, 2]
    range_stop <- range_start - 1 + map[check_row, 3]
    if (x <= range_stop) {
      out <- x + (map[check_row, 1] - map[check_row, 2])
    } else {
      out <- x
    }
  } else {
    out <- x
  }
  return(out)
}

seed_to_location <- function(x, maps) {
  if (length(x) == 1) {
    for (i in 1:length(maps)) {
      x <- mapper(x, maps[[i]])
    }
    out <- x
  } else {
    out <- unlist(lapply(x, seed_to_location, maps))
  }
  return(out)
}

calc_seed_locations <- function(path, part1 = TRUE, n_cores = 4, verbose = FALSE) {

  x <- readLines(path)

  breaks <- c(which(x == ""), length(x) + 1)

  seeds <- as.numeric(unlist(strsplit(gsub("seeds:\\s+", "", x[1]), split = "\\s+")))

  dat <- list()

  dat$`seed-to-soil` <- extract_map("seed-to-soil", x)
  dat$`soil-to-fertilizer` <- extract_map("soil-to-fertilizer", x)
  dat$`fertilizer-to-water` <- extract_map("fertilizer-to-water", x)
  dat$`water-to-light` <- extract_map("water-to-light", x)
  dat$`light-to-temperature` <- extract_map("light-to-temperature", x)
  dat$`temperature-to-humidity` <- extract_map("temperature-to-humidity", x)
  dat$`humidity-to-location` <- extract_map("humidity-to-location", x)

  if (part1) {

    locations <- seed_to_location(seeds, dat)
    out <- min(locations)

  } else {

    running_min <- Inf
    n_seed_starts <- length(seeds) / 2

    seed_mins <- mclapply(1:n_seed_starts, function(j) {
        seed_start <- seeds[2 * (j-1) + 1]
        seed_stop <- seed_start - 1 + seeds[2 * (j-1) + 2]
        check <- seed_start
        while (check <= seed_stop) {
          check_location <- seed_to_location(check, dat)
          if (check_location < running_min) running_min <- check_location
          check <- check + 1
        }
        if (verbose) cat("checked seeds starting at", seeds[2 * (j-1) + 1], "\n")
        return(running_min)
      }, mc.cores = n_cores)

    out <- min(unlist(seed_mins))

  }

  return(out)

}

stopifnot(calc_seed_locations("day05/test_input.txt") == 35)

tic("day 05, part 1")
stopifnot(calc_seed_locations("day05/input.txt") == 199602917)
toc(log = TRUE)

stopifnot(calc_seed_locations("day05/test_input.txt", part1 = FALSE) == 46)

cat("day 05, part 2 works but is too slow\n")
# stopifnot(calc_seed_locations("day05/input.txt", part1 = FALSE, n_cores = 20) == 2254686)
# toc(log = TRUE) # takes 2.3 hours with 20 cores on a mac studio

