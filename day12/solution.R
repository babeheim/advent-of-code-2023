
library(gtools)
library(stringr)

match_runs <- function(x, pattern, verbose = FALSE) {
  if (!is.matrix(x)) {
    d <- diff(x)
    out <- 1
    for (i in 1:length(d)) {
      if (d[i] == 1) {
        out[length(out)] <- out[length(out)] + 1
        if (out[length(out)] > pattern[length(out)]) {
          if (verbose) cat("too long!\n")
          return(FALSE)
        }
      } else {
        if (out[length(out)] != pattern[length(out)]) {
          if (verbose) cat("too short!\n")
          return(FALSE)
        }
        out <- c(out, 1)
      }
    }
    if (all.equal(out, pattern)) {
      return(TRUE)
    } else {
      stop("impossible!")
    }
  } else {
    out <- rep(NA, nrow(x))
    for (i in 1:nrow(x)) out[i] <- match_runs(x[i,], pattern, verbose = FALSE)
    return(out)
  }
}

count_possibilities <- function(path, part1 = TRUE, verbose = FALSE) {

  x <- readLines(path)
  set_str <- gsub("\\s.+$", "", x)
  bad_count_strs <- gsub("^.+\\s", "", x)

  if (!part1) {
    for (i in 1:length(x)) {
      set_str[i] <- paste(rep(set_str[i], 5), collapse = "?")
      bad_count_strs[i] <- paste(rep(bad_count_strs[i], 5), collapse = ",")
    }
  }

  # how many ? are there
  # how many # are missing?
  n_slots <- nchar(set_str)
  free_slots <- gregexpr("\\?", set_str)
  # bug: it will give -1 when no free slots are visible
  # n_free_slots <- unlist(lapply(free_slots, length))
  n_free_slots <- stringr::str_count(set_str, "\\?")

  bad_counts <- strsplit(bad_count_strs, ",")
  bad_counts <- lapply(bad_counts, as.numeric)
  n_bad <- unlist(lapply(bad_counts, sum))
  bad_slots <- gregexpr("#", set_str)
  # bug: it will give -1 when no bad slots are visible
  # n_bad_visible <- unlist(lapply(bad_slots, length))
  n_bad_visible <- stringr::str_count(set_str, "#")
  n_bad_left <- n_bad - n_bad_visible

  stopifnot(all(n_bad_left <= n_free_slots))
  stopifnot(all(0 <= n_bad_left))

  # first approach: brute-force every combination

  n_ways <- rep(NA, length(x))

  for (i in 1:length(x)) {

    if (n_bad_left[i] == 0 | n_bad_left[i] == n_free_slots[i]) {
      n_ways[i] <- 1
    } else {
      poss <- combinations(n_free_slots[i], n_bad_left[i], free_slots[[i]])
      if (n_bad_visible[i] > 0) {
        add <- matrix(bad_slots[[i]], byrow = TRUE, nrow = nrow(poss), ncol = length(bad_slots[[i]]))
        poss <- cbind(poss, add)
        poss <- t(apply(poss, 1, sort)) # possible locations of the bad slots
      }

      # check: all the slots are in the possible set
      stopifnot(apply(poss, 1, function(z) all(z %in% c(free_slots[[i]], bad_slots[[i]]))))

      # check:
      stopifnot(n_bad[i] == ncol(poss))

      fits <- poss[which(match_runs(poss, pattern = bad_counts[[i]])),,drop = FALSE]

      n_ways[i] <- sum(match_runs(poss, pattern = bad_counts[[i]]))
    }
    if (verbose) cat(i, "of", length(x), "\n")

  }

  out <- sum(n_ways)
  return(out)

}

stopifnot(count_possibilities("day12/test_input.txt") == 21)
tic("day 12, part 1")
stopifnot(count_possibilities("day12/input.txt") == 6488)
toc(log = TRUE)

cat("day 12, part 2 is unfinished\n")

# its not possible on even the test input using my original approach!
# count_possibilities("day12/test_input.txt", part1 = FALSE) == 525152
