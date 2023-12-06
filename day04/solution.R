
extract_nums <- function(x, pattern) {
  all_matches <- gregexpr(pattern, x, perl = TRUE)
  out <- regmatches(x, all_matches)
  for (i in 1:length(out)) {
    # to account for single-digit numbers having a leading whitespace:
    out[[i]] <- gsub("^\\s", "", out[[i]])
    out[[i]] |>
      strsplit(split = "\\s+") |>
      unlist() |>
      as.numeric() -> out[[i]]
  }
  return(out)
}

count_cards <- function(path, part1 = TRUE) {
  x <- readLines(path)
  winners <- extract_nums(x, "(?<=:\\s{1})(\\s|\\d)+(?=|)")
  mine <- extract_nums(x, "(?<=\\|\\s{1})(\\s|\\d)+$")
  cards <- data.frame(n_matches = rep(NA, length(x)), count = 1)
  for (i in 1:length(x)) cards$n_matches[i] <- sum(mine[[i]] %in% winners[[i]])
  if (part1) {
    out <- sum((cards$n_matches > 0) * 2^(cards$n_matches - 1))
  } else {
    for (i in 1:nrow(cards)) {
      if (cards$n_matches[i] > 0) {
        cards$count[i + 1:cards$n_matches[i]] <- (cards$count[i + 1:cards$n_matches[i]] + cards$count[i])
      }
    }
    out <- sum(cards$count)
  }
  return(out)
}

stopifnot(count_cards("day04/test_input.txt") == 13)
tic("day 04, part 1")
stopifnot(count_cards("day04/input.txt") == 25004)
toc(log = TRUE)

stopifnot(count_cards("day04/test_input.txt", part1 = FALSE) == 30)
tic("day 04, part 2")
stopifnot(count_cards("day04/input.txt", part1 = FALSE) == 14427616)
toc(log = TRUE)
