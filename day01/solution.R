
sum_values <- function(path, part1 = TRUE) {
  
  x_raw <- readLines(path)
  x <- x_raw
  # fix overlaps...
  x <- gsub("twone", "twoone", x)
  x <- gsub("oneight", "oneeight", x)
  x <- gsub("threeight", "threeeight", x)
  x <- gsub("fiveight", "fiveeight", x)
  x <- gsub("nineight", "nineeight", x)
  x <- gsub("eightwo", "eighttwo", x)
  x <- gsub("eighthree", "eightthree", x)
  x <- gsub("sevenine", "sevennine", x)

  if (part1) {
    pattern <- "\\d"
  } else {
    pattern <- "(\\d|one|two|three|four|five|six|seven|eight|nine)"
  }
  all_matches <- gregexpr(pattern, x)
  matched_strings <- regmatches(x, all_matches)

  matched_strings <- lapply(matched_strings, function(x) gsub("one", "1", x))
  matched_strings <- lapply(matched_strings, function(x) gsub("two", "2", x))
  matched_strings <- lapply(matched_strings, function(x) gsub("three", "3", x))
  matched_strings <- lapply(matched_strings, function(x) gsub("four", "4", x))
  matched_strings <- lapply(matched_strings, function(x) gsub("five", "5", x))
  matched_strings <- lapply(matched_strings, function(x) gsub("six", "6", x))
  matched_strings <- lapply(matched_strings, function(x) gsub("seven", "7", x))
  matched_strings <- lapply(matched_strings, function(x) gsub("eight", "8", x))
  matched_strings <- lapply(matched_strings, function(x) gsub("nine", "9", x))

  matched_strings |>
    lapply(function(x) as.numeric(paste0(head(x, 1), tail(x, 1)))) |>
    unlist() |>
    sum() -> out
  return(out)
}

stopifnot(sum_values("day01/test_input.txt") == 142)

tic("day 01, part 1")
stopifnot(sum_values("day01/input.txt") == 56049)
toc()

stopifnot(sum_values("day01/test_input_ii.txt", part1 = FALSE) == 281)

tic("day 01, part 2")
stopifnot(sum_values("day01/input.txt", part1 = FALSE) == 54530) 
# 54538, too high (allegedly)..
# also 54539 which is also too high??
toc()

