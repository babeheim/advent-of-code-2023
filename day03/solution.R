

calc_gear_ratios <- function(path, verbose = FALSE, part1 = TRUE) {
  x <- readLines(path)
  map <- matrix(NA, ncol = nchar(x[1]), nrow = length(x))
  for (i in 1:nrow(map)) {
    map[i,] <- strsplit(x[i], "")[[1]]
  }
  for (i in 1:length(x)) {
    if (grepl("\\*", x[i])) {
      gears_found <- gregexpr("\\*", x[i])
      add <- data.frame(
        row = i,
        col = as.numeric(gears_found[[1]])
      )
      if (!exists("gears")) {
        gears <- add
      } else {
        gears <- bind_rows(gears, add)
      }
    }
  }
  # strategy: for each number, we identify its start and stop characters, and then use those as address points to check if there's an adjacent symbol in the map object
  for (i in 1:length(x)) {
    if (grepl("\\d+", x[i])) {
      nums_found <- gregexpr("\\d+", x[i])
      add <- data.frame(
        row = i,
        nums = regmatches(x[i], nums_found)[[1]],
        col_start = as.numeric(nums_found[[1]])
      )
      add$col_stop <- add$col_start + nchar(add$nums) - 1
      add$valid <- FALSE
      add$gear_num <- NA
      for (j in 1:nrow(add)) {
        # for each number j in line i of the map...
        # check the map entries above and below
        # check the adjacent entries too
        # then check the four diagonal corners
        after_first_line <- i > 1
        before_last_line <- i < nrow(map)
        after_first_col <- add$col_start[j] > 1
        before_last_col <- add$col_stop[j] < ncol(map)
        if (after_first_line) {
          if (verbose) print("checked above")
          check_row <- i - 1
          check <- map[check_row, add$col_start[j]:add$col_stop[j]]
          if (!all(check %in% c(0:9, "."))) add$valid[j] <- add$valid[j] | TRUE
          if (any(check %in% c("*"))) {
            if (sum(check == "*") > 1) stop()
            gear_col <- which(check == "*") - 1 + add$col_start[j]
            add$gear_num[j] <- which(gears$row == check_row & gears$col == gear_col)
          }
        }
        if (before_last_line) {
          if (verbose) print("checked below")
          check_row <- i + 1
          check <- map[check_row, add$col_start[j]:add$col_stop[j]]
          if (!all(check %in% c(0:9, "."))) add$valid[j] <- add$valid[j] | TRUE
          if (any(check %in% c("*"))) {
            if (sum(check == "*") > 1) stop()
            gear_col <- which(check == "*") - 1 + add$col_start[j]
            add$gear_num[j] <- which(gears$row == check_row & gears$col == gear_col)
          }
        }
        if (after_first_col) {
          if (verbose) print("checked left")
          check_row <- i
          check <- map[check_row, add$col_start[j] - 1]
          if (!all(check %in% c(0:9, "."))) add$valid[j] <- add$valid[j] | TRUE
          if (any(check %in% c("*"))) {
            if (sum(check == "*") > 1) stop()
            gear_col <- add$col_start[j] - 1
            add$gear_num[j] <- which(gears$row == check_row & gears$col == gear_col)
          }
        }
        if (before_last_col) {
          if (verbose) print("checked right")
          check_row <- i
          check <- map[check_row, add$col_stop[j] + 1]
          if (!all(check %in% c(0:9, "."))) add$valid[j] <- add$valid[j] | TRUE
          if (any(check %in% c("*"))) {
            if (sum(check == "*") > 1) stop()
            gear_col <- add$col_stop[j] + 1
            add$gear_num[j] <- which(gears$row == check_row & gears$col == gear_col)
          }
        }
        if (after_first_line & after_first_col) {
          if (verbose) print("checked upper-left diagonal")
          check_row <- i - 1
          check <- map[check_row, add$col_start[j] - 1]
          if (!all(check %in% c(0:9, "."))) add$valid[j] <- add$valid[j] | TRUE
          if (any(check %in% c("*"))) {
            if (sum(check == "*") > 1) stop()
            gear_col <- add$col_start[j] - 1
            add$gear_num[j] <- which(gears$row == check_row & gears$col == gear_col)
          }
        }
        if (after_first_line & before_last_col) {
          if (verbose) print("checked upper-right diagonal")
          check_row <- i - 1
          check <- map[check_row, add$col_stop[j] + 1]
          if (!all(check %in% c(0:9, "."))) add$valid[j] <- add$valid[j] | TRUE
          if (any(check %in% c("*"))) {
            if (sum(check == "*") > 1) stop()
            gear_col <- add$col_stop[j] + 1
            add$gear_num[j] <- which(gears$row == check_row & gears$col == gear_col)
          }
        }
        if (before_last_line & after_first_col) {
          if (verbose) print("checked lower-left diagonal")
          check_row <- i + 1
          check <- map[check_row, add$col_start[j] - 1]
          if (!all(check %in% c(0:9, "."))) add$valid[j] <- add$valid[j] | TRUE
          if (any(check %in% c("*"))) {
            if (sum(check == "*") > 1) stop()
            gear_col <- add$col_start[j] - 1
            add$gear_num[j] <- which(gears$row == check_row & gears$col == gear_col)
          }
        }
        if (before_last_line & before_last_col) {
          if (verbose) print("checked lower-right diagonal")
          check_row <- i + 1
          check <- map[check_row, add$col_stop[j] + 1]
          if (!all(check %in% c(0:9, "."))) add$valid[j] <- add$valid[j] | TRUE
          if (any(check %in% c("*"))) {
            if (sum(check == "*") > 1) stop()
            gear_col <- add$col_stop[j] + 1
            add$gear_num[j] <- which(gears$row == check_row & gears$col == gear_col)
          }
        }
      }
      if (i == 1) {
        inv <- add
      } else {
        inv <- bind_rows(inv, add)
      }
    }
  }

  inv$nums <- as.numeric(inv$nums)

  if (part1) {
    out <- sum(inv$nums[inv$valid])
  } else {
    out <- 0
    for (g in 1:nrow(gears)) {
      if (sum(inv$gear_num == g, na.rm = TRUE) == 2) {
        out <- out + prod(inv$nums[which(inv$gear_num == g)])
      }
    }
  }

  return(out)

}

stopifnot(calc_gear_ratios("day03/test_input.txt") == 4361)
tic("day 03, part 1")
stopifnot(calc_gear_ratios("day03/input.txt") == 540131)
toc(log = TRUE)

stopifnot(calc_gear_ratios("day03/test_input.txt", part1 = FALSE) == 467835)
tic("day 03, part 2")
stopifnot(calc_gear_ratios("day03/input.txt", part1 = FALSE) == 86879020)
toc(log = TRUE)

# more examples from reddit

stopifnot(calc_gear_ratios("day03/test_input_ii.txt") == 413)
stopifnot(calc_gear_ratios("day03/test_input_ii.txt", part1 = FALSE) == 6756)

stopifnot(calc_gear_ratios("day03/test_input_iii.txt") == 925)
stopifnot(calc_gear_ratios("day03/test_input_iii.txt", part1 = FALSE) == 6756)

