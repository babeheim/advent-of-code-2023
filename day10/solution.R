
calc_loop_dist <- function(path, part1 = TRUE) {
  x <- readLines(path)
  maze <- matrix(NA, nrow = length(x), ncol = nchar(x[1]))
  for (i in 1:length(x)) maze[i,] <- strsplit(x[i], split = "")[[1]]
  n_row <- nrow(maze)
  start_row <- grep("S", x)
  start_col <- which(maze[start_row,] == "S")
  start_i <- (start_col - 1) * n_row + start_row
  stopifnot(maze[start_i] == "S")
  loop <- data.frame(i = start_i, x = start_col,
    y = start_row, heading = NA, dist = 0)
  current_i <- start_i
  closed_loop <- FALSE
  heading <- NA
  while (!closed_loop) {
    if (is.na(heading)) {
      # initialization at S position (gotta find a heading)
      stopifnot(maze[current_i] == "S")
      # check north from S
      check_row <- loop$y - 1
      check_col <- loop$x
      if (start_row > 1 && maze[check_row, check_col] %in% c("|", "7", "F")) {
        new_heading <- 1
        next_col <- check_col
        next_row <- check_row
      } else {
        # check east from S
        check_row <- loop$y
        check_col <- loop$x + 1
        if (start_col < ncol(maze) && maze[check_row, check_col] %in% c("-", "7", "J")) {
          new_heading <- 0
          next_col <- check_col
          next_row <- check_row
        } else {
          # check south from S
          check_row <- loop$y + 1
          check_col <- loop$x
          if (start_row > nrow(maze) && maze[check_row, check_col] %in% c("|", "J", "L")) {
            new_heading <- 3
            next_col <- check_col
            next_row <- check_row
          } else {
            # check west from S
            check_row <- loop$y
            check_col <- loop$x - 1
            if (start_col > 1 && maze[check_row, check_col] %in% c("-", "F", "L")) {
              new_heading <- 2
              next_col <- check_col
              next_row <- check_row
            } else {
              stop("no valid next step found from start!")
            }
          }
        }
      }
    } else {
      # normal movement through maze
      if (heading == 0) {
        if (maze[current_i] == "-") {
          new_heading <- 0
          next_row <- current_row
          next_col <- current_col + 1
        } else if (maze[current_i] == "J") {
          new_heading <- 1
          next_row <- current_row - 1
          next_col <- current_col
        } else if (maze[current_i] == "7") {
          new_heading <- 3
          next_row <- current_row + 1
          next_col <- current_col
        }
      } else if (heading == 1) {
        if (maze[current_i] == "F") {
          new_heading <- 0
          next_row <- current_row
          next_col <- current_col + 1
        } else if (maze[current_i] == "|") {
          new_heading <- 1
          next_row <- current_row - 1
          next_col <- current_col
        } else if (maze[current_i] == "7") {
          new_heading <- 2
          next_row <- current_row
          next_col <- current_col - 1
        }
      } else if (heading == 2) {
        if (maze[current_i] == "L") {
          new_heading <- 1
          next_row <- current_row - 1
          next_col <- current_col
        } else if (maze[current_i] == "-") {
          new_heading <- 2
          next_row <- current_row
          next_col <- current_col - 1
        } else if (maze[current_i] == "F") {
          new_heading <- 3
          next_row <- current_row + 1
          next_col <- current_col
        }
      } else if (heading == 3) {
        if (maze[current_i] == "L") {
          new_heading <- 0
          next_row <- current_row
          next_col <- current_col + 1
        } else if (maze[current_i] == "|") {
          new_heading <- 3
          next_row <- current_row + 1
          next_col <- current_col
        } else if (maze[current_i] == "J") {
          new_heading <- 2
          next_row <- current_row
          next_col <- current_col - 1
        }
      }
    }
    # update position
    heading <- new_heading
    current_row <- next_row
    current_col <- next_col
    current_i <- (current_col - 1) * n_row + current_row
    # record move to history
    add <- list(i = current_i, x = current_col, y = current_row,
      heading = heading, dist = loop$dist[nrow(loop)] + 1)
    loop <- bind_rows(loop, add)
    closed_loop <- (current_i == start_i & nrow(loop) > 4)
  }
  if (part1) {
    loop$dist_rev <- max(loop$dist, na.rm = TRUE) - loop$dist + 1
    loop$dist_both <- pmin(loop$dist, loop$dist_rev)
    out <- max(loop$dist_both, na.rm = TRUE)
  } else {
    # https://www.reddit.com/r/adventofcode/comments/18fgddy/2023_day_10_part_2_using_a_rendering_algorithm_to/
    S_faces_north <- as.numeric(
      (loop$x[2] == loop$x[1] & loop$y[2] == loop$y[1] - 1) | 
      (loop$x[nrow(loop)] == loop$x[1] & loop$y[nrow(loop)] == loop$y[1] - 1)
    )
    if (S_faces_north) {
      north_set <- c("|", "J", "L", "S")
    } else {
      north_set <- c("|", "J", "L")
    }
    n_inside <- 0L
    inside_loop <- FALSE 
    for (i in 1:nrow(maze)) {
      for (j in 1:ncol(maze)) {
        on_loop <- any(loop$y == i & loop$x == j)
        if (on_loop) {
          # flip "for each pipe that has a side pointing north"
          if (maze[i,j] %in% north_set) {
            inside_loop <- !inside_loop
          }
        } else {
          if (inside_loop) {
            n_inside <- n_inside + 1L
          }
        }
      }
      n_inside
    }
    out <- n_inside
  }
  return(out)
}

stopifnot(calc_loop_dist("day10/test_input.txt") == 4)
stopifnot(calc_loop_dist("day10/test_input_ii.txt") == 8)

tic("day 10, part 1")
stopifnot(calc_loop_dist("day10/input.txt") == 6697)
toc(log = TRUE)

stopifnot(calc_loop_dist("day10/test_input_iii.txt", part1 = FALSE) == 4)
stopifnot(calc_loop_dist("day10/test_input_iv.txt", part1 = FALSE) == 8)
stopifnot(calc_loop_dist("day10/test_input_v.txt", part1 = FALSE) == 10)

tic("day 10, part 2")
stopifnot(calc_loop_dist("day10/input.txt", part1 = FALSE) == 423)
toc(log = TRUE)