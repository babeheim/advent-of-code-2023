
options(scipen = 999)

calc_pool_area <- function(path, part1 = TRUE) {

  x <- readLines(path)
  x <- strsplit(x, "\\s")

  for (i in 1:length(x)) {
    add <- data.frame(direction = x[[i]][1], length = x[[i]][2], color = x[[i]][3])
    if (i == 1) {
      inst <- add
    } else {
      inst <- bind_rows(inst, add)
    }
  }
  inst$color <- gsub("\\(", "", inst$color)
  inst$color <- gsub("\\)", "", inst$color)
  inst$length <- as.integer(inst$length)

  if (!part1) {
    # The first five hexadecimal digits encode the distance in meters as a five-digit hexadecimal numbe
    hex_length <- paste0("0x", substr(inst$color, 2, 6))
    inst$length <- strtoi(hex_length)
    # 0 means R, 1 means D, 2 means L, and 3 means U.
    inst$direction <- substr(inst$color, 7, 7)
    inst$direction[inst$direction == "0"] <- "R"
    inst$direction[inst$direction == "1"] <- "D"
    inst$direction[inst$direction == "2"] <- "L"
    inst$direction[inst$direction == "3"] <- "U"
  }

  corners <- data.frame(x = 0L, y = 0L, last_color = NA, turn = NA)

  for (i in 1:nrow(inst)) {
    if (inst$direction[i] == "R") {
      add <- data.frame(x = corners$x[i] + inst$length[i], y = corners$y[i])
    }
    if (inst$direction[i] == "L") {
      add <- data.frame(x = corners$x[i] - inst$length[i], y = corners$y[i])
    }
    if (inst$direction[i] == "U") {
      add <- data.frame(x = corners$x[i], y = corners$y[i] + inst$length[i])
    }
    if (inst$direction[i] == "D") {
      add <- data.frame(x = corners$x[i], y = corners$y[i] - inst$length[i])
    }
    add$turn <- inst$next_turn[i]
    add$last_color <- inst$color[i]
    corners <- bind_rows(corners, add)
    if (i == nrow(inst)) {
      stopifnot(add$x == 0L & add$y == 0L)
      corners$last_color[1] <- add$last_color
    }
  }

  edges <- data.frame(x_start = corners$x[-nrow(corners)], x_stop = corners$x[-1],
    y_start = corners$y[-nrow(corners)], y_stop = corners$y[-1], color = inst$color)

  xs <- sort(unique(corners$x))
  xs <- as.numeric(xs)

  pool_area <- 0

  for (i in 1:(length(xs)-1)) {
    x_start <- xs[i]
    x_stop <- xs[i+1]
    
    tar <- which(edges$x_start <= x_start & edges$x_stop >= x_stop |
      edges$x_stop <= x_start & edges$x_start >= x_stop)
    box_edges <- edges[tar,]
    box_edges <- box_edges[order(box_edges$y_start),]
    box_edges$y_start <- as.numeric(box_edges$y_start)
    box_edges$y_stop <- as.numeric(box_edges$y_stop)
    stopifnot(length(tar) %% 2 == 0)

    strip_i_area <- 0
    n_boxes <- length(tar) %/% 2
    for (j in 1:n_boxes) {
      strip_top <- box_edges$y_start[2 * (j - 1) + 1]
      strip_bottom <- box_edges$y_start[2 * (j - 1) + 2]
      strip_i_area <- strip_i_area + abs(x_start - x_stop) * abs(strip_top - strip_bottom)
    }

    pool_area <- pool_area + strip_i_area

  }

  corner_pieces <- 1 # always true!
  out <- pool_area + sum(inst$length)/2 + corner_pieces 

  return(out)

}

stopifnot(calc_pool_area("day18/test_input.txt") == 62)
tic("day 18, part 1")
stopifnot(calc_pool_area("day18/input.txt") == 95356)
toc(log = TRUE)

stopifnot(calc_pool_area("day18/test_input.txt", part1 = FALSE) == 952408144115)
tic("day 18, part 2")
stopifnot(calc_pool_area("day18/input.txt", part1 = FALSE) == 92291468914147)
toc(log = TRUE)


