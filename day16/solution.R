
turn_right <- function(x) (x - 1) %% 4

turn_left <- function(x) (x + 1) %% 4

# hacky, repetitive...

beam_dfs_sim <- function(edge, obj, beam) {
  is_energized <- rep(FALSE, length.out = edge^2)
  hist <- list(integer(), integer(), integer(), integer()) # is this memoization?
  stack <- list(beam)
  while (length(stack) > 0) {
    beam <- stack[[length(stack)]]
    stack[length(stack)] <- NULL
    active <- TRUE
    while (active) {
      if (beam$heading == 0) { # beam is going east
        split_char <- "|"
        right_char <- "\\"
        left_char <- "/"
        path_objs <- which(
          obj$row == beam$row &
          obj$col > beam$col &
          obj$type %in% c(split_char, left_char, right_char)
        )
        if (length(path_objs) > 0) {
          next_obj <- path_objs[which.min(obj$col[path_objs])] # going east
          # trace out path, energize tiles, and do backtrack test
          add <- data.frame(row = beam$row, col = (beam$col + 1):obj$col[next_obj])
          add$i <- (add$col - 1L) * edge + add$row
          is_energized[add$i] <- TRUE
          backtracked <- any(add$i %in% hist[[beam$heading + 1]])
          if (backtracked) {
            active <- FALSE
          } else {
            # send beam to next object
            beam$col <- obj$col[next_obj]
            beam$row <- obj$row[next_obj]
            hist[[beam$heading + 1]] <- c(hist[[beam$heading + 1]], add$i)
            if (obj$type[next_obj] == split_char) {
              split <- beam
              split$heading <- turn_left(beam$heading)
              stack[[length(stack) + 1]] <- split
              beam$heading <- turn_right(beam$heading)
            } else if (obj$type[next_obj] == right_char) {
              beam$heading <- turn_right(beam$heading)
            } else if (obj$type[next_obj] == left_char) {
              beam$heading <- turn_left(beam$heading)
            }
          }
        } else {
          if (beam$col < edge) {
            add <- data.frame(row = beam$row, col = (beam$col + 1):edge)
            add$i <- (add$col - 1) * edge + add$row
            is_energized[add$i] <- TRUE
          }
          active <- FALSE
        }
      } else if (beam$heading == 2) { # beam is going west
        split_char <- "|"
        right_char <- "\\"
        left_char <- "/"
        path_objs <- which(
          obj$row == beam$row &
          obj$col < beam$col &
          obj$type %in% c(split_char, left_char, right_char)
        )
        if (length(path_objs) > 0) {
          # send beam to next object
          next_obj <- path_objs[which.max(obj$col[path_objs])] # going west
          # trace out path, energize tiles, and do backtrack test
          add <- data.frame(row = beam$row, col = (beam$col - 1):obj$col[next_obj])
          add$i <- (add$col - 1) * edge + add$row
          is_energized[add$i] <- TRUE
          backtracked <- any(add$i %in% hist[[beam$heading + 1]])
          if (backtracked) {
            active <- FALSE
          } else {
            # send beam to next object
            beam$col <- obj$col[next_obj]
            beam$row <- obj$row[next_obj]
            hist[[beam$heading + 1]] <- c(hist[[beam$heading + 1]], add$i)
            if (obj$type[next_obj] == split_char) {
              split <- beam
              split$heading <- turn_left(beam$heading)
              stack[[length(stack) + 1]] <- split
              beam$heading <- turn_right(beam$heading)
            } else if (obj$type[next_obj] == right_char) {
              beam$heading <- turn_right(beam$heading)
            } else if (obj$type[next_obj] == left_char) {
              beam$heading <- turn_left(beam$heading)
            }
          }
        } else {
          if (beam$col > 1) {
            add <- data.frame(row = beam$row, col = (beam$col - 1):1)
            add$i <- (add$col - 1) * edge + add$row
            is_energized[add$i] <- TRUE
          }
          active <- FALSE
        }
      } else if (beam$heading == 1) { # beam is going north
        split_char <- "-"
        left_char <- "\\"
        right_char <- "/"
        path_objs <- which(
          obj$row < beam$row &
          obj$col == beam$col &
          obj$type %in% c(split_char, left_char, right_char)
        )
        if (length(path_objs) > 0) {
          next_obj <- path_objs[which.max(obj$row[path_objs])] # going north
          # trace out path, energize tiles, and do backtrack test
          add <- data.frame(row = (beam$row - 1):obj$row[next_obj], col = beam$col)
          add$i <- (add$col - 1) * edge + add$row
          is_energized[add$i] <- TRUE
          backtracked <- any(add$i %in% hist[[beam$heading + 1]])
          if (backtracked) {
            active <- FALSE
          } else {
            beam$col <- obj$col[next_obj]
            beam$row <- obj$row[next_obj]
            hist[[beam$heading + 1]] <- c(hist[[beam$heading + 1]], add$i)
            if (obj$type[next_obj] == split_char) {
              split <- beam
              split$heading <- turn_left(beam$heading)
              stack[[length(stack) + 1]] <- split
              beam$heading <- turn_right(beam$heading)
            } else if (obj$type[next_obj] == left_char) {
              beam$heading <- turn_left(beam$heading)
            } else if (obj$type[next_obj] == right_char) {
              beam$heading <- turn_right(beam$heading)
            }
          }
        } else {
          if (beam$row > 1) {
            add <- data.frame(row = (beam$row - 1):1, col = beam$col)
            add$i <- (add$col - 1) * edge + add$row
            is_energized[add$i] <- TRUE
          }
          active <- FALSE
        }
      } else if (beam$heading == 3) { # beam is going south
        split_char <- "-"
        left_char <- "\\"
        right_char <- "/"
        path_objs <- which(
          obj$row > beam$row &
          obj$col == beam$col &
          obj$type %in% c(split_char, left_char, right_char)
        )
        if (length(path_objs) > 0) {
          next_obj <- path_objs[which.min(obj$row[path_objs])] # going south
          # trace out path, energize tiles, and do backtrack test
          add <- data.frame(row = (beam$row + 1):obj$row[next_obj], col = beam$col)
          add$i <- (add$col - 1) * edge + add$row
          is_energized[add$i] <- TRUE
          backtracked <- any(add$i %in% hist[[beam$heading + 1]])
          if (backtracked) {
            active <- FALSE
          } else {
            # send beam to next object
            beam$col <- obj$col[next_obj]
            beam$row <- obj$row[next_obj]
            hist[[beam$heading + 1]] <- c(hist[[beam$heading + 1]], add$i)
            if (obj$type[next_obj] == split_char) {
              split <- beam
              split$heading <- turn_left(beam$heading)
              stack[[length(stack) + 1]] <- split
              beam$heading <- turn_right(beam$heading)
            } else if (obj$type[next_obj] == left_char) {
              beam$heading <- turn_left(beam$heading)
            } else if (obj$type[next_obj] == right_char) {
              beam$heading <- turn_right(beam$heading)
            }
          }
        } else {
          if (beam$row < edge) {
            add <- data.frame(row = (beam$row + 1):edge, col = beam$col)
            add$i <- (add$col - 1) * edge + add$row
            is_energized[add$i] <- TRUE
          }
          active <- FALSE
        }
      }
    }
  } # DFS while loop
  out <- sum(is_energized)
  return(out)
}


count_energized_tiles <- function(path, part1 = TRUE, verbose = FALSE) {

  x <- readLines(path)
  map <- str_to_mat(x)

  edge <- nrow(map)
  stopifnot(ncol(map) == edge)

  # create object table
  add <- data.frame(
    i = which(map == "|")
  )
  add$row <- (add$i - 1L) %% edge + 1L
  add$col <- (add$i - 1L) %/% edge + 1L
  add$type <- "|"
  obj <- add
  add <- data.frame(
    i = which(map == "-")
  )
  add$row <- (add$i - 1L) %% edge + 1L
  add$col <- (add$i - 1L) %/% edge + 1L
  add$type <- "-"
  obj <- bind_rows(obj, add)
  add <- data.frame(
    i = which(map == "/")
  )
  add$row <- (add$i - 1L) %% edge + 1L
  add$col <- (add$i - 1L) %/% edge + 1L
  add$type <- "/"
  obj <- bind_rows(obj, add)
  add <- data.frame(
    i = which(map == "\\")
  )
  add$row <- (add$i - 1L) %% edge + 1L
  add$col <- (add$i - 1L) %/% edge + 1L
  add$type <- "\\"
  obj <- bind_rows(obj, add)

  if (part1) {
    # initialize beam outside map
    beam <- list(row = 1L, col = 0L, heading = 0)
    # use DFS to simulate beam paths and count energized tiles
    out <- beam_dfs_sim(edge, obj, beam)
  } else {
    north_scores <- rep(NA, edge)
    for (i in 1:length(north_scores)) {
      beam <- list(row = 0L, col = i, heading = 3)
      north_scores[i] <- beam_dfs_sim(edge, obj, beam)
    }
    south_scores <- rep(NA, edge)
    for (i in 1:length(south_scores)) {
      beam <- list(row = edge + 1L, col = i, heading = 1)
      south_scores[i] <- beam_dfs_sim(edge, obj, beam)
    }
    west_scores <- rep(NA, edge)
    for (i in 1:length(west_scores)) {
      beam <- list(row = i, col = 0L, heading = 0)
      west_scores[i] <- beam_dfs_sim(edge, obj, beam)
    }
    east_scores <- rep(NA, edge)
    for (i in 1:length(east_scores)) {
      beam <- list(row = i, col = edge + 1L, heading = 2)
      east_scores[i] <- beam_dfs_sim(edge, obj, beam)
    }
    out <- max(north_scores, south_scores, west_scores, east_scores)
  }

  return(out)

}

stopifnot(count_energized_tiles("day16/test_input.txt") == 46)

tic("day 16, part 1")
stopifnot(count_energized_tiles("day16/input.txt", verbose = FALSE) == 7870)
toc(log = TRUE)

stopifnot(count_energized_tiles("day16/test_input.txt", part1 = FALSE) == 51)

tic("day 16, part 2")
stopifnot(count_energized_tiles("day16/input.txt", part1 = FALSE) == 8143)
toc(log = TRUE)
