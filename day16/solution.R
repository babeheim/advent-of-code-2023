
rm(list = ls())

source("project_support.R")

check_overlap <- function(x, y, min_length = 2) {
  if (length(x) >= min_length) {
    if (x[1] %in% y) {
      tar <- which(y == x[1])
      for (i in 1:length(tar)) {
        if (tar[i] + (length(x) - 1) <= length(y)) {
          check <- tar[i] + 0:(length(x)-1)
          if (identical(x, y[check])) {
            return(TRUE)
          } else if (i == length(tar)) {
            return(FALSE)
          }
        } else {
          return(FALSE)
        }    
      }
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

count_illuminated_tiles <- function(path, verbose = FALSE) {

  x <- readLines(path)
  map <- str_to_mat(x)

  edge <- nrow(map) # check its square
  stopifnot(ncol(map) == edge)

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

  # create a DFS stack storing row, col and heading

  east_hist <- integer()
  west_hist <- integer()
  north_hist <- integer()
  south_hist <- integer()

  beam <- list(row = 1L, col = 1L, heading = 0, beam_id = 1)
  if (map[1,1] == "\\") {
    # crazy but this appears in my input data, implication is
    # the beam is actually firing downwards!
    beam$heading <- 3
    south_hist <- 1L
    # my code was never written to evaluate the first point...
  } else {
    east_hist <- 1L
  }

  stack <- list(beam)

  beam_counter <- 1

  is_energized <- rep(FALSE, length.out = edge^2)
  is_energized[1] <- TRUE

  while (length(stack) > 0) {

    if (beam_counter %% 1000 == 0) print(beam_counter)
    beam <- stack[[length(stack)]]
    stack[length(stack)] <- NULL
    active <- TRUE
    while (active) {
      if (beam$heading == 0) { # beam is going east
        path_objs <- which(
          obj$row == beam$row &
          obj$col > beam$col &
          obj$type %in% c("|", "\\", "/")
        )
        if (length(path_objs) > 0) {
          next_obj <- path_objs[which.min(obj$col[path_objs])] # going east
          
          # trace out path, energize tiles, and do backtrack test
          add <- data.frame(row = beam$row, col = (beam$col + 1):obj$col[next_obj])
          add$i <- (add$col - 1L) * edge + add$row
          is_energized[add$i] <- TRUE
          backtracked <- any(add$i %in% east_hist)

          if (backtracked) {
            if (verbose) cat("backtracked!\n")
            active <- FALSE
          } else {
            # send beam to next object
            beam$col <- obj$col[next_obj]
            beam$row <- obj$row[next_obj]
            east_hist <- c(east_hist, add$i)
            if (obj$type[next_obj] == "|") {
              if (verbose) cat("encountered |\n")
              split <- beam
              beam_counter <- beam_counter + 1
              split$beam_id <- beam_counter
              # current beam executes right turn, going south
              beam$heading <- 3
              # split beam executes left turn, going north
              split$heading <- 1
              stack[[length(stack) + 1]] <- split
            } else if (obj$type[next_obj] == "\\") {
              if (verbose) cat("encountered \\\n")
              # right turn, going south
              beam$heading <- 3
            } else if (obj$type[next_obj] == "/") {
              if (verbose) cat("encountered /\n")
              # left turn, going north
              beam$heading <- 1
            }
          }
        } else {
          if (verbose) cat("exiting map on east edge\n")
          if (beam$col < edge) {
            add <- data.frame(row = beam$row, col = (beam$col + 1):edge)
            add$i <- (add$col - 1) * edge + add$row
            is_energized[add$i] <- TRUE
          }
          active <- FALSE
        }
      } else if (beam$heading == 2) { # beam is going west
        path_objs <- which(
          obj$row == beam$row &
          obj$col < beam$col &
          obj$type %in% c("|", "\\", "/")
        )
        if (length(path_objs) > 0) {
          # send beam to next object
          next_obj <- path_objs[which.max(obj$col[path_objs])] # going west

          # trace out path, energize tiles, and do backtrack test
          add <- data.frame(row = beam$row, col = (beam$col - 1):obj$col[next_obj])
          add$i <- (add$col - 1) * edge + add$row
          is_energized[add$i] <- TRUE
          backtracked <- any(add$i %in% west_hist)

          if (backtracked) {
            if (verbose) cat("backtracked!\n")
            active <- FALSE
          } else {
            # send beam to next object
            beam$col <- obj$col[next_obj]
            beam$row <- obj$row[next_obj]
            west_hist <- c(west_hist, add$i)
            if (obj$type[next_obj] == "|") {
              if (verbose) cat("encountered |\n")
              split <- beam
              beam_counter <- beam_counter + 1
              split$beam_id <- beam_counter
              # current beam executes right turn, going north
              beam$heading <- 1
              # split beam executes left turn, going south
              split$heading <- 3
              stack[[length(stack) + 1]] <- split
            } else if (obj$type[next_obj] == "\\") {
              if (verbose) cat("encountered \\\n")
              # right turn, going north
              beam$heading <- 1
            } else if (obj$type[next_obj] == "/") {
              if (verbose) cat("encountered /\n")
              # left turn, going south
              beam$heading <- 3
            }
          }
        } else {
          if (verbose) cat("exiting map on west edge\n")
          if (beam$col > 1) {
            add <- data.frame(row = beam$row, col = (beam$col - 1):1)
            add$i <- (add$col - 1) * edge + add$row
            is_energized[add$i] <- TRUE
          }
          active <- FALSE
        }
      } else if (beam$heading == 1) { # beam is going north
        path_objs <- which(
          obj$row < beam$row &
          obj$col == beam$col &
          obj$type %in% c("-", "\\", "/")
        )
        if (length(path_objs) > 0) {
          next_obj <- path_objs[which.max(obj$row[path_objs])] # going north

          # trace out path, energize tiles, and do backtrack test
          add <- data.frame(row = (beam$row - 1):obj$row[next_obj], col = beam$col)
          add$i <- (add$col - 1) * edge + add$row
          is_energized[add$i] <- TRUE
          backtracked <- any(add$i %in% north_hist)

          if (backtracked) {
            if (verbose) cat("backtracked!\n")
            active <- FALSE
          } else {
            beam$col <- obj$col[next_obj]
            beam$row <- obj$row[next_obj]
            north_hist <- c(north_hist, add$i)
            if (obj$type[next_obj] == "-") {
              if (verbose) cat("encountered -\n")
              split <- beam
              beam_counter <- beam_counter + 1
              split$beam_id <- beam_counter
              # current beam executes right turn, going east
              beam$heading <- 0
              # split beam executes left turn, going west
              split$heading <- 2
              stack[[length(stack) + 1]] <- split
            } else if (obj$type[next_obj] == "\\") {
              if (verbose) cat("encountered \\\n")
              # left turn, going west
              beam$heading <- 2
            } else if (obj$type[next_obj] == "/") {
              if (verbose) cat("encountered /\n")
              # right turn, going east
              beam$heading <- 0
            }
          }
        } else {
          if (verbose) cat("exiting map on north edge\n")
          if (beam$row > 1) {
            add <- data.frame(row = (beam$row - 1):1, col = beam$col)
            add$i <- (add$col - 1) * edge + add$row
            is_energized[add$i] <- TRUE
          }
          active <- FALSE
        }
      } else if (beam$heading == 3) { # beam is going south
        path_objs <- which(
          obj$row > beam$row &
          obj$col == beam$col &
          obj$type %in% c("-", "\\", "/")
        )
        if (length(path_objs) > 0) {
          next_obj <- path_objs[which.min(obj$row[path_objs])] # going south
          
          # trace out path, energize tiles, and do backtrack test
          add <- data.frame(row = (beam$row + 1):obj$row[next_obj], col = beam$col)
          add$i <- (add$col - 1) * edge + add$row
          is_energized[add$i] <- TRUE
          backtracked <- any(add$i %in% south_hist)
          if (backtracked) {
            if (verbose) cat("backtracked!\n")
            active <- FALSE
          } else {
            # send beam to next object
            beam$col <- obj$col[next_obj]
            beam$row <- obj$row[next_obj]
            south_hist <- c(south_hist, add$i)
            if (obj$type[next_obj] == "-") {
              if (verbose) cat("encountered -\n")
              split <- beam
              beam_counter <- beam_counter + 1
              split$beam_id <- beam_counter
              # current beam executes right turn, going west
              beam$heading <- 2
              # split beam executes left turn, going east
              split$heading <- 0
              stack[[length(stack) + 1]] <- split
            } else if (obj$type[next_obj] == "\\") {
              if (verbose) cat("encountered \\\n")
              # left turn, going east
              beam$heading <- 0
            } else if (obj$type[next_obj] == "/") {
              if (verbose) cat("encountered /\n")
              # right turn, going west
              beam$heading <- 2
            }
          }
        } else {
          if (verbose) cat("exiting map on south edge\n")
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

stopifnot(count_illuminated_tiles("day16/test_input.txt") == 46)

tic("day 16, part 1")
stopifnot(count_illuminated_tiles("day16/input.txt", verbose = FALSE) == 7870)
toc(log = TRUE)

# gotta go in from any start value!

