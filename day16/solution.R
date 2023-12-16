
# problem: we need the path to prevent backtracking, but we cant let the thing grow so big that it slows everything down

# solution: if the add is two tiles long, we can use adjacent illuminations...somehow?
# there's backtracking, and thee's backtracking. if the beam is stuck in its own loop we want that to die (so don't grow the path overall). but there IS efficiency gains when you know one beam has already travelled on the path the current beam is on, but how to store that??

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

count_illuminated_tiles <- function(path, verbose = TRUE) {

  x <- readLines(path)
  map <- str_to_mat(x)

  edge <- nrow(map) # check its square
  stopifnot(ncol(map) == edge)

  add <- data.frame(
    i = which(map == "|")
  )
  add$row <- (add$i - 1) %% edge + 1
  add$col <- (add$i - 1) %/% edge + 1
  add$type <- "|"

  obj <- add

  add <- data.frame(
    i = which(map == "-")
  )
  add$row <- (add$i - 1) %% edge + 1
  add$col <- (add$i - 1) %/% edge + 1
  add$type <- "-"

  obj <- bind_rows(obj, add)

  add <- data.frame(
    i = which(map == "/")
  )
  add$row <- (add$i - 1) %% edge + 1
  add$col <- (add$i - 1) %/% edge + 1
  add$type <- "/"

  obj <- bind_rows(obj, add)

  add <- data.frame(
    i = which(map == "\\")
  )
  add$row <- (add$i - 1) %% edge + 1
  add$col <- (add$i - 1) %/% edge + 1
  add$type <- "\\"

  obj <- bind_rows(obj, add)

  # create a DFS stack storing row, col and heading

  beam <- list(row = 1, col = 1, heading = 0, beam_id = 1)
  if (map[1,1] == "\\") {
    # crazy but this appears in my input data, implication is
    # the beam is actually firing downwards!
    beam <- list(row = 1, col = 1, heading = 3, beam_id = 1)
    # my code was never written to evaluate the first point...
  }

  stack <- list(beam)

  path <- data.frame(row = beam$row, col = beam$col)
  path$i <- (path$col - 1) * edge + path$row
  path$beam_id <- 1
  beam_counter <- 1

  while (length(stack) > 0) {
    if (beam_counter %% 100 == 0) print(beam_counter)
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
          add <- data.frame(row = beam$row, col = (beam$col + 1):obj$col[next_obj])
          # do a backtracking check on add: does this new additional path overlap at all with the existing path?
          add$i <- (add$col - 1) * edge + add$row
          add$beam_id <- beam$beam_id
          backtracked <- check_overlap(add$i, path$i)
          path <- bind_rows(path, add)
          if (backtracked) {
            if (verbose) cat("backtracked!\n")
            active <- FALSE
          } else {
            # go to the next object
            beam$col <- obj$col[next_obj]
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
            add$beam_id <- beam$beam_id
            path <- bind_rows(path, add)
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
          next_obj <- path_objs[which.max(obj$col[path_objs])] # going west
          add <- data.frame(row = beam$row, col = (beam$col - 1):obj$col[next_obj])
          # do a backtracking check on add: does this new additional path overlap at all with the existing path?
          add$i <- (add$col - 1) * edge + add$row
          add$beam_id <- beam$beam_id
          backtracked <- check_overlap(add$i, path$i)
          path <- bind_rows(path, add)
          if (backtracked) {
            if (verbose) cat("backtracked!\n")
            active <- FALSE
          } else {
            # go to the next object
            beam$col <- obj$col[next_obj]
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
            add$beam_id <- beam$beam_id
            path <- bind_rows(path, add)
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
          add <- data.frame(row = (beam$row - 1):obj$row[next_obj], col = beam$col)
          # do a backtracking check on add: does this new additional path overlap at all with the existing path?
          add$i <- (add$col - 1) * edge + add$row
          add$beam_id <- beam$beam_id
          backtracked <- check_overlap(add$i, path$i)
          path <- bind_rows(path, add)
          if (backtracked) {
            if (verbose) cat("backtracked!\n")
            active <- FALSE
          } else {
            # go to the next object
            beam$row <- obj$row[next_obj]
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
            add$beam_id <- beam$beam_id
            path <- bind_rows(path, add)
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
          add <- data.frame(row = (beam$row + 1):obj$row[next_obj], col = beam$col)
          # do a backtracking check on add: does this new additional path overlap at all with the existing path?
          add$i <- (add$col - 1) * edge + add$row
          add$beam_id <- beam$beam_id
          backtracked <- check_overlap(add$i, path$i)
          path <- bind_rows(path, add)
          if (backtracked) {
            if (verbose) cat("backtracked!\n")
            active <- FALSE
          } else {
            # go to the next object
            beam$row <- obj$row[next_obj]
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
            add$beam_id <- beam$beam_id
            path <- bind_rows(path, add)
          }
          active <- FALSE
        }
      }
    }
  }
  out <- length(unique(path$i))
  return(out)
}

stopifnot(count_illuminated_tiles("day16/test_input.txt") == 46)
