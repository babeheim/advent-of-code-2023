
# https://www.r-bloggers.com/2011/03/ascii-code-table-in-r/
asc <- function(x) {
  if (length(x) == 1) {
    out <- strtoi(charToRaw(x),16L)
  } else {
    out <- unlist(lapply(x, asc))
  }
  return(out)
}

stopifnot(asc("H") == 72)
stopifnot(asc("A") == 65)
stopifnot(asc("S") == 83)
stopifnot(asc("H") == 72)

calc_hash <- function(x) {
  if (length(x) == 1) {
    x <- strsplit(x, "")[[1]]
    y <- asc(x)
    current_value <- 0
    for (i in 1:length(y)) {
      current_value <- ((current_value + y[i]) * 17) %% 256
    }
    out <- current_value
  } else {
    out <- unlist(lapply(x, calc_hash))
  }
  return(out)
}

stopifnot(calc_hash("HASH") == 52)

# in part 2

# the hash is the LABEL
# each lens with that label can have a DIFFERENT focal length

calc_focusing_power <- function(path, part1 = TRUE) {
  x <- readLines(path)
  x <- strsplit(x, ",")[[1]]

  if (part1) return(sum(calc_hash(x)))

  box <- calc_hash(x) # the corresponding box!

  label <- x
  label <- gsub("=", "", label)
  label <- gsub("-", "", label)
  label <- gsub("\\d", "", label)

  focal_length <- x
  focal_length <- gsub("[a-z]", "", focal_length)
  focal_length <- gsub("=", "", focal_length)
  focal_length <- gsub("-", "", focal_length)
  focal_length <- as.integer(focal_length)

  overwrite <- grepl("=", x)

  inst <- data.frame(
    label = label,
    box = calc_hash(label) + 1,
    overwrite = overwrite,
    focal_length = focal_length
  )

  boxes <- vector("list", 256)
  for (i in 1:length(boxes)) boxes[[i]] <- data.frame(label = character(), focal_length = integer())

  stopifnot(!any(is.na(inst$focal_length[inst$overwrite])))

  for (i in 1:nrow(inst)) {
    tar <- which(boxes[[inst$box[i]]]$label == inst$label[i])
    if (inst$overwrite[i]) {
      if (length(tar) == 1) {
        # if the lens is in the box, update the focal_length
        boxes[[inst$box[i]]]$focal_length[tar] <- inst$focal_length[i]
      } else {
        # if not in the box, add it to the box..
        add <- data.frame(label = inst$label[i], focal_length = inst$focal_length[i])
        if (is.null(boxes[[inst$box[i]]])) {
          # if the box is empty, its the only entry
          boxes[[inst$box[i]]] <- add
        } else {
          # if the box has a lens already, add it
          boxes[[inst$box[i]]] <- bind_rows(boxes[[inst$box[i]]], add)
        }
      }
      stopifnot(ncol(boxes[[inst$box[i]]]) == 2)
    } else {
      if (length(tar) > 0) {
        boxes[[inst$box[i]]] <- boxes[[inst$box[i]]][-tar,]
      }
    }
  }
  out <- 0
  for (i in 1:length(boxes)) {
    box <- boxes[[i]]
    if (nrow(boxes[[i]]) > 0) {
      out <- out + sum(i * 1:nrow(box) * box$focal_length)
    }
  }
  return(out)
}

stopifnot(calc_focusing_power("day15/test_input.txt") == 1320)

tic("day 15, part 1")
stopifnot(calc_focusing_power("day15/input.txt") == 516804)
toc(log = TRUE)

stopifnot(calc_focusing_power("day15/test_input.txt", part1 = FALSE) == 145)

tic("day 15, part 2")
stopifnot(calc_focusing_power("day15/input.txt", part1 = FALSE) == 231844)
toc(log = TRUE)
