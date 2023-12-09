
typer <- function(x, wild = "") {
  if (length(x) == 1 && nchar(x) == 5) x <- strsplit(x, split = "")[[1]]
  n_wild <- sum(x == wild)
  n_unique <- length(unique(x[x != wild]))
  counts <- rev(sort(table(x[x != wild])))
  if (n_unique %in% c(0, 1)) out <- "Five of a kind"
  # AAAAA, AAAAJ, AAAJJ, AJJJJ, JJJJJ
  else if (n_unique == 2)
    if (all(counts == c(4 - n_wild, 1))) out <- "Four of a kind" # AAAAB, AAABJ, AABJJ, ABJJJ
    else if (all(counts == c(3 - n_wild, 2))) out <- "Full house"
    # AAABB, AABBJ
    else stop("invalid hand with 2 unique cards")
  else if (n_unique == 3)
    if (counts[1] == 3 - n_wild) out <- "Three of a kind"
    # AAABC, AABCJ, ABCJJ
    else if (counts[1] == 2 - n_wild) out <- "Two pair"
    # AABBC
    else stop("invalid hand with 3 unique cards")
  else if (n_unique == 4) out <- "One pair"
  # AABCD, ABCDJ
  else if (n_unique == 5) out <- "High card"
  # ABCDE
  else stop("unable to find type!")
  return(out)
}

calc_order_sum <- function(path, part1 = TRUE) {
  if (part1) {
    wild_card <- ""
  } else { 
    wild_card <- "J"
  }
  x <- readLines(path)
  dat <- data.frame(
    bid = as.numeric(gsub("^\\w*\\s", "", x)),
    hand = gsub("\\s+\\d+$", "", x)
  )
  handmat <- matrix(NA, ncol = 5, nrow = length(x))
  for (i in 1:length(x)) {
    handmat[i,] <- strsplit(dat$hand[i], split = "")[[1]]
  }
  dat$type <- rep(NA, length(x))
  for (i in 1:length(x)) dat$type[i] <- typer(handmat[i,], wild = wild_card)
  types <- c("Five of a kind", "Four of a kind", "Full house", "Three of a kind", "Two pair", "One pair", "High card")
  dat$type <- factor(dat$type, levels = types)
  o <- order(dat$type, decreasing = TRUE)
  dat <- dat[o,]
  handmat <- handmat[o,]
  handmat[which(handmat == "A")] <- "14"
  handmat[which(handmat == "K")] <- "13"
  handmat[which(handmat == "Q")] <- "12"
  if (part1) {
    handmat[which(handmat == "J")] <- "11"
  } else { 
    handmat[which(handmat == "J")] <- "1"
  }
  handmat[which(handmat == "T")] <- "10"
  class(handmat) <- "integer"
  for (i in 1:length(types)) {
    my_rows <- which(dat$type == types[i])
    mat <- handmat[my_rows, , drop = FALSE]
    o <- order(mat[,1], mat[,2], mat[,3], mat[,4], mat[,5], decreasing = FALSE)
    # put re-orderings back into data
    dat[my_rows,] <- dat[my_rows[o],]
    handmat[my_rows, ] <- mat[o,]
  }
  out <- sum(dat$bid * 1:nrow(dat))
  return(out)
}

stopifnot(calc_order_sum("day07/test_input.txt") == 6440)
tic("day 07, part 1")
stopifnot(calc_order_sum("day07/input.txt") == 249204891)
toc(log = TRUE)

stopifnot(calc_order_sum("day07/test_input.txt", part1 = FALSE) == 5905)
tic("day 07, part 2")
stopifnot(calc_order_sum("day07/input.txt", part1 = FALSE) == 249666369)
toc(log = TRUE)
