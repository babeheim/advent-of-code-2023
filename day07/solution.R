
typer <- function(x) {
  # without joker rule
  n_unique <- length(unique(x))
  counts <- rev(sort(table(x)))
  if (n_unique == 1) out <- "Five of a kind"
  else if (n_unique == 2)
    if (all(counts == c(4, 1))) out <- "Four of a kind"
    else if (all(counts == c(3, 2))) out <- "Full house"
    else stop("invalid hand with 2 unique cards")
  else if (n_unique == 3)
    if (counts[1] == 3) out <- "Three of a kind"
    else if (counts[1] == 2) out <- "Two pair"
    else stop("invalid hand with 3 unique cards")
  else if (n_unique == 4) out <- "One pair"
  else if (n_unique == 5) out <- "High card"
  else stop("unable to find type!")
  # need new logic for joker rule...
  return(out)
}

calc_order_sum <- function(path) {
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
  for (i in 1:length(x)) dat$type[i] <- typer(handmat[i,])
  types <- c("Five of a kind", "Four of a kind", "Full house", "Three of a kind", "Two pair", "One pair", "High card")
  dat$type <- factor(dat$type, levels = types)
  o <- order(dat$type, decreasing = TRUE)
  dat <- dat[o,]
  handmat <- handmat[o,]
  handmat[which(handmat == "A")] <- "14"
  handmat[which(handmat == "K")] <- "13"
  handmat[which(handmat == "Q")] <- "12"
  handmat[which(handmat == "J")] <- "11" # joker is worth 1 in part 2 instead of 10, lowest card rank
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
stopifnot(calc_order_sum("day07/input.txt") == 249204891)