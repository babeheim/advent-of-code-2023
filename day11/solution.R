
calc_galaxy_dist <- function(path, factor = 2) {
  factor <- as.integer(factor)
  x <- readLines(path)
  mat <- matrix(NA, nrow = length(x), ncol = nchar(x[1]))
  for (i in 1:length(x)) mat[i,] <- strsplit(x[i], "")[[1]]

  gal <- data.frame(
    i = which(mat == "#")
  )
  n_gal <- nrow(gal)
  gal$row = (gal$i - 1L) %% nrow(mat) + 1L
  gal$col = (gal$i - 1L) %/% nrow(mat) + 1L
  o <- order(gal$row, gal$col)
  gal <- gal[o,]

  col_n_galaxies <- apply(mat, 2, function(z) sum(z == "#"))
  cols_to_expand <- which(col_n_galaxies == 0)

  row_n_galaxies <- apply(mat, 1, function(z) sum(z == "#"))
  rows_to_expand <- which(row_n_galaxies == 0)

  total_dist <- 0L
  for (i in 1:(n_gal-1)) {
    for (j in (i+1):n_gal) {
      dyad_rows <- c(gal$row[i], gal$row[j])
      dyad_cols <- c(gal$col[i], gal$col[j])

      col_dist <- abs(diff(dyad_cols))
      col_expand <- sum(cols_to_expand > min(dyad_cols) & cols_to_expand < max(dyad_cols))
      col_expand <- col_expand * (factor - 1)

      row_dist <- abs(diff(dyad_rows))
      row_expand <- sum(rows_to_expand > min(dyad_rows) & rows_to_expand < max(dyad_rows))
      row_expand <- row_expand * (factor - 1)

      dyad_dist <- col_dist + col_expand + row_dist + row_expand
      total_dist <- total_dist + dyad_dist
    }
  }

  out <- total_dist
  return(out)

}

stopifnot(calc_galaxy_dist("day11/test_input.txt") == 374)

tic("day 11, part 1")
stopifnot(calc_galaxy_dist("day11/input.txt") == 9805264)
toc(log = TRUE)

stopifnot(calc_galaxy_dist("day11/test_input.txt", 10) == 1030)
stopifnot(calc_galaxy_dist("day11/test_input.txt", 100) == 8410)

tic("day 11, part 2")
stopifnot(calc_galaxy_dist("day11/input.txt", 1e6) == 779032247216)
toc(log = TRUE)


