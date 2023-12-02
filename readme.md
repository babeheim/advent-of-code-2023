

# [Day 2: Cube Conundrum](https://adventofcode.com/2023/day/2)

We are playing a game with a bag containing an unknown number of reg, green, and blue cubes. We get to see random draws from the bag over and over, and have to decide if the game's bag is consistent with a specific number of drawn cubes of each color, e.g. the bag contains at least 20 red cubes if we saw a sample with that many cubes.

```R

path <- "day02/test_input.txt"

x <- readLines(path)

# part1: allowed max is 12 red cubes, 13 green cubes, and 14 blue cubes

# for each game, we want to extract the number of cubes for each type...maybe as a data frame?

valid_game <- rep(TRUE, length(x))

red_max <- 12
green_max <- 13
blue_max <- 14

for (i in 1:length(x)) {

  pattern <- "\\d+ green"
  all_matches <- gregexpr(pattern, x[i])
  counts <- regmatches(x[i], all_matches)[[1]]
  green_counts <- as.numeric(gsub("green", "", counts))

  if (any(green_counts > green_max)) valid_game[i] <- FALSE

  pattern <- "\\d+ red"
  all_matches <- gregexpr(pattern, x[i])
  counts <- regmatches(x[i], all_matches)[[1]]
  red_counts <- as.numeric(gsub("red", "", counts))

  if (any(red_counts > red_max)) valid_game[i] <- FALSE

  pattern <- "\\d+ blue"
  all_matches <- gregexpr(pattern, x[i])
  counts <- regmatches(x[i], all_matches)[[1]]
  blue_counts <- as.numeric(gsub("blue", "", counts))

  if (any(blue_counts > blue_max)) valid_game[i] <- FALSE

}

sum(which(valid_game))

```


# [Day 1: Trebuchet?!](https://adventofcode.com/2023/day/1)

The task is to identify digits inside an alphanumeric text string and do operations on them. In Part II, some of the digts are spelled out as letters, and there's a trick because some letters are shared between two numbers, e.g. `oneight` is both 1 and 8.
