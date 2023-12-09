
 
# [Day 9: Mirage Maintenance](https://adventofcode.com/2023/day/9)

Sounds like we just need to compute a discrete polynomial extrapolation??

ha, just reverse the strings to make part2 work!

https://www.reddit.com/r/adventofcode/comments/18e838f/2023_day_9_is_this_a_real_thing/

[Why don't they teach Newton's calculus of 'What comes next?'](https://www.youtube.com/watch?v=4AuV93LOPcE)

https://en.wikipedia.org/wiki/Difference_engine#Method_of_differences

https://en.wikipedia.org/wiki/Divided_differences

https://en.wikipedia.org/wiki/Polynomial_interpolation



# [Day 8: Haunted Wasteland](https://adventofcode.com/2023/day/8)

This part 2 has a trick to it that is something like a spoiler. For each starting position, we can observe periodic visiting of the end positions in a simple cyclic pattern. Calculating the least-common multiple of each of these period lengths gives us the part 2 answer. Recognizing this pattern is the real challenge, and it quickly spread online.

https://www.reddit.com/r/adventofcode/comments/18e6vdf/2023_day_8_part_2_an_explanation_for_why_the/


# [Day 7: Camel Cards](https://adventofcode.com/2023/day/7)

Sort-5-card hands into ranked categories, then inside each category, by the strength of the highest card.



# [Day 6: Wait For It](https://adventofcode.com/2023/day/6)

Charging up speed trades off against time to move (like the home run contest in Smash Bros).



# [Day 5: If You Give A Seed A Fertilizer](https://adventofcode.com/2023/day/5)

seeds
soil
fertilizer
water
light
temperature
humidity
location

Every type of seed, soil, fertilizer and so on is identified with a number, but numbers are reused by each category - that is, soil 123 and fertilizer 123 aren't necessarily related to each other.

Each line within a map contains three numbers: the *destination range start*, the *source range start*, and the *range length*. (i bet that will change in part 2...)



# [Day 4: Scratchcards](https://adventofcode.com/2023/day/4)

In part 1, we count up the number of winning numbers on each card to find the score. In part 2, the number of winning numbers causes subsequent cards to duplicate in a cascade.



# [Day 3: Gear Ratios](https://adventofcode.com/2023/day/3)

If a number is adjacent to a "symbol" (`+`, `-`, `*`, `/`, `=`, `@`, `&`, `#`, `%`, `$`), we want to sum it up in part 1. If not adjacent to a symbol, we ignore it!

In part 2, we multiply each pair of numbers that are adjacent to a gear symbol ("*"), and add those up.



# [Day 2: Cube Conundrum](https://adventofcode.com/2023/day/2)

We are playing a game with a bag containing an unknown number of reg, green, and blue cubes. We get to see random draws from the bag over and over, and have to decide if the game's bag is consistent with a specific number of drawn cubes of each color, e.g. the bag contains at least 20 red cubes if we saw a sample with that many cubes.



# [Day 1: Trebuchet?!](https://adventofcode.com/2023/day/1)

The task is to identify digits inside an alphanumeric text string and do operations on them. In Part II, some of the digts are spelled out as letters, and there's a trick because some letters are shared between two numbers, e.g. `oneight` is both 1 and 8.
