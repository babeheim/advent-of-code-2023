
rm(list = ls())

source("project_support.R")

tic.clearlog()

days <- list.files(".", pattern = "day")

tic("run 2023 advent")

for (i in seq_along(days)) {
  if (file.exists(file.path(days[i], "solution.R"))) {
    source(file.path(days[i], "solution.R"))
  }
}

toc()


