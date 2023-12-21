
library(numbers) # for LCM function
options(scipen = 999)

lcmer <- function(x) {
  out <- LCM(x[1], x[2])
  if (length(x) > 2) {
    for (i in 3:length(x)) {
      out <- LCM(out, x[i])
    }
  }
  return(out)
}

identify_group <- function(start, edges) {
  queue <- start
  hist <- character()
  while(length(queue) > 0) {
    focal <- queue[1]
    hist <- c(hist, focal)
    queue <- queue[-1]
    add <- edges$to[which(edges$from == focal)]
    add <- setdiff(add, hist)
    queue <- c(queue, add)
  }
  hist <- sort(unique(hist))
  return(hist)
}


sim_button_push <- function(sim, n_times = 1, verbose = FALSE, part1 = TRUE) {
  out <- sim
  if (!part1) n_times <- 5e3
  for (j in 1:n_times) {
    queue <- list()
    queue[[1]] <- list(from = "button", is_high = FALSE)

    while (length(queue) > 0) {
      pulse <- queue[[1]]
      queue <- queue[-1]
      receivers <- out$edges$to[which(out$edges$from == pulse$from)]
      if (pulse$is_high) {
        out$high_pulse_counter <- out$high_pulse_counter + length(receivers)
      } else {
        out$low_pulse_counter <-  out$low_pulse_counter + length(receivers)
      }
      if (length(receivers) > 0) {
        receiver_rows <- which(out$nodes$name %in% receivers)
        receivers <- out$nodes$name[receiver_rows]
        for (i in 1:length(receivers)) {
          if (verbose) cat(pulse$from, "sends", pulse$is_high, "signal to", out$nodes$name[receiver_rows[i]], "\n")
          if (out$nodes$type[receiver_rows[i]] == "flip-flop" & !pulse$is_high) {
            out$nodes$is_on[receiver_rows[i]] <- !out$nodes$is_on[receiver_rows[i]]
            new_pulse <- list(from = out$nodes$name[receiver_rows[i]],
              is_high = out$nodes$is_on[receiver_rows[i]])
            queue[[length(queue)+1]] <- new_pulse
          } else if (out$nodes$type[receiver_rows[i]] == "conjunction") {
            out$memory[[receivers[i]]][pulse$from] <- pulse$is_high
            if (all(out$memory[[receivers[i]]]) == 1) {
              new_pulse <- list(from = out$nodes$name[receiver_rows[i]], is_high = FALSE)
              queue[[length(queue)+1]] <- new_pulse
            } else {
              new_pulse <- list(from = out$nodes$name[receiver_rows[i]], is_high = TRUE)
              queue[[length(queue)+1]] <- new_pulse
            }
          } else if (out$nodes$type[receiver_rows[i]] == "broadcaster") {
            new_pulse <- list(from = out$nodes$name[receiver_rows[i]], is_high = pulse$is_high)
            queue[[length(queue)+1]] <- new_pulse
          } else if (!part1 & out$nodes$type[receiver_rows[i]] == "output" & !pulse$is_high) {
            out$n_pushed <- j
            return(out)
          }
        }
      }
    }
  }
  out$n_pushed <- j
  return(out)
}

analyze_pulse_network <- function(path, n_times = 1000, part1 = TRUE) {

  x <- readLines(path)
  x <- c(x, "button -> broadcaster")
  module_name <- gsub(" -> .+$", "", x)
  is_flip_flop <- grepl("^%", x)
  is_conjunction <- grepl("^&", x)
  type <- rep(NA, length(module_name))
  type[is_flip_flop] <- "flip-flop"
  type[is_conjunction] <- "conjunction"
  type[module_name == "broadcaster"] <- "broadcaster"
  type[module_name == "button"] <- "button"
  stopifnot(!any(is.na(type)))

  nodes <- data.frame(
    name = module_name,
    type = type
  )
  nodes$name <- gsub("^%", "", nodes$name)
  nodes$name <- gsub("^&", "", nodes$name)

  edges_char <- gsub("^.+ -> ", "", x)
  for (i in 1:length(edges_char)) {
    add <- data.frame(
      from = nodes$name[i],
      to = strsplit(edges_char[i], ", ")[[1]]
    )
    if (i == 1) {
      edges <- add
    } else {
      edges <- bind_rows(edges, add)
    }
  }

  if(any(!(edges$to %in% nodes$name))) {
    tar <- which(!(edges$to %in% nodes$name))
    add_names <- sort(unique(edges$to[tar]))
    add <- data.frame(
      name = add_names, type = "output")
    nodes <- bind_rows(nodes, add)
  }

  # in part 2, we isolate the four starting nodes after broadcaster
  if (!part1) {
    start_nodes <- edges$to[which(edges$from == "broadcaster")]
  } else {
    start_nodes <- "button"
  }
  group_periods <- rep(NA, length(start_nodes))

  nodes_full <- nodes
  edges_full <- edges

  for (j in 1:length(start_nodes)) {
  
    group <- identify_group(start_nodes[j], edges_full)
    nodes <- nodes_full[which(nodes_full$name %in% c(group, "button", "broadcaster")),]
    edges <- edges_full[which(edges_full$from %in% nodes$name & edges_full$to %in% nodes$name),]

    nodes$is_on <- NA
    nodes$is_on[which(nodes$type == "flip-flop")] <- FALSE # default is off

    # create the state variables we track
    memory <- list()
    conjunction_nodes <- which(nodes$type == "conjunction")
    n_conjunctions <- length(conjunction_nodes)
    for (i in 1:length(conjunction_nodes)) {
      tar <- which(edges$to == nodes$name[conjunction_nodes[i]])
      memory[[i]] <- rep(FALSE, length(tar))
      names(memory[[i]]) <- edges$from[tar]
      names(memory)[i] <- nodes$name[conjunction_nodes[i]]
    }

    stopifnot(all(edges$to %in% nodes$name))
    stopifnot(all(edges$from %in% nodes$name))

    high_pulse_counter <- 0L
    low_pulse_counter <- 0L

    sim <- list(
      nodes = nodes,
      edges = edges,
      memory = memory,
      high_pulse_counter = high_pulse_counter,
      low_pulse_counter = low_pulse_counter
    )

    sim <- sim_button_push(sim, n_times = n_times, part1 = part1)
    group_periods[j] <- sim$n_pushed
  }

  if (part1) {
    out <- sim$high_pulse_counter * sim$low_pulse_counter
  } else {
    out <- lcmer(group_periods)
  }
  return(out)

}

stopifnot(analyze_pulse_network("day20/test_input.txt") == 32000000)

stopifnot(analyze_pulse_network("day20/test_input_2.txt") == 11687500)

tic("day 20, part 1")
stopifnot(analyze_pulse_network("day20/input.txt") == 794930686)
toc(log = TRUE)

tic("day 20, part 2")
stopifnot(analyze_pulse_network("day20/input.txt", part1 = FALSE) == 244465191362269)
toc(log = TRUE)
