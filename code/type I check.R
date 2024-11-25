library(tidyverse)

###############
# one sample
###############
load_df <- function(fn) {
  if (str_detect(fn, "two")) {
    print(str_c("Possible Error. fn: ", fn))
  }
  fn <- str_c("results/", fn, collapse = "")
  DF <- readRDS(fn)
  DF <- DF %>%
    select(test, alt, stat, pvalue, CI_LB, CI_UB)
  return(DF)
}

fns <- c(
  "geometric_type_one.rds"
)

typeI <- map_dfr(fns, load_df)

typeI %>%
  drop_na() %>%
  nrow() == typeI %>%
  nrow()

typeI %>%
  distinct(test) %>%
  nrow() == 1

typeI %>%
  distinct(alt) %>%
  nrow() == 3

typeI %>%
  filter(alt == "two.sided") %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeI %>%
  filter(alt == "two.sided", stat < 0) %>%
  distinct(test)

typeI %>%
  filter(alt != "two.sided") %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeI %>%
  filter(alt != "two.sided") %>%
  group_by(test) %>%
  summarise(minStat = min(stat), maxStat = max(stat)) %>%
  arrange(test) %>%
  print(n = Inf)

typeI %>%
  summarise(
    P_LB = all(pvalue >= 0),
    P_UB = all(pvalue <= 1),
    CI_CORRECT = all(CI_LB < CI_UB)
  )

rm(list = ls())

###############
# two sample
###############
load_df <- function(fn) {
  if (str_detect(fn, "two")) {
    print(str_c("Possible Error. fn: ", fn))
  }
  fn <- str_c("results/", fn, collapse = "")
  DF <- readRDS(fn)
  DF <- DF %>%
    select(test, alt, stat, pvalue)
  return(DF)
}

fns <- c(
  "geometric_type_one_one_way.rds"
)

typeI <- map_dfr(fns, load_df)

typeI %>%
  drop_na() %>%
  nrow() == typeI %>%
  nrow()

typeI %>%
  distinct(test) %>%
  nrow() == 1

typeI %>%
  distinct(alt) %>%
  nrow() == 1

typeI %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeI %>%
  summarise(
    P_LB = all(pvalue >= 0),
    P_UB = all(pvalue <= 1)
  )

rm(list = ls())
