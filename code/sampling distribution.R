library(LRTesteR)
library(tidyverse)

###############
# Load data
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
  "geometric_type_one.rds",
  "geometric_type_one_one_way.rds"
)

typeI <- map_dfr(fns, load_df)

###############
# Check sampling distribution
###############
typeI %>%
  filter(alt == "less") %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Normal) Less Tests", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_less.png", width = 10, height = 10)

typeI %>%
  filter(alt == "greater") %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Normal) Greater Tests", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_greater.png", width = 10, height = 10)

param <- list(df = 1)
typeI %>%
  filter(alt == "two.sided") %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qchisq, dparams = param["df"]) +
  stat_qq_line(distribution = qchisq, dparams = param["df"]) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Chi Square) Two Sided Tests", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_two_sided.png", width = 10, height = 10)

param <- list(df = 1)
typeI %>%
  filter(alt == "two.sided", !str_detect(test, "one_way")) %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qchisq, dparams = param["df"]) +
  stat_qq_line(distribution = qchisq, dparams = param["df"]) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Chi Square) Two Sided Tests", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_two_sided_ref.png", width = 10, height = 10)

# The one sided QQ plots based on normal distribution look great.
# If X ~ N(), than X^2 ~ chi-square(df=1)
# So the QQ plot of squared stat should look just as great as well.
#
# Does the chi square QQ plot for one sided tests look about the same
# as the two sided's QQ plot?
# If yes, two sided test looks good.
# If no, there is something off with the two sided test.
typeI %>%
  filter(alt != "two.sided") %>%
  mutate(stat = stat^2) %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qchisq, dparams = param["df"]) +
  stat_qq_line(distribution = qchisq, dparams = param["df"]) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Chi Square) One Sided Tests Squared", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_one_sided_squared.png", width = 10, height = 10)

rm(list = ls())
