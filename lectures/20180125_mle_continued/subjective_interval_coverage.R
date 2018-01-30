library(tidyverse)

si_res <- read_csv("http://www.evanlray.com/stat343_s2018/lectures/20180125_mle_continued/subjective_interval_results.csv")

si_truth <- data.frame(
  Q_num = seq_len(10),
  answer = c(100, 164, 1971, 26.3, 12.45, 28, 49000, 850, 1963, 85)
)

si_res <- si_res[, -8]

si_res <- si_res %>%
  left_join(si_truth, by = "Q_num") %>%
  mutate(
    c50 = lb_50 <= answer & ub_50 >= answer,
    c90 = lb_90 <= answer & ub_90 >= answer
  )

si_res$c50[is.na(si_res$c50)] <- FALSE
si_res$c90[is.na(si_res$c90)] <- FALSE

si_res %>%
  group_by(Name) %>%
  summarize(
    prop_correct_50 = mean(c50),
    prop_correct_90 = mean(c90)) %>%
  mutate(
    total_dist = (.5 - prop_correct_50) + (.9 - prop_correct_90)
  )


