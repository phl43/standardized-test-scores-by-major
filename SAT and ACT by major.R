library(tidyverse)
library(sjlabelled)
library(stats)

# load the data about incoming freshmen's standardized test scores and intended major
# from the Freshmen Survey for the years between 2000 and 2006 (this is a file I created
# from the data available on HERI's website: https://heri.ucla.edu/cirp-freshman-survey/)
tfs <- read_rds("test_scores_by_major.rds")

labels <- get_labels(tfs$MAJOR)

# compute the weighted mean standardized test scores by major
test_scores_by_major <- tfs %>%
  filter(is.na(STUDWGT) == FALSE & is.na(MAJOR) == FALSE) %>%
  group_by(MAJOR) %>%
  summarize(ACT = weighted.mean(ACTCOMP, STUDWGT, na.rm = TRUE),
            SATM = weighted.mean(SATM, STUDWGT, na.rm = TRUE),
            SATV = weighted.mean(SATV, STUDWGT, na.rm = TRUE)) %>%
  mutate(MAJOR = labels[MAJOR])

ggplot(data = test_scores_by_major, mapping = aes(x = reorder(MAJOR, -ACT), y = ACT)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = round(ACT, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3,
            angle = 90) +
  theme_bw() +
  ggtitle("Mean ACT score by major") +
  xlab("Major") +
  ylab("Mean ACT score") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none")

ggplot(data = test_scores_by_major, mapping = aes(x = reorder(MAJOR, -SATM), y = SATM)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = round(SATM, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3,
            angle = 90) +
  theme_bw() +
  ggtitle("Mean score on the mathematical part of the SAT by major") +
  xlab("Major") +
  ylab("Mean SAT score") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none")

ggplot(data = test_scores_by_major, mapping = aes(x = reorder(MAJOR, -SATV), y = SATV)) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = round(SATV, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3,
            angle = 90) +
  theme_bw() +
  ggtitle("Mean score on the verbal part of the SAT by major") +
  xlab("Major") +
  ylab("Mean SAT score") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none")

ggplot(data = test_scores_by_major, mapping = aes(x = reorder(MAJOR, -(SATM + SATV)), y = (SATM + SATV))) +
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = round(SATM + SATV, 1)),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 3,
            angle = 90) +
  theme_bw() +
  ggtitle("Mean SAT score by major") +
  xlab("Major") +
  ylab("Mean SAT score") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(legend.position = "none")
