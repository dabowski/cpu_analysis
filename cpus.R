library(tidyverse)

# Smartphone Processors Ranking & Scores

cpus <- read_csv("ML_ALL_benchmarks.csv")
head(cpus)

# Top CPU Performance
cpus %>%
    arrange(desc(cpuScore)) %>%
    head(10)

# Top GPU Performance
cpus %>%
    arrange(desc(gpuScore)) %>%
    head(10)

# Top TPU Performance
cpus %>%
    arrange(desc(npuScore)) %>%
    head(10)

# Clock Speed Distribution
ggplot(cpus, mapping = aes(x = clock)) +
    geom_histogram(binwidth = 500) +
    xlab("Clock speed") +
    ggtitle("Clock speed distribution")

# Cores Distribution
ggplot(cpus, mapping = aes(x = cores)) +
    geom_histogram(binwidth = 1) +
    xlab("Number of Cores") +
    ggtitle("Core count distribution")

# Top Performance by Producent
performance_pivot <- cpus %>%
    group_by(company) %>%
    summarize(averageCPUPerformance = mean(cpuScore),
              averageGPUPerformance = mean(gpuScore),
              averageNPUPerformance = mean(npuScore))

performance_pivot

# Price predictions
