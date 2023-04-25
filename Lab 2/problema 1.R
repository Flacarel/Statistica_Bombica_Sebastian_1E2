df <- read.csv("life_expect.csv")
intervals <- seq(min(df$female, df$male), max(df$female, df$male), length = 8)
hist(df$female, breaks = intervals, main = "Femei", xlab = "Speranța de viață")
hist(df$male, breaks = intervals, main = "Barbati", xlab = "Speranța de viață")

