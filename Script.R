pannes = read.csv(
    file = "FailureTimes_5.csv",
    header = TRUE,
    sep = ",",
    dec = ".",
    colClasses = c("NULL", NA)
);
hist(
     pannes$Heures,
     breaks = 20
);

library(stats4);

