# title: Data Preparation
# description: This R script modifies the data more intuitively, such as changing shot_no to n and shot_yes to y. Not only that, it adds the elapsed time for the shot made by the players and extracts the summary file from the data. 
# input(s): andre-iguodala.csv, draymond-green.csv, kevin-durant.csv, klay-thompson.csv, stephen-curry.csv
# output(s): andre-iguodala-summary.txt, draymond-green-summary.txt, kevin-durant-summary.txt, klay-thompson-summary.txt, stephen-curry-summary.txt, shots-data.csv, shots-data-summary.txt

iguodala <- read.csv("../data/andre-iguodala.csv", stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv", stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv", stringsAsFactors = FALSE)
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)

iguodala$name <- "Andre Iguodala"
green$name <- "Draymond Green"
durant$name <- "Kevin Durant"
thompson$name <- "Klay Thompson"
curry$name <- "Stephen Curry"

replace(iguodala$shot_made_flag,"shot_no","n")
replace(green$shot_made_flag,"shot_no","n")
replace(durant$shot_made_flag,"shot_no","n")
replace(thompson$shot_made_flag,"shot_no","n")
replace(curry$shot_made_flag,"shot_no","n")

iguodala$minute <- (iguodala$period*12-iguodala$minutes_remaining)
green$minute <- (green$period*12-green$minutes_remaining)
durant$minute <- (durant$period*12-durant$minutes_remaining)
thompson$minute <- (thompson$period*12-thompson$minutes_remaining)
curry$minute <- (curry$period*12-curry$minutes_remaining)

sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file = '../output/draymond-green-summary.txt')
summary(green)
sink()

sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()

sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()

shots_data <- rbind(iguodala, green, durant, thompson, curry, stringsAsFactors = FALSE)

write.csv(shots_data, file = '../data/shots-data.csv')

sink('../output/shots-data-summary.txt')
summary(shots_data)
sink()