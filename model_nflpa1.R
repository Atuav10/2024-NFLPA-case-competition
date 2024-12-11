library(tidyverse)
### Quarterbacks ----
qb_stats <- read.csv("qb_data.csv")

# Extract the statistics we want
qb_stats$td_int_ratio <- qb_stats$TD/(qb_stats$Int + 1)
qb_stats <- select(qb_stats, Player, Pos, Season, Age, GS, Snap_perc = Off..1, AV, Stat_1 = td_int_ratio, Stat_2 = Rate)
qb_stats$Pos <- "QB"

# Get the individual seasons
qb_2014 <- filter(qb_stats, Season == "2014")
qb_2015 <- filter(qb_stats, Season == "2015")
qb_2016 <- filter(qb_stats, Season == "2016")
qb_2017 <- filter(qb_stats, Season == "2017")
qb_2018 <- filter(qb_stats, Season == "2018")
qb_2019 <- filter(qb_stats, Season == "2019")
qb_2020 <- filter(qb_stats, Season == "2020")
qb_2021 <- filter(qb_stats, Season == "2021")
qb_2022 <- filter(qb_stats, Season == "2022")
qb_2023 <- filter(qb_stats, Season == "2023")

# Add a suffix to each column so we can differentiate based on year
colnames(qb_2014)[3:9] <- paste0(colnames(qb_2014)[3:9], "_2014")
colnames(qb_2015)[3:9] <- paste0(colnames(qb_2015)[3:9], "_2015")
colnames(qb_2016)[3:9] <- paste0(colnames(qb_2016)[3:9], "_2016")
colnames(qb_2017)[3:9] <- paste0(colnames(qb_2017)[3:9], "_2017")
colnames(qb_2018)[3:9] <- paste0(colnames(qb_2018)[3:9], "_2018")
colnames(qb_2019)[3:9] <- paste0(colnames(qb_2019)[3:9], "_2019")
colnames(qb_2020)[3:9] <- paste0(colnames(qb_2020)[3:9], "_2020")
colnames(qb_2021)[3:9] <- paste0(colnames(qb_2021)[3:9], "_2021")
colnames(qb_2022)[3:9] <- paste0(colnames(qb_2022)[3:9], "_2022")
colnames(qb_2023)[3:9] <- paste0(colnames(qb_2023)[3:9], "_2023")

# Each contract year takes the statistics from the last two years. We store this in what we call a "filler column".
filler_2016 <- merge(qb_2015, qb_2014, by = c("Player", "Pos"), all = TRUE)
filler_2017 <- merge(qb_2016, qb_2015, by = c("Player", "Pos"), all = TRUE)
filler_2018 <- merge(qb_2017, qb_2016, by = c("Player", "Pos"), all = TRUE)
filler_2019 <- merge(qb_2018, qb_2017, by = c("Player", "Pos"), all = TRUE)
filler_2020 <- merge(qb_2019, qb_2018, by = c("Player", "Pos"), all = TRUE)
filler_2021 <- merge(qb_2020, qb_2019, by = c("Player", "Pos"), all = TRUE)
filler_2022 <- merge(qb_2021, qb_2020, by = c("Player", "Pos"), all = TRUE)
filler_2023 <- merge(qb_2022, qb_2021, by = c("Player", "Pos"), all = TRUE)
filler_2024 <- merge(qb_2023, qb_2022, by = c("Player", "Pos"), all = TRUE)

# Remove NA values
filler_2016[3:16] <- lapply(filler_2016[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2017[3:16] <- lapply(filler_2017[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2018[3:16] <- lapply(filler_2018[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2019[3:16] <- lapply(filler_2019[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2020[3:16] <- lapply(filler_2020[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2021[3:16] <- lapply(filler_2021[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2022[3:16] <- lapply(filler_2022[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2023[3:16] <- lapply(filler_2023[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2024[3:16] <- lapply(filler_2024[3:16], function(x) ifelse(is.na(x), 0, x))

# Make it such that most recent season is weighted at 0.66, second most at 0.33 for every stat
qb_stats_2016 <- filler_2016
qb_stats_2016$Age <- qb_stats_2016$Age_2015
qb_stats_2016$Age <- ifelse(qb_stats_2016$Age_2015 == 0, qb_stats_2016$Age_2014 + 1, qb_stats_2016$Age)
qb_stats_2016$GS <- 0.66*qb_stats_2016$GS_2015 + 0.33*qb_stats_2016$GS_2014
qb_stats_2016$Snap_perc <- 0.66*qb_stats_2016$Snap_perc_2015 + 0.33*qb_stats_2016$Snap_perc_2014
qb_stats_2016$AV <- 0.66*qb_stats_2016$AV_2015 + 0.33*qb_stats_2016$AV_2014
qb_stats_2016$Stat_1 <- 0.66*qb_stats_2016$Stat_1_2015 + 0.33*qb_stats_2016$Stat_1_2014
qb_stats_2016$Stat_2 <- 0.66*qb_stats_2016$Stat_2_2015 + 0.33*qb_stats_2016$Stat_2_2014
qb_stats_2016 <- select(qb_stats_2016, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

qb_stats_2017 <- filler_2017
qb_stats_2017$Age <- qb_stats_2017$Age_2016
qb_stats_2017$Age <- ifelse(qb_stats_2017$Age_2016 == 0, qb_stats_2017$Age_2015 + 1, qb_stats_2017$Age)
qb_stats_2017$GS <- 0.66*qb_stats_2017$GS_2016 + 0.33*qb_stats_2017$GS_2015
qb_stats_2017$Snap_perc <- 0.66*qb_stats_2017$Snap_perc_2016 + 0.33*qb_stats_2017$Snap_perc_2015
qb_stats_2017$AV <- 0.66*qb_stats_2017$AV_2016 + 0.33*qb_stats_2017$AV_2015
qb_stats_2017$Stat_1 <- 0.66*qb_stats_2017$Stat_1_2016 + 0.33*qb_stats_2017$Stat_1_2015
qb_stats_2017$Stat_2 <- 0.66*qb_stats_2017$Stat_2_2016 + 0.33*qb_stats_2017$Stat_2_2015
qb_stats_2017 <- select(qb_stats_2017, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

qb_stats_2018 <- filler_2018
qb_stats_2018$Age <- qb_stats_2018$Age_2017
qb_stats_2018$Age <- ifelse(qb_stats_2018$Age_2017 == 0, qb_stats_2018$Age_2016 + 1, qb_stats_2018$Age)
qb_stats_2018$GS <- 0.66*qb_stats_2018$GS_2017 + 0.33*qb_stats_2018$GS_2016
qb_stats_2018$Snap_perc <- 0.66*qb_stats_2018$Snap_perc_2017 + 0.33*qb_stats_2018$Snap_perc_2016
qb_stats_2018$AV <- 0.66*qb_stats_2018$AV_2017 + 0.33*qb_stats_2018$AV_2016
qb_stats_2018$Stat_1 <- 0.66*qb_stats_2018$Stat_1_2017 + 0.33*qb_stats_2018$Stat_1_2016
qb_stats_2018$Stat_2 <- 0.66*qb_stats_2018$Stat_2_2017 + 0.33*qb_stats_2018$Stat_2_2016
qb_stats_2018 <- select(qb_stats_2018, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

qb_stats_2019 <- filler_2019
qb_stats_2019$Age <- qb_stats_2019$Age_2018
qb_stats_2019$Age <- ifelse(qb_stats_2019$Age_2018 == 0, qb_stats_2019$Age_2017 + 1, qb_stats_2019$Age)
qb_stats_2019$GS <- 0.66*qb_stats_2019$GS_2018 + 0.33*qb_stats_2019$GS_2017
qb_stats_2019$Snap_perc <- 0.66*qb_stats_2019$Snap_perc_2018 + 0.33*qb_stats_2019$Snap_perc_2017
qb_stats_2019$AV <- 0.66*qb_stats_2019$AV_2018 + 0.33*qb_stats_2019$AV_2017
qb_stats_2019$Stat_1 <- 0.66*qb_stats_2019$Stat_1_2018 + 0.33*qb_stats_2019$Stat_1_2017
qb_stats_2019$Stat_2 <- 0.66*qb_stats_2019$Stat_2_2018 + 0.33*qb_stats_2019$Stat_2_2017
qb_stats_2019 <- select(qb_stats_2019, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

qb_stats_2020 <- filler_2020
qb_stats_2020$Age <- qb_stats_2020$Age_2019
qb_stats_2020$Age <- ifelse(qb_stats_2020$Age_2019 == 0, qb_stats_2020$Age_2018 + 1, qb_stats_2020$Age)
qb_stats_2020$GS <- 0.66*qb_stats_2020$GS_2019 + 0.33*qb_stats_2020$GS_2018
qb_stats_2020$Snap_perc <- 0.66*qb_stats_2020$Snap_perc_2019 + 0.33*qb_stats_2020$Snap_perc_2018
qb_stats_2020$AV <- 0.66*qb_stats_2020$AV_2019 + 0.33*qb_stats_2020$AV_2018
qb_stats_2020$Stat_1 <- 0.66*qb_stats_2020$Stat_1_2019 + 0.33*qb_stats_2020$Stat_1_2018
qb_stats_2020$Stat_2 <- 0.66*qb_stats_2020$Stat_2_2019 + 0.33*qb_stats_2020$Stat_2_2018
qb_stats_2020 <- select(qb_stats_2020, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

qb_stats_2021 <- filler_2021
qb_stats_2021$Age <- qb_stats_2021$Age_2020
qb_stats_2021$Age <- ifelse(qb_stats_2021$Age_2020 == 0, qb_stats_2021$Age_2019 + 1, qb_stats_2021$Age)
qb_stats_2021$GS <- 0.66*qb_stats_2021$GS_2020 + 0.33*qb_stats_2021$GS_2019
qb_stats_2021$Snap_perc <- 0.66*qb_stats_2021$Snap_perc_2020 + 0.33*qb_stats_2021$Snap_perc_2019
qb_stats_2021$AV <- 0.66*qb_stats_2021$AV_2020 + 0.33*qb_stats_2021$AV_2019
qb_stats_2021$Stat_1 <- 0.66*qb_stats_2021$Stat_1_2020 + 0.33*qb_stats_2021$Stat_1_2019
qb_stats_2021$Stat_2 <- 0.66*qb_stats_2021$Stat_2_2020 + 0.33*qb_stats_2021$Stat_2_2019
qb_stats_2021 <- select(qb_stats_2021, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

qb_stats_2022 <- filler_2022
qb_stats_2022$Age <- qb_stats_2022$Age_2021
qb_stats_2022$Age <- ifelse(qb_stats_2022$Age_2021 == 0, qb_stats_2022$Age_2020 + 1, qb_stats_2022$Age)
qb_stats_2022$GS <- 0.66*qb_stats_2022$GS_2021 + 0.33*qb_stats_2022$GS_2020
qb_stats_2022$Snap_perc <- 0.66*qb_stats_2022$Snap_perc_2021 + 0.33*qb_stats_2022$Snap_perc_2020
qb_stats_2022$AV <- 0.66*qb_stats_2022$AV_2021 + 0.33*qb_stats_2022$AV_2020
qb_stats_2022$Stat_1 <- 0.66*qb_stats_2022$Stat_1_2021 + 0.33*qb_stats_2022$Stat_1_2020
qb_stats_2022$Stat_2 <- 0.66*qb_stats_2022$Stat_2_2021 + 0.33*qb_stats_2022$Stat_2_2020
qb_stats_2022 <- select(qb_stats_2022, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

qb_stats_2023 <- filler_2023
qb_stats_2023$Age <- qb_stats_2023$Age_2022
qb_stats_2023$Age <- ifelse(qb_stats_2023$Age_2022 == 0, qb_stats_2023$Age_2021 + 1, qb_stats_2023$Age)
qb_stats_2023$GS <- 0.66*qb_stats_2023$GS_2022 + 0.33*qb_stats_2023$GS_2021
qb_stats_2023$Snap_perc <- 0.66*qb_stats_2023$Snap_perc_2022 + 0.33*qb_stats_2023$Snap_perc_2021
qb_stats_2023$AV <- 0.66*qb_stats_2023$AV_2022 + 0.33*qb_stats_2023$AV_2021
qb_stats_2023$Stat_1 <- 0.66*qb_stats_2023$Stat_1_2022 + 0.33*qb_stats_2023$Stat_1_2021
qb_stats_2023$Stat_2 <- 0.66*qb_stats_2023$Stat_2_2022 + 0.33*qb_stats_2023$Stat_2_2021
qb_stats_2023 <- select(qb_stats_2023, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

qb_stats_2024 <- filler_2024
qb_stats_2024$Age <- qb_stats_2024$Age_2023
qb_stats_2024$Age <- ifelse(qb_stats_2024$Age_2023 == 0, qb_stats_2024$Age_2022 + 1, qb_stats_2024$Age)
qb_stats_2024$GS <- 0.66*qb_stats_2024$GS_2023 + 0.33*qb_stats_2024$GS_2022
qb_stats_2024$Snap_perc <- 0.66*qb_stats_2024$Snap_perc_2023 + 0.33*qb_stats_2024$Snap_perc_2022
qb_stats_2024$AV <- 0.66*qb_stats_2024$AV_2023 + 0.33*qb_stats_2024$AV_2022
qb_stats_2024$Stat_1 <- 0.66*qb_stats_2024$Stat_1_2023 + 0.33*qb_stats_2024$Stat_1_2022
qb_stats_2024$Stat_2 <- 0.66*qb_stats_2024$Stat_2_2023 + 0.33*qb_stats_2024$Stat_2_2022
qb_stats_2024 <- select(qb_stats_2024, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

# Change names before joining with contract data
qb_stats_2024$Player <- ifelse(qb_stats_2024$Player == "Gardner Minshew II", "Gardner Minshew", qb_stats_2024$Player)
qb_stats_2023$Player <- ifelse(qb_stats_2023$Player == "Gardner Minshew II", "Gardner Minshew", qb_stats_2023$Player)
qb_stats_2022$Player <- ifelse(qb_stats_2022$Player == "Gardner Minshew II", "Gardner Minshew", qb_stats_2022$Player)
qb_stats_2021$Player <- ifelse(qb_stats_2021$Player == "Gardner Minshew II", "Gardner Minshew", qb_stats_2021$Player)
qb_stats_2020$Player <- ifelse(qb_stats_2020$Player == "Gardner Minshew II", "Gardner Minshew", qb_stats_2020$Player)
qb_stats_2019$Player <- ifelse(qb_stats_2019$Player == "Gardner Minshew II", "Gardner Minshew", qb_stats_2019$Player)
qb_stats_2018$Player <- ifelse(qb_stats_2018$Player == "Gardner Minshew II", "Gardner Minshew", qb_stats_2018$Player)
qb_stats_2017$Player <- ifelse(qb_stats_2017$Player == "Gardner Minshew II", "Gardner Minshew", qb_stats_2017$Player)

all_qb_contracts$Player <- ifelse(all_qb_contracts$Player == "Matt Stafford", "Matthew Stafford", all_qb_contracts$Player)

# Get our contract data from the last R script (read.csv if necessary)
qb_contract_2016 <- filter(all_qb_contracts, Year.Signed == 2016)
qb_contract_2017 <- filter(all_qb_contracts, Year.Signed == 2017)
qb_contract_2018 <- filter(all_qb_contracts, Year.Signed == 2018)
qb_contract_2019 <- filter(all_qb_contracts, Year.Signed == 2019)
qb_contract_2020 <- filter(all_qb_contracts, Year.Signed == 2020)
qb_contract_2021 <- filter(all_qb_contracts, Year.Signed == 2021)
qb_contract_2022 <- filter(all_qb_contracts, Year.Signed == 2022)
qb_contract_2023 <- filter(all_qb_contracts, Year.Signed == 2023)
qb_contract_2024 <- filter(all_qb_contracts, Year.Signed == 2024)

# Join the two dataframes
qbs_2024 <- left_join(qb_contract_2024, qb_stats_2024, by = c("Player" = "Player"))
qbs_2023 <- left_join(qb_contract_2023, qb_stats_2023, by = c("Player" = "Player"))
qbs_2022 <- left_join(qb_contract_2022, qb_stats_2022, by = c("Player" = "Player"))
qbs_2021 <- left_join(qb_contract_2021, qb_stats_2021, by = c("Player" = "Player"))
qbs_2020 <- left_join(qb_contract_2020, qb_stats_2020, by = c("Player" = "Player"))
qbs_2019 <- left_join(qb_contract_2019, qb_stats_2019, by = c("Player" = "Player"))
qbs_2018 <- left_join(qb_contract_2018, qb_stats_2018, by = c("Player" = "Player"))
qbs_2017 <- left_join(qb_contract_2017, qb_stats_2017, by = c("Player" = "Player"))
qbs_2016 <- left_join(qb_contract_2016, qb_stats_2016, by = c("Player" = "Player"))

# Combine and save
qbs_masterfile <- bind_rows(qbs_2024, qbs_2023, qbs_2022, qbs_2021, qbs_2020, qbs_2019, qbs_2018, qbs_2017, qbs_2016)
write.csv(qbs_masterfile, "qbs_masterfile.csv")

# Now we repeat for every position


### Wide Receivers ----
receiver_data <- read.csv("wr_te_data.csv")
wr_stats <- filter(receiver_data, Pos == "WR")

wr_stats <- select(wr_stats, Player, Pos, Season, Age, GS, Snap_perc, AV, Stat_1 = YScm, Stat_2 = Y.Tgt)
wr_stats$Pos <- "WR"
wr_2014 <- filter(wr_stats, Season == "2014")
wr_2015 <- filter(wr_stats, Season == "2015")
wr_2016 <- filter(wr_stats, Season == "2016")
wr_2017 <- filter(wr_stats, Season == "2017")
wr_2018 <- filter(wr_stats, Season == "2018")
wr_2019 <- filter(wr_stats, Season == "2019")
wr_2020 <- filter(wr_stats, Season == "2020")
wr_2021 <- filter(wr_stats, Season == "2021")
wr_2022 <- filter(wr_stats, Season == "2022")
wr_2023 <- filter(wr_stats, Season == "2023")

colnames(wr_2014)[3:9] <- paste0(colnames(wr_2014)[3:9], "_2014")
colnames(wr_2015)[3:9] <- paste0(colnames(wr_2015)[3:9], "_2015")
colnames(wr_2016)[3:9] <- paste0(colnames(wr_2016)[3:9], "_2016")
colnames(wr_2017)[3:9] <- paste0(colnames(wr_2017)[3:9], "_2017")
colnames(wr_2018)[3:9] <- paste0(colnames(wr_2018)[3:9], "_2018")
colnames(wr_2019)[3:9] <- paste0(colnames(wr_2019)[3:9], "_2019")
colnames(wr_2020)[3:9] <- paste0(colnames(wr_2020)[3:9], "_2020")
colnames(wr_2021)[3:9] <- paste0(colnames(wr_2021)[3:9], "_2021")
colnames(wr_2022)[3:9] <- paste0(colnames(wr_2022)[3:9], "_2022")
colnames(wr_2023)[3:9] <- paste0(colnames(wr_2023)[3:9], "_2023")

filler_2016 <- merge(wr_2015, wr_2014, by = c("Player", "Pos"), all = TRUE)
filler_2017 <- merge(wr_2016, wr_2015, by = c("Player", "Pos"), all = TRUE)
filler_2018 <- merge(wr_2017, wr_2016, by = c("Player", "Pos"), all = TRUE)
filler_2019 <- merge(wr_2018, wr_2017, by = c("Player", "Pos"), all = TRUE)
filler_2020 <- merge(wr_2019, wr_2018, by = c("Player", "Pos"), all = TRUE)
filler_2021 <- merge(wr_2020, wr_2019, by = c("Player", "Pos"), all = TRUE)
filler_2022 <- merge(wr_2021, wr_2020, by = c("Player", "Pos"), all = TRUE)
filler_2023 <- merge(wr_2022, wr_2021, by = c("Player", "Pos"), all = TRUE)
filler_2024 <- merge(wr_2023, wr_2022, by = c("Player", "Pos"), all = TRUE)

filler_2016[3:16] <- lapply(filler_2016[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2017[3:16] <- lapply(filler_2017[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2018[3:16] <- lapply(filler_2018[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2019[3:16] <- lapply(filler_2019[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2020[3:16] <- lapply(filler_2020[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2021[3:16] <- lapply(filler_2021[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2022[3:16] <- lapply(filler_2022[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2023[3:16] <- lapply(filler_2023[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2024[3:16] <- lapply(filler_2024[3:16], function(x) ifelse(is.na(x), 0, x))


wr_stats_2016 <- filler_2016
wr_stats_2016$Age <- wr_stats_2016$Age_2015
wr_stats_2016$Age <- ifelse(wr_stats_2016$Age_2015 == 0, wr_stats_2016$Age_2014 + 1, wr_stats_2016$Age)
wr_stats_2016$GS <- 0.66*wr_stats_2016$GS_2015 + 0.33*wr_stats_2016$GS_2014
wr_stats_2016$Snap_perc <- 0.66*wr_stats_2016$Snap_perc_2015 + 0.33*wr_stats_2016$Snap_perc_2014
wr_stats_2016$AV <- 0.66*wr_stats_2016$AV_2015 + 0.33*wr_stats_2016$AV_2014
wr_stats_2016$Stat_1 <- 0.66*wr_stats_2016$Stat_1_2015 + 0.33*wr_stats_2016$Stat_1_2014
wr_stats_2016$Stat_2 <- 0.66*wr_stats_2016$Stat_2_2015 + 0.33*wr_stats_2016$Stat_2_2014
wr_stats_2016 <- select(wr_stats_2016, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

wr_stats_2017 <- filler_2017
wr_stats_2017$Age <- wr_stats_2017$Age_2016
wr_stats_2017$Age <- ifelse(wr_stats_2017$Age_2016 == 0, wr_stats_2017$Age_2015 + 1, wr_stats_2017$Age)
wr_stats_2017$GS <- 0.66*wr_stats_2017$GS_2016 + 0.33*wr_stats_2017$GS_2015
wr_stats_2017$Snap_perc <- 0.66*wr_stats_2017$Snap_perc_2016 + 0.33*wr_stats_2017$Snap_perc_2015
wr_stats_2017$AV <- 0.66*wr_stats_2017$AV_2016 + 0.33*wr_stats_2017$AV_2015
wr_stats_2017$Stat_1 <- 0.66*wr_stats_2017$Stat_1_2016 + 0.33*wr_stats_2017$Stat_1_2015
wr_stats_2017$Stat_2 <- 0.66*wr_stats_2017$Stat_2_2016 + 0.33*wr_stats_2017$Stat_2_2015
wr_stats_2017 <- select(wr_stats_2017, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

wr_stats_2018 <- filler_2018
wr_stats_2018$Age <- wr_stats_2018$Age_2017
wr_stats_2018$Age <- ifelse(wr_stats_2018$Age_2017 == 0, wr_stats_2018$Age_2016 + 1, wr_stats_2018$Age)
wr_stats_2018$GS <- 0.66*wr_stats_2018$GS_2017 + 0.33*wr_stats_2018$GS_2016
wr_stats_2018$Snap_perc <- 0.66*wr_stats_2018$Snap_perc_2017 + 0.33*wr_stats_2018$Snap_perc_2016
wr_stats_2018$AV <- 0.66*wr_stats_2018$AV_2017 + 0.33*wr_stats_2018$AV_2016
wr_stats_2018$Stat_1 <- 0.66*wr_stats_2018$Stat_1_2017 + 0.33*wr_stats_2018$Stat_1_2016
wr_stats_2018$Stat_2 <- 0.66*wr_stats_2018$Stat_2_2017 + 0.33*wr_stats_2018$Stat_2_2016
wr_stats_2018 <- select(wr_stats_2018, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

wr_stats_2019 <- filler_2019
wr_stats_2019$Age <- wr_stats_2019$Age_2018
wr_stats_2019$Age <- ifelse(wr_stats_2019$Age_2018 == 0, wr_stats_2019$Age_2017 + 1, wr_stats_2019$Age)
wr_stats_2019$GS <- 0.66*wr_stats_2019$GS_2018 + 0.33*wr_stats_2019$GS_2017
wr_stats_2019$Snap_perc <- 0.66*wr_stats_2019$Snap_perc_2018 + 0.33*wr_stats_2019$Snap_perc_2017
wr_stats_2019$AV <- 0.66*wr_stats_2019$AV_2018 + 0.33*wr_stats_2019$AV_2017
wr_stats_2019$Stat_1 <- 0.66*wr_stats_2019$Stat_1_2018 + 0.33*wr_stats_2019$Stat_1_2017
wr_stats_2019$Stat_2 <- 0.66*wr_stats_2019$Stat_2_2018 + 0.33*wr_stats_2019$Stat_2_2017
wr_stats_2019 <- select(wr_stats_2019, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

wr_stats_2020 <- filler_2020
wr_stats_2020$Age <- wr_stats_2020$Age_2019
wr_stats_2020$Age <- ifelse(wr_stats_2020$Age_2019 == 0, wr_stats_2020$Age_2018 + 1, wr_stats_2020$Age)
wr_stats_2020$GS <- 0.66*wr_stats_2020$GS_2019 + 0.33*wr_stats_2020$GS_2018
wr_stats_2020$Snap_perc <- 0.66*wr_stats_2020$Snap_perc_2019 + 0.33*wr_stats_2020$Snap_perc_2018
wr_stats_2020$AV <- 0.66*wr_stats_2020$AV_2019 + 0.33*wr_stats_2020$AV_2018
wr_stats_2020$Stat_1 <- 0.66*wr_stats_2020$Stat_1_2019 + 0.33*wr_stats_2020$Stat_1_2018
wr_stats_2020$Stat_2 <- 0.66*wr_stats_2020$Stat_2_2019 + 0.33*wr_stats_2020$Stat_2_2018
wr_stats_2020 <- select(wr_stats_2020, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

wr_stats_2021 <- filler_2021
wr_stats_2021$Age <- wr_stats_2021$Age_2020
wr_stats_2021$Age <- ifelse(wr_stats_2021$Age_2020 == 0, wr_stats_2021$Age_2019 + 1, wr_stats_2021$Age)
wr_stats_2021$GS <- 0.66*wr_stats_2021$GS_2020 + 0.33*wr_stats_2021$GS_2019
wr_stats_2021$Snap_perc <- 0.66*wr_stats_2021$Snap_perc_2020 + 0.33*wr_stats_2021$Snap_perc_2019
wr_stats_2021$AV <- 0.66*wr_stats_2021$AV_2020 + 0.33*wr_stats_2021$AV_2019
wr_stats_2021$Stat_1 <- 0.66*wr_stats_2021$Stat_1_2020 + 0.33*wr_stats_2021$Stat_1_2019
wr_stats_2021$Stat_2 <- 0.66*wr_stats_2021$Stat_2_2020 + 0.33*wr_stats_2021$Stat_2_2019
wr_stats_2021 <- select(wr_stats_2021, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

wr_stats_2022 <- filler_2022
wr_stats_2022$Age <- wr_stats_2022$Age_2021
wr_stats_2022$Age <- ifelse(wr_stats_2022$Age_2021 == 0, wr_stats_2022$Age_2020 + 1, wr_stats_2022$Age)
wr_stats_2022$GS <- 0.66*wr_stats_2022$GS_2021 + 0.33*wr_stats_2022$GS_2020
wr_stats_2022$Snap_perc <- 0.66*wr_stats_2022$Snap_perc_2021 + 0.33*wr_stats_2022$Snap_perc_2020
wr_stats_2022$AV <- 0.66*wr_stats_2022$AV_2021 + 0.33*wr_stats_2022$AV_2020
wr_stats_2022$Stat_1 <- 0.66*wr_stats_2022$Stat_1_2021 + 0.33*wr_stats_2022$Stat_1_2020
wr_stats_2022$Stat_2 <- 0.66*wr_stats_2022$Stat_2_2021 + 0.33*wr_stats_2022$Stat_2_2020
wr_stats_2022 <- select(wr_stats_2022, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

wr_stats_2023 <- filler_2023
wr_stats_2023$Age <- wr_stats_2023$Age_2022
wr_stats_2023$Age <- ifelse(wr_stats_2023$Age_2022 == 0, wr_stats_2023$Age_2021 + 1, wr_stats_2023$Age)
wr_stats_2023$GS <- 0.66*wr_stats_2023$GS_2022 + 0.33*wr_stats_2023$GS_2021
wr_stats_2023$Snap_perc <- 0.66*wr_stats_2023$Snap_perc_2022 + 0.33*wr_stats_2023$Snap_perc_2021
wr_stats_2023$AV <- 0.66*wr_stats_2023$AV_2022 + 0.33*wr_stats_2023$AV_2021
wr_stats_2023$Stat_1 <- 0.66*wr_stats_2023$Stat_1_2022 + 0.33*wr_stats_2023$Stat_1_2021
wr_stats_2023$Stat_2 <- 0.66*wr_stats_2023$Stat_2_2022 + 0.33*wr_stats_2023$Stat_2_2021
wr_stats_2023 <- select(wr_stats_2023, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

wr_stats_2024 <- filler_2024
wr_stats_2024$Age <- wr_stats_2024$Age_2023
wr_stats_2024$Age <- ifelse(wr_stats_2024$Age_2023 == 0, wr_stats_2024$Age_2022 + 1, wr_stats_2024$Age)
wr_stats_2024$GS <- 0.66*wr_stats_2024$GS_2023 + 0.33*wr_stats_2024$GS_2022
wr_stats_2024$Snap_perc <- 0.66*wr_stats_2024$Snap_perc_2023 + 0.33*wr_stats_2024$Snap_perc_2022
wr_stats_2024$AV <- 0.66*wr_stats_2024$AV_2023 + 0.33*wr_stats_2024$AV_2022
wr_stats_2024$Stat_1 <- 0.66*wr_stats_2024$Stat_1_2023 + 0.33*wr_stats_2024$Stat_1_2022
wr_stats_2024$Stat_2 <- 0.66*wr_stats_2024$Stat_2_2023 + 0.33*wr_stats_2024$Stat_2_2022
wr_stats_2024 <- select(wr_stats_2024, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

all_wr_contracts$Player <- ifelse(all_wr_contracts$Player == "Ted Ginn", "Ted Ginn Jr.", all_wr_contracts$Player)
all_wr_contracts$Player <- ifelse(all_wr_contracts$Player == "Matt Slater", "Matthew Slater", all_wr_contracts$Player)

wr_contract_2016 <- filter(all_wr_contracts, Year.Signed == 2016)
wr_contract_2017 <- filter(all_wr_contracts, Year.Signed == 2017)
wr_contract_2018 <- filter(all_wr_contracts, Year.Signed == 2018)
wr_contract_2019 <- filter(all_wr_contracts, Year.Signed == 2019)
wr_contract_2020 <- filter(all_wr_contracts, Year.Signed == 2020)
wr_contract_2021 <- filter(all_wr_contracts, Year.Signed == 2021)
wr_contract_2022 <- filter(all_wr_contracts, Year.Signed == 2022)
wr_contract_2023 <- filter(all_wr_contracts, Year.Signed == 2023)
wr_contract_2024 <- filter(all_wr_contracts, Year.Signed == 2024)


wrs_2024 <- left_join(wr_contract_2024, wr_stats_2024, by = c("Player" = "Player"))
wrs_2023 <- left_join(wr_contract_2023, wr_stats_2023, by = c("Player" = "Player"))
wrs_2022 <- left_join(wr_contract_2022, wr_stats_2022, by = c("Player" = "Player"))
wrs_2021 <- left_join(wr_contract_2021, wr_stats_2021, by = c("Player" = "Player"))
wrs_2020 <- left_join(wr_contract_2020, wr_stats_2020, by = c("Player" = "Player"))
wrs_2019 <- left_join(wr_contract_2019, wr_stats_2019, by = c("Player" = "Player"))
wrs_2018 <- left_join(wr_contract_2018, wr_stats_2018, by = c("Player" = "Player"))
wrs_2017 <- left_join(wr_contract_2017, wr_stats_2017, by = c("Player" = "Player"))
wrs_2016 <- left_join(wr_contract_2016, wr_stats_2016, by = c("Player" = "Player"))


wrs_masterfile <- bind_rows(wrs_2024, wrs_2023, wrs_2022, wrs_2021, wrs_2020, wrs_2019, wrs_2018, wrs_2017, wrs_2016)
write.csv(wrs_masterfile, "wrs_masterfile.csv")

### Tight Ends ----
te_stats <- filter(receiver_data, Pos == "TE")

te_stats <- select(te_stats, Player, Pos, Season, Age, GS, Snap_perc, AV, Stat_1 = YScm, Stat_2 = Y.Tgt)
te_stats$Pos <- "TE"
te_2014 <- filter(te_stats, Season == "2014")
te_2015 <- filter(te_stats, Season == "2015")
te_2016 <- filter(te_stats, Season == "2016")
te_2017 <- filter(te_stats, Season == "2017")
te_2018 <- filter(te_stats, Season == "2018")
te_2019 <- filter(te_stats, Season == "2019")
te_2020 <- filter(te_stats, Season == "2020")
te_2021 <- filter(te_stats, Season == "2021")
te_2022 <- filter(te_stats, Season == "2022")
te_2023 <- filter(te_stats, Season == "2023")

colnames(te_2014)[3:9] <- paste0(colnames(te_2014)[3:9], "_2014")
colnames(te_2015)[3:9] <- paste0(colnames(te_2015)[3:9], "_2015")
colnames(te_2016)[3:9] <- paste0(colnames(te_2016)[3:9], "_2016")
colnames(te_2017)[3:9] <- paste0(colnames(te_2017)[3:9], "_2017")
colnames(te_2018)[3:9] <- paste0(colnames(te_2018)[3:9], "_2018")
colnames(te_2019)[3:9] <- paste0(colnames(te_2019)[3:9], "_2019")
colnames(te_2020)[3:9] <- paste0(colnames(te_2020)[3:9], "_2020")
colnames(te_2021)[3:9] <- paste0(colnames(te_2021)[3:9], "_2021")
colnames(te_2022)[3:9] <- paste0(colnames(te_2022)[3:9], "_2022")
colnames(te_2023)[3:9] <- paste0(colnames(te_2023)[3:9], "_2023")

filler_2016 <- merge(te_2015, te_2014, by = c("Player", "Pos"), all = TRUE)
filler_2017 <- merge(te_2016, te_2015, by = c("Player", "Pos"), all = TRUE)
filler_2018 <- merge(te_2017, te_2016, by = c("Player", "Pos"), all = TRUE)
filler_2019 <- merge(te_2018, te_2017, by = c("Player", "Pos"), all = TRUE)
filler_2020 <- merge(te_2019, te_2018, by = c("Player", "Pos"), all = TRUE)
filler_2021 <- merge(te_2020, te_2019, by = c("Player", "Pos"), all = TRUE)
filler_2022 <- merge(te_2021, te_2020, by = c("Player", "Pos"), all = TRUE)
filler_2023 <- merge(te_2022, te_2021, by = c("Player", "Pos"), all = TRUE)
filler_2024 <- merge(te_2023, te_2022, by = c("Player", "Pos"), all = TRUE)

filler_2016[3:16] <- lapply(filler_2016[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2017[3:16] <- lapply(filler_2017[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2018[3:16] <- lapply(filler_2018[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2019[3:16] <- lapply(filler_2019[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2020[3:16] <- lapply(filler_2020[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2021[3:16] <- lapply(filler_2021[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2022[3:16] <- lapply(filler_2022[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2023[3:16] <- lapply(filler_2023[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2024[3:16] <- lapply(filler_2024[3:16], function(x) ifelse(is.na(x), 0, x))


te_stats_2016 <- filler_2016
te_stats_2016$Age <- te_stats_2016$Age_2015
te_stats_2016$Age <- ifelse(te_stats_2016$Age_2015 == 0, te_stats_2016$Age_2014 + 1, te_stats_2016$Age)
te_stats_2016$GS <- 0.66*te_stats_2016$GS_2015 + 0.33*te_stats_2016$GS_2014
te_stats_2016$Snap_perc <- 0.66*te_stats_2016$Snap_perc_2015 + 0.33*te_stats_2016$Snap_perc_2014
te_stats_2016$AV <- 0.66*te_stats_2016$AV_2015 + 0.33*te_stats_2016$AV_2014
te_stats_2016$Stat_1 <- 0.66*te_stats_2016$Stat_1_2015 + 0.33*te_stats_2016$Stat_1_2014
te_stats_2016$Stat_2 <- 0.66*te_stats_2016$Stat_2_2015 + 0.33*te_stats_2016$Stat_2_2014
te_stats_2016 <- select(te_stats_2016, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

te_stats_2017 <- filler_2017
te_stats_2017$Age <- te_stats_2017$Age_2016
te_stats_2017$Age <- ifelse(te_stats_2017$Age_2016 == 0, te_stats_2017$Age_2015 + 1, te_stats_2017$Age)
te_stats_2017$GS <- 0.66*te_stats_2017$GS_2016 + 0.33*te_stats_2017$GS_2015
te_stats_2017$Snap_perc <- 0.66*te_stats_2017$Snap_perc_2016 + 0.33*te_stats_2017$Snap_perc_2015
te_stats_2017$AV <- 0.66*te_stats_2017$AV_2016 + 0.33*te_stats_2017$AV_2015
te_stats_2017$Stat_1 <- 0.66*te_stats_2017$Stat_1_2016 + 0.33*te_stats_2017$Stat_1_2015
te_stats_2017$Stat_2 <- 0.66*te_stats_2017$Stat_2_2016 + 0.33*te_stats_2017$Stat_2_2015
te_stats_2017 <- select(te_stats_2017, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

te_stats_2018 <- filler_2018
te_stats_2018$Age <- te_stats_2018$Age_2017
te_stats_2018$Age <- ifelse(te_stats_2018$Age_2017 == 0, te_stats_2018$Age_2016 + 1, te_stats_2018$Age)
te_stats_2018$GS <- 0.66*te_stats_2018$GS_2017 + 0.33*te_stats_2018$GS_2016
te_stats_2018$Snap_perc <- 0.66*te_stats_2018$Snap_perc_2017 + 0.33*te_stats_2018$Snap_perc_2016
te_stats_2018$AV <- 0.66*te_stats_2018$AV_2017 + 0.33*te_stats_2018$AV_2016
te_stats_2018$Stat_1 <- 0.66*te_stats_2018$Stat_1_2017 + 0.33*te_stats_2018$Stat_1_2016
te_stats_2018$Stat_2 <- 0.66*te_stats_2018$Stat_2_2017 + 0.33*te_stats_2018$Stat_2_2016
te_stats_2018 <- select(te_stats_2018, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

te_stats_2019 <- filler_2019
te_stats_2019$Age <- te_stats_2019$Age_2018
te_stats_2019$Age <- ifelse(te_stats_2019$Age_2018 == 0, te_stats_2019$Age_2017 + 1, te_stats_2019$Age)
te_stats_2019$GS <- 0.66*te_stats_2019$GS_2018 + 0.33*te_stats_2019$GS_2017
te_stats_2019$Snap_perc <- 0.66*te_stats_2019$Snap_perc_2018 + 0.33*te_stats_2019$Snap_perc_2017
te_stats_2019$AV <- 0.66*te_stats_2019$AV_2018 + 0.33*te_stats_2019$AV_2017
te_stats_2019$Stat_1 <- 0.66*te_stats_2019$Stat_1_2018 + 0.33*te_stats_2019$Stat_1_2017
te_stats_2019$Stat_2 <- 0.66*te_stats_2019$Stat_2_2018 + 0.33*te_stats_2019$Stat_2_2017
te_stats_2019 <- select(te_stats_2019, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

te_stats_2020 <- filler_2020
te_stats_2020$Age <- te_stats_2020$Age_2019
te_stats_2020$Age <- ifelse(te_stats_2020$Age_2019 == 0, te_stats_2020$Age_2018 + 1, te_stats_2020$Age)
te_stats_2020$GS <- 0.66*te_stats_2020$GS_2019 + 0.33*te_stats_2020$GS_2018
te_stats_2020$Snap_perc <- 0.66*te_stats_2020$Snap_perc_2019 + 0.33*te_stats_2020$Snap_perc_2018
te_stats_2020$AV <- 0.66*te_stats_2020$AV_2019 + 0.33*te_stats_2020$AV_2018
te_stats_2020$Stat_1 <- 0.66*te_stats_2020$Stat_1_2019 + 0.33*te_stats_2020$Stat_1_2018
te_stats_2020$Stat_2 <- 0.66*te_stats_2020$Stat_2_2019 + 0.33*te_stats_2020$Stat_2_2018
te_stats_2020 <- select(te_stats_2020, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

te_stats_2021 <- filler_2021
te_stats_2021$Age <- te_stats_2021$Age_2020
te_stats_2021$Age <- ifelse(te_stats_2021$Age_2020 == 0, te_stats_2021$Age_2019 + 1, te_stats_2021$Age)
te_stats_2021$GS <- 0.66*te_stats_2021$GS_2020 + 0.33*te_stats_2021$GS_2019
te_stats_2021$Snap_perc <- 0.66*te_stats_2021$Snap_perc_2020 + 0.33*te_stats_2021$Snap_perc_2019
te_stats_2021$AV <- 0.66*te_stats_2021$AV_2020 + 0.33*te_stats_2021$AV_2019
te_stats_2021$Stat_1 <- 0.66*te_stats_2021$Stat_1_2020 + 0.33*te_stats_2021$Stat_1_2019
te_stats_2021$Stat_2 <- 0.66*te_stats_2021$Stat_2_2020 + 0.33*te_stats_2021$Stat_2_2019
te_stats_2021 <- select(te_stats_2021, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

te_stats_2022 <- filler_2022
te_stats_2022$Age <- te_stats_2022$Age_2021
te_stats_2022$Age <- ifelse(te_stats_2022$Age_2021 == 0, te_stats_2022$Age_2020 + 1, te_stats_2022$Age)
te_stats_2022$GS <- 0.66*te_stats_2022$GS_2021 + 0.33*te_stats_2022$GS_2020
te_stats_2022$Snap_perc <- 0.66*te_stats_2022$Snap_perc_2021 + 0.33*te_stats_2022$Snap_perc_2020
te_stats_2022$AV <- 0.66*te_stats_2022$AV_2021 + 0.33*te_stats_2022$AV_2020
te_stats_2022$Stat_1 <- 0.66*te_stats_2022$Stat_1_2021 + 0.33*te_stats_2022$Stat_1_2020
te_stats_2022$Stat_2 <- 0.66*te_stats_2022$Stat_2_2021 + 0.33*te_stats_2022$Stat_2_2020
te_stats_2022 <- select(te_stats_2022, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

te_stats_2023 <- filler_2023
te_stats_2023$Age <- te_stats_2023$Age_2022
te_stats_2023$Age <- ifelse(te_stats_2023$Age_2022 == 0, te_stats_2023$Age_2021 + 1, te_stats_2023$Age)
te_stats_2023$GS <- 0.66*te_stats_2023$GS_2022 + 0.33*te_stats_2023$GS_2021
te_stats_2023$Snap_perc <- 0.66*te_stats_2023$Snap_perc_2022 + 0.33*te_stats_2023$Snap_perc_2021
te_stats_2023$AV <- 0.66*te_stats_2023$AV_2022 + 0.33*te_stats_2023$AV_2021
te_stats_2023$Stat_1 <- 0.66*te_stats_2023$Stat_1_2022 + 0.33*te_stats_2023$Stat_1_2021
te_stats_2023$Stat_2 <- 0.66*te_stats_2023$Stat_2_2022 + 0.33*te_stats_2023$Stat_2_2021
te_stats_2023 <- select(te_stats_2023, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

te_stats_2024 <- filler_2024
te_stats_2024$Age <- te_stats_2024$Age_2023
te_stats_2024$Age <- ifelse(te_stats_2024$Age_2023 == 0, te_stats_2024$Age_2022 + 1, te_stats_2024$Age)
te_stats_2024$GS <- 0.66*te_stats_2024$GS_2023 + 0.33*te_stats_2024$GS_2022
te_stats_2024$Snap_perc <- 0.66*te_stats_2024$Snap_perc_2023 + 0.33*te_stats_2024$Snap_perc_2022
te_stats_2024$AV <- 0.66*te_stats_2024$AV_2023 + 0.33*te_stats_2024$AV_2022
te_stats_2024$Stat_1 <- 0.66*te_stats_2024$Stat_1_2023 + 0.33*te_stats_2024$Stat_1_2022
te_stats_2024$Stat_2 <- 0.66*te_stats_2024$Stat_2_2023 + 0.33*te_stats_2024$Stat_2_2022
te_stats_2024 <- select(te_stats_2024, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

te_contract_2016 <- filter(all_te_contracts, Year.Signed == 2016)
te_contract_2017 <- filter(all_te_contracts, Year.Signed == 2017)
te_contract_2018 <- filter(all_te_contracts, Year.Signed == 2018)
te_contract_2019 <- filter(all_te_contracts, Year.Signed == 2019)
te_contract_2020 <- filter(all_te_contracts, Year.Signed == 2020)
te_contract_2021 <- filter(all_te_contracts, Year.Signed == 2021)
te_contract_2022 <- filter(all_te_contracts, Year.Signed == 2022)
te_contract_2023 <- filter(all_te_contracts, Year.Signed == 2023)
te_contract_2024 <- filter(all_te_contracts, Year.Signed == 2024)


tes_2024 <- left_join(te_contract_2024, te_stats_2024, by = c("Player" = "Player"))
tes_2023 <- left_join(te_contract_2023, te_stats_2023, by = c("Player" = "Player"))
tes_2022 <- left_join(te_contract_2022, te_stats_2022, by = c("Player" = "Player"))
tes_2021 <- left_join(te_contract_2021, te_stats_2021, by = c("Player" = "Player"))
tes_2020 <- left_join(te_contract_2020, te_stats_2020, by = c("Player" = "Player"))
tes_2019 <- left_join(te_contract_2019, te_stats_2019, by = c("Player" = "Player"))
tes_2018 <- left_join(te_contract_2018, te_stats_2018, by = c("Player" = "Player"))
tes_2017 <- left_join(te_contract_2017, te_stats_2017, by = c("Player" = "Player"))
tes_2016 <- left_join(te_contract_2016, te_stats_2016, by = c("Player" = "Player"))


tes_masterfile <- bind_rows(tes_2024, tes_2023, tes_2022, tes_2021, tes_2020, tes_2019, tes_2018, tes_2017, tes_2016)
write.csv(tes_masterfile, "tes_masterfile.csv")

### Runningbacks ----
rb_stats <- read.csv("rb_data.csv")

rb_stats <- select(rb_stats, Player, Pos, Season, Age, GS, Snap_perc, AV, Stat_1 = YScm, Stat_2 = YPA)
rb_stats$Pos <- "RB"
rb_2014 <- filter(rb_stats, Season == "2014")
rb_2015 <- filter(rb_stats, Season == "2015")
rb_2016 <- filter(rb_stats, Season == "2016")
rb_2017 <- filter(rb_stats, Season == "2017")
rb_2018 <- filter(rb_stats, Season == "2018")
rb_2019 <- filter(rb_stats, Season == "2019")
rb_2020 <- filter(rb_stats, Season == "2020")
rb_2021 <- filter(rb_stats, Season == "2021")
rb_2022 <- filter(rb_stats, Season == "2022")
rb_2023 <- filter(rb_stats, Season == "2023")

colnames(rb_2014)[3:9] <- paste0(colnames(rb_2014)[3:9], "_2014")
colnames(rb_2015)[3:9] <- paste0(colnames(rb_2015)[3:9], "_2015")
colnames(rb_2016)[3:9] <- paste0(colnames(rb_2016)[3:9], "_2016")
colnames(rb_2017)[3:9] <- paste0(colnames(rb_2017)[3:9], "_2017")
colnames(rb_2018)[3:9] <- paste0(colnames(rb_2018)[3:9], "_2018")
colnames(rb_2019)[3:9] <- paste0(colnames(rb_2019)[3:9], "_2019")
colnames(rb_2020)[3:9] <- paste0(colnames(rb_2020)[3:9], "_2020")
colnames(rb_2021)[3:9] <- paste0(colnames(rb_2021)[3:9], "_2021")
colnames(rb_2022)[3:9] <- paste0(colnames(rb_2022)[3:9], "_2022")
colnames(rb_2023)[3:9] <- paste0(colnames(rb_2023)[3:9], "_2023")

filler_2016 <- merge(rb_2015, rb_2014, by = c("Player", "Pos"), all = TRUE)
filler_2017 <- merge(rb_2016, rb_2015, by = c("Player", "Pos"), all = TRUE)
filler_2018 <- merge(rb_2017, rb_2016, by = c("Player", "Pos"), all = TRUE)
filler_2019 <- merge(rb_2018, rb_2017, by = c("Player", "Pos"), all = TRUE)
filler_2020 <- merge(rb_2019, rb_2018, by = c("Player", "Pos"), all = TRUE)
filler_2021 <- merge(rb_2020, rb_2019, by = c("Player", "Pos"), all = TRUE)
filler_2022 <- merge(rb_2021, rb_2020, by = c("Player", "Pos"), all = TRUE)
filler_2023 <- merge(rb_2022, rb_2021, by = c("Player", "Pos"), all = TRUE)
filler_2024 <- merge(rb_2023, rb_2022, by = c("Player", "Pos"), all = TRUE)

filler_2016[3:16] <- lapply(filler_2016[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2017[3:16] <- lapply(filler_2017[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2018[3:16] <- lapply(filler_2018[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2019[3:16] <- lapply(filler_2019[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2020[3:16] <- lapply(filler_2020[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2021[3:16] <- lapply(filler_2021[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2022[3:16] <- lapply(filler_2022[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2023[3:16] <- lapply(filler_2023[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2024[3:16] <- lapply(filler_2024[3:16], function(x) ifelse(is.na(x), 0, x))


rb_stats_2016 <- filler_2016
rb_stats_2016$Age <- rb_stats_2016$Age_2015
rb_stats_2016$Age <- ifelse(rb_stats_2016$Age_2015 == 0, rb_stats_2016$Age_2014 + 1, rb_stats_2016$Age)
rb_stats_2016$GS <- 0.66*rb_stats_2016$GS_2015 + 0.33*rb_stats_2016$GS_2014
rb_stats_2016$Snap_perc <- 0.66*rb_stats_2016$Snap_perc_2015 + 0.33*rb_stats_2016$Snap_perc_2014
rb_stats_2016$AV <- 0.66*rb_stats_2016$AV_2015 + 0.33*rb_stats_2016$AV_2014
rb_stats_2016$Stat_1 <- 0.66*rb_stats_2016$Stat_1_2015 + 0.33*rb_stats_2016$Stat_1_2014
rb_stats_2016$Stat_2 <- 0.66*rb_stats_2016$Stat_2_2015 + 0.33*rb_stats_2016$Stat_2_2014
rb_stats_2016 <- select(rb_stats_2016, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

rb_stats_2017 <- filler_2017
rb_stats_2017$Age <- rb_stats_2017$Age_2016
rb_stats_2017$Age <- ifelse(rb_stats_2017$Age_2016 == 0, rb_stats_2017$Age_2015 + 1, rb_stats_2017$Age)
rb_stats_2017$GS <- 0.66*rb_stats_2017$GS_2016 + 0.33*rb_stats_2017$GS_2015
rb_stats_2017$Snap_perc <- 0.66*rb_stats_2017$Snap_perc_2016 + 0.33*rb_stats_2017$Snap_perc_2015
rb_stats_2017$AV <- 0.66*rb_stats_2017$AV_2016 + 0.33*rb_stats_2017$AV_2015
rb_stats_2017$Stat_1 <- 0.66*rb_stats_2017$Stat_1_2016 + 0.33*rb_stats_2017$Stat_1_2015
rb_stats_2017$Stat_2 <- 0.66*rb_stats_2017$Stat_2_2016 + 0.33*rb_stats_2017$Stat_2_2015
rb_stats_2017 <- select(rb_stats_2017, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

rb_stats_2018 <- filler_2018
rb_stats_2018$Age <- rb_stats_2018$Age_2017
rb_stats_2018$Age <- ifelse(rb_stats_2018$Age_2017 == 0, rb_stats_2018$Age_2016 + 1, rb_stats_2018$Age)
rb_stats_2018$GS <- 0.66*rb_stats_2018$GS_2017 + 0.33*rb_stats_2018$GS_2016
rb_stats_2018$Snap_perc <- 0.66*rb_stats_2018$Snap_perc_2017 + 0.33*rb_stats_2018$Snap_perc_2016
rb_stats_2018$AV <- 0.66*rb_stats_2018$AV_2017 + 0.33*rb_stats_2018$AV_2016
rb_stats_2018$Stat_1 <- 0.66*rb_stats_2018$Stat_1_2017 + 0.33*rb_stats_2018$Stat_1_2016
rb_stats_2018$Stat_2 <- 0.66*rb_stats_2018$Stat_2_2017 + 0.33*rb_stats_2018$Stat_2_2016
rb_stats_2018 <- select(rb_stats_2018, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

rb_stats_2019 <- filler_2019
rb_stats_2019$Age <- rb_stats_2019$Age_2018
rb_stats_2019$Age <- ifelse(rb_stats_2019$Age_2018 == 0, rb_stats_2019$Age_2017 + 1, rb_stats_2019$Age)
rb_stats_2019$GS <- 0.66*rb_stats_2019$GS_2018 + 0.33*rb_stats_2019$GS_2017
rb_stats_2019$Snap_perc <- 0.66*rb_stats_2019$Snap_perc_2018 + 0.33*rb_stats_2019$Snap_perc_2017
rb_stats_2019$AV <- 0.66*rb_stats_2019$AV_2018 + 0.33*rb_stats_2019$AV_2017
rb_stats_2019$Stat_1 <- 0.66*rb_stats_2019$Stat_1_2018 + 0.33*rb_stats_2019$Stat_1_2017
rb_stats_2019$Stat_2 <- 0.66*rb_stats_2019$Stat_2_2018 + 0.33*rb_stats_2019$Stat_2_2017
rb_stats_2019 <- select(rb_stats_2019, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

rb_stats_2020 <- filler_2020
rb_stats_2020$Age <- rb_stats_2020$Age_2019
rb_stats_2020$Age <- ifelse(rb_stats_2020$Age_2019 == 0, rb_stats_2020$Age_2018 + 1, rb_stats_2020$Age)
rb_stats_2020$GS <- 0.66*rb_stats_2020$GS_2019 + 0.33*rb_stats_2020$GS_2018
rb_stats_2020$Snap_perc <- 0.66*rb_stats_2020$Snap_perc_2019 + 0.33*rb_stats_2020$Snap_perc_2018
rb_stats_2020$AV <- 0.66*rb_stats_2020$AV_2019 + 0.33*rb_stats_2020$AV_2018
rb_stats_2020$Stat_1 <- 0.66*rb_stats_2020$Stat_1_2019 + 0.33*rb_stats_2020$Stat_1_2018
rb_stats_2020$Stat_2 <- 0.66*rb_stats_2020$Stat_2_2019 + 0.33*rb_stats_2020$Stat_2_2018
rb_stats_2020 <- select(rb_stats_2020, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

rb_stats_2021 <- filler_2021
rb_stats_2021$Age <- rb_stats_2021$Age_2020
rb_stats_2021$Age <- ifelse(rb_stats_2021$Age_2020 == 0, rb_stats_2021$Age_2019 + 1, rb_stats_2021$Age)
rb_stats_2021$GS <- 0.66*rb_stats_2021$GS_2020 + 0.33*rb_stats_2021$GS_2019
rb_stats_2021$Snap_perc <- 0.66*rb_stats_2021$Snap_perc_2020 + 0.33*rb_stats_2021$Snap_perc_2019
rb_stats_2021$AV <- 0.66*rb_stats_2021$AV_2020 + 0.33*rb_stats_2021$AV_2019
rb_stats_2021$Stat_1 <- 0.66*rb_stats_2021$Stat_1_2020 + 0.33*rb_stats_2021$Stat_1_2019
rb_stats_2021$Stat_2 <- 0.66*rb_stats_2021$Stat_2_2020 + 0.33*rb_stats_2021$Stat_2_2019
rb_stats_2021 <- select(rb_stats_2021, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

rb_stats_2022 <- filler_2022
rb_stats_2022$Age <- rb_stats_2022$Age_2021
rb_stats_2022$Age <- ifelse(rb_stats_2022$Age_2021 == 0, rb_stats_2022$Age_2020 + 1, rb_stats_2022$Age)
rb_stats_2022$GS <- 0.66*rb_stats_2022$GS_2021 + 0.33*rb_stats_2022$GS_2020
rb_stats_2022$Snap_perc <- 0.66*rb_stats_2022$Snap_perc_2021 + 0.33*rb_stats_2022$Snap_perc_2020
rb_stats_2022$AV <- 0.66*rb_stats_2022$AV_2021 + 0.33*rb_stats_2022$AV_2020
rb_stats_2022$Stat_1 <- 0.66*rb_stats_2022$Stat_1_2021 + 0.33*rb_stats_2022$Stat_1_2020
rb_stats_2022$Stat_2 <- 0.66*rb_stats_2022$Stat_2_2021 + 0.33*rb_stats_2022$Stat_2_2020
rb_stats_2022 <- select(rb_stats_2022, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

rb_stats_2023 <- filler_2023
rb_stats_2023$Age <- rb_stats_2023$Age_2022
rb_stats_2023$Age <- ifelse(rb_stats_2023$Age_2022 == 0, rb_stats_2023$Age_2021 + 1, rb_stats_2023$Age)
rb_stats_2023$GS <- 0.66*rb_stats_2023$GS_2022 + 0.33*rb_stats_2023$GS_2021
rb_stats_2023$Snap_perc <- 0.66*rb_stats_2023$Snap_perc_2022 + 0.33*rb_stats_2023$Snap_perc_2021
rb_stats_2023$AV <- 0.66*rb_stats_2023$AV_2022 + 0.33*rb_stats_2023$AV_2021
rb_stats_2023$Stat_1 <- 0.66*rb_stats_2023$Stat_1_2022 + 0.33*rb_stats_2023$Stat_1_2021
rb_stats_2023$Stat_2 <- 0.66*rb_stats_2023$Stat_2_2022 + 0.33*rb_stats_2023$Stat_2_2021
rb_stats_2023 <- select(rb_stats_2023, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

rb_stats_2024 <- filler_2024
rb_stats_2024$Age <- rb_stats_2024$Age_2023
rb_stats_2024$Age <- ifelse(rb_stats_2024$Age_2023 == 0, rb_stats_2024$Age_2022 + 1, rb_stats_2024$Age)
rb_stats_2024$GS <- 0.66*rb_stats_2024$GS_2023 + 0.33*rb_stats_2024$GS_2022
rb_stats_2024$Snap_perc <- 0.66*rb_stats_2024$Snap_perc_2023 + 0.33*rb_stats_2024$Snap_perc_2022
rb_stats_2024$AV <- 0.66*rb_stats_2024$AV_2023 + 0.33*rb_stats_2024$AV_2022
rb_stats_2024$Stat_1 <- 0.66*rb_stats_2024$Stat_1_2023 + 0.33*rb_stats_2024$Stat_1_2022
rb_stats_2024$Stat_2 <- 0.66*rb_stats_2024$Stat_2_2023 + 0.33*rb_stats_2024$Stat_2_2022
rb_stats_2024 <- select(rb_stats_2024, Player, Pos, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

rb_contract_2016 <- filter(all_rb_contracts, Year.Signed == 2016)
rb_contract_2017 <- filter(all_rb_contracts, Year.Signed == 2017)
rb_contract_2018 <- filter(all_rb_contracts, Year.Signed == 2018)
rb_contract_2019 <- filter(all_rb_contracts, Year.Signed == 2019)
rb_contract_2020 <- filter(all_rb_contracts, Year.Signed == 2020)
rb_contract_2021 <- filter(all_rb_contracts, Year.Signed == 2021)
rb_contract_2022 <- filter(all_rb_contracts, Year.Signed == 2022)
rb_contract_2023 <- filter(all_rb_contracts, Year.Signed == 2023)
rb_contract_2024 <- filter(all_rb_contracts, Year.Signed == 2024)


rbs_2024 <- left_join(rb_contract_2024, rb_stats_2024, by = c("Player" = "Player"))
rbs_2023 <- left_join(rb_contract_2023, rb_stats_2023, by = c("Player" = "Player"))
rbs_2022 <- left_join(rb_contract_2022, rb_stats_2022, by = c("Player" = "Player"))
rbs_2021 <- left_join(rb_contract_2021, rb_stats_2021, by = c("Player" = "Player"))
rbs_2020 <- left_join(rb_contract_2020, rb_stats_2020, by = c("Player" = "Player"))
rbs_2019 <- left_join(rb_contract_2019, rb_stats_2019, by = c("Player" = "Player"))
rbs_2018 <- left_join(rb_contract_2018, rb_stats_2018, by = c("Player" = "Player"))
rbs_2017 <- left_join(rb_contract_2017, rb_stats_2017, by = c("Player" = "Player"))
rbs_2016 <- left_join(rb_contract_2016, rb_stats_2016, by = c("Player" = "Player"))


rbs_masterfile <- bind_rows(rbs_2024, rbs_2023, rbs_2022, rbs_2021, rbs_2020, rbs_2019, rbs_2018, rbs_2017, rbs_2016)
write.csv(rbs_masterfile, "rbs_masterfile.csv")

### Offensive line ----
ol_stats <- read.csv("ol_data.csv")

ol_stats <- select(ol_stats, Player, Pos, Season, Age, GS, Snap_perc, AV)

ol_stats$Player <- ifelse(ol_stats$Player == "Connor McGovern" & ol_stats$Pos == "C", "Connor McGovern (DEN)", ol_stats$Player)
ol_stats$Player <- ifelse(ol_stats$Player == "Connor McGovern" & ol_stats$Pos != "C", "Connor McGovern (DAL)", ol_stats$Player)

ol_2014 <- filter(ol_stats, Season == "2014")
ol_2015 <- filter(ol_stats, Season == "2015")
ol_2016 <- filter(ol_stats, Season == "2016")
ol_2017 <- filter(ol_stats, Season == "2017")
ol_2018 <- filter(ol_stats, Season == "2018")
ol_2019 <- filter(ol_stats, Season == "2019")
ol_2020 <- filter(ol_stats, Season == "2020")
ol_2021 <- filter(ol_stats, Season == "2021")
ol_2022 <- filter(ol_stats, Season == "2022")
ol_2023 <- filter(ol_stats, Season == "2023")

colnames(ol_2014)[3:7] <- paste0(colnames(ol_2014)[3:7], "_2014")
colnames(ol_2015)[3:7] <- paste0(colnames(ol_2015)[3:7], "_2015")
colnames(ol_2016)[3:7] <- paste0(colnames(ol_2016)[3:7], "_2016")
colnames(ol_2017)[3:7] <- paste0(colnames(ol_2017)[3:7], "_2017")
colnames(ol_2018)[3:7] <- paste0(colnames(ol_2018)[3:7], "_2018")
colnames(ol_2019)[3:7] <- paste0(colnames(ol_2019)[3:7], "_2019")
colnames(ol_2020)[3:7] <- paste0(colnames(ol_2020)[3:7], "_2020")
colnames(ol_2021)[3:7] <- paste0(colnames(ol_2021)[3:7], "_2021")
colnames(ol_2022)[3:7] <- paste0(colnames(ol_2022)[3:7], "_2022")
colnames(ol_2023)[3:7] <- paste0(colnames(ol_2023)[3:7], "_2023")

filler_2016 <- merge(ol_2015, ol_2014, by = c("Player"), all = TRUE)
filler_2017 <- merge(ol_2016, ol_2015, by = c("Player"), all = TRUE)
filler_2018 <- merge(ol_2017, ol_2016, by = c("Player"), all = TRUE)
filler_2019 <- merge(ol_2018, ol_2017, by = c("Player"), all = TRUE)
filler_2020 <- merge(ol_2019, ol_2018, by = c("Player"), all = TRUE)
filler_2021 <- merge(ol_2020, ol_2019, by = c("Player"), all = TRUE)
filler_2022 <- merge(ol_2021, ol_2020, by = c("Player"), all = TRUE)
filler_2023 <- merge(ol_2022, ol_2021, by = c("Player"), all = TRUE)
filler_2024 <- merge(ol_2023, ol_2022, by = c("Player"), all = TRUE)

filler_2016[3:12] <- lapply(filler_2016[3:12], function(x) ifelse(is.na(x), 0, x))
filler_2017[3:12] <- lapply(filler_2017[3:12], function(x) ifelse(is.na(x), 0, x))
filler_2018[3:12] <- lapply(filler_2018[3:12], function(x) ifelse(is.na(x), 0, x))
filler_2019[3:12] <- lapply(filler_2019[3:12], function(x) ifelse(is.na(x), 0, x))
filler_2020[3:12] <- lapply(filler_2020[3:12], function(x) ifelse(is.na(x), 0, x))
filler_2021[3:12] <- lapply(filler_2021[3:12], function(x) ifelse(is.na(x), 0, x))
filler_2022[3:12] <- lapply(filler_2022[3:12], function(x) ifelse(is.na(x), 0, x))
filler_2023[3:12] <- lapply(filler_2023[3:12], function(x) ifelse(is.na(x), 0, x))
filler_2024[3:12] <- lapply(filler_2024[3:12], function(x) ifelse(is.na(x), 0, x))



ol_stats_2016 <- filler_2016
ol_stats_2016$Age <- ol_stats_2016$Age_2015
ol_stats_2016$Age <- ifelse(ol_stats_2016$Age_2015 == 0, ol_stats_2016$Age_2014 + 1, ol_stats_2016$Age)
ol_stats_2016$GS <- 0.66*ol_stats_2016$GS_2015 + 0.33*ol_stats_2016$GS_2014
ol_stats_2016$Snap_perc <- 0.66*ol_stats_2016$Snap_perc_2015 + 0.33*ol_stats_2016$Snap_perc_2014
ol_stats_2016$AV <- 0.66*ol_stats_2016$AV_2015 + 0.33*ol_stats_2016$AV_2014
ol_stats_2016 <- select(ol_stats_2016, Player, Age, GS, Snap_perc, AV)

ol_stats_2017 <- filler_2017
ol_stats_2017$Age <- ol_stats_2017$Age_2016
ol_stats_2017$Age <- ifelse(ol_stats_2017$Age_2016 == 0, ol_stats_2017$Age_2015 + 1, ol_stats_2017$Age)
ol_stats_2017$GS <- 0.66*ol_stats_2017$GS_2016 + 0.33*ol_stats_2017$GS_2015
ol_stats_2017$Snap_perc <- 0.66*ol_stats_2017$Snap_perc_2016 + 0.33*ol_stats_2017$Snap_perc_2015
ol_stats_2017$AV <- 0.66*ol_stats_2017$AV_2016 + 0.33*ol_stats_2017$AV_2015
ol_stats_2017 <- select(ol_stats_2017, Player, Age, GS, Snap_perc, AV)

ol_stats_2018 <- filler_2018
ol_stats_2018$Age <- ol_stats_2018$Age_2017
ol_stats_2018$Age <- ifelse(ol_stats_2018$Age_2017 == 0, ol_stats_2018$Age_2016 + 1, ol_stats_2018$Age)
ol_stats_2018$GS <- 0.66*ol_stats_2018$GS_2017 + 0.33*ol_stats_2018$GS_2016
ol_stats_2018$Snap_perc <- 0.66*ol_stats_2018$Snap_perc_2017 + 0.33*ol_stats_2018$Snap_perc_2016
ol_stats_2018$AV <- 0.66*ol_stats_2018$AV_2017 + 0.33*ol_stats_2018$AV_2016
ol_stats_2018 <- select(ol_stats_2018, Player, Age, GS, Snap_perc, AV)

ol_stats_2019 <- filler_2019
ol_stats_2019$Age <- ol_stats_2019$Age_2018
ol_stats_2019$Age <- ifelse(ol_stats_2019$Age_2018 == 0, ol_stats_2019$Age_2017 + 1, ol_stats_2019$Age)
ol_stats_2019$GS <- 0.66*ol_stats_2019$GS_2018 + 0.33*ol_stats_2019$GS_2017
ol_stats_2019$Snap_perc <- 0.66*ol_stats_2019$Snap_perc_2018 + 0.33*ol_stats_2019$Snap_perc_2017
ol_stats_2019$AV <- 0.66*ol_stats_2019$AV_2018 + 0.33*ol_stats_2019$AV_2017
ol_stats_2019 <- select(ol_stats_2019, Player, Age, GS, Snap_perc, AV)

ol_stats_2020 <- filler_2020
ol_stats_2020$Age <- ol_stats_2020$Age_2019
ol_stats_2020$Age <- ifelse(ol_stats_2020$Age_2019 == 0, ol_stats_2020$Age_2018 + 1, ol_stats_2020$Age)
ol_stats_2020$GS <- 0.66*ol_stats_2020$GS_2019 + 0.33*ol_stats_2020$GS_2018
ol_stats_2020$Snap_perc <- 0.66*ol_stats_2020$Snap_perc_2019 + 0.33*ol_stats_2020$Snap_perc_2018
ol_stats_2020$AV <- 0.66*ol_stats_2020$AV_2019 + 0.33*ol_stats_2020$AV_2018
ol_stats_2020 <- select(ol_stats_2020, Player, Age, GS, Snap_perc, AV)

ol_stats_2021 <- filler_2021
ol_stats_2021$Age <- ol_stats_2021$Age_2020
ol_stats_2021$Age <- ifelse(ol_stats_2021$Age_2020 == 0, ol_stats_2021$Age_2019 + 1, ol_stats_2021$Age)
ol_stats_2021$GS <- 0.66*ol_stats_2021$GS_2020 + 0.33*ol_stats_2021$GS_2019
ol_stats_2021$Snap_perc <- 0.66*ol_stats_2021$Snap_perc_2020 + 0.33*ol_stats_2021$Snap_perc_2019
ol_stats_2021$AV <- 0.66*ol_stats_2021$AV_2020 + 0.33*ol_stats_2021$AV_2019
ol_stats_2021 <- select(ol_stats_2021, Player, Age, GS, Snap_perc, AV)

ol_stats_2022 <- filler_2022
ol_stats_2022$Age <- ol_stats_2022$Age_2021
ol_stats_2022$Age <- ifelse(ol_stats_2022$Age_2021 == 0, ol_stats_2022$Age_2020 + 1, ol_stats_2022$Age)
ol_stats_2022$GS <- 0.66*ol_stats_2022$GS_2021 + 0.33*ol_stats_2022$GS_2020
ol_stats_2022$Snap_perc <- 0.66*ol_stats_2022$Snap_perc_2021 + 0.33*ol_stats_2022$Snap_perc_2020
ol_stats_2022$AV <- 0.66*ol_stats_2022$AV_2021 + 0.33*ol_stats_2022$AV_2020
ol_stats_2022 <- select(ol_stats_2022, Player, Age, GS, Snap_perc, AV)

ol_stats_2023 <- filler_2023
ol_stats_2023$Age <- ol_stats_2023$Age_2022
ol_stats_2023$Age <- ifelse(ol_stats_2023$Age_2022 == 0, ol_stats_2023$Age_2021 + 1, ol_stats_2023$Age)
ol_stats_2023$GS <- 0.66*ol_stats_2023$GS_2022 + 0.33*ol_stats_2023$GS_2021
ol_stats_2023$Snap_perc <- 0.66*ol_stats_2023$Snap_perc_2022 + 0.33*ol_stats_2023$Snap_perc_2021
ol_stats_2023$AV <- 0.66*ol_stats_2023$AV_2022 + 0.33*ol_stats_2023$AV_2021
ol_stats_2023 <- select(ol_stats_2023, Player, Age, GS, Snap_perc, AV)

ol_stats_2024 <- filler_2024
ol_stats_2024$Age <- ol_stats_2024$Age_2023
ol_stats_2024$Age <- ifelse(ol_stats_2024$Age_2023 == 0, ol_stats_2024$Age_2022 + 1, ol_stats_2024$Age)
ol_stats_2024$GS <- 0.66*ol_stats_2024$GS_2023 + 0.33*ol_stats_2024$GS_2022
ol_stats_2024$Snap_perc <- 0.66*ol_stats_2024$Snap_perc_2023 + 0.33*ol_stats_2024$Snap_perc_2022
ol_stats_2024$AV <- 0.66*ol_stats_2024$AV_2023 + 0.33*ol_stats_2024$AV_2022
ol_stats_2024 <- select(ol_stats_2024, Player, Age, GS, Snap_perc, AV)

all_og_contracts$Player <- ifelse(all_og_contracts$Player == "Jon Runyan, Jr.", "Jon Runyan Jr.", all_og_contracts$Player)
all_og_contracts$Player <- ifelse(all_og_contracts$Player == "Iosua Opeta", "Sua Opeta", all_og_contracts$Player)
all_og_contracts$Player <- ifelse(all_og_contracts$Player == "Shaquille Mason", "Shaq Mason", all_og_contracts$Player)
all_og_contracts$Player <- ifelse(all_og_contracts$Player == "K.C. McDermott", "KC McDermott", all_og_contracts$Player)
all_ot_contracts$Player <- ifelse(all_ot_contracts$Player == "Yosuah Nijman", "Yosh Nijman", all_ot_contracts$Player)
all_ot_contracts$Player <- ifelse(all_ot_contracts$Player == "Olisaemeka Udoh", "Oli Udoh", all_ot_contracts$Player)
all_ot_contracts$Player <- ifelse(all_ot_contracts$Player == "Julie'n Davenport", "Julién Davenport", all_ot_contracts$Player)
all_ot_contracts$Player <- ifelse(all_ot_contracts$Player == "Joseph Barksdale", "Joe Barksdale", all_ot_contracts$Player)



ot_contract_2016 <- filter(all_ot_contracts, Year.Signed == 2016)
ot_contract_2017 <- filter(all_ot_contracts, Year.Signed == 2017)
ot_contract_2018 <- filter(all_ot_contracts, Year.Signed == 2018)
ot_contract_2019 <- filter(all_ot_contracts, Year.Signed == 2019)
ot_contract_2020 <- filter(all_ot_contracts, Year.Signed == 2020)
ot_contract_2021 <- filter(all_ot_contracts, Year.Signed == 2021)
ot_contract_2022 <- filter(all_ot_contracts, Year.Signed == 2022)
ot_contract_2023 <- filter(all_ot_contracts, Year.Signed == 2023)
ot_contract_2024 <- filter(all_ot_contracts, Year.Signed == 2024)

og_contract_2016 <- filter(all_og_contracts, Year.Signed == 2016)
og_contract_2017 <- filter(all_og_contracts, Year.Signed == 2017)
og_contract_2018 <- filter(all_og_contracts, Year.Signed == 2018)
og_contract_2019 <- filter(all_og_contracts, Year.Signed == 2019)
og_contract_2020 <- filter(all_og_contracts, Year.Signed == 2020)
og_contract_2021 <- filter(all_og_contracts, Year.Signed == 2021)
og_contract_2022 <- filter(all_og_contracts, Year.Signed == 2022)
og_contract_2023 <- filter(all_og_contracts, Year.Signed == 2023)
og_contract_2024 <- filter(all_og_contracts, Year.Signed == 2024)

c_contract_2016 <- filter(all_c_contracts, Year.Signed == 2016)
c_contract_2017 <- filter(all_c_contracts, Year.Signed == 2017)
c_contract_2018 <- filter(all_c_contracts, Year.Signed == 2018)
c_contract_2019 <- filter(all_c_contracts, Year.Signed == 2019)
c_contract_2020 <- filter(all_c_contracts, Year.Signed == 2020)
c_contract_2021 <- filter(all_c_contracts, Year.Signed == 2021)
c_contract_2022 <- filter(all_c_contracts, Year.Signed == 2022)
c_contract_2023 <- filter(all_c_contracts, Year.Signed == 2023)
c_contract_2024 <- filter(all_c_contracts, Year.Signed == 2024)


ots_2024 <- left_join(ot_contract_2024, ol_stats_2024, by = c("Player" = "Player"))
ots_2023 <- left_join(ot_contract_2023, ol_stats_2023, by = c("Player" = "Player"))
ots_2022 <- left_join(ot_contract_2022, ol_stats_2022, by = c("Player" = "Player"))
ots_2021 <- left_join(ot_contract_2021, ol_stats_2021, by = c("Player" = "Player"))
ots_2020 <- left_join(ot_contract_2020, ol_stats_2020, by = c("Player" = "Player"))
ots_2019 <- left_join(ot_contract_2019, ol_stats_2019, by = c("Player" = "Player"))
ots_2018 <- left_join(ot_contract_2018, ol_stats_2018, by = c("Player" = "Player"))
ots_2017 <- left_join(ot_contract_2017, ol_stats_2017, by = c("Player" = "Player"))
ots_2016 <- left_join(ot_contract_2016, ol_stats_2016, by = c("Player" = "Player"))

ogs_2024 <- left_join(og_contract_2024, ol_stats_2024, by = c("Player" = "Player"))
ogs_2023 <- left_join(og_contract_2023, ol_stats_2023, by = c("Player" = "Player"))
ogs_2022 <- left_join(og_contract_2022, ol_stats_2022, by = c("Player" = "Player"))
ogs_2021 <- left_join(og_contract_2021, ol_stats_2021, by = c("Player" = "Player"))
ogs_2020 <- left_join(og_contract_2020, ol_stats_2020, by = c("Player" = "Player"))
ogs_2019 <- left_join(og_contract_2019, ol_stats_2019, by = c("Player" = "Player"))
ogs_2018 <- left_join(og_contract_2018, ol_stats_2018, by = c("Player" = "Player"))
ogs_2017 <- left_join(og_contract_2017, ol_stats_2017, by = c("Player" = "Player"))
ogs_2016 <- left_join(og_contract_2016, ol_stats_2016, by = c("Player" = "Player"))

cs_2024 <- left_join(c_contract_2024, ol_stats_2024, by = c("Player" = "Player"))
cs_2023 <- left_join(c_contract_2023, ol_stats_2023, by = c("Player" = "Player"))
cs_2022 <- left_join(c_contract_2022, ol_stats_2022, by = c("Player" = "Player"))
cs_2021 <- left_join(c_contract_2021, ol_stats_2021, by = c("Player" = "Player"))
cs_2020 <- left_join(c_contract_2020, ol_stats_2020, by = c("Player" = "Player"))
cs_2019 <- left_join(c_contract_2019, ol_stats_2019, by = c("Player" = "Player"))
cs_2018 <- left_join(c_contract_2018, ol_stats_2018, by = c("Player" = "Player"))
cs_2017 <- left_join(c_contract_2017, ol_stats_2017, by = c("Player" = "Player"))
cs_2016 <- left_join(c_contract_2016, ol_stats_2016, by = c("Player" = "Player"))

ots_masterfile <- bind_rows(ots_2024, ots_2023, ots_2022, ots_2021, ots_2020, ots_2019, ots_2018, ots_2017, ots_2016)
ogs_masterfile <- bind_rows(ogs_2024, ogs_2023, ogs_2022, ogs_2021, ogs_2020, ogs_2019, ogs_2018, ogs_2017, ogs_2016)
cs_masterfile <- bind_rows(cs_2024, cs_2023, cs_2022, cs_2021, cs_2020, cs_2019, cs_2018, cs_2017, cs_2016)

ots_masterfile$Pos.y <- "OT"
ogs_masterfile$Pos.y <- "OG"
cs_masterfile$Pos.y <- "C"

write.csv(ots_masterfile, "ots_masterfile.csv")
write.csv(ogs_masterfile, "ogs_masterfile.csv")
write.csv(cs_masterfile, "cs_masterfile.csv")

### Edge rushers and IDL ----
edge_stats <- read.csv("def_data.csv")
edge_stats <- select(edge_stats, Player, Pos, Season, Age, GS, Snap_perc = Def., AV, Stat_1 = Sk, Stat_2 = Solo)
edge_stats$Pos <- "EDGE"
edge_2014 <- filter(edge_stats, Season == "2014")
edge_2015 <- filter(edge_stats, Season == "2015")
edge_2016 <- filter(edge_stats, Season == "2016")
edge_2017 <- filter(edge_stats, Season == "2017")
edge_2018 <- filter(edge_stats, Season == "2018")
edge_2019 <- filter(edge_stats, Season == "2019")
edge_2020 <- filter(edge_stats, Season == "2020")
edge_2021 <- filter(edge_stats, Season == "2021")
edge_2022 <- filter(edge_stats, Season == "2022")
edge_2023 <- filter(edge_stats, Season == "2023")

colnames(edge_2014)[3:9] <- paste0(colnames(edge_2014)[3:9], "_2014")
colnames(edge_2015)[3:9] <- paste0(colnames(edge_2015)[3:9], "_2015")
colnames(edge_2016)[3:9] <- paste0(colnames(edge_2016)[3:9], "_2016")
colnames(edge_2017)[3:9] <- paste0(colnames(edge_2017)[3:9], "_2017")
colnames(edge_2018)[3:9] <- paste0(colnames(edge_2018)[3:9], "_2018")
colnames(edge_2019)[3:9] <- paste0(colnames(edge_2019)[3:9], "_2019")
colnames(edge_2020)[3:9] <- paste0(colnames(edge_2020)[3:9], "_2020")
colnames(edge_2021)[3:9] <- paste0(colnames(edge_2021)[3:9], "_2021")
colnames(edge_2022)[3:9] <- paste0(colnames(edge_2022)[3:9], "_2022")
colnames(edge_2023)[3:9] <- paste0(colnames(edge_2023)[3:9], "_2023")

filler_2016 <- merge(edge_2015, edge_2014, by = c("Player"), all = TRUE)
filler_2017 <- merge(edge_2016, edge_2015, by = c("Player"), all = TRUE)
filler_2018 <- merge(edge_2017, edge_2016, by = c("Player"), all = TRUE)
filler_2019 <- merge(edge_2018, edge_2017, by = c("Player"), all = TRUE)
filler_2020 <- merge(edge_2019, edge_2018, by = c("Player"), all = TRUE)
filler_2021 <- merge(edge_2020, edge_2019, by = c("Player"), all = TRUE)
filler_2022 <- merge(edge_2021, edge_2020, by = c("Player"), all = TRUE)
filler_2023 <- merge(edge_2022, edge_2021, by = c("Player"), all = TRUE)
filler_2024 <- merge(edge_2023, edge_2022, by = c("Player"), all = TRUE)

filler_2016[3:16] <- lapply(filler_2016[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2017[3:16] <- lapply(filler_2017[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2018[3:16] <- lapply(filler_2018[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2019[3:16] <- lapply(filler_2019[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2020[3:16] <- lapply(filler_2020[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2021[3:16] <- lapply(filler_2021[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2022[3:16] <- lapply(filler_2022[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2023[3:16] <- lapply(filler_2023[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2024[3:16] <- lapply(filler_2024[3:16], function(x) ifelse(is.na(x), 0, x))


edge_stats_2016 <- filler_2016
edge_stats_2016$Age <- edge_stats_2016$Age_2015
edge_stats_2016$Age <- ifelse(edge_stats_2016$Age_2015 == 0, edge_stats_2016$Age_2014 + 1, edge_stats_2016$Age)
edge_stats_2016$GS <- 0.66*edge_stats_2016$GS_2015 + 0.33*edge_stats_2016$GS_2014
edge_stats_2016$Snap_perc <- 0.66*edge_stats_2016$Snap_perc_2015 + 0.33*edge_stats_2016$Snap_perc_2014
edge_stats_2016$AV <- 0.66*edge_stats_2016$AV_2015 + 0.33*edge_stats_2016$AV_2014
edge_stats_2016$Stat_1 <- 0.66*edge_stats_2016$Stat_1_2015 + 0.33*edge_stats_2016$Stat_1_2014
edge_stats_2016$Stat_2 <- 0.66*edge_stats_2016$Stat_2_2015 + 0.33*edge_stats_2016$Stat_2_2014
edge_stats_2016 <- select(edge_stats_2016, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

edge_stats_2017 <- filler_2017
edge_stats_2017$Age <- edge_stats_2017$Age_2016
edge_stats_2017$Age <- ifelse(edge_stats_2017$Age_2016 == 0, edge_stats_2017$Age_2015 + 1, edge_stats_2017$Age)
edge_stats_2017$GS <- 0.66*edge_stats_2017$GS_2016 + 0.33*edge_stats_2017$GS_2015
edge_stats_2017$Snap_perc <- 0.66*edge_stats_2017$Snap_perc_2016 + 0.33*edge_stats_2017$Snap_perc_2015
edge_stats_2017$AV <- 0.66*edge_stats_2017$AV_2016 + 0.33*edge_stats_2017$AV_2015
edge_stats_2017$Stat_1 <- 0.66*edge_stats_2017$Stat_1_2016 + 0.33*edge_stats_2017$Stat_1_2015
edge_stats_2017$Stat_2 <- 0.66*edge_stats_2017$Stat_2_2016 + 0.33*edge_stats_2017$Stat_2_2015
edge_stats_2017 <- select(edge_stats_2017, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

edge_stats_2018 <- filler_2018
edge_stats_2018$Age <- edge_stats_2018$Age_2017
edge_stats_2018$Age <- ifelse(edge_stats_2018$Age_2017 == 0, edge_stats_2018$Age_2016 + 1, edge_stats_2018$Age)
edge_stats_2018$GS <- 0.66*edge_stats_2018$GS_2017 + 0.33*edge_stats_2018$GS_2016
edge_stats_2018$Snap_perc <- 0.66*edge_stats_2018$Snap_perc_2017 + 0.33*edge_stats_2018$Snap_perc_2016
edge_stats_2018$AV <- 0.66*edge_stats_2018$AV_2017 + 0.33*edge_stats_2018$AV_2016
edge_stats_2018$Stat_1 <- 0.66*edge_stats_2018$Stat_1_2017 + 0.33*edge_stats_2018$Stat_1_2016
edge_stats_2018$Stat_2 <- 0.66*edge_stats_2018$Stat_2_2017 + 0.33*edge_stats_2018$Stat_2_2016
edge_stats_2018 <- select(edge_stats_2018, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

edge_stats_2019 <- filler_2019
edge_stats_2019$Age <- edge_stats_2019$Age_2018
edge_stats_2019$Age <- ifelse(edge_stats_2019$Age_2018 == 0, edge_stats_2019$Age_2017 + 1, edge_stats_2019$Age)
edge_stats_2019$GS <- 0.66*edge_stats_2019$GS_2018 + 0.33*edge_stats_2019$GS_2017
edge_stats_2019$Snap_perc <- 0.66*edge_stats_2019$Snap_perc_2018 + 0.33*edge_stats_2019$Snap_perc_2017
edge_stats_2019$AV <- 0.66*edge_stats_2019$AV_2018 + 0.33*edge_stats_2019$AV_2017
edge_stats_2019$Stat_1 <- 0.66*edge_stats_2019$Stat_1_2018 + 0.33*edge_stats_2019$Stat_1_2017
edge_stats_2019$Stat_2 <- 0.66*edge_stats_2019$Stat_2_2018 + 0.33*edge_stats_2019$Stat_2_2017
edge_stats_2019 <- select(edge_stats_2019, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

edge_stats_2020 <- filler_2020
edge_stats_2020$Age <- edge_stats_2020$Age_2019
edge_stats_2020$Age <- ifelse(edge_stats_2020$Age_2019 == 0, edge_stats_2020$Age_2018 + 1, edge_stats_2020$Age)
edge_stats_2020$GS <- 0.66*edge_stats_2020$GS_2019 + 0.33*edge_stats_2020$GS_2018
edge_stats_2020$Snap_perc <- 0.66*edge_stats_2020$Snap_perc_2019 + 0.33*edge_stats_2020$Snap_perc_2018
edge_stats_2020$AV <- 0.66*edge_stats_2020$AV_2019 + 0.33*edge_stats_2020$AV_2018
edge_stats_2020$Stat_1 <- 0.66*edge_stats_2020$Stat_1_2019 + 0.33*edge_stats_2020$Stat_1_2018
edge_stats_2020$Stat_2 <- 0.66*edge_stats_2020$Stat_2_2019 + 0.33*edge_stats_2020$Stat_2_2018
edge_stats_2020 <- select(edge_stats_2020, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

edge_stats_2021 <- filler_2021
edge_stats_2021$Age <- edge_stats_2021$Age_2020
edge_stats_2021$Age <- ifelse(edge_stats_2021$Age_2020 == 0, edge_stats_2021$Age_2019 + 1, edge_stats_2021$Age)
edge_stats_2021$GS <- 0.66*edge_stats_2021$GS_2020 + 0.33*edge_stats_2021$GS_2019
edge_stats_2021$Snap_perc <- 0.66*edge_stats_2021$Snap_perc_2020 + 0.33*edge_stats_2021$Snap_perc_2019
edge_stats_2021$AV <- 0.66*edge_stats_2021$AV_2020 + 0.33*edge_stats_2021$AV_2019
edge_stats_2021$Stat_1 <- 0.66*edge_stats_2021$Stat_1_2020 + 0.33*edge_stats_2021$Stat_1_2019
edge_stats_2021$Stat_2 <- 0.66*edge_stats_2021$Stat_2_2020 + 0.33*edge_stats_2021$Stat_2_2019
edge_stats_2021 <- select(edge_stats_2021, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

edge_stats_2022 <- filler_2022
edge_stats_2022$Age <- edge_stats_2022$Age_2021
edge_stats_2022$Age <- ifelse(edge_stats_2022$Age_2021 == 0, edge_stats_2022$Age_2020 + 1, edge_stats_2022$Age)
edge_stats_2022$GS <- 0.66*edge_stats_2022$GS_2021 + 0.33*edge_stats_2022$GS_2020
edge_stats_2022$Snap_perc <- 0.66*edge_stats_2022$Snap_perc_2021 + 0.33*edge_stats_2022$Snap_perc_2020
edge_stats_2022$AV <- 0.66*edge_stats_2022$AV_2021 + 0.33*edge_stats_2022$AV_2020
edge_stats_2022$Stat_1 <- 0.66*edge_stats_2022$Stat_1_2021 + 0.33*edge_stats_2022$Stat_1_2020
edge_stats_2022$Stat_2 <- 0.66*edge_stats_2022$Stat_2_2021 + 0.33*edge_stats_2022$Stat_2_2020
edge_stats_2022 <- select(edge_stats_2022, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

edge_stats_2023 <- filler_2023
edge_stats_2023$Age <- edge_stats_2023$Age_2022
edge_stats_2023$Age <- ifelse(edge_stats_2023$Age_2022 == 0, edge_stats_2023$Age_2021 + 1, edge_stats_2023$Age)
edge_stats_2023$GS <- 0.66*edge_stats_2023$GS_2022 + 0.33*edge_stats_2023$GS_2021
edge_stats_2023$Snap_perc <- 0.66*edge_stats_2023$Snap_perc_2022 + 0.33*edge_stats_2023$Snap_perc_2021
edge_stats_2023$AV <- 0.66*edge_stats_2023$AV_2022 + 0.33*edge_stats_2023$AV_2021
edge_stats_2023$Stat_1 <- 0.66*edge_stats_2023$Stat_1_2022 + 0.33*edge_stats_2023$Stat_1_2021
edge_stats_2023$Stat_2 <- 0.66*edge_stats_2023$Stat_2_2022 + 0.33*edge_stats_2023$Stat_2_2021
edge_stats_2023 <- select(edge_stats_2023, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

edge_stats_2024 <- filler_2024
edge_stats_2024$Age <- edge_stats_2024$Age_2023
edge_stats_2024$Age <- ifelse(edge_stats_2024$Age_2023 == 0, edge_stats_2024$Age_2022 + 1, edge_stats_2024$Age)
edge_stats_2024$GS <- 0.66*edge_stats_2024$GS_2023 + 0.33*edge_stats_2024$GS_2022
edge_stats_2024$Snap_perc <- 0.66*edge_stats_2024$Snap_perc_2023 + 0.33*edge_stats_2024$Snap_perc_2022
edge_stats_2024$AV <- 0.66*edge_stats_2024$AV_2023 + 0.33*edge_stats_2024$AV_2022
edge_stats_2024$Stat_1 <- 0.66*edge_stats_2024$Stat_1_2023 + 0.33*edge_stats_2024$Stat_1_2022
edge_stats_2024$Stat_2 <- 0.66*edge_stats_2024$Stat_2_2023 + 0.33*edge_stats_2024$Stat_2_2022
edge_stats_2024 <- select(edge_stats_2024, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)


all_edge_contracts$Player <- ifelse(all_edge_contracts$Player == "Demarcus Lawrence", "DeMarcus Lawrence", all_edge_contracts$Player)
all_edge_contracts$Player <- ifelse(all_edge_contracts$Player == "Dante Fowler Jr.", "Dante Fowler", all_edge_contracts$Player)
all_edge_contracts$Player <- ifelse(all_edge_contracts$Player == "Deatrich Wise", "Deatrich Wise Jr.", all_edge_contracts$Player)
all_idl_contracts$Player <- ifelse(all_idl_contracts$Player == "Broderick Washington", "Broderick Washington Jr.", all_idl_contracts$Player)
all_idl_contracts$Player <- ifelse(all_idl_contracts$Player == "Matt Ioannidas", "Matthew Ioannidas", all_idl_contracts$Player)
all_idl_contracts$Player <- ifelse(all_idl_contracts$Player == "Nicholas Williams", "Nick Williams", all_idl_contracts$Player)
all_idl_contracts$Player <- ifelse(all_idl_contracts$Player == "Mario Edwards Jr.", "Mario Edwards", all_idl_contracts$Player)
all_idl_contracts$Player <- ifelse(all_idl_contracts$Player == "Ziggy Hood", "Evander Hood", all_idl_contracts$Player)
all_idl_contracts$Player <- ifelse(all_idl_contracts$Player == "Maurice Hurst", "Maurice Hurst Jr.", all_idl_contracts$Player)

edge_contract_2016 <- filter(all_edge_contracts, Year.Signed == 2016)
edge_contract_2017 <- filter(all_edge_contracts, Year.Signed == 2017)
edge_contract_2018 <- filter(all_edge_contracts, Year.Signed == 2018)
edge_contract_2019 <- filter(all_edge_contracts, Year.Signed == 2019)
edge_contract_2020 <- filter(all_edge_contracts, Year.Signed == 2020)
edge_contract_2021 <- filter(all_edge_contracts, Year.Signed == 2021)
edge_contract_2022 <- filter(all_edge_contracts, Year.Signed == 2022)
edge_contract_2023 <- filter(all_edge_contracts, Year.Signed == 2023)
edge_contract_2024 <- filter(all_edge_contracts, Year.Signed == 2024)


edges_2024 <- left_join(edge_contract_2024, edge_stats_2024, by = c("Player" = "Player"))
edges_2023 <- left_join(edge_contract_2023, edge_stats_2023, by = c("Player" = "Player"))
edges_2022 <- left_join(edge_contract_2022, edge_stats_2022, by = c("Player" = "Player"))
edges_2021 <- left_join(edge_contract_2021, edge_stats_2021, by = c("Player" = "Player"))
edges_2020 <- left_join(edge_contract_2020, edge_stats_2020, by = c("Player" = "Player"))
edges_2019 <- left_join(edge_contract_2019, edge_stats_2019, by = c("Player" = "Player"))
edges_2018 <- left_join(edge_contract_2018, edge_stats_2018, by = c("Player" = "Player"))
edges_2017 <- left_join(edge_contract_2017, edge_stats_2017, by = c("Player" = "Player"))
edges_2016 <- left_join(edge_contract_2016, edge_stats_2016, by = c("Player" = "Player"))


edges_masterfile <- bind_rows(edges_2024, edges_2023, edges_2022, edges_2021, edges_2020, edges_2019, edges_2018, edges_2017, edges_2016)

edges_masterfile$Pos.y <- "EDGE"
write.csv(edges_masterfile, "edges_masterfile.csv")

#Interior dlinemen
idl_contract_2016 <- filter(all_idl_contracts, Year.Signed == 2016)
idl_contract_2017 <- filter(all_idl_contracts, Year.Signed == 2017)
idl_contract_2018 <- filter(all_idl_contracts, Year.Signed == 2018)
idl_contract_2019 <- filter(all_idl_contracts, Year.Signed == 2019)
idl_contract_2020 <- filter(all_idl_contracts, Year.Signed == 2020)
idl_contract_2021 <- filter(all_idl_contracts, Year.Signed == 2021)
idl_contract_2022 <- filter(all_idl_contracts, Year.Signed == 2022)
idl_contract_2023 <- filter(all_idl_contracts, Year.Signed == 2023)
idl_contract_2024 <- filter(all_idl_contracts, Year.Signed == 2024)


idls_2024 <- left_join(idl_contract_2024, edge_stats_2024, by = c("Player" = "Player"))
idls_2023 <- left_join(idl_contract_2023, edge_stats_2023, by = c("Player" = "Player"))
idls_2022 <- left_join(idl_contract_2022, edge_stats_2022, by = c("Player" = "Player"))
idls_2021 <- left_join(idl_contract_2021, edge_stats_2021, by = c("Player" = "Player"))
idls_2020 <- left_join(idl_contract_2020, edge_stats_2020, by = c("Player" = "Player"))
idls_2019 <- left_join(idl_contract_2019, edge_stats_2019, by = c("Player" = "Player"))
idls_2018 <- left_join(idl_contract_2018, edge_stats_2018, by = c("Player" = "Player"))
idls_2017 <- left_join(idl_contract_2017, edge_stats_2017, by = c("Player" = "Player"))
idls_2016 <- left_join(idl_contract_2016, edge_stats_2016, by = c("Player" = "Player"))


idls_masterfile <- bind_rows(idls_2024, idls_2023, idls_2022, idls_2021, idls_2020, idls_2019, idls_2018, idls_2017, idls_2016)
idls_masterfile$Pos.y <- "IDL"
write.csv(idls_masterfile, "idls_masterfile.csv")
### Linebackers ----
lb_stats <- read.csv("def_data.csv")
lb_stats$Player <- ifelse(lb_stats$Player == "David Long" & lb_stats$Team == "TEN" | lb_stats$Player == "David Long" & lb_stats$Team == "MIA", "David Long (Ten)", lb_stats$Player)

lb_stats <- select(lb_stats, Player, Pos, Season, Age, GS, Snap_perc = Def., AV, Stat_1 = PD, Stat_2 = Solo)
lb_stats$Pos <- "LB"

lb_2014 <- filter(lb_stats, Season == "2014")
lb_2015 <- filter(lb_stats, Season == "2015")
lb_2016 <- filter(lb_stats, Season == "2016")
lb_2017 <- filter(lb_stats, Season == "2017")
lb_2018 <- filter(lb_stats, Season == "2018")
lb_2019 <- filter(lb_stats, Season == "2019")
lb_2020 <- filter(lb_stats, Season == "2020")
lb_2021 <- filter(lb_stats, Season == "2021")
lb_2022 <- filter(lb_stats, Season == "2022")
lb_2023 <- filter(lb_stats, Season == "2023")

colnames(lb_2014)[3:9] <- paste0(colnames(lb_2014)[3:9], "_2014")
colnames(lb_2015)[3:9] <- paste0(colnames(lb_2015)[3:9], "_2015")
colnames(lb_2016)[3:9] <- paste0(colnames(lb_2016)[3:9], "_2016")
colnames(lb_2017)[3:9] <- paste0(colnames(lb_2017)[3:9], "_2017")
colnames(lb_2018)[3:9] <- paste0(colnames(lb_2018)[3:9], "_2018")
colnames(lb_2019)[3:9] <- paste0(colnames(lb_2019)[3:9], "_2019")
colnames(lb_2020)[3:9] <- paste0(colnames(lb_2020)[3:9], "_2020")
colnames(lb_2021)[3:9] <- paste0(colnames(lb_2021)[3:9], "_2021")
colnames(lb_2022)[3:9] <- paste0(colnames(lb_2022)[3:9], "_2022")
colnames(lb_2023)[3:9] <- paste0(colnames(lb_2023)[3:9], "_2023")

filler_2016 <- merge(lb_2015, lb_2014, by = c("Player"), all = TRUE)


filler_2017 <- merge(lb_2016, lb_2015, by = c("Player"), all = TRUE)
filler_2018 <- merge(lb_2017, lb_2016, by = c("Player"), all = TRUE)
filler_2019 <- merge(lb_2018, lb_2017, by = c("Player"), all = TRUE)
filler_2020 <- merge(lb_2019, lb_2018, by = c("Player"), all = TRUE)
filler_2021 <- merge(lb_2020, lb_2019, by = c("Player"), all = TRUE)
filler_2022 <- merge(lb_2021, lb_2020, by = c("Player"), all = TRUE)

filler_2023 <- merge(lb_2022, lb_2021, by = c("Player"), all = TRUE)
filler_2024 <- merge(lb_2023, lb_2022, by = c("Player"), all = TRUE)

filler_2016[3:16] <- lapply(filler_2016[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2017[3:16] <- lapply(filler_2017[3:16], function(x) ifelse(is.na(x), 0, x))





filler_2018[3:16] <- lapply(filler_2018[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2019[3:16] <- lapply(filler_2019[3:16], function(x) ifelse(is.na(x), 0, x))





filler_2020[3:16] <- lapply(filler_2020[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2021[3:16] <- lapply(filler_2021[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2022[3:16] <- lapply(filler_2022[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2023[3:16] <- lapply(filler_2023[3:16], function(x) ifelse(is.na(x), 0, x))
filler_2024[3:16] <- lapply(filler_2024[3:16], function(x) ifelse(is.na(x), 0, x))


lb_stats_2016 <- filler_2016
lb_stats_2016$Age <- lb_stats_2016$Age_2015
lb_stats_2016$Age <- ifelse(lb_stats_2016$Age_2015 == 0, lb_stats_2016$Age_2014 + 1, lb_stats_2016$Age)
lb_stats_2016$GS <- 0.66*lb_stats_2016$GS_2015 + 0.33*lb_stats_2016$GS_2014
lb_stats_2016$Snap_perc <- 0.66*lb_stats_2016$Snap_perc_2015 + 0.33*lb_stats_2016$Snap_perc_2014
lb_stats_2016$AV <- 0.66*lb_stats_2016$AV_2015 + 0.33*lb_stats_2016$AV_2014
lb_stats_2016$Stat_1 <- 0.66*lb_stats_2016$Stat_1_2015 + 0.33*lb_stats_2016$Stat_1_2014
lb_stats_2016$Stat_2 <- 0.66*lb_stats_2016$Stat_2_2015 + 0.33*lb_stats_2016$Stat_2_2014
lb_stats_2016 <- select(lb_stats_2016, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

lb_stats_2017 <- filler_2017
lb_stats_2017$Age <- lb_stats_2017$Age_2016
lb_stats_2017$Age <- ifelse(lb_stats_2017$Age_2016 == 0, lb_stats_2017$Age_2015 + 1, lb_stats_2017$Age)
lb_stats_2017$GS <- 0.66*lb_stats_2017$GS_2016 + 0.33*lb_stats_2017$GS_2015
lb_stats_2017$Snap_perc <- 0.66*lb_stats_2017$Snap_perc_2016 + 0.33*lb_stats_2017$Snap_perc_2015
lb_stats_2017$AV <- 0.66*lb_stats_2017$AV_2016 + 0.33*lb_stats_2017$AV_2015
lb_stats_2017$Stat_1 <- 0.66*lb_stats_2017$Stat_1_2016 + 0.33*lb_stats_2017$Stat_1_2015
lb_stats_2017$Stat_2 <- 0.66*lb_stats_2017$Stat_2_2016 + 0.33*lb_stats_2017$Stat_2_2015
lb_stats_2017 <- select(lb_stats_2017, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

lb_stats_2018 <- filler_2018
lb_stats_2018$Age <- lb_stats_2018$Age_2017
lb_stats_2018$Age <- ifelse(lb_stats_2018$Age_2017 == 0, lb_stats_2018$Age_2016 + 1, lb_stats_2018$Age)
lb_stats_2018$GS <- 0.66*lb_stats_2018$GS_2017 + 0.33*lb_stats_2018$GS_2016
lb_stats_2018$Snap_perc <- 0.66*lb_stats_2018$Snap_perc_2017 + 0.33*lb_stats_2018$Snap_perc_2016
lb_stats_2018$AV <- 0.66*lb_stats_2018$AV_2017 + 0.33*lb_stats_2018$AV_2016
lb_stats_2018$Stat_1 <- 0.66*lb_stats_2018$Stat_1_2017 + 0.33*lb_stats_2018$Stat_1_2016
lb_stats_2018$Stat_2 <- 0.66*lb_stats_2018$Stat_2_2017 + 0.33*lb_stats_2018$Stat_2_2016
lb_stats_2018 <- select(lb_stats_2018, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

lb_stats_2019 <- filler_2019
lb_stats_2019$Age <- lb_stats_2019$Age_2018
lb_stats_2019$Age <- ifelse(lb_stats_2019$Age_2018 == 0, lb_stats_2019$Age_2017 + 1, lb_stats_2019$Age)
lb_stats_2019$GS <- 0.66*lb_stats_2019$GS_2018 + 0.33*lb_stats_2019$GS_2017
lb_stats_2019$Snap_perc <- 0.66*lb_stats_2019$Snap_perc_2018 + 0.33*lb_stats_2019$Snap_perc_2017
lb_stats_2019$AV <- 0.66*lb_stats_2019$AV_2018 + 0.33*lb_stats_2019$AV_2017
lb_stats_2019$Stat_1 <- 0.66*lb_stats_2019$Stat_1_2018 + 0.33*lb_stats_2019$Stat_1_2017
lb_stats_2019$Stat_2 <- 0.66*lb_stats_2019$Stat_2_2018 + 0.33*lb_stats_2019$Stat_2_2017
lb_stats_2019 <- select(lb_stats_2019, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

lb_stats_2020 <- filler_2020
lb_stats_2020$Age <- lb_stats_2020$Age_2019
lb_stats_2020$Age <- ifelse(lb_stats_2020$Age_2019 == 0, lb_stats_2020$Age_2018 + 1, lb_stats_2020$Age)
lb_stats_2020$GS <- 0.66*lb_stats_2020$GS_2019 + 0.33*lb_stats_2020$GS_2018
lb_stats_2020$Snap_perc <- 0.66*lb_stats_2020$Snap_perc_2019 + 0.33*lb_stats_2020$Snap_perc_2018
lb_stats_2020$AV <- 0.66*lb_stats_2020$AV_2019 + 0.33*lb_stats_2020$AV_2018
lb_stats_2020$Stat_1 <- 0.66*lb_stats_2020$Stat_1_2019 + 0.33*lb_stats_2020$Stat_1_2018
lb_stats_2020$Stat_2 <- 0.66*lb_stats_2020$Stat_2_2019 + 0.33*lb_stats_2020$Stat_2_2018
lb_stats_2020 <- select(lb_stats_2020, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

lb_stats_2021 <- filler_2021
lb_stats_2021$Age <- lb_stats_2021$Age_2020
lb_stats_2021$Age <- ifelse(lb_stats_2021$Age_2020 == 0, lb_stats_2021$Age_2019 + 1, lb_stats_2021$Age)
lb_stats_2021$GS <- 0.66*lb_stats_2021$GS_2020 + 0.33*lb_stats_2021$GS_2019
lb_stats_2021$Snap_perc <- 0.66*lb_stats_2021$Snap_perc_2020 + 0.33*lb_stats_2021$Snap_perc_2019
lb_stats_2021$AV <- 0.66*lb_stats_2021$AV_2020 + 0.33*lb_stats_2021$AV_2019
lb_stats_2021$Stat_1 <- 0.66*lb_stats_2021$Stat_1_2020 + 0.33*lb_stats_2021$Stat_1_2019
lb_stats_2021$Stat_2 <- 0.66*lb_stats_2021$Stat_2_2020 + 0.33*lb_stats_2021$Stat_2_2019
lb_stats_2021 <- select(lb_stats_2021, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

lb_stats_2022 <- filler_2022
lb_stats_2022$Age <- lb_stats_2022$Age_2021
lb_stats_2022$Age <- ifelse(lb_stats_2022$Age_2021 == 0, lb_stats_2022$Age_2020 + 1, lb_stats_2022$Age)
lb_stats_2022$GS <- 0.66*lb_stats_2022$GS_2021 + 0.33*lb_stats_2022$GS_2020
lb_stats_2022$Snap_perc <- 0.66*lb_stats_2022$Snap_perc_2021 + 0.33*lb_stats_2022$Snap_perc_2020
lb_stats_2022$AV <- 0.66*lb_stats_2022$AV_2021 + 0.33*lb_stats_2022$AV_2020
lb_stats_2022$Stat_1 <- 0.66*lb_stats_2022$Stat_1_2021 + 0.33*lb_stats_2022$Stat_1_2020
lb_stats_2022$Stat_2 <- 0.66*lb_stats_2022$Stat_2_2021 + 0.33*lb_stats_2022$Stat_2_2020
lb_stats_2022 <- select(lb_stats_2022, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

lb_stats_2023 <- filler_2023
lb_stats_2023$Age <- lb_stats_2023$Age_2022
lb_stats_2023$Age <- ifelse(lb_stats_2023$Age_2022 == 0, lb_stats_2023$Age_2021 + 1, lb_stats_2023$Age)
lb_stats_2023$GS <- 0.66*lb_stats_2023$GS_2022 + 0.33*lb_stats_2023$GS_2021
lb_stats_2023$Snap_perc <- 0.66*lb_stats_2023$Snap_perc_2022 + 0.33*lb_stats_2023$Snap_perc_2021
lb_stats_2023$AV <- 0.66*lb_stats_2023$AV_2022 + 0.33*lb_stats_2023$AV_2021
lb_stats_2023$Stat_1 <- 0.66*lb_stats_2023$Stat_1_2022 + 0.33*lb_stats_2023$Stat_1_2021
lb_stats_2023$Stat_2 <- 0.66*lb_stats_2023$Stat_2_2022 + 0.33*lb_stats_2023$Stat_2_2021
lb_stats_2023 <- select(lb_stats_2023, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

lb_stats_2024 <- filler_2024
lb_stats_2024$Age <- lb_stats_2024$Age_2023
lb_stats_2024$Age <- ifelse(lb_stats_2024$Age_2023 == 0, lb_stats_2024$Age_2022 + 1, lb_stats_2024$Age)
lb_stats_2024$GS <- 0.66*lb_stats_2024$GS_2023 + 0.33*lb_stats_2024$GS_2022
lb_stats_2024$Snap_perc <- 0.66*lb_stats_2024$Snap_perc_2023 + 0.33*lb_stats_2024$Snap_perc_2022
lb_stats_2024$AV <- 0.66*lb_stats_2024$AV_2023 + 0.33*lb_stats_2024$AV_2022
lb_stats_2024$Stat_1 <- 0.66*lb_stats_2024$Stat_1_2023 + 0.33*lb_stats_2024$Stat_1_2022
lb_stats_2024$Stat_2 <- 0.66*lb_stats_2024$Stat_2_2023 + 0.33*lb_stats_2024$Stat_2_2022
lb_stats_2024 <- select(lb_stats_2024, Player, Age, GS, Snap_perc, AV, Stat_1, Stat_2)

all_lb_contracts$Player <- ifelse(all_lb_contracts$Player == "David Long Jr.", "David Long", all_lb_contracts$Player)
all_lb_contracts$Player <- ifelse(all_lb_contracts$Player == "Anthony Walker Jr.", "Anthony Walker", all_lb_contracts$Player)
all_lb_contracts$Player <- ifelse(all_lb_contracts$Player == "Jermaine Carter, Jr.", "Jermaine Carter Jr.", all_lb_contracts$Player)
all_cb_contracts$Player <- ifelse(all_cb_contracts$Player == "A.J. Terrell", "AJ Terrell", all_cb_contracts$Player)
all_cb_contracts$Player <- ifelse(all_cb_contracts$Player == "Janoris Jenkins", "Jackrabbit Jenkins", all_cb_contracts$Player)
all_cb_contracts$Player <- ifelse(all_cb_contracts$Player == "Michael Jackson Sr.", "Michael Jackson", all_cb_contracts$Player)
all_cb_contracts$Player <- ifelse(all_cb_contracts$Player == "Ugochukwu Amadi", "Ugo Amadi", all_cb_contracts$Player)
all_cb_contracts$Player <- ifelse(all_cb_contracts$Player == "Cre'Von LeBlanc", "Cre'von LeBlanc", all_cb_contracts$Player)
all_saf_contracts$Player <- ifelse(all_saf_contracts$Player == "Chauncey Gardner-Johnson, Jr.", "C.J. Gardner-Johnson", all_saf_contracts$Player)
all_saf_contracts$Player <- ifelse(all_saf_contracts$Player == "Ha-Ha Clinton-Dix", "Ha Ha Clinton-Dix", all_saf_contracts$Player)

lb_contract_2016 <- filter(all_lb_contracts, Year.Signed == 2016)
lb_contract_2017 <- filter(all_lb_contracts, Year.Signed == 2017)
lb_contract_2018 <- filter(all_lb_contracts, Year.Signed == 2018)
lb_contract_2019 <- filter(all_lb_contracts, Year.Signed == 2019)
lb_contract_2020 <- filter(all_lb_contracts, Year.Signed == 2020)
lb_contract_2021 <- filter(all_lb_contracts, Year.Signed == 2021)
lb_contract_2022 <- filter(all_lb_contracts, Year.Signed == 2022)
lb_contract_2023 <- filter(all_lb_contracts, Year.Signed == 2023)
lb_contract_2024 <- filter(all_lb_contracts, Year.Signed == 2024)


lbs_2024 <- left_join(lb_contract_2024, lb_stats_2024, by = c("Player" = "Player"))
lbs_2023 <- left_join(lb_contract_2023, lb_stats_2023, by = c("Player" = "Player"))
lbs_2022 <- left_join(lb_contract_2022, lb_stats_2022, by = c("Player" = "Player"))
lbs_2021 <- left_join(lb_contract_2021, lb_stats_2021, by = c("Player" = "Player"))
lbs_2020 <- left_join(lb_contract_2020, lb_stats_2020, by = c("Player" = "Player"))
lbs_2019 <- left_join(lb_contract_2019, lb_stats_2019, by = c("Player" = "Player"))
lbs_2018 <- left_join(lb_contract_2018, lb_stats_2018, by = c("Player" = "Player"))
lbs_2017 <- left_join(lb_contract_2017, lb_stats_2017, by = c("Player" = "Player"))
lbs_2016 <- left_join(lb_contract_2016, lb_stats_2016, by = c("Player" = "Player"))


lbs_masterfile <- bind_rows(lbs_2024, lbs_2023, lbs_2022, lbs_2021, lbs_2020, lbs_2019, lbs_2018, lbs_2017, lbs_2016)
write.csv(lbs_masterfile, "lbs_masterfile.csv")

#Cornerbacks
cb_contract_2016 <- filter(all_cb_contracts, Year.Signed == 2016)
cb_contract_2017 <- filter(all_cb_contracts, Year.Signed == 2017)
cb_contract_2018 <- filter(all_cb_contracts, Year.Signed == 2018)
cb_contract_2019 <- filter(all_cb_contracts, Year.Signed == 2019)
cb_contract_2020 <- filter(all_cb_contracts, Year.Signed == 2020)
cb_contract_2021 <- filter(all_cb_contracts, Year.Signed == 2021)
cb_contract_2022 <- filter(all_cb_contracts, Year.Signed == 2022)
cb_contract_2023 <- filter(all_cb_contracts, Year.Signed == 2023)
cb_contract_2024 <- filter(all_cb_contracts, Year.Signed == 2024)


cbs_2024 <- left_join(cb_contract_2024, lb_stats_2024, by = c("Player" = "Player"))
cbs_2023 <- left_join(cb_contract_2023, lb_stats_2023, by = c("Player" = "Player"))
cbs_2022 <- left_join(cb_contract_2022, lb_stats_2022, by = c("Player" = "Player"))
cbs_2021 <- left_join(cb_contract_2021, lb_stats_2021, by = c("Player" = "Player"))
cbs_2020 <- left_join(cb_contract_2020, lb_stats_2020, by = c("Player" = "Player"))
cbs_2019 <- left_join(cb_contract_2019, lb_stats_2019, by = c("Player" = "Player"))
cbs_2018 <- left_join(cb_contract_2018, lb_stats_2018, by = c("Player" = "Player"))
cbs_2017 <- left_join(cb_contract_2017, lb_stats_2017, by = c("Player" = "Player"))
cbs_2016 <- left_join(cb_contract_2016, lb_stats_2016, by = c("Player" = "Player"))


cbs_masterfile <- bind_rows(cbs_2024, cbs_2023, cbs_2022, cbs_2021, cbs_2020, cbs_2019, cbs_2018, cbs_2017, cbs_2016)
cbs_masterfile$Pos.y <- "CB"
write.csv(cbs_masterfile, "cbs_masterfile.csv")

#Safeties
saf_contract_2016 <- filter(all_saf_contracts, Year.Signed == 2016)
saf_contract_2017 <- filter(all_saf_contracts, Year.Signed == 2017)
saf_contract_2018 <- filter(all_saf_contracts, Year.Signed == 2018)
saf_contract_2019 <- filter(all_saf_contracts, Year.Signed == 2019)
saf_contract_2020 <- filter(all_saf_contracts, Year.Signed == 2020)
saf_contract_2021 <- filter(all_saf_contracts, Year.Signed == 2021)
saf_contract_2022 <- filter(all_saf_contracts, Year.Signed == 2022)
saf_contract_2023 <- filter(all_saf_contracts, Year.Signed == 2023)
saf_contract_2024 <- filter(all_saf_contracts, Year.Signed == 2024)


safs_2024 <- left_join(saf_contract_2024, lb_stats_2024, by = c("Player" = "Player"))
safs_2023 <- left_join(saf_contract_2023, lb_stats_2023, by = c("Player" = "Player"))
safs_2022 <- left_join(saf_contract_2022, lb_stats_2022, by = c("Player" = "Player"))
safs_2021 <- left_join(saf_contract_2021, lb_stats_2021, by = c("Player" = "Player"))
safs_2020 <- left_join(saf_contract_2020, lb_stats_2020, by = c("Player" = "Player"))
safs_2019 <- left_join(saf_contract_2019, lb_stats_2019, by = c("Player" = "Player"))
safs_2018 <- left_join(saf_contract_2018, lb_stats_2018, by = c("Player" = "Player"))
safs_2017 <- left_join(saf_contract_2017, lb_stats_2017, by = c("Player" = "Player"))
safs_2016 <- left_join(saf_contract_2016, lb_stats_2016, by = c("Player" = "Player"))


safs_masterfile <- bind_rows(safs_2024, safs_2023, safs_2022, safs_2021, safs_2020, safs_2019, safs_2018, safs_2017, safs_2016)
safs_masterfile$Pos.y <- "SAF"
write.csv(safs_masterfile, "safs_masterfile.csv")

