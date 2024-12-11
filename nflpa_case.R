library(tidyverse)
library(rvest)
###Wide Receivers
# URL of the page to scrape
url <- "https://overthecap.com/contract-history/wide-receiver"

# Read the page
page <- read_html(url)

# Extract table data
wr_contract <- page %>%
  html_node("table") %>%    
  html_table(fill = TRUE)   

# Display the first few rows of the data
head(wr_contract)

# Making sure the data is formatted properly
wr_contract$Value <- as.numeric(gsub("[\\$,]", "", wr_contract$Value))
wr_contract$APY <- as.numeric(gsub("[\\$,]", "", wr_contract$APY))
wr_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", wr_contract$Guaranteed))
wr_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", wr_contract$`Inflated Value`))
wr_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", wr_contract$`Inflated APY`))
wr_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", wr_contract$`Inflated Guaranteed`))
wr_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", wr_contract$`APY as % Of Cap At Signing`))

# And converting it to numerics
wr_contract$Value <- as.numeric(wr_contract$Value)
wr_contract$APY <- as.numeric(wr_contract$APY)
wr_contract$Guaranteed <- as.numeric(wr_contract$Guaranteed)
wr_contract$`Inflated Value` <- as.numeric(wr_contract$`Inflated Value`)
wr_contract$`Inflated APY` <- as.numeric(wr_contract$`Inflated APY`)
wr_contract$`Inflated Guaranteed` <- as.numeric(wr_contract$`Inflated Guaranteed`)
wr_contract$`APY as % Of Cap At Signing` <- as.numeric(wr_contract$`APY as % Of Cap At Signing`)
wr_contract$`Year Signed` <- as.numeric(wr_contract$`Year Signed`)
write.csv(wr_contract, "wr_contract.csv")

colnames(wr_contract) <- make.names(colnames(wr_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
wr_contract <- filter(wr_contract, wr_contract$Year.Signed > 2010)

# Changing some of the names in the contract dataset so it properly matches with the names in the pro football reference dataset.
wr_contract$Player <- ifelse(wr_contract$Player == "Michael Pittman, Jr.", "Michael Pittman Jr.", wr_contract$Player)
wr_contract$Player <- ifelse(wr_contract$Player == "Brian Thomas Jr.", "Brian Thomas", wr_contract$Player)
wr_contract$Player <- ifelse(wr_contract$Player == "D.J. Chark", "DJ Chark", wr_contract$Player)
wr_contract$Player <- ifelse(wr_contract$Player == "J.J. Arcega-Whiteside", "JJ Arcega-Whiteside", wr_contract$Player)
wr_contract$Player <- ifelse(wr_contract$Player == "Henry Ruggs", "Henry Ruggs III", wr_contract$Player)
wr_contract$Player <- ifelse(wr_contract$Player == "Laviska Shenault", "Laviska Shenault Jr.", wr_contract$Player)
wr_contract$Player <- ifelse(wr_contract$Player == "K.J. Hamler", "KJ Hamler", wr_contract$Player)
wr_contract$Player <- ifelse(wr_contract$Player == "John Metchie III", "John Metchie", wr_contract$Player)


# Read in the drafted players data (from PFR) and determine which players are rookies
drafted_players <- read.csv("drafted_players.csv")
table(drafted_players$Pos)
drafted_wrs <- filter(drafted_players, Pos == "WR" | Pos == "RB" | Pos == "TE" | Pos == "rb")

wr_contract <- left_join(wr_contract, drafted_wrs, by = c("Player" = "Player"))
wr_contract$is_rookie <- ifelse(wr_contract$Year.Signed == wr_contract$Draft_year, 1, 0)
wr_contract$is_rookie <- ifelse(is.na(wr_contract$is_rookie), 0, wr_contract$is_rookie)
wr_rookies <- filter(wr_contract, is_rookie == 1)
wr_contract <- filter(wr_contract, is_rookie != 1)

wr_contract_2014 <- filter(wr_contract, Year.Signed == 2014)
wr_contract_2015 <- filter(wr_contract, Year.Signed == 2015)
wr_contract_2016 <- filter(wr_contract, Year.Signed == 2016)
wr_contract_2017 <- filter(wr_contract, Year.Signed == 2017)
wr_contract_2018 <- filter(wr_contract, Year.Signed == 2018)
wr_contract_2019 <- filter(wr_contract, Year.Signed == 2019)
wr_contract_2020 <- filter(wr_contract, Year.Signed == 2020)
wr_contract_2021 <- filter(wr_contract, Year.Signed == 2021)
wr_contract_2022 <- filter(wr_contract, Year.Signed == 2022)
wr_contract_2023 <- filter(wr_contract, Year.Signed == 2023)
wr_contract_2024 <- filter(wr_contract, Year.Signed == 2024)


# Creating percentiles using the ECDF function
wr_contract_2024$apy_perc <- ecdf(wr_contract_2024$APY)(wr_contract_2024$APY) * 100
wr_contract_2023$apy_perc <- ecdf(wr_contract_2023$APY)(wr_contract_2023$APY) * 100
wr_contract_2022$apy_perc <- ecdf(wr_contract_2023$APY)(wr_contract_2022$APY) * 100
wr_contract_2021$apy_perc <- ecdf(wr_contract_2023$APY)(wr_contract_2021$APY) * 100
wr_contract_2020$apy_perc <- ecdf(wr_contract_2023$APY)(wr_contract_2020$APY) * 100
wr_contract_2019$apy_perc <- ecdf(wr_contract_2023$APY)(wr_contract_2019$APY) * 100
wr_contract_2018$apy_perc <- ecdf(wr_contract_2023$APY)(wr_contract_2018$APY) * 100
wr_contract_2017$apy_perc <- ecdf(wr_contract_2023$APY)(wr_contract_2017$APY) * 100
wr_contract_2016$apy_perc <- ecdf(wr_contract_2023$APY)(wr_contract_2016$APY) * 100
wr_contract_2015$apy_perc <- ecdf(wr_contract_2023$APY)(wr_contract_2015$APY) * 100
wr_contract_2014$apy_perc <- ecdf(wr_contract_2023$APY)(wr_contract_2014$APY) * 100

# Filtering out the minimum contracts
wr_contract_2024 <- filter(wr_contract_2024, APY > 1210000)
wr_contract_2023 <- filter(wr_contract_2023, APY > 1165000)
wr_contract_2022 <- filter(wr_contract_2022, APY > 1120000)
wr_contract_2021 <- filter(wr_contract_2021, APY > 1075000)
wr_contract_2020 <- filter(wr_contract_2020, APY > 1050000)
wr_contract_2019 <- filter(wr_contract_2019, APY > 1030000)
wr_contract_2018 <- filter(wr_contract_2018, APY > 1015000)
wr_contract_2017 <- filter(wr_contract_2017, APY > 1000000)
wr_contract_2016 <- filter(wr_contract_2016, APY > 985000)
wr_contract_2015 <- filter(wr_contract_2015, APY > 970000)
wr_contract_2014 <- filter(wr_contract_2014, APY > 955000)

# Creating the kmeans clusters for each year
column_to_cluster <- wr_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- wr_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- wr_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- wr_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- wr_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- wr_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- wr_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- wr_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- wr_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- wr_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- wr_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
wr_contract_2014$Cluster <- kmeans_result$cluster





# We have to go into our data and manually determine which cluster is the middle class.
wr_contract_2014$is_middle_class <- ifelse(wr_contract_2014$Cluster == 2, 1, 0)
wr_contract_2015$is_middle_class <- ifelse(wr_contract_2015$Cluster == 2, 1, 0)
wr_contract_2016$is_middle_class <- ifelse(wr_contract_2016$Cluster == 2, 1, 0)
wr_contract_2017$is_middle_class <- ifelse(wr_contract_2017$Cluster == 2, 1, 0)
wr_contract_2018$is_middle_class <- ifelse(wr_contract_2018$Cluster == 1, 1, 0)
wr_contract_2019$is_middle_class <- ifelse(wr_contract_2019$Cluster == 1, 1, 0)
wr_contract_2020$is_middle_class <- ifelse(wr_contract_2020$Cluster == 2, 1, 0)
wr_contract_2021$is_middle_class <- ifelse(wr_contract_2021$Cluster == 2, 1, 0)
wr_contract_2022$is_middle_class <- ifelse(wr_contract_2022$Cluster == 1, 1, 0)
wr_contract_2023$is_middle_class <- ifelse(wr_contract_2023$Cluster == 2, 1, 0)
wr_contract_2024$is_middle_class <- ifelse(wr_contract_2024$Cluster == 1, 1, 0)

# Bind rows together
all_wr_contracts <- bind_rows(wr_contract_2016, wr_contract_2017, wr_contract_2018, wr_contract_2019, wr_contract_2020, wr_contract_2021, wr_contract_2022, wr_contract_2023, wr_contract_2024)

# Create two more definitions of the middle class - Based on APYPTOS and percentiles
all_wr_contracts$second_middle_class <- ifelse(all_wr_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_wr_contracts$APY.as...Of.Cap.At.Signing < 5.0, 1, 0)
all_wr_contracts$third_middle_class <- ifelse(all_wr_contracts$apy_perc > 92 & all_wr_contracts$apy_perc < 98, 1, 0)

table(middle_class_wrs$Year.Signed)

# Middle class contracts by year - def 1
all_wr_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

# Middle class contracts by year - def 2
all_wr_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

# Middle class contracts by year - def 3
all_wr_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))

# Export as CSV
write.csv(all_wr_contracts, "all_wr_contracts.csv")

# And now we need to repeat this for every single position. Oof.

### Quarterbacks----

# URL of the page to scrape
url <- "https://overthecap.com/contract-history/quarterback"

# Read the page
page <- read_html(url)

# Extract table data
qb_contract <- page %>%
  html_node("table") %>%    
  html_table(fill = TRUE)   

# Display the first few rows of the data
head(qb_contract)

qb_contract$Value <- as.numeric(gsub("[\\$,]", "", qb_contract$Value))
qb_contract$APY <- as.numeric(gsub("[\\$,]", "", qb_contract$APY))
qb_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", qb_contract$Guaranteed))
qb_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", qb_contract$`Inflated Value`))
qb_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", qb_contract$`Inflated APY`))
qb_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", qb_contract$`Inflated Guaranteed`))
qb_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", qb_contract$`APY as % Of Cap At Signing`))

qb_contract$Value <- as.numeric(qb_contract$Value)
qb_contract$APY <- as.numeric(qb_contract$APY)
qb_contract$Guaranteed <- as.numeric(qb_contract$Guaranteed)
qb_contract$`Inflated Value` <- as.numeric(qb_contract$`Inflated Value`)
qb_contract$`Inflated APY` <- as.numeric(qb_contract$`Inflated APY`)
qb_contract$`Inflated Guaranteed` <- as.numeric(qb_contract$`Inflated Guaranteed`)
qb_contract$`APY as % Of Cap At Signing` <- as.numeric(qb_contract$`APY as % Of Cap At Signing`)
qb_contract$`Year Signed` <- as.numeric(qb_contract$`Year Signed`)
write.csv(qb_contract, "qb_contract.csv")

colnames(qb_contract) <- make.names(colnames(qb_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
qb_contract <- filter(qb_contract, qb_contract$Year.Signed > 2010)





drafted_players <- read.csv("drafted_players.csv")
table(drafted_players$Pos)
drafted_qbs <- filter(drafted_players, Pos == "QB")
qb_contract$Player <- ifelse(qb_contract$Player == "Michael Penix Jr.", "Michael Penix", qb_contract$Player)

qb_contract <- left_join(qb_contract, drafted_qbs, by = c("Player" = "Player"))
qb_contract$is_rookie <- ifelse(qb_contract$Year.Signed == qb_contract$Draft_year, 1, 0)
qb_contract$is_rookie <- ifelse(is.na(qb_contract$is_rookie), 0, qb_contract$is_rookie)
qb_rookies <- filter(qb_contract, is_rookie == 1)
qb_contract <- filter(qb_contract, is_rookie != 1)

qb_contract_2014 <- filter(qb_contract, Year.Signed == 2014)
qb_contract_2015 <- filter(qb_contract, Year.Signed == 2015)
qb_contract_2016 <- filter(qb_contract, Year.Signed == 2016)
qb_contract_2017 <- filter(qb_contract, Year.Signed == 2017)
qb_contract_2018 <- filter(qb_contract, Year.Signed == 2018)
qb_contract_2019 <- filter(qb_contract, Year.Signed == 2019)
qb_contract_2020 <- filter(qb_contract, Year.Signed == 2020)
qb_contract_2021 <- filter(qb_contract, Year.Signed == 2021)
qb_contract_2022 <- filter(qb_contract, Year.Signed == 2022)
qb_contract_2023 <- filter(qb_contract, Year.Signed == 2023)
qb_contract_2024 <- filter(qb_contract, Year.Signed == 2024)



qb_contract_2024$apy_perc <- ecdf(qb_contract_2024$APY)(qb_contract_2024$APY) * 100
qb_contract_2023$apy_perc <- ecdf(qb_contract_2023$APY)(qb_contract_2023$APY) * 100
qb_contract_2022$apy_perc <- ecdf(qb_contract_2023$APY)(qb_contract_2022$APY) * 100
qb_contract_2021$apy_perc <- ecdf(qb_contract_2023$APY)(qb_contract_2021$APY) * 100
qb_contract_2020$apy_perc <- ecdf(qb_contract_2023$APY)(qb_contract_2020$APY) * 100
qb_contract_2019$apy_perc <- ecdf(qb_contract_2023$APY)(qb_contract_2019$APY) * 100
qb_contract_2018$apy_perc <- ecdf(qb_contract_2023$APY)(qb_contract_2018$APY) * 100
qb_contract_2017$apy_perc <- ecdf(qb_contract_2023$APY)(qb_contract_2017$APY) * 100
qb_contract_2016$apy_perc <- ecdf(qb_contract_2023$APY)(qb_contract_2016$APY) * 100
qb_contract_2015$apy_perc <- ecdf(qb_contract_2023$APY)(qb_contract_2015$APY) * 100
qb_contract_2014$apy_perc <- ecdf(qb_contract_2023$APY)(qb_contract_2014$APY) * 100

qb_contract_2024 <- filter(qb_contract_2024, APY > 1210000)
qb_contract_2023 <- filter(qb_contract_2023, APY > 1165000)
qb_contract_2022 <- filter(qb_contract_2022, APY > 1120000)
qb_contract_2021 <- filter(qb_contract_2021, APY > 1075000)
qb_contract_2020 <- filter(qb_contract_2020, APY > 1050000)
qb_contract_2019 <- filter(qb_contract_2019, APY > 1030000)
qb_contract_2018 <- filter(qb_contract_2018, APY > 1015000)
qb_contract_2017 <- filter(qb_contract_2017, APY > 1000000)
qb_contract_2016 <- filter(qb_contract_2016, APY > 985000)
qb_contract_2015 <- filter(qb_contract_2015, APY > 970000)
qb_contract_2014 <- filter(qb_contract_2014, APY > 955000)

column_to_cluster <- qb_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- qb_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- qb_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- qb_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- qb_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- qb_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- qb_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- qb_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- qb_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- qb_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- qb_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
qb_contract_2014$Cluster <- kmeans_result$cluster






qb_contract_2014$is_middle_class <- ifelse(qb_contract_2014$Cluster == 2, 1, 0)
qb_contract_2015$is_middle_class <- ifelse(qb_contract_2015$Cluster == 1, 1, 0)
qb_contract_2016$is_middle_class <- ifelse(qb_contract_2016$Cluster == 2, 1, 0)
qb_contract_2017$is_middle_class <- ifelse(qb_contract_2017$Cluster == 1, 1, 0)
qb_contract_2018$is_middle_class <- ifelse(qb_contract_2018$Cluster == 2, 1, 0)
qb_contract_2019$is_middle_class <- ifelse(qb_contract_2019$Cluster == 1, 1, 0)
qb_contract_2020$is_middle_class <- ifelse(qb_contract_2020$Cluster == 2, 1, 0)
qb_contract_2021$is_middle_class <- ifelse(qb_contract_2021$Cluster == 1, 1, 0)
qb_contract_2022$is_middle_class <- ifelse(qb_contract_2022$Cluster == 2, 1, 0)
qb_contract_2023$is_middle_class <- ifelse(qb_contract_2023$Cluster == 2, 1, 0)
qb_contract_2024$is_middle_class <- ifelse(qb_contract_2024$Cluster == 2, 1, 0)


all_qb_contracts <- bind_rows(qb_contract_2016, qb_contract_2017, qb_contract_2018, qb_contract_2019, qb_contract_2020, qb_contract_2021, qb_contract_2022, qb_contract_2023, qb_contract_2024)
all_qb_contracts$second_middle_class <- ifelse(all_qb_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_qb_contracts$APY.as...Of.Cap.At.Signing < 10, 1, 0)

all_qb_contracts$third_middle_class <- ifelse(all_qb_contracts$apy_perc > 92 & all_qb_contracts$apy_perc < 98, 1, 0)


all_qb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_qb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_qb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))

write.csv(all_qb_contracts, "all_qb_contracts.csv")

### Runningbacks ----
url <- "https://overthecap.com/contract-history/running-back"

# Read the page
page <- read_html(url)

# Extract table data
rb_contract <- page %>%
  html_node("table") %>%    
  html_table(fill = TRUE)   

# Display the first few rows of the data
head(rb_contract)

rb_contract$Value <- as.numeric(gsub("[\\$,]", "", rb_contract$Value))
rb_contract$APY <- as.numeric(gsub("[\\$,]", "", rb_contract$APY))
rb_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", rb_contract$Guaranteed))
rb_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", rb_contract$`Inflated Value`))
rb_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", rb_contract$`Inflated APY`))
rb_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", rb_contract$`Inflated Guaranteed`))
rb_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", rb_contract$`APY as % Of Cap At Signing`))

rb_contract$Value <- as.numeric(rb_contract$Value)
rb_contract$APY <- as.numeric(rb_contract$APY)
rb_contract$Guaranteed <- as.numeric(rb_contract$Guaranteed)
rb_contract$`Inflated Value` <- as.numeric(rb_contract$`Inflated Value`)
rb_contract$`Inflated APY` <- as.numeric(rb_contract$`Inflated APY`)
rb_contract$`Inflated Guaranteed` <- as.numeric(rb_contract$`Inflated Guaranteed`)
rb_contract$`APY as % Of Cap At Signing` <- as.numeric(rb_contract$`APY as % Of Cap At Signing`)
rb_contract$`Year Signed` <- as.numeric(rb_contract$`Year Signed`)
write.csv(rb_contract, "rb_contract.csv")

colnames(rb_contract) <- make.names(colnames(rb_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
rb_contract <- filter(rb_contract, rb_contract$Year.Signed > 2010)





drafted_players <- read.csv("drafted_players.csv")
table(drafted_players$Pos)
drafted_rbs <- filter(drafted_players, Pos == "RB" | Pos == "WR" | Pos == "TE")
rb_contract$Player <- ifelse(rb_contract$Player == "Devon Achane", "De'Von Achane", rb_contract$Player)
rb_contract$Player <- ifelse(rb_contract$Player == "A.J. Dillon", "AJ Dillon", rb_contract$Player)
rb_contract$Player <- ifelse(rb_contract$Player == "Lynn Bowden", "Lynn Bowden Jr.", rb_contract$Player)

rb_contract <- left_join(rb_contract, drafted_rbs, by = c("Player" = "Player"))
rb_contract$is_rookie <- ifelse(rb_contract$Year.Signed == rb_contract$Draft_year, 1, 0)
rb_contract$is_rookie <- ifelse(is.na(rb_contract$is_rookie), 0, rb_contract$is_rookie)
rb_rookies <- filter(rb_contract, is_rookie == 1)
rb_contract <- filter(rb_contract, is_rookie != 1)

rb_contract_2014 <- filter(rb_contract, Year.Signed == 2014)
rb_contract_2015 <- filter(rb_contract, Year.Signed == 2015)
rb_contract_2016 <- filter(rb_contract, Year.Signed == 2016)
rb_contract_2017 <- filter(rb_contract, Year.Signed == 2017)
rb_contract_2018 <- filter(rb_contract, Year.Signed == 2018)
rb_contract_2019 <- filter(rb_contract, Year.Signed == 2019)
rb_contract_2020 <- filter(rb_contract, Year.Signed == 2020)
rb_contract_2021 <- filter(rb_contract, Year.Signed == 2021)
rb_contract_2022 <- filter(rb_contract, Year.Signed == 2022)
rb_contract_2023 <- filter(rb_contract, Year.Signed == 2023)
rb_contract_2024 <- filter(rb_contract, Year.Signed == 2024)



rb_contract_2024$apy_perc <- ecdf(rb_contract_2024$APY)(rb_contract_2024$APY) * 100
rb_contract_2023$apy_perc <- ecdf(rb_contract_2023$APY)(rb_contract_2023$APY) * 100
rb_contract_2022$apy_perc <- ecdf(rb_contract_2023$APY)(rb_contract_2022$APY) * 100
rb_contract_2021$apy_perc <- ecdf(rb_contract_2023$APY)(rb_contract_2021$APY) * 100
rb_contract_2020$apy_perc <- ecdf(rb_contract_2023$APY)(rb_contract_2020$APY) * 100
rb_contract_2019$apy_perc <- ecdf(rb_contract_2023$APY)(rb_contract_2019$APY) * 100
rb_contract_2018$apy_perc <- ecdf(rb_contract_2023$APY)(rb_contract_2018$APY) * 100
rb_contract_2017$apy_perc <- ecdf(rb_contract_2023$APY)(rb_contract_2017$APY) * 100
rb_contract_2016$apy_perc <- ecdf(rb_contract_2023$APY)(rb_contract_2016$APY) * 100
rb_contract_2015$apy_perc <- ecdf(rb_contract_2023$APY)(rb_contract_2015$APY) * 100
rb_contract_2014$apy_perc <- ecdf(rb_contract_2023$APY)(rb_contract_2014$APY) * 100

rb_contract_2024 <- filter(rb_contract_2024, APY > 1210000)
rb_contract_2023 <- filter(rb_contract_2023, APY > 1165000)
rb_contract_2022 <- filter(rb_contract_2022, APY > 1120000)
rb_contract_2021 <- filter(rb_contract_2021, APY > 1075000)
rb_contract_2020 <- filter(rb_contract_2020, APY > 1050000)
rb_contract_2019 <- filter(rb_contract_2019, APY > 1030000)
rb_contract_2018 <- filter(rb_contract_2018, APY > 1015000)
rb_contract_2017 <- filter(rb_contract_2017, APY > 1000000)
rb_contract_2016 <- filter(rb_contract_2016, APY > 985000)
rb_contract_2015 <- filter(rb_contract_2015, APY > 970000)
rb_contract_2014 <- filter(rb_contract_2014, APY > 955000)

column_to_cluster <- rb_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- rb_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- rb_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- rb_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- rb_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- rb_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- rb_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- rb_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- rb_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- rb_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- rb_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
rb_contract_2014$Cluster <- kmeans_result$cluster






rb_contract_2014$is_middle_class <- ifelse(rb_contract_2014$Cluster == 1, 1, 0)
rb_contract_2015$is_middle_class <- ifelse(rb_contract_2015$Cluster == 2, 1, 0)
rb_contract_2016$is_middle_class <- ifelse(rb_contract_2016$Cluster == 1, 1, 0)
rb_contract_2017$is_middle_class <- ifelse(rb_contract_2017$Cluster == 2, 1, 0)
rb_contract_2018$is_middle_class <- ifelse(rb_contract_2018$Cluster == 1, 1, 0)
rb_contract_2019$is_middle_class <- ifelse(rb_contract_2019$Cluster == 2, 1, 0)
rb_contract_2020$is_middle_class <- ifelse(rb_contract_2020$Cluster == 2, 1, 0)
rb_contract_2021$is_middle_class <- ifelse(rb_contract_2021$Cluster == 1, 1, 0)
rb_contract_2022$is_middle_class <- ifelse(rb_contract_2022$Cluster == 2, 1, 0)
rb_contract_2023$is_middle_class <- ifelse(rb_contract_2023$Cluster == 2, 1, 0)
rb_contract_2024$is_middle_class <- ifelse(rb_contract_2024$Cluster == 2, 1, 0)


all_rb_contracts <- bind_rows(rb_contract_2016, rb_contract_2017, rb_contract_2018, rb_contract_2019, rb_contract_2020, rb_contract_2021, rb_contract_2022, rb_contract_2023, rb_contract_2024)
all_rb_contracts$second_middle_class <- ifelse(all_rb_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_rb_contracts$APY.as...Of.Cap.At.Signing < 2.5, 1, 0)

all_rb_contracts$third_middle_class <- ifelse(all_rb_contracts$apy_perc > 92 & all_rb_contracts$apy_perc < 98, 1, 0)


all_rb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_rb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_rb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))


write.csv(all_rb_contracts, "all_rb_contracts.csv")


### Tight ends ----
url <- "https://overthecap.com/contract-history/tight-end"

# Read the page
page <- read_html(url)

# Extract table data
te_contract <- page %>%
  html_node("table") %>%    
  html_table(fill = TRUE)   

# Display the first few rows of the data
head(te_contract)

te_contract$Value <- as.numeric(gsub("[\\$,]", "", te_contract$Value))
te_contract$APY <- as.numeric(gsub("[\\$,]", "", te_contract$APY))
te_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", te_contract$Guaranteed))
te_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", te_contract$`Inflated Value`))
te_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", te_contract$`Inflated APY`))
te_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", te_contract$`Inflated Guaranteed`))
te_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", te_contract$`APY as % Of Cap At Signing`))

te_contract$Value <- as.numeric(te_contract$Value)
te_contract$APY <- as.numeric(te_contract$APY)
te_contract$Guaranteed <- as.numeric(te_contract$Guaranteed)
te_contract$`Inflated Value` <- as.numeric(te_contract$`Inflated Value`)
te_contract$`Inflated APY` <- as.numeric(te_contract$`Inflated APY`)
te_contract$`Inflated Guaranteed` <- as.numeric(te_contract$`Inflated Guaranteed`)
te_contract$`APY as % Of Cap At Signing` <- as.numeric(te_contract$`APY as % Of Cap At Signing`)
te_contract$`Year Signed` <- as.numeric(te_contract$`Year Signed`)
write.csv(te_contract, "te_contract.csv")

colnames(te_contract) <- make.names(colnames(te_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
te_contract <- filter(te_contract, te_contract$Year.Signed > 2010)





drafted_players <- read.csv("drafted_players.csv")
table(drafted_players$Pos)
drafted_tes <- filter(drafted_players, Pos == "WR" | Pos == "TE")
te_contract$Player <- ifelse(te_contract$Player == "Devon Achane", "De'Von Achane", te_contract$Player)
te_contract$Player <- ifelse(te_contract$Player == "A.J. Dillon", "AJ Dillon", te_contract$Player)
te_contract$Player <- ifelse(te_contract$Player == "Lynn Bowden", "Lynn Bowden Jr.", te_contract$Player)

te_contract <- left_join(te_contract, drafted_tes, by = c("Player" = "Player"))
te_contract$is_rookie <- ifelse(te_contract$Year.Signed == te_contract$Draft_year, 1, 0)
te_contract$is_rookie <- ifelse(is.na(te_contract$is_rookie), 0, te_contract$is_rookie)
te_rookies <- filter(te_contract, is_rookie == 1)
te_contract <- filter(te_contract, is_rookie != 1)

te_contract_2014 <- filter(te_contract, Year.Signed == 2014)
te_contract_2015 <- filter(te_contract, Year.Signed == 2015)
te_contract_2016 <- filter(te_contract, Year.Signed == 2016)
te_contract_2017 <- filter(te_contract, Year.Signed == 2017)
te_contract_2018 <- filter(te_contract, Year.Signed == 2018)
te_contract_2019 <- filter(te_contract, Year.Signed == 2019)
te_contract_2020 <- filter(te_contract, Year.Signed == 2020)
te_contract_2021 <- filter(te_contract, Year.Signed == 2021)
te_contract_2022 <- filter(te_contract, Year.Signed == 2022)
te_contract_2023 <- filter(te_contract, Year.Signed == 2023)
te_contract_2024 <- filter(te_contract, Year.Signed == 2024)



te_contract_2024$apy_perc <- ecdf(te_contract_2024$APY)(te_contract_2024$APY) * 100
te_contract_2023$apy_perc <- ecdf(te_contract_2023$APY)(te_contract_2023$APY) * 100
te_contract_2022$apy_perc <- ecdf(te_contract_2023$APY)(te_contract_2022$APY) * 100
te_contract_2021$apy_perc <- ecdf(te_contract_2023$APY)(te_contract_2021$APY) * 100
te_contract_2020$apy_perc <- ecdf(te_contract_2023$APY)(te_contract_2020$APY) * 100
te_contract_2019$apy_perc <- ecdf(te_contract_2023$APY)(te_contract_2019$APY) * 100
te_contract_2018$apy_perc <- ecdf(te_contract_2023$APY)(te_contract_2018$APY) * 100
te_contract_2017$apy_perc <- ecdf(te_contract_2023$APY)(te_contract_2017$APY) * 100
te_contract_2016$apy_perc <- ecdf(te_contract_2023$APY)(te_contract_2016$APY) * 100
te_contract_2015$apy_perc <- ecdf(te_contract_2023$APY)(te_contract_2015$APY) * 100
te_contract_2014$apy_perc <- ecdf(te_contract_2023$APY)(te_contract_2014$APY) * 100

te_contract_2024 <- filter(te_contract_2024, APY > 1210000)
te_contract_2023 <- filter(te_contract_2023, APY > 1165000)
te_contract_2022 <- filter(te_contract_2022, APY > 1120000)
te_contract_2021 <- filter(te_contract_2021, APY > 1075000)
te_contract_2020 <- filter(te_contract_2020, APY > 1050000)
te_contract_2019 <- filter(te_contract_2019, APY > 1030000)
te_contract_2018 <- filter(te_contract_2018, APY > 1015000)
te_contract_2017 <- filter(te_contract_2017, APY > 1000000)
te_contract_2016 <- filter(te_contract_2016, APY > 985000)
te_contract_2015 <- filter(te_contract_2015, APY > 970000)
te_contract_2014 <- filter(te_contract_2014, APY > 955000)

### Rest 
column_to_cluster <- te_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- te_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- te_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- te_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- te_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- te_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- te_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- te_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- te_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- te_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- te_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
te_contract_2014$Cluster <- kmeans_result$cluster






te_contract_2014$is_middle_class <- ifelse(te_contract_2014$Cluster == 2, 1, 0)
te_contract_2015$is_middle_class <- ifelse(te_contract_2015$Cluster == 1, 1, 0)
te_contract_2016$is_middle_class <- ifelse(te_contract_2016$Cluster == 1, 1, 0)
te_contract_2017$is_middle_class <- ifelse(te_contract_2017$Cluster == 1, 1, 0)
te_contract_2018$is_middle_class <- ifelse(te_contract_2018$Cluster == 1, 1, 0)
te_contract_2019$is_middle_class <- ifelse(te_contract_2019$Cluster == 1, 1, 0)
te_contract_2020$is_middle_class <- ifelse(te_contract_2020$Cluster == 1, 1, 0)
te_contract_2021$is_middle_class <- ifelse(te_contract_2021$Cluster == 2, 1, 0)
te_contract_2022$is_middle_class <- ifelse(te_contract_2022$Cluster == 2, 1, 0)
te_contract_2023$is_middle_class <- ifelse(te_contract_2023$Cluster == 2, 1, 0)
te_contract_2024$is_middle_class <- ifelse(te_contract_2024$Cluster == 1, 1, 0)


all_te_contracts <- bind_rows(te_contract_2016, te_contract_2017, te_contract_2018, te_contract_2019, te_contract_2020, te_contract_2021, te_contract_2022, te_contract_2023, te_contract_2024)
all_te_contracts$second_middle_class <- ifelse(all_te_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_te_contracts$APY.as...Of.Cap.At.Signing < 2.5, 1, 0)

all_te_contracts$third_middle_class <- ifelse(all_te_contracts$apy_perc > 92 & all_te_contracts$apy_perc < 98, 1, 0)


all_te_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_te_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_te_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))


write.csv(all_te_contracts, "all_te_contracts.csv")


### Offensive tackles ----
url1 <- "https://overthecap.com/contract-history/left-tackle"
url2 <- "https://overthecap.com/contract-history/right-tackle"
# Read the page
page1 <- read_html(url1)
page2 <- read_html(url2)

# Extract table data
lt_contract <- page1 %>%
  html_node("table") %>%    
  html_table(fill = TRUE)  
rt_contract <- page2 %>%
  html_node("table") %>%    
  html_table(fill = TRUE) 
ot_contract <- bind_rows(lt_contract, rt_contract)
# Display the first few rows of the data
head(ot_contract)

ot_contract$Value <- as.numeric(gsub("[\\$,]", "", ot_contract$Value))
ot_contract$APY <- as.numeric(gsub("[\\$,]", "", ot_contract$APY))
ot_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", ot_contract$Guaranteed))
ot_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", ot_contract$`Inflated Value`))
ot_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", ot_contract$`Inflated APY`))
ot_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", ot_contract$`Inflated Guaranteed`))
ot_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", ot_contract$`APY as % Of Cap At Signing`))

ot_contract$Value <- as.numeric(ot_contract$Value)
ot_contract$APY <- as.numeric(ot_contract$APY)
ot_contract$Guaranteed <- as.numeric(ot_contract$Guaranteed)
ot_contract$`Inflated Value` <- as.numeric(ot_contract$`Inflated Value`)
ot_contract$`Inflated APY` <- as.numeric(ot_contract$`Inflated APY`)
ot_contract$`Inflated Guaranteed` <- as.numeric(ot_contract$`Inflated Guaranteed`)
ot_contract$`APY as % Of Cap At Signing` <- as.numeric(ot_contract$`APY as % Of Cap At Signing`)
ot_contract$`Year Signed` <- as.numeric(ot_contract$`Year Signed`)
write.csv(ot_contract, "ot_contract.csv")

colnames(ot_contract) <- make.names(colnames(ot_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
ot_contract <- filter(ot_contract, ot_contract$Year.Signed > 2010)





drafted_players <- read.csv("drafted_players.csv")
table(drafted_players$Pos)
drafted_ots <- filter(drafted_players, Pos == "T" | Pos == "OT" | Pos == "OL" | Pos == "G" | Pos == "C")
ot_contract$Player <- ifelse(ot_contract$Player == "Jedrick Wills", "Jedrick Wills Jr.", ot_contract$Player)
ot_contract$Player <- ifelse(ot_contract$Player == "Paris Johnson Jr.", "Paris Johnson", ot_contract$Player)
ot_contract$Player <- ifelse(ot_contract$Player == "J.C. Latham", "JC Latham", ot_contract$Player)
ot_contract$Player <- ifelse(ot_contract$Player == "Olu Fashanu", "Olumuyiwa Fashanu", ot_contract$Player)

ot_contract <- left_join(ot_contract, drafted_ots, by = c("Player" = "Player"))
ot_contract$is_rookie <- ifelse(ot_contract$Year.Signed == ot_contract$Draft_year, 1, 0)
ot_contract$is_rookie <- ifelse(is.na(ot_contract$is_rookie), 0, ot_contract$is_rookie)
ot_rookies <- filter(ot_contract, is_rookie == 1)
ot_contract <- filter(ot_contract, is_rookie != 1)

ot_contract_2014 <- filter(ot_contract, Year.Signed == 2014)
ot_contract_2015 <- filter(ot_contract, Year.Signed == 2015)
ot_contract_2016 <- filter(ot_contract, Year.Signed == 2016)
ot_contract_2017 <- filter(ot_contract, Year.Signed == 2017)
ot_contract_2018 <- filter(ot_contract, Year.Signed == 2018)
ot_contract_2019 <- filter(ot_contract, Year.Signed == 2019)
ot_contract_2020 <- filter(ot_contract, Year.Signed == 2020)
ot_contract_2021 <- filter(ot_contract, Year.Signed == 2021)
ot_contract_2022 <- filter(ot_contract, Year.Signed == 2022)
ot_contract_2023 <- filter(ot_contract, Year.Signed == 2023)
ot_contract_2024 <- filter(ot_contract, Year.Signed == 2024)



ot_contract_2024$apy_perc <- ecdf(ot_contract_2024$APY)(ot_contract_2024$APY) * 100
ot_contract_2023$apy_perc <- ecdf(ot_contract_2023$APY)(ot_contract_2023$APY) * 100
ot_contract_2022$apy_perc <- ecdf(ot_contract_2023$APY)(ot_contract_2022$APY) * 100
ot_contract_2021$apy_perc <- ecdf(ot_contract_2023$APY)(ot_contract_2021$APY) * 100
ot_contract_2020$apy_perc <- ecdf(ot_contract_2023$APY)(ot_contract_2020$APY) * 100
ot_contract_2019$apy_perc <- ecdf(ot_contract_2023$APY)(ot_contract_2019$APY) * 100
ot_contract_2018$apy_perc <- ecdf(ot_contract_2023$APY)(ot_contract_2018$APY) * 100
ot_contract_2017$apy_perc <- ecdf(ot_contract_2023$APY)(ot_contract_2017$APY) * 100
ot_contract_2016$apy_perc <- ecdf(ot_contract_2023$APY)(ot_contract_2016$APY) * 100
ot_contract_2015$apy_perc <- ecdf(ot_contract_2023$APY)(ot_contract_2015$APY) * 100
ot_contract_2014$apy_perc <- ecdf(ot_contract_2023$APY)(ot_contract_2014$APY) * 100

ot_contract_2024 <- filter(ot_contract_2024, APY > 1210000)
ot_contract_2023 <- filter(ot_contract_2023, APY > 1165000)
ot_contract_2022 <- filter(ot_contract_2022, APY > 1120000)
ot_contract_2021 <- filter(ot_contract_2021, APY > 1075000)
ot_contract_2020 <- filter(ot_contract_2020, APY > 1050000)
ot_contract_2019 <- filter(ot_contract_2019, APY > 1030000)
ot_contract_2018 <- filter(ot_contract_2018, APY > 1015000)
ot_contract_2017 <- filter(ot_contract_2017, APY > 1000000)
ot_contract_2016 <- filter(ot_contract_2016, APY > 985000)
ot_contract_2015 <- filter(ot_contract_2015, APY > 970000)
ot_contract_2014 <- filter(ot_contract_2014, APY > 955000)

column_to_cluster <- ot_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- ot_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- ot_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- ot_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- ot_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- ot_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- ot_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- ot_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- ot_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- ot_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- ot_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
ot_contract_2014$Cluster <- kmeans_result$cluster






ot_contract_2014$is_middle_class <- ifelse(ot_contract_2014$Cluster == 1, 1, 0)
ot_contract_2015$is_middle_class <- ifelse(ot_contract_2015$Cluster == 1, 1, 0)
ot_contract_2016$is_middle_class <- ifelse(ot_contract_2016$Cluster == 2, 1, 0)
ot_contract_2017$is_middle_class <- ifelse(ot_contract_2017$Cluster == 2, 1, 0)
ot_contract_2018$is_middle_class <- ifelse(ot_contract_2018$Cluster == 2, 1, 0)
ot_contract_2019$is_middle_class <- ifelse(ot_contract_2019$Cluster == 2, 1, 0)
ot_contract_2020$is_middle_class <- ifelse(ot_contract_2020$Cluster == 2, 1, 0)
ot_contract_2021$is_middle_class <- ifelse(ot_contract_2021$Cluster == 1, 1, 0)
ot_contract_2022$is_middle_class <- ifelse(ot_contract_2022$Cluster == 2, 1, 0)
ot_contract_2023$is_middle_class <- ifelse(ot_contract_2023$Cluster == 1, 1, 0)
ot_contract_2024$is_middle_class <- ifelse(ot_contract_2024$Cluster == 2, 1, 0)


all_ot_contracts <- bind_rows(ot_contract_2016, ot_contract_2017, ot_contract_2018, ot_contract_2019, ot_contract_2020, ot_contract_2021, ot_contract_2022, ot_contract_2023, ot_contract_2024)
all_ot_contracts$second_middle_class <- ifelse(all_ot_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_ot_contracts$APY.as...Of.Cap.At.Signing < 4.0, 1, 0)

all_ot_contracts$third_middle_class <- ifelse(all_ot_contracts$apy_perc > 92 & all_ot_contracts$apy_perc < 98, 1, 0)


all_ot_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_ot_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_ot_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))


write.csv(all_ot_contracts, "all_ot_contracts.csv")

### Offensive guards ----
url1 <- "https://overthecap.com/contract-history/left-guard"
url2 <- "https://overthecap.com/contract-history/right-guard"
# Read the page
page1 <- read_html(url1)
page2 <- read_html(url2)

# Extract table data
lg_contract <- page1 %>%
  html_node("table") %>%    
  html_table(fill = TRUE)  
rg_contract <- page2 %>%
  html_node("table") %>%    
  html_table(fill = TRUE) 
og_contract <- bind_rows(lg_contract, rg_contract)
# Display the first few rows of the data
head(og_contract)

og_contract$Value <- as.numeric(gsub("[\\$,]", "", og_contract$Value))
og_contract$APY <- as.numeric(gsub("[\\$,]", "", og_contract$APY))
og_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", og_contract$Guaranteed))
og_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", og_contract$`Inflated Value`))
og_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", og_contract$`Inflated APY`))
og_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", og_contract$`Inflated Guaranteed`))
og_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", og_contract$`APY as % Of Cap At Signing`))

og_contract$Value <- as.numeric(og_contract$Value)
og_contract$APY <- as.numeric(og_contract$APY)
og_contract$Guaranteed <- as.numeric(og_contract$Guaranteed)
og_contract$`Inflated Value` <- as.numeric(og_contract$`Inflated Value`)
og_contract$`Inflated APY` <- as.numeric(og_contract$`Inflated APY`)
og_contract$`Inflated Guaranteed` <- as.numeric(og_contract$`Inflated Guaranteed`)
og_contract$`APY as % Of Cap At Signing` <- as.numeric(og_contract$`APY as % Of Cap At Signing`)
og_contract$`Year Signed` <- as.numeric(og_contract$`Year Signed`)
write.csv(og_contract, "og_contract.csv")

colnames(og_contract) <- make.names(colnames(og_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
og_contract <- filter(og_contract, og_contract$Year.Signed > 2010)

drafted_players <- read.csv("drafted_players.csv")
drafted_players$Player <- ifelse(drafted_players$Player == "Connor McGovern" & drafted_players$Tm == "DAL", "Connor McGovern (DAL)", drafted_players$Player)
drafted_players$Player <- ifelse(drafted_players$Player == "Jordan Morgan" & drafted_players$Tm == "GB", "Jordan Morgan (GB)", drafted_players$Player)

table(drafted_players$Pos)
drafted_ogs <- filter(drafted_players, Pos == "G" | Pos == "OG" | Pos == "C" | Pos == "OL" | Pos == "T")
og_contract$Player <- ifelse(og_contract$Player == "Connor McGovern" & og_contract$Team == "Cowboys" | og_contract$Player == "Connor McGovern" & og_contract$Team == "Bills", "Connor McGovern (DAL)", og_contract$Player)
og_contract$Player <- ifelse(og_contract$Player == "Jordan Morgan" & og_contract$Team == "Packers", "Jordan Morgan (GB)", og_contract$Player)

og_contract <- left_join(og_contract, drafted_ogs, by = c("Player" = "Player"))
og_contract$is_rookie <- ifelse(og_contract$Year.Signed == og_contract$Draft_year, 1, 0)
og_contract$is_rookie <- ifelse(is.na(og_contract$is_rookie), 0, og_contract$is_rookie)
og_rookies <- filter(og_contract, is_rookie == 1)
og_contract <- filter(og_contract, is_rookie != 1)

og_contract_2014 <- filter(og_contract, Year.Signed == 2014)
og_contract_2015 <- filter(og_contract, Year.Signed == 2015)
og_contract_2016 <- filter(og_contract, Year.Signed == 2016)
og_contract_2017 <- filter(og_contract, Year.Signed == 2017)
og_contract_2018 <- filter(og_contract, Year.Signed == 2018)
og_contract_2019 <- filter(og_contract, Year.Signed == 2019)
og_contract_2020 <- filter(og_contract, Year.Signed == 2020)
og_contract_2021 <- filter(og_contract, Year.Signed == 2021)
og_contract_2022 <- filter(og_contract, Year.Signed == 2022)
og_contract_2023 <- filter(og_contract, Year.Signed == 2023)
og_contract_2024 <- filter(og_contract, Year.Signed == 2024)



og_contract_2024$apy_perc <- ecdf(og_contract_2024$APY)(og_contract_2024$APY) * 100
og_contract_2023$apy_perc <- ecdf(og_contract_2023$APY)(og_contract_2023$APY) * 100
og_contract_2022$apy_perc <- ecdf(og_contract_2023$APY)(og_contract_2022$APY) * 100
og_contract_2021$apy_perc <- ecdf(og_contract_2023$APY)(og_contract_2021$APY) * 100
og_contract_2020$apy_perc <- ecdf(og_contract_2023$APY)(og_contract_2020$APY) * 100
og_contract_2019$apy_perc <- ecdf(og_contract_2023$APY)(og_contract_2019$APY) * 100
og_contract_2018$apy_perc <- ecdf(og_contract_2023$APY)(og_contract_2018$APY) * 100
og_contract_2017$apy_perc <- ecdf(og_contract_2023$APY)(og_contract_2017$APY) * 100
og_contract_2016$apy_perc <- ecdf(og_contract_2023$APY)(og_contract_2016$APY) * 100
og_contract_2015$apy_perc <- ecdf(og_contract_2023$APY)(og_contract_2015$APY) * 100
og_contract_2014$apy_perc <- ecdf(og_contract_2023$APY)(og_contract_2014$APY) * 100

og_contract_2024 <- filter(og_contract_2024, APY > 1210000)
og_contract_2023 <- filter(og_contract_2023, APY > 1165000)
og_contract_2022 <- filter(og_contract_2022, APY > 1120000)
og_contract_2021 <- filter(og_contract_2021, APY > 1075000)
og_contract_2020 <- filter(og_contract_2020, APY > 1050000)
og_contract_2019 <- filter(og_contract_2019, APY > 1030000)
og_contract_2018 <- filter(og_contract_2018, APY > 1015000)
og_contract_2017 <- filter(og_contract_2017, APY > 1000000)
og_contract_2016 <- filter(og_contract_2016, APY > 985000)
og_contract_2015 <- filter(og_contract_2015, APY > 970000)
og_contract_2014 <- filter(og_contract_2014, APY > 955000)

column_to_cluster <- og_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- og_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- og_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- og_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- og_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- og_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- og_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- og_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- og_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- og_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- og_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
og_contract_2014$Cluster <- kmeans_result$cluster






og_contract_2014$is_middle_class <- ifelse(og_contract_2014$Cluster == 1, 1, 0)
og_contract_2015$is_middle_class <- ifelse(og_contract_2015$Cluster == 1, 1, 0)
og_contract_2016$is_middle_class <- ifelse(og_contract_2016$Cluster == 1, 1, 0)
og_contract_2017$is_middle_class <- ifelse(og_contract_2017$Cluster == 2, 1, 0)
og_contract_2018$is_middle_class <- ifelse(og_contract_2018$Cluster == 1, 1, 0)
og_contract_2019$is_middle_class <- ifelse(og_contract_2019$Cluster == 2, 1, 0)
og_contract_2020$is_middle_class <- ifelse(og_contract_2020$Cluster == 2, 1, 0)
og_contract_2021$is_middle_class <- ifelse(og_contract_2021$Cluster == 2, 1, 0)
og_contract_2022$is_middle_class <- ifelse(og_contract_2022$Cluster == 1, 1, 0)
og_contract_2023$is_middle_class <- ifelse(og_contract_2023$Cluster == 1, 1, 0)
og_contract_2024$is_middle_class <- ifelse(og_contract_2024$Cluster == 1, 1, 0)


all_og_contracts <- bind_rows(og_contract_2016, og_contract_2017, og_contract_2018, og_contract_2019, og_contract_2020, og_contract_2021, og_contract_2022, og_contract_2023, og_contract_2024)
all_og_contracts$second_middle_class <- ifelse(all_og_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_og_contracts$APY.as...Of.Cap.At.Signing < 3.5, 1, 0)

all_og_contracts$third_middle_class <- ifelse(all_og_contracts$apy_perc > 92 & all_og_contracts$apy_perc < 98, 1, 0)


all_og_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_og_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_og_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))


write.csv(all_og_contracts, "all_og_contracts.csv")

### Centers ----
url <- "https://overthecap.com/contract-history/center"

# Read the page
page <- read_html(url)

# Extract table data
c_contract <- page %>%
  html_node("table") %>%    
  html_table(fill = TRUE)   

# Display the first few rows of the data
head(c_contract)

c_contract$Value <- as.numeric(gsub("[\\$,]", "", c_contract$Value))
c_contract$APY <- as.numeric(gsub("[\\$,]", "", c_contract$APY))
c_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", c_contract$Guaranteed))
c_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", c_contract$`Inflated Value`))
c_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", c_contract$`Inflated APY`))
c_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", c_contract$`Inflated Guaranteed`))
c_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", c_contract$`APY as % Of Cap At Signing`))

c_contract$Value <- as.numeric(c_contract$Value)
c_contract$APY <- as.numeric(c_contract$APY)
c_contract$Guaranteed <- as.numeric(c_contract$Guaranteed)
c_contract$`Inflated Value` <- as.numeric(c_contract$`Inflated Value`)
c_contract$`Inflated APY` <- as.numeric(c_contract$`Inflated APY`)
c_contract$`Inflated Guaranteed` <- as.numeric(c_contract$`Inflated Guaranteed`)
c_contract$`APY as % Of Cap At Signing` <- as.numeric(c_contract$`APY as % Of Cap At Signing`)
c_contract$`Year Signed` <- as.numeric(c_contract$`Year Signed`)
write.csv(c_contract, "c_contract.csv")

colnames(c_contract) <- make.names(colnames(c_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
c_contract <- filter(c_contract, c_contract$Year.Signed > 2010)






drafted_players$Player <- ifelse(drafted_players$Player == "Connor McGovern" & drafted_players$Tm == "DEN", "Connor McGovern (DEN)", drafted_players$Player)

drafted_cs <- filter(drafted_players, Pos == "C" | Pos == "OG" | Pos == "OL" | Pos == "T" | Pos == "G")
c_contract$Player <- ifelse(c_contract$Player == "Connor McGovern", "Connor McGovern (DEN)", c_contract$Player)
c_contract$Player <- ifelse(c_contract$Player == "Cameron Jurgens", "Cam Jurgens", c_contract$Player)
c_contract$Player <- ifelse(c_contract$Player == "Lloyd Cushenberry", "Lloyd Cushenberry III", c_contract$Player)



c_contract <- left_join(c_contract, drafted_cs, by = c("Player" = "Player"))
c_contract$is_rookie <- ifelse(c_contract$Year.Signed == c_contract$Draft_year, 1, 0)
c_contract$is_rookie <- ifelse(is.na(c_contract$is_rookie), 0, c_contract$is_rookie)
c_rookies <- filter(c_contract, is_rookie == 1)
c_contract <- filter(c_contract, is_rookie != 1)

c_contract_2014 <- filter(c_contract, Year.Signed == 2014)
c_contract_2015 <- filter(c_contract, Year.Signed == 2015)
c_contract_2016 <- filter(c_contract, Year.Signed == 2016)
c_contract_2017 <- filter(c_contract, Year.Signed == 2017)
c_contract_2018 <- filter(c_contract, Year.Signed == 2018)
c_contract_2019 <- filter(c_contract, Year.Signed == 2019)
c_contract_2020 <- filter(c_contract, Year.Signed == 2020)
c_contract_2021 <- filter(c_contract, Year.Signed == 2021)
c_contract_2022 <- filter(c_contract, Year.Signed == 2022)
c_contract_2023 <- filter(c_contract, Year.Signed == 2023)
c_contract_2024 <- filter(c_contract, Year.Signed == 2024)

c_contract_2024$apy_perc <- ecdf(c_contract_2024$APY)(c_contract_2024$APY) * 100
c_contract_2023$apy_perc <- ecdf(c_contract_2023$APY)(c_contract_2023$APY) * 100
c_contract_2022$apy_perc <- ecdf(c_contract_2023$APY)(c_contract_2022$APY) * 100
c_contract_2021$apy_perc <- ecdf(c_contract_2023$APY)(c_contract_2021$APY) * 100
c_contract_2020$apy_perc <- ecdf(c_contract_2023$APY)(c_contract_2020$APY) * 100
c_contract_2019$apy_perc <- ecdf(c_contract_2023$APY)(c_contract_2019$APY) * 100
c_contract_2018$apy_perc <- ecdf(c_contract_2023$APY)(c_contract_2018$APY) * 100
c_contract_2017$apy_perc <- ecdf(c_contract_2023$APY)(c_contract_2017$APY) * 100
c_contract_2016$apy_perc <- ecdf(c_contract_2023$APY)(c_contract_2016$APY) * 100
c_contract_2015$apy_perc <- ecdf(c_contract_2023$APY)(c_contract_2015$APY) * 100
c_contract_2014$apy_perc <- ecdf(c_contract_2023$APY)(c_contract_2014$APY) * 100

c_contract_2024 <- filter(c_contract_2024, APY > 1210000)
c_contract_2023 <- filter(c_contract_2023, APY > 1165000)
c_contract_2022 <- filter(c_contract_2022, APY > 1120000)
c_contract_2021 <- filter(c_contract_2021, APY > 1075000)
c_contract_2020 <- filter(c_contract_2020, APY > 1050000)
c_contract_2019 <- filter(c_contract_2019, APY > 1030000)
c_contract_2018 <- filter(c_contract_2018, APY > 1015000)
c_contract_2017 <- filter(c_contract_2017, APY > 1000000)
c_contract_2016 <- filter(c_contract_2016, APY > 985000)
c_contract_2015 <- filter(c_contract_2015, APY > 970000)
c_contract_2014 <- filter(c_contract_2014, APY > 955000)

column_to_cluster <- c_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- c_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- c_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- c_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- c_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- c_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- c_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- c_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- c_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- c_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- c_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
c_contract_2014$Cluster <- kmeans_result$cluster






c_contract_2014$is_middle_class <- ifelse(c_contract_2014$Cluster == 2, 1, 0)
c_contract_2015$is_middle_class <- ifelse(c_contract_2015$Cluster == 1, 1, 0)
c_contract_2016$is_middle_class <- ifelse(c_contract_2016$Cluster == 2, 1, 0)
c_contract_2017$is_middle_class <- ifelse(c_contract_2017$Cluster == 2, 1, 0)
c_contract_2018$is_middle_class <- ifelse(c_contract_2018$Cluster == 1, 1, 0)
c_contract_2019$is_middle_class <- ifelse(c_contract_2019$Cluster == 1, 1, 0)
c_contract_2020$is_middle_class <- ifelse(c_contract_2020$Cluster == 2, 1, 0)
c_contract_2021$is_middle_class <- ifelse(c_contract_2021$Cluster == 1, 1, 0)
c_contract_2022$is_middle_class <- ifelse(c_contract_2022$Cluster == 1, 1, 0)
c_contract_2023$is_middle_class <- ifelse(c_contract_2023$Cluster == 1, 1, 0)
c_contract_2024$is_middle_class <- ifelse(c_contract_2024$Cluster == 1, 1, 0)


all_c_contracts <- bind_rows(c_contract_2015, c_contract_2016, c_contract_2017, c_contract_2018, c_contract_2019, c_contract_2020, c_contract_2021, c_contract_2022, c_contract_2023, c_contract_2024)
all_c_contracts$second_middle_class <- ifelse(all_c_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_c_contracts$APY.as...Of.Cap.At.Signing < 2, 1, 0)

all_c_contracts$third_middle_class <- ifelse(all_c_contracts$apy_perc > 92 & all_c_contracts$apy_perc < 98, 1, 0)


all_c_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_c_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_c_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))


write.csv(all_c_contracts, "all_c_contracts.csv")

### Cornerbacks ----
url <- "https://overthecap.com/contract-history/cornerback"

# Read the page
page <- read_html(url)

# Extract table data
cb_contract <- page %>%
  html_node("table") %>%    
  html_table(fill = TRUE)   

# Display the first few rows of the data
head(cb_contract)

cb_contract$Value <- as.numeric(gsub("[\\$,]", "", cb_contract$Value))
cb_contract$APY <- as.numeric(gsub("[\\$,]", "", cb_contract$APY))
cb_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", cb_contract$Guaranteed))
cb_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", cb_contract$`Inflated Value`))
cb_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", cb_contract$`Inflated APY`))
cb_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", cb_contract$`Inflated Guaranteed`))
cb_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", cb_contract$`APY as % Of Cap At Signing`))

cb_contract$Value <- as.numeric(cb_contract$Value)
cb_contract$APY <- as.numeric(cb_contract$APY)
cb_contract$Guaranteed <- as.numeric(cb_contract$Guaranteed)
cb_contract$`Inflated Value` <- as.numeric(cb_contract$`Inflated Value`)
cb_contract$`Inflated APY` <- as.numeric(cb_contract$`Inflated APY`)
cb_contract$`Inflated Guaranteed` <- as.numeric(cb_contract$`Inflated Guaranteed`)
cb_contract$`APY as % Of Cap At Signing` <- as.numeric(cb_contract$`APY as % Of Cap At Signing`)
cb_contract$`Year Signed` <- as.numeric(cb_contract$`Year Signed`)
write.csv(cb_contract, "cb_contract.csv")

colnames(cb_contract) <- make.names(colnames(cb_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
cb_contract <- filter(cb_contract, cb_contract$Year.Signed > 2010)





table(drafted_players$Pos)
drafted_cbs <- filter(drafted_players, Pos == "CB" | Pos == "DB" | Pos == "S" | Pos == "SAF")
cb_contract$Player <- ifelse(cb_contract$Player == "A.J. Terrell", "AJ Terrell", cb_contract$Player)
cb_contract$Player <- ifelse(cb_contract$Player == "Ahmad Gardner", "Sauce Gardner", cb_contract$Player)

cb_contract <- left_join(cb_contract, drafted_cbs, by = c("Player" = "Player"))
cb_contract$is_rookie <- ifelse(cb_contract$Year.Signed == cb_contract$Draft_year, 1, 0)
cb_contract$is_rookie <- ifelse(is.na(cb_contract$is_rookie), 0, cb_contract$is_rookie)
cb_rookies <- filter(cb_contract, is_rookie == 1)
cb_contract <- filter(cb_contract, is_rookie != 1)

cb_contract_2014 <- filter(cb_contract, Year.Signed == 2014)
cb_contract_2015 <- filter(cb_contract, Year.Signed == 2015)
cb_contract_2016 <- filter(cb_contract, Year.Signed == 2016)
cb_contract_2017 <- filter(cb_contract, Year.Signed == 2017)
cb_contract_2018 <- filter(cb_contract, Year.Signed == 2018)
cb_contract_2019 <- filter(cb_contract, Year.Signed == 2019)
cb_contract_2020 <- filter(cb_contract, Year.Signed == 2020)
cb_contract_2021 <- filter(cb_contract, Year.Signed == 2021)
cb_contract_2022 <- filter(cb_contract, Year.Signed == 2022)
cb_contract_2023 <- filter(cb_contract, Year.Signed == 2023)
cb_contract_2024 <- filter(cb_contract, Year.Signed == 2024)



cb_contract_2024$apy_perc <- ecdf(cb_contract_2024$APY)(cb_contract_2024$APY) * 100
cb_contract_2023$apy_perc <- ecdf(cb_contract_2023$APY)(cb_contract_2023$APY) * 100
cb_contract_2022$apy_perc <- ecdf(cb_contract_2023$APY)(cb_contract_2022$APY) * 100
cb_contract_2021$apy_perc <- ecdf(cb_contract_2023$APY)(cb_contract_2021$APY) * 100
cb_contract_2020$apy_perc <- ecdf(cb_contract_2023$APY)(cb_contract_2020$APY) * 100
cb_contract_2019$apy_perc <- ecdf(cb_contract_2023$APY)(cb_contract_2019$APY) * 100
cb_contract_2018$apy_perc <- ecdf(cb_contract_2023$APY)(cb_contract_2018$APY) * 100
cb_contract_2017$apy_perc <- ecdf(cb_contract_2023$APY)(cb_contract_2017$APY) * 100
cb_contract_2016$apy_perc <- ecdf(cb_contract_2023$APY)(cb_contract_2016$APY) * 100
cb_contract_2015$apy_perc <- ecdf(cb_contract_2023$APY)(cb_contract_2015$APY) * 100
cb_contract_2014$apy_perc <- ecdf(cb_contract_2023$APY)(cb_contract_2014$APY) * 100

cb_contract_2024 <- filter(cb_contract_2024, APY > 1210000)
cb_contract_2023 <- filter(cb_contract_2023, APY > 1165000)
cb_contract_2022 <- filter(cb_contract_2022, APY > 1120000)
cb_contract_2021 <- filter(cb_contract_2021, APY > 1075000)
cb_contract_2020 <- filter(cb_contract_2020, APY > 1050000)
cb_contract_2019 <- filter(cb_contract_2019, APY > 1030000)
cb_contract_2018 <- filter(cb_contract_2018, APY > 1015000)
cb_contract_2017 <- filter(cb_contract_2017, APY > 1000000)
cb_contract_2016 <- filter(cb_contract_2016, APY > 985000)
cb_contract_2015 <- filter(cb_contract_2015, APY > 970000)
cb_contract_2014 <- filter(cb_contract_2014, APY > 955000)

column_to_cluster <- cb_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- cb_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- cb_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- cb_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- cb_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- cb_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- cb_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- cb_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- cb_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- cb_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- cb_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
cb_contract_2014$Cluster <- kmeans_result$cluster






cb_contract_2014$is_middle_class <- ifelse(cb_contract_2014$Cluster == 1, 1, 0)
cb_contract_2015$is_middle_class <- ifelse(cb_contract_2015$Cluster == 2, 1, 0)
cb_contract_2016$is_middle_class <- ifelse(cb_contract_2016$Cluster == 2, 1, 0)
cb_contract_2017$is_middle_class <- ifelse(cb_contract_2017$Cluster == 2, 1, 0)
cb_contract_2018$is_middle_class <- ifelse(cb_contract_2018$Cluster == 2, 1, 0)
cb_contract_2019$is_middle_class <- ifelse(cb_contract_2019$Cluster == 2, 1, 0)
cb_contract_2020$is_middle_class <- ifelse(cb_contract_2020$Cluster == 2, 1, 0)
cb_contract_2021$is_middle_class <- ifelse(cb_contract_2021$Cluster == 2, 1, 0)
cb_contract_2022$is_middle_class <- ifelse(cb_contract_2022$Cluster == 2, 1, 0)
cb_contract_2023$is_middle_class <- ifelse(cb_contract_2023$Cluster == 2, 1, 0)
cb_contract_2024$is_middle_class <- ifelse(cb_contract_2024$Cluster == 2, 1, 0)


all_cb_contracts <- bind_rows(cb_contract_2016, cb_contract_2017, cb_contract_2018, cb_contract_2019, cb_contract_2020, cb_contract_2021, cb_contract_2022, cb_contract_2023, cb_contract_2024)
all_cb_contracts$second_middle_class <- ifelse(all_cb_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_cb_contracts$APY.as...Of.Cap.At.Signing < 3.5, 1, 0)

all_cb_contracts$third_middle_class <- ifelse(all_cb_contracts$apy_perc > 92 & all_cb_contracts$apy_perc < 98, 1, 0)


all_cb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_cb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_cb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))


write.csv(all_cb_contracts, "all_cb_contracts.csv")

### Edge rushers ----
url <- "https://overthecap.com/contract-history/edge-rusher"

# Read the page
page <- read_html(url)

# Extract table data
edge_contract <- page %>%
  html_node("table") %>%    
  html_table(fill = TRUE)   

# Display the first few rows of the data
head(edge_contract)

edge_contract$Value <- as.numeric(gsub("[\\$,]", "", edge_contract$Value))
edge_contract$APY <- as.numeric(gsub("[\\$,]", "", edge_contract$APY))
edge_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", edge_contract$Guaranteed))
edge_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", edge_contract$`Inflated Value`))
edge_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", edge_contract$`Inflated APY`))
edge_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", edge_contract$`Inflated Guaranteed`))
edge_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", edge_contract$`APY as % Of Cap At Signing`))

edge_contract$Value <- as.numeric(edge_contract$Value)
edge_contract$APY <- as.numeric(edge_contract$APY)
edge_contract$Guaranteed <- as.numeric(edge_contract$Guaranteed)
edge_contract$`Inflated Value` <- as.numeric(edge_contract$`Inflated Value`)
edge_contract$`Inflated APY` <- as.numeric(edge_contract$`Inflated APY`)
edge_contract$`Inflated Guaranteed` <- as.numeric(edge_contract$`Inflated Guaranteed`)
edge_contract$`APY as % Of Cap At Signing` <- as.numeric(edge_contract$`APY as % Of Cap At Signing`)
edge_contract$`Year Signed` <- as.numeric(edge_contract$`Year Signed`)
write.csv(edge_contract, "edge_contract.csv")

colnames(edge_contract) <- make.names(colnames(edge_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
edge_contract <- filter(edge_contract, edge_contract$Year.Signed > 2010)





table(drafted_players$Pos)
drafted_edges <- filter(drafted_players, Pos == "DE" | Pos == "DL" | Pos == "DT" | Pos == "LB" | Pos == "OLB")
edge_contract$Player <- ifelse(edge_contract$Player == "Will Anderson Jr.", "Will Anderson", edge_contract$Player)
edge_contract$Player <- ifelse(edge_contract$Player == "Joshua Uche", "Josh Uche", edge_contract$Player)
edge_contract$Player <- ifelse(edge_contract$Player == "Carlos Basham Jr.", "Boogie Basham", edge_contract$Player)
edge_contract$Player <- ifelse(edge_contract$Player == "George Karlaftis", "George Karlaftis III", edge_contract$Player)
edge_contract$Player <- ifelse(edge_contract$Player == "Will McDonald IV", "Will McDonald", edge_contract$Player)
edge_contract$Player <- ifelse(edge_contract$Player == "B.J. Ojulari", "BJ Ojulari", edge_contract$Player)
edge_contract$Player <- ifelse(edge_contract$Player == "D.J. Johnson", "DJ Johnson", edge_contract$Player)

edge_contract <- left_join(edge_contract, drafted_edges, by = c("Player" = "Player"))
edge_contract$is_rookie <- ifelse(edge_contract$Year.Signed == edge_contract$Draft_year, 1, 0)
edge_contract$is_rookie <- ifelse(is.na(edge_contract$is_rookie), 0, edge_contract$is_rookie)
edge_rookies <- filter(edge_contract, is_rookie == 1)
edge_contract <- filter(edge_contract, is_rookie != 1)

edge_contract_2014 <- filter(edge_contract, Year.Signed == 2014)
edge_contract_2015 <- filter(edge_contract, Year.Signed == 2015)
edge_contract_2016 <- filter(edge_contract, Year.Signed == 2016)
edge_contract_2017 <- filter(edge_contract, Year.Signed == 2017)
edge_contract_2018 <- filter(edge_contract, Year.Signed == 2018)
edge_contract_2019 <- filter(edge_contract, Year.Signed == 2019)
edge_contract_2020 <- filter(edge_contract, Year.Signed == 2020)
edge_contract_2021 <- filter(edge_contract, Year.Signed == 2021)
edge_contract_2022 <- filter(edge_contract, Year.Signed == 2022)
edge_contract_2023 <- filter(edge_contract, Year.Signed == 2023)
edge_contract_2024 <- filter(edge_contract, Year.Signed == 2024)



edge_contract_2024$apy_perc <- ecdf(edge_contract_2024$APY)(edge_contract_2024$APY) * 100
edge_contract_2023$apy_perc <- ecdf(edge_contract_2023$APY)(edge_contract_2023$APY) * 100
edge_contract_2022$apy_perc <- ecdf(edge_contract_2023$APY)(edge_contract_2022$APY) * 100
edge_contract_2021$apy_perc <- ecdf(edge_contract_2023$APY)(edge_contract_2021$APY) * 100
edge_contract_2020$apy_perc <- ecdf(edge_contract_2023$APY)(edge_contract_2020$APY) * 100
edge_contract_2019$apy_perc <- ecdf(edge_contract_2023$APY)(edge_contract_2019$APY) * 100
edge_contract_2018$apy_perc <- ecdf(edge_contract_2023$APY)(edge_contract_2018$APY) * 100
edge_contract_2017$apy_perc <- ecdf(edge_contract_2023$APY)(edge_contract_2017$APY) * 100
edge_contract_2016$apy_perc <- ecdf(edge_contract_2023$APY)(edge_contract_2016$APY) * 100
edge_contract_2015$apy_perc <- ecdf(edge_contract_2023$APY)(edge_contract_2015$APY) * 100
edge_contract_2014$apy_perc <- ecdf(edge_contract_2023$APY)(edge_contract_2014$APY) * 100

edge_contract_2024 <- filter(edge_contract_2024, APY > 1210000)
edge_contract_2023 <- filter(edge_contract_2023, APY > 1165000)
edge_contract_2022 <- filter(edge_contract_2022, APY > 1120000)
edge_contract_2021 <- filter(edge_contract_2021, APY > 1075000)
edge_contract_2020 <- filter(edge_contract_2020, APY > 1050000)
edge_contract_2019 <- filter(edge_contract_2019, APY > 1030000)
edge_contract_2018 <- filter(edge_contract_2018, APY > 1015000)
edge_contract_2017 <- filter(edge_contract_2017, APY > 1000000)
edge_contract_2016 <- filter(edge_contract_2016, APY > 985000)
edge_contract_2015 <- filter(edge_contract_2015, APY > 970000)
edge_contract_2014 <- filter(edge_contract_2014, APY > 955000)

column_to_cluster <- edge_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- edge_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- edge_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- edge_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- edge_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- edge_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- edge_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- edge_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- edge_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- edge_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- edge_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
edge_contract_2014$Cluster <- kmeans_result$cluster






edge_contract_2014$is_middle_class <- ifelse(edge_contract_2014$Cluster == 1, 1, 0)
edge_contract_2015$is_middle_class <- ifelse(edge_contract_2015$Cluster == 2, 1, 0)
edge_contract_2016$is_middle_class <- ifelse(edge_contract_2016$Cluster == 2, 1, 0)
edge_contract_2017$is_middle_class <- ifelse(edge_contract_2017$Cluster == 2, 1, 0)
edge_contract_2018$is_middle_class <- ifelse(edge_contract_2018$Cluster == 2, 1, 0)
edge_contract_2019$is_middle_class <- ifelse(edge_contract_2019$Cluster == 1, 1, 0)
edge_contract_2020$is_middle_class <- ifelse(edge_contract_2020$Cluster == 2, 1, 0)
edge_contract_2021$is_middle_class <- ifelse(edge_contract_2021$Cluster == 1, 1, 0)
edge_contract_2022$is_middle_class <- ifelse(edge_contract_2022$Cluster == 2, 1, 0)
edge_contract_2023$is_middle_class <- ifelse(edge_contract_2023$Cluster == 2, 1, 0)
edge_contract_2024$is_middle_class <- ifelse(edge_contract_2024$Cluster == 1, 1, 0)


all_edge_contracts <- bind_rows(edge_contract_2016, edge_contract_2017, edge_contract_2018, edge_contract_2019, edge_contract_2020, edge_contract_2021, edge_contract_2022, edge_contract_2023, edge_contract_2024)
all_edge_contracts$second_middle_class <- ifelse(all_edge_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_edge_contracts$APY.as...Of.Cap.At.Signing < 5, 1, 0)

all_edge_contracts$third_middle_class <- ifelse(all_edge_contracts$apy_perc > 92 & all_edge_contracts$apy_perc < 98, 1, 0)


all_edge_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_edge_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_edge_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))


write.csv(all_edge_contracts, "all_edge_contracts.csv")
### Interior Defensive linemen ----
url <- "https://overthecap.com/contract-history/interior-defensive-line"

# Read the page
page <- read_html(url)

# Extract table data
idl_contract <- page %>%
  html_node("table") %>%    
  html_table(fill = TRUE)   

# Display the first few rows of the data
head(idl_contract)

idl_contract$Value <- as.numeric(gsub("[\\$,]", "", idl_contract$Value))
idl_contract$APY <- as.numeric(gsub("[\\$,]", "", idl_contract$APY))
idl_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", idl_contract$Guaranteed))
idl_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", idl_contract$`Inflated Value`))
idl_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", idl_contract$`Inflated APY`))
idl_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", idl_contract$`Inflated Guaranteed`))
idl_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", idl_contract$`APY as % Of Cap At Signing`))

idl_contract$Value <- as.numeric(idl_contract$Value)
idl_contract$APY <- as.numeric(idl_contract$APY)
idl_contract$Guaranteed <- as.numeric(idl_contract$Guaranteed)
idl_contract$`Inflated Value` <- as.numeric(idl_contract$`Inflated Value`)
idl_contract$`Inflated APY` <- as.numeric(idl_contract$`Inflated APY`)
idl_contract$`Inflated Guaranteed` <- as.numeric(idl_contract$`Inflated Guaranteed`)
idl_contract$`APY as % Of Cap At Signing` <- as.numeric(idl_contract$`APY as % Of Cap At Signing`)
idl_contract$`Year Signed` <- as.numeric(idl_contract$`Year Signed`)
write.csv(idl_contract, "idl_contract.csv")

colnames(idl_contract) <- make.names(colnames(idl_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
idl_contract <- filter(idl_contract, idl_contract$Year.Signed > 2010)





table(drafted_players$Pos)
drafted_idls <- filter(drafted_players, Pos == "DE" | Pos == "DL" | Pos == "DT" | Pos == "OLB" | Pos == "NT")
idl_contract$Player <- ifelse(idl_contract$Player == "Davon Hamilton", "DaVon Hamilton", idl_contract$Player)
idl_contract$Player <- ifelse(idl_contract$Player == "Kris Jenkins Jr.", "Kris Jenkins", idl_contract$Player)
idl_contract$Player <- ifelse(idl_contract$Player == "Michael Hall Jr.", "Michael Hall", idl_contract$Player)

idl_contract <- left_join(idl_contract, drafted_idls, by = c("Player" = "Player"))
idl_contract$is_rookie <- ifelse(idl_contract$Year.Signed == idl_contract$Draft_year, 1, 0)
idl_contract$is_rookie <- ifelse(is.na(idl_contract$is_rookie), 0, idl_contract$is_rookie)
idl_rookies <- filter(idl_contract, is_rookie == 1)
idl_contract <- filter(idl_contract, is_rookie != 1)

idl_contract_2014 <- filter(idl_contract, Year.Signed == 2014)
idl_contract_2015 <- filter(idl_contract, Year.Signed == 2015)
idl_contract_2016 <- filter(idl_contract, Year.Signed == 2016)
idl_contract_2017 <- filter(idl_contract, Year.Signed == 2017)
idl_contract_2018 <- filter(idl_contract, Year.Signed == 2018)
idl_contract_2019 <- filter(idl_contract, Year.Signed == 2019)
idl_contract_2020 <- filter(idl_contract, Year.Signed == 2020)
idl_contract_2021 <- filter(idl_contract, Year.Signed == 2021)
idl_contract_2022 <- filter(idl_contract, Year.Signed == 2022)
idl_contract_2023 <- filter(idl_contract, Year.Signed == 2023)
idl_contract_2024 <- filter(idl_contract, Year.Signed == 2024)



idl_contract_2024$apy_perc <- ecdf(idl_contract_2024$APY)(idl_contract_2024$APY) * 100
idl_contract_2023$apy_perc <- ecdf(idl_contract_2023$APY)(idl_contract_2023$APY) * 100
idl_contract_2022$apy_perc <- ecdf(idl_contract_2023$APY)(idl_contract_2022$APY) * 100
idl_contract_2021$apy_perc <- ecdf(idl_contract_2023$APY)(idl_contract_2021$APY) * 100
idl_contract_2020$apy_perc <- ecdf(idl_contract_2023$APY)(idl_contract_2020$APY) * 100
idl_contract_2019$apy_perc <- ecdf(idl_contract_2023$APY)(idl_contract_2019$APY) * 100
idl_contract_2018$apy_perc <- ecdf(idl_contract_2023$APY)(idl_contract_2018$APY) * 100
idl_contract_2017$apy_perc <- ecdf(idl_contract_2023$APY)(idl_contract_2017$APY) * 100
idl_contract_2016$apy_perc <- ecdf(idl_contract_2023$APY)(idl_contract_2016$APY) * 100
idl_contract_2015$apy_perc <- ecdf(idl_contract_2023$APY)(idl_contract_2015$APY) * 100
idl_contract_2014$apy_perc <- ecdf(idl_contract_2023$APY)(idl_contract_2014$APY) * 100

idl_contract_2024 <- filter(idl_contract_2024, APY > 1210000)
idl_contract_2023 <- filter(idl_contract_2023, APY > 1165000)
idl_contract_2022 <- filter(idl_contract_2022, APY > 1120000)
idl_contract_2021 <- filter(idl_contract_2021, APY > 1075000)
idl_contract_2020 <- filter(idl_contract_2020, APY > 1050000)
idl_contract_2019 <- filter(idl_contract_2019, APY > 1030000)
idl_contract_2018 <- filter(idl_contract_2018, APY > 1015000)
idl_contract_2017 <- filter(idl_contract_2017, APY > 1000000)
idl_contract_2016 <- filter(idl_contract_2016, APY > 985000)
idl_contract_2015 <- filter(idl_contract_2015, APY > 970000)
idl_contract_2014 <- filter(idl_contract_2014, APY > 955000)

column_to_cluster <- idl_contract_2024$APY.as...Of.Cap.At.Signing

set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- idl_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- idl_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- idl_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- idl_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- idl_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- idl_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- idl_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- idl_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- idl_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- idl_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
idl_contract_2014$Cluster <- kmeans_result$cluster






idl_contract_2014$is_middle_class <- ifelse(idl_contract_2014$Cluster == 1, 1, 0)
idl_contract_2015$is_middle_class <- ifelse(idl_contract_2015$Cluster == 2, 1, 0)
idl_contract_2016$is_middle_class <- ifelse(idl_contract_2016$Cluster == 2, 1, 0)
idl_contract_2017$is_middle_class <- ifelse(idl_contract_2017$Cluster == 2, 1, 0)
idl_contract_2018$is_middle_class <- ifelse(idl_contract_2018$Cluster == 2, 1, 0)
idl_contract_2019$is_middle_class <- ifelse(idl_contract_2019$Cluster == 2, 1, 0)
idl_contract_2020$is_middle_class <- ifelse(idl_contract_2020$Cluster == 1, 1, 0)
idl_contract_2021$is_middle_class <- ifelse(idl_contract_2021$Cluster == 1, 1, 0)
idl_contract_2022$is_middle_class <- ifelse(idl_contract_2022$Cluster == 1, 1, 0)
idl_contract_2023$is_middle_class <- ifelse(idl_contract_2023$Cluster == 1, 1, 0)
idl_contract_2024$is_middle_class <- ifelse(idl_contract_2024$Cluster == 1, 1, 0)


all_idl_contracts <- bind_rows(idl_contract_2016, idl_contract_2017, idl_contract_2018, idl_contract_2019, idl_contract_2020, idl_contract_2021, idl_contract_2022, idl_contract_2023, idl_contract_2024)
all_idl_contracts$second_middle_class <- ifelse(all_idl_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_idl_contracts$APY.as...Of.Cap.At.Signing < 4.5, 1, 0)

all_idl_contracts$third_middle_class <- ifelse(all_idl_contracts$apy_perc > 92 & all_idl_contracts$apy_perc < 98, 1, 0)


all_idl_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_idl_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_idl_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))


write.csv(all_idl_contracts, "all_idl_contracts.csv")
### Linebackers ----
url <- "https://overthecap.com/contract-history/linebacker"

# Read the page
page <- read_html(url)

# Extract table data
lb_contract <- page %>%
  html_node("table") %>%    
  html_table(fill = TRUE)   

# Display the first few rows of the data
head(lb_contract)

lb_contract$Value <- as.numeric(gsub("[\\$,]", "", lb_contract$Value))
lb_contract$APY <- as.numeric(gsub("[\\$,]", "", lb_contract$APY))
lb_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", lb_contract$Guaranteed))
lb_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", lb_contract$`Inflated Value`))
lb_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", lb_contract$`Inflated APY`))
lb_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", lb_contract$`Inflated Guaranteed`))
lb_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", lb_contract$`APY as % Of Cap At Signing`))

lb_contract$Value <- as.numeric(lb_contract$Value)
lb_contract$APY <- as.numeric(lb_contract$APY)
lb_contract$Guaranteed <- as.numeric(lb_contract$Guaranteed)
lb_contract$`Inflated Value` <- as.numeric(lb_contract$`Inflated Value`)
lb_contract$`Inflated APY` <- as.numeric(lb_contract$`Inflated APY`)
lb_contract$`Inflated Guaranteed` <- as.numeric(lb_contract$`Inflated Guaranteed`)
lb_contract$`APY as % Of Cap At Signing` <- as.numeric(lb_contract$`APY as % Of Cap At Signing`)
lb_contract$`Year Signed` <- as.numeric(lb_contract$`Year Signed`)
write.csv(lb_contract, "lb_contract.csv")

colnames(lb_contract) <- make.names(colnames(lb_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
lb_contract <- filter(lb_contract, lb_contract$Year.Signed > 2010)





table(drafted_players$Pos)
drafted_lbs <- filter(drafted_players, Pos == "LB" | Pos == "ILB" | Pos == "OLB" | Pos == "DE" | POS == "DB")
lb_contract$Player <- ifelse(lb_contract$Player == "Devin Bush", "Devin Bush Jr.", lb_contract$Player)
lb_contract$Player <- ifelse(lb_contract$Player == "Willie Gay, Jr.", "Willie Gay Jr.", lb_contract$Player)

lb_contract <- left_join(lb_contract, drafted_lbs, by = c("Player" = "Player"))
lb_contract$is_rookie <- ifelse(lb_contract$Year.Signed == lb_contract$Draft_year, 1, 0)
lb_contract$is_rookie <- ifelse(is.na(lb_contract$is_rookie), 0, lb_contract$is_rookie)
lb_rookies <- filter(lb_contract, is_rookie == 1)
lb_contract <- filter(lb_contract, is_rookie != 1)

lb_contract_2014 <- filter(lb_contract, Year.Signed == 2014)
lb_contract_2015 <- filter(lb_contract, Year.Signed == 2015)
lb_contract_2016 <- filter(lb_contract, Year.Signed == 2016)
lb_contract_2017 <- filter(lb_contract, Year.Signed == 2017)
lb_contract_2018 <- filter(lb_contract, Year.Signed == 2018)
lb_contract_2019 <- filter(lb_contract, Year.Signed == 2019)
lb_contract_2020 <- filter(lb_contract, Year.Signed == 2020)
lb_contract_2021 <- filter(lb_contract, Year.Signed == 2021)
lb_contract_2022 <- filter(lb_contract, Year.Signed == 2022)
lb_contract_2023 <- filter(lb_contract, Year.Signed == 2023)
lb_contract_2024 <- filter(lb_contract, Year.Signed == 2024)



lb_contract_2024$apy_perc <- ecdf(lb_contract_2024$APY)(lb_contract_2024$APY) * 100
lb_contract_2023$apy_perc <- ecdf(lb_contract_2023$APY)(lb_contract_2023$APY) * 100
lb_contract_2022$apy_perc <- ecdf(lb_contract_2023$APY)(lb_contract_2022$APY) * 100
lb_contract_2021$apy_perc <- ecdf(lb_contract_2023$APY)(lb_contract_2021$APY) * 100
lb_contract_2020$apy_perc <- ecdf(lb_contract_2023$APY)(lb_contract_2020$APY) * 100
lb_contract_2019$apy_perc <- ecdf(lb_contract_2023$APY)(lb_contract_2019$APY) * 100
lb_contract_2018$apy_perc <- ecdf(lb_contract_2023$APY)(lb_contract_2018$APY) * 100
lb_contract_2017$apy_perc <- ecdf(lb_contract_2023$APY)(lb_contract_2017$APY) * 100
lb_contract_2016$apy_perc <- ecdf(lb_contract_2023$APY)(lb_contract_2016$APY) * 100
lb_contract_2015$apy_perc <- ecdf(lb_contract_2023$APY)(lb_contract_2015$APY) * 100
lb_contract_2014$apy_perc <- ecdf(lb_contract_2023$APY)(lb_contract_2014$APY) * 100

lb_contract_2024 <- filter(lb_contract_2024, APY > 1210000)
lb_contract_2023 <- filter(lb_contract_2023, APY > 1165000)
lb_contract_2022 <- filter(lb_contract_2022, APY > 1120000)
lb_contract_2021 <- filter(lb_contract_2021, APY > 1075000)
lb_contract_2020 <- filter(lb_contract_2020, APY > 1050000)
lb_contract_2019 <- filter(lb_contract_2019, APY > 1030000)
lb_contract_2018 <- filter(lb_contract_2018, APY > 1015000)
lb_contract_2017 <- filter(lb_contract_2017, APY > 1000000)
lb_contract_2016 <- filter(lb_contract_2016, APY > 985000)
lb_contract_2015 <- filter(lb_contract_2015, APY > 970000)
lb_contract_2014 <- filter(lb_contract_2014, APY > 955000)

column_to_cluster <- lb_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- lb_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- lb_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- lb_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- lb_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- lb_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- lb_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- lb_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- lb_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- lb_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- lb_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
lb_contract_2014$Cluster <- kmeans_result$cluster






lb_contract_2014$is_middle_class <- ifelse(lb_contract_2014$Cluster == 1, 1, 0)
lb_contract_2015$is_middle_class <- ifelse(lb_contract_2015$Cluster == 2, 1, 0)
lb_contract_2016$is_middle_class <- ifelse(lb_contract_2016$Cluster == 1, 1, 0)
lb_contract_2017$is_middle_class <- ifelse(lb_contract_2017$Cluster == 2, 1, 0)
lb_contract_2018$is_middle_class <- ifelse(lb_contract_2018$Cluster == 2, 1, 0)
lb_contract_2019$is_middle_class <- ifelse(lb_contract_2019$Cluster == 2, 1, 0)
lb_contract_2020$is_middle_class <- ifelse(lb_contract_2020$Cluster == 1, 1, 0)
lb_contract_2021$is_middle_class <- ifelse(lb_contract_2021$Cluster == 2, 1, 0)
lb_contract_2022$is_middle_class <- ifelse(lb_contract_2022$Cluster == 2, 1, 0)
lb_contract_2023$is_middle_class <- ifelse(lb_contract_2023$Cluster == 1, 1, 0)
lb_contract_2024$is_middle_class <- ifelse(lb_contract_2024$Cluster == 2, 1, 0)


all_lb_contracts <- bind_rows(lb_contract_2016, lb_contract_2017, lb_contract_2018, lb_contract_2019, lb_contract_2020, lb_contract_2021, lb_contract_2022, lb_contract_2023, lb_contract_2024)
all_lb_contracts$second_middle_class <- ifelse(all_lb_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_lb_contracts$APY.as...Of.Cap.At.Signing < 2.0, 1, 0)

all_lb_contracts$third_middle_class <- ifelse(all_lb_contracts$apy_perc > 92 & all_lb_contracts$apy_perc < 98, 1, 0)


all_lb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_lb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_lb_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))
all_lb_contracts <- filter(all_lb_contracts, Player != "Divine Deablo")
all_lb_contracts <- filter(all_lb_contracts, Player != "Marte Mapu")


write.csv(all_lb_contracts, "all_lb_contracts.csv")
### Safeties ----
url <- "https://overthecap.com/contract-history/safety"

# Read the page
page <- read_html(url)

# Extract table data
saf_contract <- page %>%
  html_node("table") %>%    
  html_table(fill = TRUE)   

# Display the first few rows of the data
head(saf_contract)

saf_contract$Value <- as.numeric(gsub("[\\$,]", "", saf_contract$Value))
saf_contract$APY <- as.numeric(gsub("[\\$,]", "", saf_contract$APY))
saf_contract$Guaranteed <- as.numeric(gsub("[\\$,]", "", saf_contract$Guaranteed))
saf_contract$`Inflated Value` <- as.numeric(gsub("[\\$,]", "", saf_contract$`Inflated Value`))
saf_contract$`Inflated APY` <- as.numeric(gsub("[\\$,]", "", saf_contract$`Inflated APY`))
saf_contract$`Inflated Guaranteed` <- as.numeric(gsub("[\\$,]", "", saf_contract$`Inflated Guaranteed`))
saf_contract$`APY as % Of Cap At Signing` <- as.numeric(gsub("%", "", saf_contract$`APY as % Of Cap At Signing`))

saf_contract$Value <- as.numeric(saf_contract$Value)
saf_contract$APY <- as.numeric(saf_contract$APY)
saf_contract$Guaranteed <- as.numeric(saf_contract$Guaranteed)
saf_contract$`Inflated Value` <- as.numeric(saf_contract$`Inflated Value`)
saf_contract$`Inflated APY` <- as.numeric(saf_contract$`Inflated APY`)
saf_contract$`Inflated Guaranteed` <- as.numeric(saf_contract$`Inflated Guaranteed`)
saf_contract$`APY as % Of Cap At Signing` <- as.numeric(saf_contract$`APY as % Of Cap At Signing`)
saf_contract$`Year Signed` <- as.numeric(saf_contract$`Year Signed`)
write.csv(saf_contract, "saf_contract.csv")

colnames(saf_contract) <- make.names(colnames(saf_contract), unique = TRUE)

# Apply the filter to select rows where 'Year Signed' is greater than 2010
cb_contract <- filter(cb_contract, cb_contract$Year.Signed > 2010)





table(drafted_players$Pos)
drafted_safs <- filter(drafted_players, Pos == "CB" | Pos == "DB" | Pos == "S" | Pos == "SAF" | Pos == "LB" | Pos == "ILB" | Pos == "OLB")
saf_contract$Player <- ifelse(saf_contract$Player == "Antoine Winfield, Jr.", "Antoine Winfield Jr.", saf_contract$Player)
saf_contract$Player <- ifelse(saf_contract$Player == "Trevon Moehrig", "Tre'Von Moehrig", saf_contract$Player)
saf_contract$Player <- ifelse(saf_contract$Player == "J.T. Woods", "JT Woods", saf_contract$Player)

saf_contract <- left_join(saf_contract, drafted_safs, by = c("Player" = "Player"))
saf_contract$is_rookie <- ifelse(saf_contract$Year.Signed == saf_contract$Draft_year, 1, 0)
saf_contract$is_rookie <- ifelse(is.na(saf_contract$is_rookie), 0, saf_contract$is_rookie)
saf_rookies <- filter(saf_contract, is_rookie == 1)
saf_contract <- filter(saf_contract, is_rookie != 1)

saf_contract_2014 <- filter(saf_contract, Year.Signed == 2014)
saf_contract_2015 <- filter(saf_contract, Year.Signed == 2015)
saf_contract_2016 <- filter(saf_contract, Year.Signed == 2016)
saf_contract_2017 <- filter(saf_contract, Year.Signed == 2017)
saf_contract_2018 <- filter(saf_contract, Year.Signed == 2018)
saf_contract_2019 <- filter(saf_contract, Year.Signed == 2019)
saf_contract_2020 <- filter(saf_contract, Year.Signed == 2020)
saf_contract_2021 <- filter(saf_contract, Year.Signed == 2021)
saf_contract_2022 <- filter(saf_contract, Year.Signed == 2022)
saf_contract_2023 <- filter(saf_contract, Year.Signed == 2023)
saf_contract_2024 <- filter(saf_contract, Year.Signed == 2024)



saf_contract_2024$apy_perc <- ecdf(saf_contract_2024$APY)(saf_contract_2024$APY) * 100
saf_contract_2023$apy_perc <- ecdf(saf_contract_2023$APY)(saf_contract_2023$APY) * 100
saf_contract_2022$apy_perc <- ecdf(saf_contract_2023$APY)(saf_contract_2022$APY) * 100
saf_contract_2021$apy_perc <- ecdf(saf_contract_2023$APY)(saf_contract_2021$APY) * 100
saf_contract_2020$apy_perc <- ecdf(saf_contract_2023$APY)(saf_contract_2020$APY) * 100
saf_contract_2019$apy_perc <- ecdf(saf_contract_2023$APY)(saf_contract_2019$APY) * 100
saf_contract_2018$apy_perc <- ecdf(saf_contract_2023$APY)(saf_contract_2018$APY) * 100
saf_contract_2017$apy_perc <- ecdf(saf_contract_2023$APY)(saf_contract_2017$APY) * 100
saf_contract_2016$apy_perc <- ecdf(saf_contract_2023$APY)(saf_contract_2016$APY) * 100
saf_contract_2015$apy_perc <- ecdf(saf_contract_2023$APY)(saf_contract_2015$APY) * 100
saf_contract_2014$apy_perc <- ecdf(saf_contract_2023$APY)(saf_contract_2014$APY) * 100

saf_contract_2024 <- filter(saf_contract_2024, APY > 1210000)
saf_contract_2023 <- filter(saf_contract_2023, APY > 1165000)
saf_contract_2022 <- filter(saf_contract_2022, APY > 1120000)
saf_contract_2021 <- filter(saf_contract_2021, APY > 1075000)
saf_contract_2020 <- filter(saf_contract_2020, APY > 1050000)
saf_contract_2019 <- filter(saf_contract_2019, APY > 1030000)
saf_contract_2018 <- filter(saf_contract_2018, APY > 1015000)
saf_contract_2017 <- filter(saf_contract_2017, APY > 1000000)
saf_contract_2016 <- filter(saf_contract_2016, APY > 985000)
saf_contract_2015 <- filter(saf_contract_2015, APY > 970000)
saf_contract_2014 <- filter(saf_contract_2014, APY > 955000)

column_to_cluster <- saf_contract_2024$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2024$Cluster <- kmeans_result$cluster

column_to_cluster <- saf_contract_2023$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2023$Cluster <- kmeans_result$cluster

column_to_cluster <- saf_contract_2022$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2022$Cluster <- kmeans_result$cluster

column_to_cluster <- saf_contract_2021$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2021$Cluster <- kmeans_result$cluster

column_to_cluster <- saf_contract_2020$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2020$Cluster <- kmeans_result$cluster

column_to_cluster <- saf_contract_2019$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2019$Cluster <- kmeans_result$cluster

column_to_cluster <- saf_contract_2018$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2018$Cluster <- kmeans_result$cluster

column_to_cluster <- saf_contract_2017$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2017$Cluster <- kmeans_result$cluster

column_to_cluster <- saf_contract_2016$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2016$Cluster <- kmeans_result$cluster

column_to_cluster <- saf_contract_2015$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2015$Cluster <- kmeans_result$cluster

column_to_cluster <- saf_contract_2014$APY.as...Of.Cap.At.Signing
set.seed(123)  
kmeans_result <- kmeans(column_to_cluster, centers = 2, nstart = 25)
saf_contract_2014$Cluster <- kmeans_result$cluster






saf_contract_2014$is_middle_class <- ifelse(saf_contract_2014$Cluster == 1, 1, 0)
saf_contract_2015$is_middle_class <- ifelse(saf_contract_2015$Cluster == 2, 1, 0)
saf_contract_2016$is_middle_class <- ifelse(saf_contract_2016$Cluster == 2, 1, 0)
saf_contract_2017$is_middle_class <- ifelse(saf_contract_2017$Cluster == 1, 1, 0)
saf_contract_2018$is_middle_class <- ifelse(saf_contract_2018$Cluster == 1, 1, 0)
saf_contract_2019$is_middle_class <- ifelse(saf_contract_2019$Cluster == 2, 1, 0)
saf_contract_2020$is_middle_class <- ifelse(saf_contract_2020$Cluster == 2, 1, 0)
saf_contract_2021$is_middle_class <- ifelse(saf_contract_2021$Cluster == 1, 1, 0)
saf_contract_2022$is_middle_class <- ifelse(saf_contract_2022$Cluster == 2, 1, 0)
saf_contract_2023$is_middle_class <- ifelse(saf_contract_2023$Cluster == 1, 1, 0)
saf_contract_2024$is_middle_class <- ifelse(saf_contract_2024$Cluster == 1, 1, 0)


all_saf_contracts <- bind_rows(saf_contract_2016, saf_contract_2017, saf_contract_2018, saf_contract_2019, saf_contract_2020, saf_contract_2021, saf_contract_2022, saf_contract_2023, saf_contract_2024)
all_saf_contracts$second_middle_class <- ifelse(all_saf_contracts$APY.as...Of.Cap.At.Signing > 0.7 & all_saf_contracts$APY.as...Of.Cap.At.Signing < 2.0, 1, 0)

all_saf_contracts$third_middle_class <- ifelse(all_saf_contracts$apy_perc > 92 & all_saf_contracts$apy_perc < 98, 1, 0)


all_saf_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def1 = mean(is_middle_class))

all_saf_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def2 = mean(second_middle_class))

all_saf_contracts %>%
  group_by(Year.Signed) %>%
  summarize(def3 = mean(third_middle_class))


write.csv(all_saf_contracts, "all_saf_contracts.csv")

###All together----
all_c_contracts$position <- "C"
all_cb_contracts$position <- "CB"
all_edge_contracts$position <- "EDGE"
all_idl_contracts$position <- "IDL"
all_lb_contracts$position <- "LB"
all_og_contracts$position <- "OG"
all_ot_contracts$position <- "OT"
all_qb_contracts$position <- "QB"
all_rb_contracts$position <- "RB"
all_saf_contracts$position <- "SAF"
all_te_contracts$position <- "TE"
all_wr_contracts$position <- "WR"

all_contracts <- bind_rows(all_c_contracts, all_cb_contracts, all_edge_contracts, all_idl_contracts, all_lb_contracts, all_og_contracts, all_ot_contracts, all_qb_contracts, all_rb_contracts, all_saf_contracts, all_te_contracts, all_wr_contracts)
all_contracts <- select(all_contracts, -X, -X.1, -X.2, -...5, -...9, -...11)
write.csv(all_contracts, "all_contracts.csv")
