library(tidyverse)
library(ggplot2)
library(ggtext)
library(gt)
library(caret)
library(xgboost)
# Load in the data from the previous script
cbs_masterfile <- read.csv("cbs_masterfile.csv")
cs_masterfile <- read.csv("cs_masterfile.csv")
edges_masterfile <- read.csv("edges_masterfile.csv")
idls_masterfile <- read.csv("idls_masterfile.csv")
ogs_masterfile <- read.csv("ogs_masterfile.csv")
ots_masterfile <- read.csv("ots_masterfile.csv")
qbs_masterfile <- read.csv("qbs_masterfile.csv")
rbs_masterfile <- read.csv("rbs_masterfile.csv")
safs_masterfile <- read.csv("safs_masterfile.csv")
tes_masterfile <- read.csv("tes_masterfile.csv")
wrs_masterfile <- read.csv("wrs_masterfile.csv")

# Select only the stats we want

# Model 1
cbs_model <- select(cbs_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV, Stat_1, Stat_2)
edges_model <- select(edges_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV, Stat_1, Stat_2)
idls_model <- select(idls_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV, Stat_1, Stat_2)
qbs_model <- select(qbs_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV, Stat_1, Stat_2)
rbs_model <- select(rbs_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV, Stat_1, Stat_2)
safs_model <- select(safs_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV, Stat_1, Stat_2)
tes_model <- select(tes_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV, Stat_1, Stat_2)
wrs_model <- select(wrs_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV, Stat_1, Stat_2)

# Model 2
cs_model <- select(cs_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV)
ogs_model <- select(ogs_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV)
ots_model <- select(ots_masterfile, Player, Team, Year_signed = Year.Signed, second_middle_class, apy_perc_of_cap_at_signing = APY.as...Of.Cap.At.Signing, Position = Pos.y, Age = Age.y, GS, Snap_perc, AV)

# Select the columns in the dataset that are either the features or the label
cbs_model <- cbs_model[complete.cases(cbs_model[, 7:12]), ]
edges_model <- edges_model[complete.cases(edges_model[, 7:12]), ]
idls_model <- idls_model[complete.cases(idls_model[, 7:12]), ]
qbs_model <- qbs_model[complete.cases(qbs_model[, 7:12]), ]
rbs_model <- rbs_model[complete.cases(rbs_model[, 7:12]), ]
safs_model <- safs_model[complete.cases(safs_model[, 7:12]), ]
tes_model <- tes_model[complete.cases(tes_model[, 7:12]), ]
wrs_model <- wrs_model[complete.cases(wrs_model[, 7:12]), ]

cs_model <- cs_model[complete.cases(cs_model[, 7:10]), ]
ogs_model <- ogs_model[complete.cases(ogs_model[, 7:10]), ]
ots_model <- ots_model[complete.cases(ots_model[, 7:10]), ]

# Standardizing values
cbs_model$Stat_1 <- ecdf(cbs_model$Stat_1)(cbs_model$Stat_1) * 100
cbs_model$Stat_2 <- ecdf(cbs_model$Stat_2)(cbs_model$Stat_2) * 100
edges_model$Stat_1 <- ecdf(edges_model$Stat_1)(edges_model$Stat_1) * 100
edges_model$Stat_2 <- ecdf(edges_model$Stat_2)(edges_model$Stat_2) * 100
idls_model$Stat_1 <- ecdf(idls_model$Stat_1)(idls_model$Stat_1) * 100
idls_model$Stat_2 <- ecdf(idls_model$Stat_2)(idls_model$Stat_2) * 100
qbs_model$Stat_1 <- ecdf(qbs_model$Stat_1)(qbs_model$Stat_1) * 100
qbs_model$Stat_2 <- ecdf(qbs_model$Stat_2)(qbs_model$Stat_2) * 100
rbs_model$Stat_1 <- ecdf(rbs_model$Stat_1)(rbs_model$Stat_1) * 100
rbs_model$Stat_2 <- ecdf(rbs_model$Stat_2)(rbs_model$Stat_2) * 100
safs_model$Stat_1 <- ecdf(safs_model$Stat_1)(safs_model$Stat_1) * 100
safs_model$Stat_2 <- ecdf(safs_model$Stat_2)(safs_model$Stat_2) * 100
tes_model$Stat_1 <- ecdf(tes_model$Stat_1)(tes_model$Stat_1) * 100
tes_model$Stat_2 <- ecdf(tes_model$Stat_2)(tes_model$Stat_2) * 100
wrs_model$Stat_1 <- ecdf(wrs_model$Stat_1)(wrs_model$Stat_1) * 100
wrs_model$Stat_2 <- ecdf(wrs_model$Stat_2)(wrs_model$Stat_2) * 100

# Separate into two models
model_1 <- bind_rows(cbs_model, edges_model, idls_model, qbs_model, rbs_model, safs_model, tes_model, wrs_model)
model_2 <- bind_rows(cs_model, ogs_model, ots_model)

# Starting with model

# Convert to factor variables and add dummy variables to the dataset
model_1$Position <- as.factor(model_1$Position)
model_1_data <- model_1[5:12]
dmy <- dummyVars(" ~ .", data = model_1_data)
model_1_final <- data.frame(predict(dmy, newdata = model_1_data))

# Train and test split
smp_size <- floor(0.80 * nrow(model_1_final))
set.seed(617) 
indices <- sample(seq_len(nrow(model_1_final)), size = smp_size)
train1 <- (as.matrix(model_1_final[indices, ]))
test1 <- (as.matrix(model_1_final[-indices, ]))
str(train1)

# Actual model

xgb_model1 <-
  xgboost(
    data = train1[, 2:15],
    label = train1[, 1],
    nrounds = 100,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 3,
    eta = .1,
    verbose = 1
  )   

# See the main factos
vip(xgb_model1)

# See the RMSE
pred_xgb <- predict(xgb_model1, test1[, 2:15])
yhat <- pred_xgb
y <- test1[, 1]
postResample(yhat, y)

# Now we fit the model onto the original dataset.
model1_matrix <- as.matrix(model_1_final)
final_predictions <- predict(xgb_model1, model1_matrix[, 2:15])
model1_df <- as.data.frame(model1_matrix)
final_predictions <- as.data.frame(final_predictions)
results_model1 <- cbind(model1_df, final_predictions)
model1_desc <- model_1[1:4]
results_model1 <- cbind(model1_desc, results_model1)


# Repeat everything for the 2nd model
model_2$Position <- as.factor(model_2$Position)
model_2_data <- model_2[5:10]
dmy <- dummyVars(" ~ .", data = model_2_data)
model_2_final <- data.frame(predict(dmy, newdata = model_2_data))

smp_size <- floor(0.80 * nrow(model_2_final))
set.seed(617) 
indices <- sample(seq_len(nrow(model_2_final)), size = smp_size)
train2 <- (as.matrix(model_2_final[indices, ]))
test2 <- (as.matrix(model_2_final[-indices, ]))
str(train2)

xgb_model2 <-
  xgboost(
    data = train2[, 2:8],
    label = train2[, 1],
    nrounds = 100,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 2,
    eta = .1,
    verbose = 1
  )   

vip(xgb_model2)
pred_xgb <- predict(xgb_model2, test2[, 2:8])

yhat <- pred_xgb
y <- test2[, 1]
postResample(yhat, y)

model2_matrix <- as.matrix(model_2_final)
final_predictions <- predict(xgb_model2, model2_matrix[, 2:8])

model2_df <- as.data.frame(model2_matrix)
final_predictions <- as.data.frame(final_predictions)

results_model2 <- cbind(model2_df, final_predictions)
model2_desc <- model_2[1:4]
results_model2 <- cbind(model2_desc, results_model2)

# Add a residual variable
results_model1$residual <- results_model1$apy_perc_of_cap_at_signing - results_model1$final_predictions  
results_model2$residual <- results_model2$apy_perc_of_cap_at_signing - results_model2$final_predictions

# Convert the dummy variables back to one column
columns_to_process <- 6:13
positions <- colnames(results_model1)[columns_to_process]
results_model1$Position <- apply(results_model1[, columns_to_process], 1, function(row) positions[which.max(row)])
results_model1 <- results_model1[, -columns_to_process]
results_model1$Position <- gsub("Position\\.", "", results_model1$Position)

columns_to_process <- 6:8
positions <- colnames(results_model2)[columns_to_process]
results_model2$Position <- apply(results_model2[, columns_to_process], 1, function(row) positions[which.max(row)])
results_model2 <- results_model2[, -columns_to_process]
results_model2$Position <- gsub("Position\\.", "", results_model2$Position)

#Bind the datasets from both models together

results <- bind_rows(results_model1, results_model2)

# Some tables and results
m <- results %>% 
  group_by(Position) %>%
  summarize(SOE = mean(residual))
gt(m)
results %>% 
  group_by(second_middle_class) %>%
  summarize(voe = mean(residual))

ggplot(results, aes(x = apy_perc_of_cap_at_signing, y = final_predictions)) +
  geom_point()

results_sample <- results[sample(nrow(results), 500), ]

ggplot(results_sample, aes(x = final_predictions, y = apy_perc_of_cap_at_signing)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#1b9983", fill = "#80a8a1") +
  labs( title = "<span style = 'font-size:25pt;'>**Visualizing the effectiveness of the CFT model across all positions**</span> <br> 
       <span style = 'font-size:17pt;'> Being **above** the line indicates the player was paid above our predictions. *Random sample of 500 datapoints* </span>", x = "Expected APYPTOS", y = "Observed APYPTOS") +
  theme_minimal() + theme(title =element_text(family = "serif"), axis.title.y = element_text(size = 17),  axis.text = element_text(family = "serif", color = "black", size = 13), plot.title = element_textbox_simple(size = 18, lineheight = 1, padding = margin(0,0,5,0)), axis.title.x = element_text(size = 17, family = "serif" ),     panel.background = element_rect(fill = "gray85", color = NA),  # Light blue-gray background
                          plot.background = element_rect(fill = "white", color = NA)) 
results$Class <- ifelse(results$second_middle_class == 1, "Middle Class", "Not Middle Class")
rm <- filter(results, Class == "Middle Class")
m <- rm %>%
  group_by(Class, Year_signed) %>%
  summarize(SOE = mean(residual)) %>%
  mutate(SOE = round(SOE, 3)) # Round VOE to 3 decimal places

# Create gt table with conditional formatting
gt_table <- m %>%
  gt() %>%
  tab_header(
    title = md("**Is the middle class undervalued?**"),
    subtitle = md("**It probably is!**")
  ) %>%
  data_color(
    columns = vars(SOE),
    colors = scales::col_numeric(
      palette = c("lightcoral", "white","lightgreen"),
      domain = c(-0.6, 0.6) # Adjust domain range as needed
    )
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.font.names = "serif",
    row.striping.include_table_body = TRUE
  )

# Display the table
gt_table
library(ggrepel)

ggplot(m, aes(x = Year_signed, y = SOE)) +
  geom_point(color = ifelse(m$Year_signed < 2022, "#1b9983AA", "#922724AA"), size = 3) +
  geom_line(color = ifelse(m$Year_signed < 2022, "#1b9983", "#922724"), size = 1) +# Add lines to connect the dots
  scale_y_continuous(limits = c(-0.6, 0.005)) + # Set y-axis limits
  geom_hline(yintercept = 0, color = "blue") +
  labs( title = "<span style = 'font-size:25pt;'>**The middle class is experiencing unprecedented levels of *underpayment***</span> <br> 
       <span style = 'font-size:17pt;'> Looking at the average SOE per year for middle class contracts </span>", x = " ", 
        y = "Salary Over Expected (Middle Class contracts)") +
  theme_minimal() + theme(title =element_text(family = "serif"), axis.title.y = element_text(size = 17),  axis.text = element_text(family = "serif", color = "black", size = 13), plot.title = element_textbox_simple(size = 18, lineheight = 1, padding = margin(0,0,5,0)), axis.title.x = element_text(size = 17, family = "serif" ),     panel.background = element_rect(fill = "gray85", color = NA),  # Light blue-gray background
                          plot.background = element_rect(fill = "white", color = NA)) 

