library(tidyverse)
library(GGally)


train <- read_delim(file = "Challenge_Data/Data/train.txt", delim = " ", col_names = c("unit_number", "time", "op_setting1", "op_setting2", "op_setting3", "sensor1", "sensor2", "sensor3", "sensor4", "sensor5", "sensor6", "sensor7", "sensor 8", "sensor9", "sensor10", "sensor11", "sensor12", "sensor13", "sensor14", "sensor15", "sensor16", "sensor17", "sensor18", "sensor19", "sensor20", "sensor21"))

train <- train %>%
  select(unit_number:sensor21)

total_life <- train %>%
  group_by(unit_number) %>%
  summarize(unit_life = n_distinct(time))

train_full = merge(x = train, y = total_life, by = "unit_number")

train_full <- train_full %>%
  mutate(RUL = unit_life - time)

train_full <- train_full %>%
  select(-unit_life)

corr_mat <- round(cor(train_full),2)


# Install and load reshape2 package
install.packages("reshape2")
library(reshape2)

# creating correlation matrix

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)

