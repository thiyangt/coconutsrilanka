library(tidyverse)
library(here)
library(GGally)

# Read data
coconut <- read_csv(here("coconut.csv"))
head(coconut)
dim(coconut)
colnames(coconut)

coconut$water <- coconut$FullWeight - (coconut$WeightWithOutWater)
coconut$coconut_flesh <- coconut$FullWeight - ( coconut$water + coconut$ShellWeight)

colnames(coconut)
head(coconut)

coconut$Palapi <- factor(coconut$Palapi)
coconut$Kalati <- factor(coconut$Kalati)

model <- lm(coconut_flesh ~ FullWeight +
              Hcircumference + Vcircumference +
              VCircumferencewithHusk + Palapi +
              Kalati, data=coconut)
summary(model)


# Scatterplot matrix
coconut[, c(1, 4:7, 11, 12)] %>% ggpairs()
