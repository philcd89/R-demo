
# Setup -------------------------------------------------------------------

#install.packages('tidyverse')

library(tidyverse)

# I can write a comment this way!!


# Let's play with variables! ----------------------------------------------

a <- 1

b <- 2

c <- a + b

d <- "Hello world"


# Data Types --------------------------------------------------------------

integer <- 8675309

double <- 3.14159

string <- "Motor Development Lab"

logical <- TRUE

# Vectors: an R object consisting of a set of single items OF THE SAME TYPE
vector <- c(1, 2, 3, 4, 5)
vector2 <- c("Simone", "Gill", "is", "our", "PI")
vector3 <- c("I am", 31, "years old") # RStudio tries to help you out a bit...

# Lists: Vectors of vectors - can be DIFFERENT types
myList <- list(c("a", "b", "c"), c(1, 2, 3), "do re mi") # baby you and me girllll

# Matrices
myMatrix <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)

# Other data types: Arrays, Factors, Dataframes


# Indexing ----------------------------------------------------------------

# Identifying or selecting components of larger datasets

vector2[1]
vector2[1:2]

myList[1]
myList[[1]]

myMatrix[1,2]
myMatrix[1,]
myMatrix[1:2,2:3]

# Modify an element of an object at a specific index
myMatrix[1,3] <- integer


# Let's play with some data -----------------------------------------------

demoData <- read.table("demoData.txt")

names(demoData)
str(demoData)
head(demoData)
tail(demoData)
summary(demoData)

# Delete a variable (column)
demoData$Group <- NULL
any(names(demoData) == "Group")

demoData[, which(names(demoData)=="Trial")] <- NULL
any(names(demoData) == "Trial")

demoData <- demoData %>% # USE THE TIDYVERSE
  select(-c(Group, Trial))

# Create a variable (column)
demoData$money <- "dolla dolla billz"
demoData$Group <- ifelse(demoData$Group_N == 0, "Controls", "Patients")

#....use the TIDYVERSE
demoData <- demoData %>%
  mutate(Trial = Trial_N %% 100)
demoData <- demoData %>%
  mutate(Trial2 = as.integer(substr(as.character(Trial_N), 3, 3)))

# Set some factors
demoData$Sub_N = factor(demoData$Sub_N)
demoData$Gender = factor(demoData$Gender)
demoData$Group = factor(demoData$Group)
demoData$Trial = factor(demoData$Trial)

nlevels(demoData$Sub_N)
levels(demoData$Sub_N)

# Frequency plots ---------------------------------------------------------

ggplot(data = demoData, aes(x = Age))+
  geom_histogram()

demographics <- demoData %>%
  select(Sub_N, Gender, Age, Height, Weight, BMI, Group) %>%
  distinct()
head(demographics)

Age_boxplot <- ggplot(data = demographics, aes(x = Age))+
  geom_histogram(binwidth = 5)

BMI_boxplot <- ggplot(data = demographics, aes(x = BMI))+
  geom_histogram()


# Boxplots, barplots, and grouped data ------------------------------------

# Boxplot
ggplot(data = demographics, aes(x = Group, y = BMI))+
  geom_boxplot()

ggplot(data = demographics, aes(x = Group, y = BMI))+
  geom_boxplot()+
  geom_dotplot(binaxis = "y", stackdir = "center")

ggplot(data = demographics, aes(x = Group, y = BMI))+
  geom_boxplot(color = "blue", fill = "orange")+
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "#64FF33")

#Barplot
ggplot(data = demographics, aes(x = Group, y = BMI))+
  geom_bar(stat = "summary", fun = "mean") +
  geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.5)

# Crossbars
ggplot(data = demographics, aes(x = Group, y = BMI))+
  geom_crossbar(stat = "summary", fun = "mean")+
  geom_jitter()


# Line plots --------------------------------------------------------------

FGA_data <- demoData %>%
  select(Sub_N, Group, FGA_1, FGA_2, FGA_3, FGA_4, FGA_5, FGA_6, FGA_7, FGA_8, FGA_9, FGA_10, FGA_Total) %>%
  gather("Item", "FGA_score", FGA_1:FGA_10) %>%
  group_by(Group, Item) %>%
  summarise_all(mean)
FGA_data$Item = factor(FGA_data$Item, levels = c('FGA_1', 'FGA_2', 'FGA_3', 'FGA_4', 'FGA_5', 'FGA_6', 'FGA_7', 'FGA_8', 'FGA_9', 'FGA_10'))

ggplot(data = FGA_data, aes(x = Item, y = FGA_score, group = Group, color = Group, fill = Group, shape = Group))+
  geom_line(size = 1)+
  geom_point(size = 3)+
  ylim(c(0,3))+
  theme_bw()+
  theme(axis.text = element_text(size = 14))



# Scatterplots, Correlations, and Regressions -----------------------------

demoData_IB <- demoData %>%
  filter(Condition_N == 1) %>%
  group_by(Sub_N, Group)%>%
  summarize_all(mean)

ggplot(data = demoData_IB, aes(x = StepVel, y = StepLength))+
  geom_point()

ggplot(data = demoData_IB, aes(x = StepVel, y = StepLength, color = Group))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x)

ggplot(data = demoData_IB, aes(x = StepVel, y = StepLength, color = Group))+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, aes(fill = Group), alpha = 0.2)

#faceting
ggplot(data = demoData_IB, aes(x = StepVel, y = StepLength, color = Group))+
  facet_grid(Group~.)+
  geom_point()+
  geom_smooth(method = "lm", formula = y~x, aes(fill = Group), alpha = 0.2)


# Stats!!! ----------------------------------------------------------------

# Isolate some data
Control_StepVel = subset(demoData_IB$StepVel, demoData_IB$Group == "Controls")
Obese_StepVel = subset(demoData_IB$StepVel, demoData_IB$Group == "Patients")
Obese_StepLength = subset(demoData_IB$StepLength, demoData_IB$Group == "Patients")

# T-test
t.test(Control_StepVel, Obese_StepVel, paired = FALSE, var.equal = TRUE)

#Correlation
cor.test(Obese_StepLength, Obese_StepVel, method = "pearson")

#Simple Regression
Obese_LengthVel_Reg <- lm(Obese_StepVel ~ Obese_StepLength)
summary(Obese_LengthVel_Reg)

# https://www.r-graph-gallery.com/all-graphs.html
