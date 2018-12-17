library(dplyr)        # Data wrangling taskick
library(tidyr)        # Data wrangling taskick
library(ggplot2)      # Plotting/ Data visualization taskick
library(lubridate)    # Date/ Time manipulation
library(magrittr)     # Pipe operator
library(corrplot)     # Correlation function
library(formattable)  # Data Preview section
library(knitr)        # Data Preview section
library(broom)        # Glance function
library(boot)         # Bootstrapping function
library(glmnet)       # Cross validation function
library(mgcv)         # GAM function
library(verification) # ROC plots
library(rpart)        # Classification Tree
library(rpart.plot)   # Classification Tree plot
library(caret)        # Confusion Matrix function
library(randomForest) # Random Forest function

#Data Import
kick <- read.csv("Projects.csv")
str(kick)

#Data type conversions
kick$ID <- as.character(kick$ID)
kick$name <- as.character(kick$name)
kick$deadline <- as.Date(kick$deadline)
kick$launched <- as.Date(kick$launched)

#Re-ordering
kick <- kick[, c(1:4, 12, 8, 6, 5, 7, 9, 11, 13:15, 10)]

#Data subsetting
ggplot(kick, aes(state)) +
  geom_bar() +
  ylab("# of Projects") + xlab("Final State") +
  ggtitle("Final State of the Kickicktarter projects")

#Success & Failure subsetting
kick.proj <- kick %>% filter(state == "failed" | state == "successful") 
kick.proj$state <- as.character(kick.proj$state)
kick.proj$state <- as.factor(kick.proj$state)
summary(kick.proj$state)

#Checking missing values
sum(is.na(kick.proj)) 
colSums(is.na(kick.proj))

# As it's a very small value (0.063%), I'm deleting the values
missing <- kick.proj %>% filter(is.na(usd.pledged)) 
kick.proj <- na.omit(kick.proj)

#Feauture creation
kick.proj$duration <- as.numeric(kick.proj$deadline - kick.proj$launched)
kick.proj <- kick.proj %>% 
  separate(col = "deadline", into = c("deadline_year", "deadline_month", "deadline_day"), sep = "-") %>%
  separate(col = "launched_year", into = c("launched_year", "launched_year_month", "launched_year_day"), sep = "-")

#Feauture Engineering
kick.proj$country <- as.character(kick.proj$country)
kick.proj$country <- as.factor(kick.proj$country)

head(kick.proj$country)

# Reducing levels in Country
kick.proj$country <- as.character(kick.proj$country)
kick.proj$country[kick.proj$country %in% c("JP", "LU", "AT", "HK", "SG", "BE", "CH", "IE", "NO", "DK", 
                                       "MX", "NZ", "SE", "ES", "IT", "NL", "FR", "DE")] <- "Other"
kick.proj$country <- as.factor(kick.proj$country)

#Country based
levels(kick.proj$country) # 5 levels

sort(round(prop.table(table(kick.proj$country)),2))

head(kick.proj$launched)
#launched_year year
levels(kick.proj$launched) # 9 levels

colnames(kick.proj)

# Reducing levels in launched_year Year
kick.proj$launched <- as.character(kick.proj$launched)
kick.proj$launched[kick.proj$launched %in% c("2009", "2010", "2011")] <- "Before 2012"
kick.proj$launched <- as.factor(kick.proj$launched)

#c. Currency:
levels(kick.proj$currency)

sort(round(prop.table(table(kick.proj$currency)),2))
# Reducing levels in Country
kick.proj$currency <- as.character(kick.proj$currency)
kick.proj$currency[kick.proj$currency %in% c("JPY", "HKD", "SGD", "CHF", "NOK", "DKK", "MXN", "NZD", 
                                         "SEK")] <- "Other"
kick.proj$currency <- as.factor(kick.proj$currency)

round(prop.table(table(kick.proj$currency)),2)

#Data Preview
head(kick.proj, n = 50) %>%
  formattable() %>%
  as.datatable(options = list(dom = 't',scrollX = TRUE,scrollCollapse = TRUE))

#Data Description
variable.type <- lapply(kick.proj, class)
variable.description <- c("ID of Kickicktarter Project", "Name of Kickicktarter Project", 
                          "Category of Kickicktarter Project", "Main Category of Kickicktarter Project",
                          "Country where Kickicktarter Project was launched_year",
                          "Year when Kickicktarter Project was launched_year",
                          "Month when Kickicktarter Project was launched_year",
                          "Year when Kickicktarter Project ended",
                          "Month when Kickicktarter Project ended",
                          "Active Duration of the Kickicktarter Project", "Currency of amount pledged",
                          "Goal of Kickicktarter Project in original currency", 
                          "Total amount pledged in original currency", "Number of Backers", 
                          "Conversion in USD of the pledged column (conversion done by Kickicktarter)",
                          "Conversion in USD of the pledged column (conversion from Fixer.io API)",
                          "Conversion in USD of the goal column (conversion from Fixer.io API)",
                          "Final State of Kickicktarter Project")
variable.name <- colnames(kick.proj)

kick_datadesc <- as_data_frame(cbind(variable.name, variable.type, variable.description))
colnames(kick_datadesc) <- c("Variable Name","Data Type","Variable Description")

kable(kick_datadesc)

#Cleaned data structure
str(kick.proj)

#Univariate Analysis
# Final State of the kick project
ggplot(kick.proj, aes(state, fill = state)) +
  geom_bar() +
  ylab("# of Projects") + xlab("Final State") +
  theme(legend.position = "bottom") +
  ggtitle("Final State of the Kickicktarter projects")

# 1. Main Categories present in the dataset
p2 <- ggplot(kick.proj, aes(x = main_category, fill = kick.proj$state)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("") +
  ggtitle("Main categories of the kick Projects") 
p2

# Main categories of the kick Projects - percent to whole
p1 <- kick.proj %>% 
  count(main_category) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(reorder(main_category, pct), pct)) +
  geom_col() +
  coord_flip() + 
  ylab("% of projects") + xlab("") +
  ggtitle("Main categories of the kick Projects") 

gridExtra::grid.arrange(p1, p2, ncol = 2)

# Final State of the kick project
ggplot(kick.proj, aes(x = country, fill = kick.proj$state)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma) +
  ylab("Number of projects") + xlab("") +
  ggtitle("kick Projects by country") 

#launched_year year based
ggplot(kick.proj, aes(x = launched, fill = kick.proj$state)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("Year launched_year") +
  ggtitle("kick projects by Year")

#Deadlin year
kick.proj$deadline
ggplot(kick.proj, aes(x = deadline, fill = kick.proj$state)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("Deadline Year") +
  ggtitle("kick projects by Deadline")
#Currency
ggplot(kick.proj, aes(x = currency, fill = kick.proj$state)) +
  geom_bar() +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "bottom") +
  ylab("Number of projects") + xlab("") +
  ggtitle("Currency of the kick Projects") 

#Amount pledged

quantile(kick.proj$usd_pledged_real, probs = seq(from = 0, to = 1, by = .2))

#Multi-variate analysis
#Category by year
ggplot(kick.proj, aes(launched, fill = kick.proj$state)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ main_category) +
  ylab("Number of Projects") + xlab("launched_year Year") +
  ggtitle("kick projects launched_year over time by Category")

 #Country by year
ggplot(kick.proj, aes(launched, fill = kick.proj$state)) +
  geom_bar() +
  theme(legend.position = "bottom") +
  facet_wrap( ~ country) +
  ylab("Number of Projects") + xlab("launched_year Year") +
  ggtitle("kickstarter projects launched_year over time by Country") 

#Correlations
# Correlation between all numerical variables

corMat <- cor(kick.proj[, c(9,10,11:13)])
corrplot.mixed(corMat,tl.pos = "lt")

sapply(kick.proj, class)
class(kick.proj[, c(15, 8:13)])
#kick.proj %>% 
#  +   summarise_all(typeof) %>% 
#  +   gather()

#Modeling
# create Training - Test and Validation set (60 - 20 - 20%)
set.seed(11)
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(seq(nrow(kick.proj)), 
               nrow(kick.proj)*cumsum(c(0,spec)), 
               labels = names(spec)))
gc()
memory.limit()
res = split(kick.proj, g)
train.data <- res$train
test.data <- res$test
validation.data <- res$validate

library(h2o)
h2o.init()
#Logistic Regression
nullmodel <- glm(state~1, data = train.data, family = "binomial")

predictors <- c("main_category","launched","backers","usd_pledged_real","usd_goal_real")
response <- "state"
df <- as.h2o(train.data)
class(train.data)
file.info("train.data")
memory.limit()
?memory.limit
memory.size()
head(df)
fullmodel <- h2o.glm(x = predictors, y= response, family = "binomial",training_frame= df, lambda = 0, compute_p_values = TRUE)

glm_kick <- h2o.glm(x = predictors, y= response, training_frame = df, family = "binomial", lambda = 0, compute_p_values = TRUE)

#Unable to share 246 GB ,So had to use h2o framework
#trn_prd <- h2o.predict(glm_kick,newdata = df[,[x])


