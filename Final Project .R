# Talal Khodr Final Project for Machine Learning with Prof:Alfonso [UCLA Extension]
# POKEMON DATA SET 

# Importing the necessary libraries
install.packages("readr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("DataExplorer")
install.packages("caret")
install.packages("rattle")
install.packages("rpart")

#magritter was not downloading so I decided to install get tidyverse 
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(DataExplorer)
library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
## Manipulating and cleaning the Data + dealing with NA's
pokemon <- read_csv("Desktop/Pokemon.csv")
#pokemon <- as.data.frame(na.omit(pokemon_csv))
#pokemon.db <- as.data.frame(pokemon)

colnames(pokemon) 
# Renaming some columns
# This is done to give the # a name (Number) and makes it better readable 
colnames(pokemon) <- c("Number", "Name", "Type_1",
                       "Type_2", "Total", "Hp",
                       "Attack", "Defense", "Sp.atk", 
                       "Sp.def", "Speed", "Generation",
                       "Legendary")

## The code above must be run so the rest of the code can follow the column names propperly

#Checking the head of each data column
head(pokemon)
head(pokemon$Number)
head(pokemon$Name)
head(pokemon$Type_1)
head(pokemon$Type_2)
head(pokemon$Total)
head(pokemon$Sp.atk)
head(pokemon$Sp.def)
head(pokemon$Total)
head(pokemon$Hp)
head(pokemon$Generation)
head(pokemon$Legendary)

summary(pokemon)
min(pokemon$Defense)
max(pokemon$Defense)
# Double checking

sum(is.na(pokemon))
#386 values as NA
sum(is.na(pokemon$Number))
# 0 missing
sum(is.na(pokemon$Name))
# 0 missing
sum(is.na(pokemon$Type_1))
# 0 missing
sum(is.na(pokemon$Type_2))
# 386 missing Type 2 Pokemon is missing the most values
sum(is.na(pokemon$Total))
# 0 missing
sum(is.na(pokemon$Hp))
# 0 missing
sum(is.na(pokemon$Attack))
# 0 missing
sum(is.na(pokemon$Defense))
# 0 missing
sum(is.na(pokemon$Sp.atk))
# 0 missing
sum(is.na(pokemon$Sp.def))
# 0 missing
sum(is.na(pokemon$Speed))
# 0 missing
sum(is.na(pokemon$Legendary))
# 0 missing

# Pokemon Type 2 is the only one with misssing values
#Attempted to change the NA to Neutral however there is a neutral class for Pokemon
#Therefore NA Values won't be changed as NA is preffered over NULL as it takes 1 space.
# However when I tried to model the Database with NA's in it it caused me a lot of problems.
# So I decided to tackle the challange again and change the values.
# I tried all the codes below to make it work but the only one which did is not commented out.

#lapply(pokemon$Type_2, FUN = replace(.=="NA", "Neutral"))
#pokemon[pokemon$Type_2 == "NA"] <- "Neutral"
#mutate(pokemon$Type_2)
#pokemon <- pokemon %>% mutate(Type_2 = replace(Type_2, Type_2 == is.na , "Normal"))
#pokemon[pokemon$Type_2 == ""] <- "Normal"
#replace(pokemon, pokemon =="NA", "Normal")

# Changing NA's (mutating without mutating)
pokemon[c("Type_2")][is.na(pokemon[c("Type_2")])] <- "Normal"

head(pokemon$Type_2)
summary(pokemon$Type_2)
sum(is.na(pokemon$Type_2))

# 0 missing Type 2 Pokemon now

glimpse(pokemon)
str(pokemon)
typeof(pokemon)
#List
typeof(pokemon$Attack)
#Double

pokemon$Generation <- as.factor(pokemon$Generation)

# If levels is run on Generation prior to factoring the result would be NULL
levels(pokemon$Generation)
levels(pokemon$Legendary)
typeof(pokemon$Legendary)

unique(pokemon$Type_1)

unique(pokemon$Type_2)
# Type 2 has NA as a type, since some pokemon do not have a second type and are just purely 1 type.
# It is not necessary for Pokemon to have two types.

summary(pokemon$Type_1)

types_poke <- pokemon %>%
  group_by(Type_1,Type_2) %>%
  summarise(count=n()) 

head(types_poke)

# 386 of the NA's are shown at the top ofthe tally. Type 1 doesn't have this issue.

## Frequency Tables and plots 

# Types 1 and 2

ggplot(pokemon, aes(x=fct_infreq(Type_1))) + 
  geom_bar(fill="black", colour="red") +
  labs(x="Type_1 Pokemon", y="Frequency of total Pokemon", title="How many each Type_1 are there?") +
  theme(axis.text.x=element_text(angle=75, hjust=1))

ggplot(pokemon, aes(x=fct_infreq(Type_2))) + 
  geom_bar(fill="black", colour="magenta") +
  labs(x="Type_2 Pokemon", y="Frequency of total Pokemon", title="How many each Type_2 are there?") +
  theme(axis.text.x=element_text(angle=75, hjust=1))

ggplot(types_poke, aes(x=Type_1, y = Type_2)) + 
  geom_tile(aes(fill=count), show.legend=FALSE) +
  geom_text(aes(label=count)) +
  labs(x="Type 1 Pokemon", y="Type 2 Pokemon", title="What are the combinations?") +
  theme(axis.text.x=element_text(angle=75, hjust=1)) +
  scale_fill_gradient(low="grey", high="blue") 

# Generation
#Tried to build/edit my own function to show me the data ranges as an output.
#get_hist <- function(p) {
#  d <- ggplot_build(p)$data[[1]]
#  data.frame(x = d$x, xmin = d$xmin, xmax = d$xmax, y = d$y, ymax = d$ymax)
#}

generationplot <- ggplot(pokemon, aes(x=fct_infreq(Generation))) + 
  geom_bar(fill="grey", colour="yellow") +
  labs(x="Generations", y="Frequency of total Pokemon", title="How many pokemon are there in each Generation?") +
  theme(axis.text.x=element_text(angle=1, hjust=1))


# To plot the graph
generationplot

# Plot these before the factoring below otherwise it will not run
# Data Cleaning and processing has been done
# Now onto managing to run a model or two on the data
# Aswell as create a few models to visualize the data
# Pokemon has 800 rows


# Correlations


cor(pokemon$Legendary,pokemon$Defense) # 0.2463768
cor(pokemon$Hp,pokemon$Defense) # 0.2396223

cor(pokemon$Defense,pokemon$Sp.atk) # 0.3963618
cor(pokemon$Defense,pokemon$Sp.def) # 0.5107466

cor(pokemon$Attack,pokemon$Defense) # 0.4386871
cor(pokemon$Attack,pokemon$Sp.def) # 0.3963618
cor(pokemon$Attack,pokemon$Sp.atk) # 0.3963618
cor(pokemon$Attack,pokemon$Sp.atk) # 0.3963618

cor(pokemon$Legendary,pokemon$Hp) # 0.0.2736196
cor(pokemon$Legendary,pokemon$Attack) # 0.345408




#Factoring here is important as the generations of pokemon needs to represent 
#Actual generations and factoring it makes it easier to categorize them
#As these pokemon are completely different from their former or future generations.
# Turned the Logical into numeric for easier modeling as logicals will cause issues with modeling (Linear).
#pokemon$Type_2 <- as.numeric(pokemon$Type_2)
#pokemon$Type_1 <- as.numeric(pokemon$Type_1)




# Turned character strings into facotrs 
pokemon$Type_2 <- as.factor(pokemon$Type_2)
pokemon$Type_1 <- as.factor(pokemon$Type_1)
pokemon$Legendary <- as.numeric(pokemon$Legendary)

#800*0.7
#800*0.2
#train=sample(560,160)

# This create a detailed report on essential statistics and points out 
# create_report(pokemon,y="Hp")


#Creating a cleaner data base for modeling

# Removed the name
clean_poke <- pokemon[c("Type_1",
                        "Type_2", "Total", "Hp",
                        "Attack", "Defense", "Sp.atk", 
                        "Sp.def", "Speed", "Generation",
                        "Legendary")]


types_only <- pokemon[c("Type_1", "Type_2","Legendary")]

# Creating a training and testing split for Total and Hp Testing the data below

set.seed(118)
poke_sampling_vector<- createDataPartition(clean_poke$Total, p = 0.70, list = FALSE)

poke_train <- clean_poke[poke_sampling_vector,]
poke_test <-  clean_poke[-poke_sampling_vector,]

# Linear Regression
# On HP and defense

linear_clean <- lm(Total~., data=poke_train)
summary(linear_clean)
plot(linear_clean)

#  Hp              1.000e+00  7.438e-16  1.345e+15  < 2e-16 ***
#  Attack          1.000e+00  7.160e-16  1.397e+15  < 2e-16 ***
#  Defense         1.000e+00  7.362e-16  1.358e+15  < 2e-16 ***
#  Sp.atk          1.000e+00  7.226e-16  1.384e+15  < 2e-16 ***
#  Sp.def          1.000e+00  7.733e-16  1.293e+15  < 2e-16 ***
#  Speed           1.000e+00  7.036e-16  1.421e+15  < 2e-16 ***

# Adding the inputs from earlier

linear_clean_2 <- lm(Total~ Hp+Attack+Defense+Sp.def+Sp.atk+Speed, data=poke_train)
summary(linear_clean_2)
plot(linear_clean_2)

#Anova Difference between the two models
##########################################

anova(linear_clean,linear_clean_2)

##########################################

linear_clean_hp <- lm(Hp~., data=poke_train)
summary(linear_clean_hp)
plot(linear_clean_hp)

# Type_1Grass    -2.866e-13  7.686e-14 -3.729e+00 0.000214 ***
#  Total           1.000e+00  7.552e-16  1.324e+15  < 2e-16 ***
#  Attack         -1.000e+00  1.211e-15 -8.258e+14  < 2e-16 ***
#  Defense        -1.000e+00  1.032e-15 -9.692e+14  < 2e-16 ***
#  Sp.atk         -1.000e+00  1.152e-15 -8.681e+14  < 2e-16 ***
#  Sp.def         -1.000e+00  1.181e-15 -8.464e+14  < 2e-16 ***
#  Speed          -1.000e+00  9.690e-16 -1.032e+15  < 2e-16 ***
#  Speed and Total have the lowest Standard Error

#linear_reg_poke <-lm(Hp~Defense+Attack+Sp.atk+Sp.def,data=pokemon) 
#linear_reg_poke_full <-lm(Hp~.,data=pokemon)
#summary(linear_reg_poke_full)
#summary(linear_reg_poke)

# Sp Def SP and Attack are significant

# Variable created to test misclassification rate.

misclassification_error_rate <- sum(poke_test$Total != prediction_poke) /nrow(poke_test)*100
misclassification_error_rate 

# Checking for Legendary pokemon ######################################## Main Model #####################
set.seed(118)
poke_sampling_vector_L<- createDataPartition(clean_poke$Legendary, p = 0.70, list = FALSE)
poke_train_L <- clean_poke[poke_sampling_vector_L,]
poke_test_L <-  clean_poke[-poke_sampling_vector_L,]

linear_clean_L <- lm(Legendary ~., data=poke_train_L)
summary(linear_clean_L)

plot(linear_clean_L)

rf_poke_L <- randomForest(Legendary~., data = poke_train_L, ntree= 500, mtry =2, importance=TRUE)

prediction_poke_L <- predict(rf_poke_L, newdata=poke_test_L, type="class")

table(prediction_poke_L,poke_test_L$Legendary)

misclassification_error_rate <- sum(poke_test_L$Legendary != prediction_poke_L) / nrow(poke_test_L)*100
misclassification_error_rate 

train.tree <- rpart(Legendary~., data = poke_train_L,method = "class")

summary(train.tree)

plot(train.tree)
text(train.tree)

fancyRpartPlot(model = train.tree, main = "Tree for Legendary")

printcp(train.tree)
plotcp(train.tree)

# Clean version of the Modeling above

########################################################################################################################
# END OF MODELING
########################################################################################################################











# For Defense
#set.seed(118)
#pokedef_sampling_vector<- createDataPartition(clean_poke$Defense, p = 0.70, list = FALSE)
#pokedef_train <- clean_poke[pokedef_sampling_vector,]
#pokedef_test <-  clean_poke[-pokedef_sampling_vector,]
#linear_clean_def <- lm(Defense ~ Sp.def, data=pokedef_train)
#summary(linear_clean_def)
#plot(linear_clean_def)
#rf_poke_def <- randomForest(Defense~., data = pokedef_train, ntree= 500, mtry =2, importance=TRUE)
#predictiondef_poke <- predict(rf_poke_def, newdata=pokedef_test, type="class")
#table(predictiondef_poke,pokedef_test$Defense)
#misclassification_error_rate <- sum(pokedef_test$Legendary != predictiondef_poke) / nrow(pokedef_test)*100
#misclassification_error_rate 
#train.tree <- rpart(Defense~ Legendary, data = pokedef_train,method = "class")
#summary(train.tree)
#plot(train.tree)
#text(train.tree)
#fancyRpartPlot(model = train.tree, main = "Tree for Legendary")
#printcp(train.tree)
#plotcp(train.tree)
# Edits
## Renamed some columns
## Removed the # from the columns
## Glimpsed at the data
## Generated a report for corrrelations and plotting general graphs
## Checked for NA's in every column and dealt with them
## Decision tree easier to viualize for modeling