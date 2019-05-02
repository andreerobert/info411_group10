THIS IS MY CODE

# Define the problem and the goal

library(readr)

convict_registers <- read_csv("slqbritishconvictregisters201605.csv")
# column 19, 20 missing column names
View(convict_registers)

convict_records <- read.delim("tmpMA2c8PConvict_records.txt", header = FALSE, sep = "\t")
# all unknown column names
# need to name the columns
# code ..
View(convict_records)

# You are required to restrict your analysis to the last 40 years of transportation, i.e. the
# period 1828–1867.

convict_registers_40years <- subset(convict_registers, 

# "A particular feature of the sentence durations is that some are life sentences rather than a
# number of years. So you will need to consider how to handle this mixed measurement type."                                    
                                    
duration_of_sentence <- # column 10
                        # if term = life -> life
                        # else the number -> number of years 


# Requirements 1
# 1. Propose two different prediction algorithms to predict the duration of the sentence as a function of
# the place of trial, the place of arrival, and the date of departure.

# target variable - duration_of_sentence

# input variable
place_of_trial <- # column 10
date_of_departure <- # column 11
place_of_arrival <- # column 12

# algorithm 1 : mlp function to predict just like in lab to predict creditworthiness (variables are words not numerical)
#               need to decodeclasslabels??
# algorithm 2 : decision tree (better algorithm?)
# algorithm 3 : ??

# Requirements 2
# 2. Also, propose a classification algorithms to predict whether the convict received a ticket of leave as a
# function of the place of trial, the place of arrival, and the date of departure, and the sentence.

# target variable - ticket_of_leave

# input variable - place_of_trial, place_of_arrival, date_of_departure, sentence
  
# Requirements 3
# 3. Present details of data pre-processing. This could include some merging of categories, if this is
# considered to be appropriate.

# Requirements 4
# 4. Explain why weak prediction or classification performance might still be of potential historical interest
# in this particular application.

# Requirements 5
# 5. Discuss the strengths and weaknesses of proposed models, and present your preferred models.

# Requirements 6
# 6. Present performance measures of your classification results.

confusionMatrix <- #code

# Requirements 7
# 7. Discuss whether or not the duration of sentences tended to change over time.