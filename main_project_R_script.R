library(readr)

convict_registers <- read_csv("slqbritishconvictregisters201605.csv")
# column 19, 20 missing column names


convict_records <- read.delim("tmpMA2c8PConvict_records.txt", header = FALSE, sep = "\t")
# all unknown column names
# need to name the columns
# code ..

View(convict_registers)
View(convict_records)


# Requirements 1
# 1. Propose two different prediction algorithms to predict the duration of the sentence as a function of
# the place of trial, the place of arrival, and the date of departure.

duration_of_sentence <- #code
place_of_trial <- #code
place_of_arrival <- #code
date_of_departure <- #code
  
  
# Requirements 2
# 2. Also, propose a classification algorithms to predict whether the convict received a ticket of leave as a
# function of the place of trial, the place of arrival, and the date of departure, and the sentence.

  
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