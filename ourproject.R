##########################################################
##       0. import data and reduce misc                 ##
##########################################################


install.packages("kohonen")
install.packages("RSNNS")
install.packages("party")
install.packages("rpart")


#load libraries
library(kohonen)
library(RSNNS)
library(party)
library(rpart)

#read csv as dataraw, header is true and ensure that stringsAsFactors = FALSE because we want to convert them all into other data types
orginaldata <- read.csv("slqbritishconvictregisters201605.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, strip.white=TRUE)


#create our dataset of reduced attributes called convict - we remove all redundant columns leaving only; 
#name, sentence details, departure date, place of arrival
convictrelevantdata <- orginaldata[c(1,9,11,12)]


#removing all trailing and leading '.', if existing, from Convict Name, Date of Departure, Place of Arrival
convictrelevantdata$Convict.Name <- gsub('^\\.|\\.$', '', convictrelevantdata$Convict.Name)

convictrelevantdata$Date.of.Departure <- gsub('^\\.|\\.$', '', convictrelevantdata$Date.of.Departure)

convictrelevantdata$Place.of.Arrival <- gsub('^\\.|\\.$', '', convictrelevantdata$Place.of.Arrival)

convictrelevantdata$Sentence.details <- gsub('^\\.|\\.$', '', convictrelevantdata$Sentence.details)


##########################################################
##       1. cleansing of (Date.of.Departure)            ##
##########################################################
#we modify date of departure if;
#as date returns a NA (most often because the day of the month is missing)
#in that case we pad this value as '01' indicating the first day of the month
#in all other cases we leave Date.of.Departure unchanged
convictrelevantdata$Date.of.Departure <- ifelse( is.na( as.Date(convictrelevantdata$Date.of.Departure, "%d %B %Y") ), 
                                    paste("01", convictrelevantdata$Date.of.Departure), #case if NA date convsersion
                                    convictrelevantdata$Date.of.Departure)              #case if date conversion is ok

#convert all date strings to Date data type
convictrelevantdata$Date.of.Departure <- as.Date(convictrelevantdata$Date.of.Departure, "%d %B %Y")

#removing those rows where date of departure is 'NA'
convictrelevantdata <- convictrelevantdata[!(is.na(convictrelevantdata$Date.of.Departure)),]

#removing all the data where date of departure is less than "1828-01-01"
convictrelevantdata <- convictrelevantdata[!(convictrelevantdata$Date.of.Departure < "1828-01-01"),]

#removing all the data where date of departure is greater than "1867-12-31"
convictrelevantdata <- convictrelevantdata[!(convictrelevantdata$Date.of.Departure > "1867-12-31"),]







##########################################################
##       2. cleansing of (Place.of.Arrival)             ##
##########################################################
#convert 414 cases of 'VDI' where convicts disembarked at 'Norfolk Island' instead
#convert 1 cases of 'VDI' where convicts disembarked at 'Norfok Island' instead
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("Norfo+[lk]+ Island only", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "Norfolk Island")



#convert 588 cases of where convicts disembarked at 'Port Phillip' instead of a scheduled place
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("disembark[ed]+ at Port Phillip", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "Port Phillip")


#convert 279 cases of where convicts appear to arrive at VDL
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("appear to have all landed in Van Diemen's Land", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "Van Diemen's Land")


#convert 299 cases of Port Phillip where convicts were forwarded to Sydney instead ('New South Wales')
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("but were sent on to Sydney", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "New South Wales")


#categorize 200 cases of joint arrival between NSW AND Norfolk
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("New South Wales and Norfolk Island", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "NSW/NFK")


#categorize 60 cases of joint arrival between VDL AND Norfolk
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("Norfolk Island and Van Diemen's Land", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "VDL/NFK")


#categorize 971 cases of joint arrival between VDL AND Norfolk
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("Van Diemen's Land and Norfolk Island", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "VDL/NFK")



#categorize 801 cases of joint arrival between VDI AND Port Phillip
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("and Port Phillip", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "VDI/PPH")


#categorize 99 cases of joint arrival between VDL AND New South Wales
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("Van Diemen's Land] New South Wales", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "VDL/NSW")


#convert 434 cases of where convicts arrive at VDL - Norfolk Island
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("Norfolk Island", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "Norfolk Island")


#convert all remaining VDI variants to 'Van Diemen's Land'
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("V[an . ]+D[iemen's . ]+[I . ]", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "Van Diemen's Land")


#convert all remaining NSW variants to 'New South Wales'
convictrelevantdata$Place.of.Arrival <- ifelse(regexpr("New[ ]+South[ ]+Wales", convictrelevantdata$Place.of.Arrival) == "-1",
                                               convictrelevantdata$Place.of.Arrival,
                                       "New South Wales")
#For those categories that have dual arrivals. Allocate in 50/50 splits to each listed arrival
convictrelevantdata$Place.of.Arrival[convictrelevantdata$Place.of.Arrival == "NSW/NFK"] <- c("New South Wales", "Norfolk Island")

convictrelevantdata$Place.of.Arrival[convictrelevantdata$Place.of.Arrival == "VDI/PPH"] <- c("Van Diemen's Land", "Port Phillip")

convictrelevantdata$Place.of.Arrival[convictrelevantdata$Place.of.Arrival == "VDL/NFK"] <- c("Van Diemen's Land", "Norfolk Island")

convictrelevantdata$Place.of.Arrival[convictrelevantdata$Place.of.Arrival == "VDL/NSW"] <- c("Van Diemen's Land", "New South Wales")


##########################################################
##       3. extract/clean (Place.of.Sentence)           ##
##########################################################
#read csv of towns in the UK (and from colonies)
towns <- read.csv("towns.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
#read csv of counties in the UK during the 1800s (and colonies)
counties <- read.csv("counties.csv", header = FALSE, sep = "\t", stringsAsFactors = FALSE)



#fix some sentence data before string matching
convictrelevantdata$Sentence.details <- gsub('HMS \\"Trafalgar\\"', 'H.M.S Trafalgar', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('H.M. Ship \\"Agamemnon\\"', 'H.M.S Agamemnon', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('HMS \\"Hibernia\\"', 'H.M.S Hibernia', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('HMS \\"Impregnable\\"', 'H.M.S Impregnable', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('H.M. Ship Impregnable', 'H.M.S Impregnable', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('HMS \\"Meanda\\"', 'H.M.S Meanda', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Ship \\"Hannibal\\"', 'H.M.S Hannibal', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Ship \\"Neptune\\"', 'H.M.S Neptune', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Carmarthan', 'Carmarthen', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Carmarthon', 'Carmarthen', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Carmerthen', 'Carmarthen', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Camarthen', 'Carmarthen', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Court of King\'s', 'Court of Kings', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Edinbro', 'Edinburgh', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Esses', 'Essex', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Mean Meer', 'Meanmeer', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Caernarnvon', 'Carnarvon', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Saint Christopher', 'St Christopher', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('St Alban', 'St Albans', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Meenmeer', 'Meanmeer', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('St. Helena', 'St Helena', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Rio [Dd]e Janiero', 'Rio de Janeiro', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Nott\'s', 'Notts', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('Middlex', 'Middlesex', convictrelevantdata$Sentence.details)

convictrelevantdata$Sentence.details <- gsub('[CC]entral [Cc]riminal [Cc]ourt', 'Central Criminal Court', convictrelevantdata$Sentence.details)


#compare our Sentence.details against a list of counties in the UK
#if partially matched, store from the list of counties - the first instance of a matched counties
i = 1
for (rows in rownames(counties))
{
  convictrelevantdata[grep(counties$V1[i], convictrelevantdata$Sentence.details, value = F),"County.Colony"]  <- counties$V1[i]
  i = i + 1
}


#compare our Sentence.details against a list of towns in the UK
#if partially matched, store from the list of towns - the first instance of a matched town
i = 1
for (rows in rownames(towns))
{
  convictrelevantdata[grep(towns$V2[i], convictrelevantdata$Sentence.details, value = F),"Town"]  <- towns$V2[i]
  i = i + 1
}


#create attribute Place.Of.Sentence
convictrelevantdata["Place.of.Sentence"] = NA



#merge town, county/colony as Place.of.Sentence
i = 1
for (rows in rownames(convictrelevantdata))
{
  ifelse(is.na(convictrelevantdata[i,6]),  
         ifelse(is.na(convictrelevantdata[i,5]),
                convictrelevantdata[i,"Place.of.Sentence"] <- NA,
                convictrelevantdata[i,"Place.of.Sentence"] <- convictrelevantdata[i,5]),
         ifelse(is.na(convictrelevantdata[i,5]),  
                convictrelevantdata[i,"Place.of.Sentence"] <- convictrelevantdata[i,6],
                convictrelevantdata[i,"Place.of.Sentence"] <- paste(convictrelevantdata[i,c(6,5)], collapse = ", ")))
  i = i + 1
}


#clean up after loops. Most convicts from Gibraltar are tried at other courts, hence it lists Gibraltar alongside UK towns.
convictrelevantdata$Place.of.Sentence <- gsub('*Gibraltar*', 'Gibraltar', convictrelevantdata$Place.of.Sentence)

##########################################################
##       4. extract/clean (Sentence.Duration)           ##
##########################################################
#for sentence duration we search for 'year' and include characters in positions -3 to -1 of 'y'
#we use 'year' because it is either entered as year or years
#for 'life' sentences we populate 50 years (arbitrary for now) sentencing. Note the max on record is 35.
#note regexpr returns -1 if it fails to find an expression
convictrelevantdata["Sentence.Duration"] <- ifelse(regexpr("life", convictrelevantdata$Sentence.details) == "-1",
                                           substring(convictrelevantdata$Sentence.details, 
                                                     regexpr("year", convictrelevantdata$Sentence.details) - 3, 
                                                     regexpr("year", convictrelevantdata$Sentence.details) - 1),
                                           "50")


#handle two entries which have 'seven' as sentence duration (as opposed to '7')
convictrelevantdata$Sentence.Duration[(convictrelevantdata$Sentence.Duration) == "en "] <- 7
#handle three entries which have illegible years as sentence duration 
convictrelevantdata$Sentence.Duration[(convictrelevantdata$Sentence.Duration) == "e] "] <- NA

#convert all sentence duration to numeric value. 
#this creates NAs which we can check against sentence details (manually)
convictrelevantdata$Sentence.Duration <- as.integer(convictrelevantdata$Sentence.Duration)

#run this to see all NA's - they are listed as "sentence not recorded"
convictrelevantdata[is.na(convictrelevantdata$Sentence.Duration),2]


##########################################################
##       5. extract/clean (Date.of.Sentence)            ##
##########################################################
#rectify some erroneous date data case-by case... 63 items
#this took some time!!!
convictrelevantdata[convictrelevantdata$Convict.Name == 'Nicol, James',2] <- gsub('22 April 841', '22 April 1841', convictrelevantdata[convictrelevantdata$Convict.Name == 'Nicol, James',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Thurling, John',2] <- gsub('30 January 183', '30 January 1837', convictrelevantdata[convictrelevantdata$Convict.Name == 'Thurling, John',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Wright, Thomas (The younger)',2] <- gsub('02 April 853', '02 April 1853', convict.sub[convict.sub$Convict.Name == 'Wright, Thomas (The younger)',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Elworthy, John (The younger)',2] <- gsub('23 January 18.5', '23 January 1835', convictrelevantdata[convictrelevantdata$Convict.Name == 'Elworthy, John (The younger)',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Hardiment, William',2] <- gsub('07 April', '07 April 1854', convictrelevantdata[convictrelevantdata$Convict.Name == 'Hardiment, William',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Barnes, William',2] <- gsub('07 March', '07 March 1853', convictrelevantdata[convictrelevantdata$Convict.Name == 'Barnes, William',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Outhwaite, Thomas',2] <- gsub('12 January 835', '12 January 1835', convictrelevantdata[convictrelevantdata$Convict.Name == 'Outhwaite, Thomas',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Cole, Joseph',2] <- gsub('29 October [year illegible]', '29 October 1849', convictrelevantdata[convictrelevantdata$Convict.Name == 'Cole, Joseph',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Pocketts, James',2] <- gsub('03 August', '03 August 1836', convictrelevantdata[convictrelevantdata$Convict.Name == 'Pocketts, James',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Downes, Thomas',2] <- gsub('on August 1837', 'on 01 August 1837', convictrelevantdata[convictrelevantdata$Convict.Name == 'Downes, Thomas',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Rosenberg, Harris',2] <- gsub('life April 1842', 'life on 01 April 1842', convictrelevantdata[convictrelevantdata$Convict.Name == 'Rosenberg, Harris',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Place, James',2] <- gsub('d] April 1833', 'd] 01 April 1833', convictrelevantdata[convictrelevantdata$Convict.Name == 'Place, James',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Murphy, Matthew',2] <- gsub('\\[no day\\] May 1832', '01 May 1832', convictrelevantdata[convictrelevantdata$Convict.Name == 'Murphy, Matthew',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Flannagan, George',2] <- gsub('15 Ap\\[ril 1844', '15 April 1844', convictrelevantdata[convictrelevantdata$Convict.Name == 'Flannagan, George',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Bradley, John',2] <- gsub('014 July 1834', '04 July 1834', convictrelevantdata[convictrelevantdata$Convict.Name == 'Bradley, John',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Plunkett, James',2] <- gsub('158 May 1848', '18 May 1848', convictrelevantdata[convictrelevantdata$Convict.Name == 'Plunkett, James',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Turner, Thomas William',2] <- gsub('140 June 1867', '14 June 1867', convictrelevantdata[convictrelevantdata$Convict.Name == 'Turner, Thomas William',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Hartley, Joseph',2] <- gsub('074 April 1856', '07 April 1856', convictrelevantdata[convictrelevantdata$Convict.Name == 'Hartley, Joseph',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Ralph, James',2] <- gsub('22 Mach 1833', '22 March 1833', convictrelevantdata[convictrelevantdata$Convict.Name == 'Ralph, James',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Shaw, Thomas',2] <- gsub('087 August 1827', '08 August 1827', convictrelevantdata[convictrelevantdata$Convict.Name == 'Shaw, Thomas',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Lord, William',2] <- gsub('06\\+ January 1834', '06 January 1834', convictrelevantdata[convictrelevantdata$Convict.Name == 'Lord, William',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Board, John',2] <- gsub('on October 1840', 'on 01 October 1840', convictrelevantdata[convictrelevantdata$Convict.Name == 'Board, John',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Morrison, James',2] <- gsub('years 24 May 1830', 'years      24 May 1830', convictrelevantdata[convictrelevantdata$Convict.Name == 'Morrison, James',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Braid, Mary',2] <- gsub('in January 1834', 'on 01 January 1834', convictrelevantdata[convictrelevantdata$Convict.Name == 'Braid, Mary',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'McKinlay, William',2] <- gsub('life November 1829', 'life on 01 November 1829', convictrelevantdata[convictrelevantdata$Convict.Name == 'McKinlay, William',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Clark, Samuel',2] <- gsub('31 November 1833', '30 November 1833', convictrelevantdata[convictrelevantdata$Convict.Name == 'Clark, Samuel',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Cadman, Robert',2] <- gsub('s 10 May 1842', 's       10 May 1842', convictrelevantdata[convictrelevantdata$Convict.Name == 'Cadman, Robert',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Matheson, Charles',2] <- gsub('years 03 May 1834', 'years       03 May 1834', convictrelevantdata[convictrelevantdata$Convict.Name == 'Matheson, Charles',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Downes, John',2] <- gsub('89714', '', convictrelevantdata[convictrelevantdata$Convict.Name == 'Downes, John',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Pierre',2] <- gsub('in December 1833', 'on 01 December 1833', convictrelevantdata[convictrelevantdata$Convict.Name == 'Pierre',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Rickett, William',2] <- gsub('on July 1827', 'on 01 July 1827', convictrelevantdata[convictrelevantdata$Convict.Name == 'Rickett, William',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Maria (A Slave)',2] <- gsub('life September 1825', 'life on 01 September 1825', convictrelevantdata[convictrelevantdata$Convict.Name == 'Maria (A Slave)',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Ayres, Robert (The younger)',2] <- gsub('12 A\\[pril 1833', '12 April 1833', convictrelevantdata[convictrelevantdata$Convict.Name == 'Ayres, Robert (The younger)',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Robertson, John',2] <- gsub('22 A\\[pril 1840', '22 April 1840', convictrelevantdata[convictrelevantdata$Convict.Name == 'Robertson, John',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Kennedy, T F',2] <- gsub('07 Hanuary 1848', '07 January 1848', convictrelevantdata[convictrelevantdata$Convict.Name == 'Kennedy, T F',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Buchanan, William',2] <- gsub('life in May 1833', 'life on 01 May 1833', convictrelevantdata[convictrelevantdata$Convict.Name == 'Buchanan, William',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Pickett, Edward',2] <- gsub('on January 1835', 'on 01 January 1835', convictrelevantdata[convictrelevantdata$Convict.Name == 'Pickett, Edward',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Andrews, Henry',2] <- gsub('26 Nov ember 1830', '26 November 1830', convictrelevantdata[convictrelevantdata$Convict.Name == 'Andrews, Henry',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Jones, John',2] <- gsub('147 October 1846', '17 October 1846', convictrelevantdata[convictrelevantdata$Convict.Name == 'Jones, John',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Carr, John',2] <- gsub('7 years 10 May 1830', '7 years     10 May 1830', convictrelevantdata[convictrelevantdata$Convict.Name == 'Carr, John',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Greenhalgh, John',2] <- gsub('06 on January 1836', '06 January 1836', convictrelevantdata[convictrelevantdata$Convict.Name == 'Greenhalgh, John',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Walker, Henry',2] <- gsub('29 February 1837', '28 February 1837', convictrelevantdata[convictrelevantdata$Convict.Name == 'Walker, Henry',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Brown, Mary',2] <- gsub('26 Mat 1834', '26 March 1834', convictrelevantdata[convictrelevantdata$Convict.Name == 'Brown, Mary',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Booth, Stephen',2] <- gsub('02 Mach 1846', '02 March 1846', convictrelevantdata[convictrelevantdata$Convict.Name == 'Booth, Stephen',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Pearnell, Charles',2] <- gsub('17 Juner 1850', '17 June 1850', convictrelevantdata[convictrelevantdata$Convict.Name == 'Pearnell, Charles',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Scott, Edward',2] <- gsub('20ctober 1831', '20 October 1831', convictrelevantdata[convictrelevantdata$Convict.Name == 'Scott, Edward',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Page, Ann',2] <- gsub('in    October 1829', 'on 01 October 1829', convictrelevantdata[convictrelevantdata$Convict.Name == 'Page, Ann',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Priestley, Mary Ann',2] <- gsub('in October 1829', 'on 01 October 1829', convictrelevantdata[convictrelevantdata$Convict.Name == 'Priestley, Mary Ann',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Signall, Philip',2] <- gsub('05 1832', '01 May 1832', convictrelevantdata[convictrelevantdata$Convict.Name == 'Signall, Philip',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Askin, John',2] <- gsub('31 Mat 1827', '31 March 1827', convictrelevantdata[convictrelevantdata$Convict.Name == 'Askin, John',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Callanan, Ann',2] <- gsub('27 Ju 1835', '27 July 1835', convictrelevantdata[convictrelevantdata$Convict.Name == 'Callanan, Ann',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Williams, Robert',2] <- gsub('on September 1833', 'on 01 September 1833', convictrelevantdata[convictrelevantdata$Convict.Name == 'Williams, Robert',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Hooper, Joseph',2] <- gsub('on November 1844', 'on 01 November 1844', convictrelevantdata[convictrelevantdata$Convict.Name == 'Hooper, Joseph',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Newman, Martin',2] <- gsub('during 1841', '', convictrelevantdata[convictrelevantdata$Convict.Name == 'Newman, Martin',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Roberts, William',2] <- gsub('during 1841', '', convictrelevantdata[convictrelevantdata$Convict.Name == 'Roberts, William',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Beckett, Robert',2] <- gsub('50 March 1850', '05 March 1850', convictrelevantdata[convictrelevantdata$Convict.Name == 'Beckett, Robert',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Sculley, Darby',2] <- gsub('232 February 1849', '23 February 1849', convictrelevantdata[convictrelevantdata$Convict.Name == 'Sculley, Darby',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Smith, William',2] <- gsub('243 July 1835', '24 July 1835', convictrelevantdata[convictrelevantdata$Convict.Name == 'Smith, William',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Sullivan, Michael',2] <- gsub('9 - 10 August 1858', '10 August 1858', convictrelevantdata[convictrelevantdata$Convict.Name == 'Sullivan, Michael',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Lowe, Thomas',2] <- gsub('234 July 1828', '24 July 1828', convictrelevantdata[convictrelevantdata$Convict.Name == 'Lowe, Thomas',2])


convictrelevantdata[convictrelevantdata$Convict.Name == 'Collett, Thomas',2] <- gsub('Aoril', 'April', convictrelevantdata[convictrelevantdata$Convict.Name == 'Collett, Thomas',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Sandys, Horatio',2] <- gsub('[no day] June 1928', '01 June 1928', convictrelevantdata[convictrelevantdata$Convict.Name == 'Sandys, Horatio',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Williamson, Edward',2] <- gsub('26\\+ March 1835', '26 March 1835', convictrelevantdata[convictrelevantdata$Convict.Name == 'Williamson, Edward',2])

convictrelevantdata[convictrelevantdata$Convict.Name == 'Cole, Joseph',2] <- gsub('29 October \\[year', '29 October 1848 [year', convictrelevantdata[convictrelevantdata$Convict.Name == 'Cole, Joseph',2])

convictrelevantdata[1,2] <- gsub('on  August 1837', 'on 01 August 1837', convictrelevantdata[1,2]) #Downes, Thomas


#this finds the first 4 digit number (year) and trims 15 places left of it
#then it look for the first digits and strips right of it
convictrelevantdata["Date.of.Sentence"] <- substring(convictrelevantdata$Sentence.details,
                                             regexpr("[[:digit:]]{4}", convictrelevantdata$Sentence.details) - 15,
                                             regexpr("[[:digit:]]{4}", convictrelevantdata$Sentence.details) + 4)
convictrelevantdata["Date.of.Sentence"] <- substring(convictrelevantdata$Date.of.Sentence,
                                             regexpr("[[:digit:]]{1}", convictrelevantdata$Date.of.Sentence))



convictrelevantdata["Date.of.Sentence"] <- ifelse(regexpr("[[:digit:]]{4}", convictrelevantdata$Sentence.details) == -1,
                                                  "NA",
                                                  convictrelevantdata$Date.of.Sentence)


#remove all messy ., th 1st rd sept
convictrelevantdata$Date.of.Sentence <- gsub('[.,]', '', convictrelevantdata$Date.of.Sentence)
convictrelevantdata$Date.of.Sentence <- gsub('th|rd|nd', '', convictrelevantdata$Date.of.Sentence)
convictrelevantdata$Date.of.Sentence <- gsub(' Sept ', ' September ', convictrelevantdata$Date.of.Sentence)
convictrelevantdata$Date.of.Sentence <- gsub('1st', '1', convictrelevantdata$Date.of.Sentence)

#real run... convert all values to date. This creates no NAs now that all data is fixed
convictrelevantdata$Date.of.Sentence <-as.Date(convictrelevantdata$Date.of.Sentence, "%d %B %Y")

#run this to see details of NA Date.of.Sentence records 
convictrelevantdata[is.na(convictrelevantdata$Date.of.Sentence),c(1,2,3,7)]
convictrelevantdata[(is.na(convictrelevantdata$Date.of.Sentence)==FALSE),c(1,2,3,7)]


##########################################################
##       6. extract (Ticket.of.Leave)                   ##
##########################################################
#add code to strip Ticket of leave
convictrelevantdata["Ticket.of.Leave"] <- ifelse(regexpr("[Tt]icket", convictrelevantdata$Sentence.details) == "-1", "N", "Y")


##########################################################
##       7. Analysis of data                            ##
##########################################################
#quality measure information

unique(convictrelevantdata$Place.of.Sentence)

table(convictrelevantdata$County.Colony)

unique(convictrelevantdata$Sentence.Duration)

unique(convictrelevantdata$Place.of.Arrival)

ticketleaveconvicts <- sum(convictrelevantdata$Ticket.of.Leave == "Y")/nrow(convictrelevantdata)

Convict.Name_NA <- sum(is.na(convictrelevantdata$Convict.Name))
Sentence.details_NA <- sum(is.na(convictrelevantdata$Sentence.details))
Date.of.Departure_NA <- sum(is.na(convictrelevantdata$Date.of.Departure))
Place.of.Arrival_NA <- sum(is.na(convictrelevantdata$Place.of.Arrival))
Place.of.Sentence_NA <- sum(is.na(convictrelevantdata$Place.of.Sentence))
Sentence.Duration_NA <- sum(is.na(convictrelevantdata$Sentence.Duration))
Date.of.Sentence_NA <- sum(is.na(convictrelevantdata$Date.of.Sentence))
Ticket.of.Leave_NA <- sum(is.na(convictrelevantdata$Ticket.of.Leave))

##########################################################
##   8. Create dataset of no NA for use with tasks      ##
##########################################################

#re-order attributes
#1 = Convict.Name, 5 = County.Colony, 9 = Date.of.Sentence
#3 = Date.of.Departure, 4- Place.of.Arrival, 10 = Ticket.of.Leave, 8 = Sentence.Duration
finalconvictsdata <- convictrelevantdata[,c(1,5,9,3,4,10,8)] 

#include only complete cases
finalconvictsdata <- finalconvictsdata[complete.cases(finalconvictsdata),]

#remove unused data
rm(convicts, orginaldata, counties, towns, i, rows)

#write data to csv so you don't have to do everything above when crashing/reloading
write.table(finalconvictsdata, file = "finalconvictsdata.csv", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")





####################################################################################################################
##                                                                                                                ##
##   9. Predicting (start here)                                                                                   ##
##                                                                                                                ##
####################################################################################################################

#read csv of convicts data
finalconvictsdata <- read.csv("finalconvictsdata.csv", sep = " ")
#reset row index for our subset
rownames(finalconvictsdata) <- 1:nrow(finalconvictsdata)

#define a colour scale for SOMs 
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

predictionconvictsdata = finalconvictsdata

#convert sentence duration to a scale of 1(0:7) - 2(8:14) - 3(15:life sentence)
predictionconvictsdata$Sentence.Duration <- ifelse(predictionconvictsdata$Sentence.Duration <= 7,
                                         1, ifelse(predictionconvictsdata$Sentence.Duration < 15,
                                                   2, 3))

#convert date of departure to a scale of year by year
predictionconvictsdata$Date.of.Departure <- as.Date(predictionconvictsdata$Date.of.Departure)
predictionconvictsdata$Date.of.Departure <- format(predictionconvictsdata$Date.of.Departure, format = "%Y")


#convert date of departure to a scale of year by year
predictionconvictsdata$Date.of.Sentence <- as.Date(predictionconvictsdata$Date.of.Sentence)
predictionconvictsdata$Date.of.Sentence <- format(predictionconvictsdata$Date.of.Sentence, format = "%Y")


#convert all attributes to numeric values
predictionconvictsdata$County.Colony <- as.numeric(predictionconvictsdata$Origin)
predictionconvictsdata$Date.of.Sentence <- as.numeric(predictionconvictsdata$Date.of.Sentence)
predictionconvictsdata$Date.of.Departure <- as.numeric(predictionconvictsdata$Date.of.Departure)
predictionconvictsdata$Place.of.Arrival <- as.numeric(predictionconvictsdata$Place.of.Arrival)
predictionconvictsdata$Ticket.of.Leave <- as.numeric(predictionconvictsdata$Ticket.of.Leave)
predictionconvictsdata$Sentence.Duration <- as.numeric(predictionconvictsdata$Sentence.Duration)

#convert Ticket.of.Leave from 1-2 to binary
predictionconvictsdata$Ticket.of.Leave[predictionconvictsdata$Ticket.of.Leave == 1] <- 0
predictionconvictsdata$Ticket.of.Leave[predictionconvictsdata$Ticket.of.Leave == 2] <- 1



write.csv(convictrelevantdata,"dataaftercleaning.csv")
write.csv(finalconvictsdata,"dataforprediction.csv")
write.csv(predictionconvictsdata, "imputdataforalgorithms.csv")


##########################################################
##   10. SOM (Start here)                               ##
##########################################################



##########################################################
##   11. MLP  (very bad at predicting!!!)               ##
##########################################################




##########################################################
##   12. Trees                                          ##
##########################################################




##########################################################
##   13. Plots                                          ##
##########################################################




##########################################################
##   14. Linear Models                                  ##
##########################################################





