##########################################################
##       0. import data and reduce misc                 ##
##########################################################
install.packages("kohonen")
install.packages("RSNNS")
install.packages("party")
install.packages(
  "rpart"
)
#load libraries
library(kohonen)
library(RSNNS)
library(party)
library(rpart)

#read csv as dataraw, header is true and ensure that stringsAsFactors = FALSE because we want to convert them all into other data types
dataraw <- read.csv("slqbritishconvictregisters201605.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, strip.white=TRUE)

#create our dataset of reduced attributes called convict - we remove all redundant columns leaving only; 
#name, sentence details, departure date, place of arrival
convict <- dataraw[c(1,9,11,12)]

#purge all trailing '.', if existing, from Convict Name, Date of Departure, Place of Arrival
convict$Convict.Name <- gsub('^\\.|\\.$', '', convict$Convict.Name)
convict$Date.of.Departure <- gsub('^\\.|\\.$', '', convict$Date.of.Departure)
convict$Place.of.Arrival <- gsub('^\\.|\\.$', '', convict$Place.of.Arrival)
convict$Sentence.details <- gsub('^\\.|\\.$', '', convict$Sentence.details)


##########################################################
##       1. cleansing of (Date.of.Departure)            ##
##########################################################
#we modify date of departure if;
#as date returns a NA (most often because the day of the month is missing)
#in that case we pad this value as '01' indicating the first day of the month
#in all other cases we leave Date.of.Departure unchanged
convict$Date.of.Departure <- ifelse(is.na(as.Date(convict$Date.of.Departure, "%d %B %Y")), 
                                    paste("01", convict$Date.of.Departure), #case if NA date convsersion
                                    convict$Date.of.Departure)              #case if date conversion is ok

#convert all date strings to Date data type
convict$Date.of.Departure <- as.Date(convict$Date.of.Departure, "%d %B %Y")

#create a subset which restricts date to the final 40 years of data
convict.sub <- subset(convict, convict$Date.of.Departure >= "1828-01-01")

#reset row index for our subset
rownames(convict.sub) <- 1:nrow(convict.sub)


##########################################################
##       2. cleansing of (Place.of.Arrival)             ##
##########################################################
#convert 415 cases of 'VDI' where convicts disembarked at 'Norfolk Island' instead
convict.sub$Place.of.Arrival <- ifelse(regexpr("Norfo+[lk]+ Island only", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "Norfolk Island")
#convert 588 cases of where convicts disembarked at 'Port Phillip' instead of a scheduled place
convict.sub$Place.of.Arrival <- ifelse(regexpr("disembark[ed]+ at Port Phillip", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "Port Phillip")
#convert 279 cases of where convicts appear to arrive at VDL
convict.sub$Place.of.Arrival <- ifelse(regexpr("appear to have all landed in Van Diemen's Land", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "Van Diemen's Land")
#convert 299 cases of Port Phillip where convicts were forwarded to Sydney instead ('New South Wales')
convict.sub$Place.of.Arrival <- ifelse(regexpr("Sydney", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "New South Wales")
#categorize 200 cases of joint arrival between NSW AND Norfolk
convict.sub$Place.of.Arrival <- ifelse(regexpr("New South Wales and Norfolk Island", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "NSW/NFK")
#categorize 60 cases of joint arrival between VDL AND Norfolk
convict.sub$Place.of.Arrival <- ifelse(regexpr("Norfolk Island and Van Diemen's Land", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "VDL/NFK")
#categorize 971 cases of joint arrival between VDL AND Norfolk
convict.sub$Place.of.Arrival <- ifelse(regexpr("Van Diemen's Land and Norfolk Island", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "VDL/NFK")
#categorize 801 cases of joint arrival between VDI AND Port Phillip
convict.sub$Place.of.Arrival <- ifelse(regexpr("and Port Phillip", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "VDI/PPH")
#categorize 99 cases of joint arrival between VDL AND New South Wales
convict.sub$Place.of.Arrival <- ifelse(regexpr("Van Diemen's Land] New South Wales", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "VDL/NSW")
#convert 434 cases of where convicts arrive at VDL - Norfolk Island
convict.sub$Place.of.Arrival <- ifelse(regexpr("Norfolk Island", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "Norfolk Island")
#convert all remaining VDI variants to 'Van Diemen's Land'
convict.sub$Place.of.Arrival <- ifelse(regexpr("V[an . ]+D[iemen's . ]+[I . ]", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "Van Diemen's Land")
#convert all remaining NSW variants to 'New South Wales'
convict.sub$Place.of.Arrival <- ifelse(regexpr("New[ ]+South[ ]+Wales", convict.sub$Place.of.Arrival) == "-1",
                                       convict.sub$Place.of.Arrival,
                                       "New South Wales")


#For those categories that have dual arrivals. Allocate in 50/50 splits to each listed arrival
convict.sub$Place.of.Arrival[convict.sub$Place.of.Arrival == "NSW/NFK"] <- c("New South Wales", "Norfolk Island")
convict.sub$Place.of.Arrival[convict.sub$Place.of.Arrival == "VDI/PPH"] <- c("Van Diemen's Land", "Port Phillip")
convict.sub$Place.of.Arrival[convict.sub$Place.of.Arrival == "VDL/NFK"] <- c("Van Diemen's Land", "Norfolk Island")
convict.sub$Place.of.Arrival[convict.sub$Place.of.Arrival == "VDL/NSW"] <- c("Van Diemen's Land", "New South Wales")


##########################################################
##       3. extract/clean (Place.of.Sentence)           ##
##########################################################
#read csv of towns in the UK (and from colonies)
towns <- read.csv("towns.csv", header = FALSE, sep = ",", stringsAsFactors = FALSE)
#read csv of counties in the UK during the 1800s (and colonies)
counties <- read.csv("counties.csv", header = FALSE, sep = "\t", stringsAsFactors = FALSE)

#fix some sentence data before string matching
convict.sub$Sentence.details <- gsub('HMS \\"Trafalgar\\"', 'H.M.S Trafalgar', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('H.M. Ship \\"Agamemnon\\"', 'H.M.S Agamemnon', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('HMS \\"Hibernia\\"', 'H.M.S Hibernia', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('HMS \\"Impregnable\\"', 'H.M.S Impregnable', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('H.M. Ship Impregnable', 'H.M.S Impregnable', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('HMS \\"Meanda\\"', 'H.M.S Meanda', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Ship \\"Hannibal\\"', 'H.M.S Hannibal', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Ship \\"Neptune\\"', 'H.M.S Neptune', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Carmarthan', 'Carmarthen', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Carmarthon', 'Carmarthen', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Carmerthen', 'Carmarthen', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Camarthen', 'Carmarthen', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Court of King\'s', 'Court of Kings', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Edinbro', 'Edinburgh', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Esses', 'Essex', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Mean Meer', 'Meanmeer', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Caernarnvon', 'Carnarvon', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Saint Christopher', 'St Christopher', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('St Alban', 'St Albans', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Meenmeer', 'Meanmeer', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('St. Helena', 'St Helena', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Rio [Dd]e Janiero', 'Rio de Janeiro', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Nott\'s', 'Notts', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('Middlex', 'Middlesex', convict.sub$Sentence.details)
convict.sub$Sentence.details <- gsub('[CC]entral [Cc]riminal [Cc]ourt', 'Central Criminal Court', convict.sub$Sentence.details)

#compare our Sentence.details against a list of counties in the UK
#if partially matched, store from the list of counties - the first instance of a matched counties
i = 1
for (rows1 in rownames(counties))
{
  convict.sub[grep(counties$V1[i], convict.sub$Sentence.details, value = F),"County.Colony"]  <- counties$V1[i]
  i = i + 1
}

#compare our Sentence.details against a list of towns in the UK
#if partially matched, store from the list of towns - the first instance of a matched town
i = 1
for (rows1 in rownames(towns))
{
  convict.sub[grep(towns$V2[i], convict.sub$Sentence.details, value = F),"Town"]  <- towns$V2[i]
  i = i + 1
}

#create attribute Place.Of.Sentence
convict.sub["Place.of.Sentence"] = NA

#merge town, county/colony as Place.of.Sentence
i = 1
for (rows in rownames(convict.sub))
{
  ifelse(is.na(convict.sub[i,6]),  
         ifelse(is.na(convict.sub[i,5]),
                convict.sub[i,"Place.of.Sentence"] <- NA,
                convict.sub[i,"Place.of.Sentence"] <- convict.sub[i,5]),
         ifelse(is.na(convict.sub[i,5]),  
                convict.sub[i,"Place.of.Sentence"] <- convict.sub[i,6],
                convict.sub[i,"Place.of.Sentence"] <- paste(convict.sub[i,c(6,5)], collapse = ", ")))
  i = i + 1
}

#clean up after loops. Most convicts from Gibraltar are tried at other courts, hence it lists Gibraltar alongside UK towns.
convict.sub$Place.of.Sentence <- gsub('*Gibraltar*', 'Gibraltar', convict.sub$Place.of.Sentence)

#run this to see details of all unique Place.of.Sentence records
#unique(convict.sub[is.na(convict.sub$Place.of.Sentence),c(0,2)])


##########################################################
##       4. extract/clean (Sentence.Duration)           ##
##########################################################
#for sentence duration we search for 'year' and include characters in positions -3 to -1 of 'y'
#we use 'year' because it is either entered as year or years
#for 'life' sentences we populate 50 years (arbitrary for now) sentencing. Note the max on record is 35.
#note regexpr returns -1 if it fails to find an expression
convict.sub["Sentence.Duration"] <- ifelse(regexpr("life", convict.sub$Sentence.details) == "-1",
                                           substring(convict.sub$Sentence.details, 
                                                     regexpr("year", convict.sub$Sentence.details) - 3, 
                                                     regexpr("year", convict.sub$Sentence.details) - 1),
                                           "50")
#handle two entries which have 'seven' as sentence duration (as opposed to '7')
convict.sub$Sentence.Duration[(convict.sub$Sentence.Duration) == "en "] <- 7
#handle three entries which have illegible years as sentence duration 
convict.sub$Sentence.Duration[(convict.sub$Sentence.Duration) == "e] "] <- NA

#convert all sentence duration to numeric value. 
#this creates NAs which we can check against sentence details (manually)
convict.sub$Sentence.Duration <- as.integer(convict.sub$Sentence.Duration)

#run this to see all NA's - they are listed as "sentence not recorded"
convict.sub[is.na(convict.sub$Sentence.Duration),2]


##########################################################
##       5. extract/clean (Date.of.Sentence)            ##
##########################################################
#rectify some erroneous date data case-by case... 63 items
#this took some time!!!
convict.sub[convict.sub$Convict.Name == 'Nicol, James',2] <- gsub('22 April 841', '22 April 1841', convict.sub[convict.sub$Convict.Name == 'Nicol, James',2])
convict.sub[convict.sub$Convict.Name == 'Thurling, John',2] <- gsub('30 January 183', '30 January 1837', convict.sub[convict.sub$Convict.Name == 'Thurling, John',2])
convict.sub[convict.sub$Convict.Name == 'Wright, Thomas (The younger)',2] <- gsub('02 April 853', '02 April 1853', convict.sub[convict.sub$Convict.Name == 'Wright, Thomas (The younger)',2])
convict.sub[convict.sub$Convict.Name == 'Elworthy, John (The younger)',2] <- gsub('23 January 18.5', '23 January 1835', convict.sub[convict.sub$Convict.Name == 'Elworthy, John (The younger)',2])
convict.sub[convict.sub$Convict.Name == 'Hardiment, William',2] <- gsub('07 April', '07 April 1854', convict.sub[convict.sub$Convict.Name == 'Hardiment, William',2])
convict.sub[convict.sub$Convict.Name == 'Barnes, William',2] <- gsub('07 March', '07 March 1853', convict.sub[convict.sub$Convict.Name == 'Barnes, William',2])
convict.sub[convict.sub$Convict.Name == 'Outhwaite, Thomas',2] <- gsub('12 January 835', '12 January 1835', convict.sub[convict.sub$Convict.Name == 'Outhwaite, Thomas',2])
convict.sub[convict.sub$Convict.Name == 'Cole, Joseph',2] <- gsub('29 October [year illegible]', '29 October 1849', convict.sub[convict.sub$Convict.Name == 'Cole, Joseph',2])
convict.sub[convict.sub$Convict.Name == 'Pocketts, James',2] <- gsub('03 August', '03 August 1836', convict.sub[convict.sub$Convict.Name == 'Pocketts, James',2])
convict.sub[convict.sub$Convict.Name == 'Downes, Thomas',2] <- gsub('on August 1837', 'on 01 August 1837', convict.sub[convict.sub$Convict.Name == 'Downes, Thomas',2])
convict.sub[convict.sub$Convict.Name == 'Rosenberg, Harris',2] <- gsub('life April 1842', 'life on 01 April 1842', convict.sub[convict.sub$Convict.Name == 'Rosenberg, Harris',2])
convict.sub[convict.sub$Convict.Name == 'Place, James',2] <- gsub('d] April 1833', 'd] 01 April 1833', convict.sub[convict.sub$Convict.Name == 'Place, James',2])
convict.sub[convict.sub$Convict.Name == 'Murphy, Matthew',2] <- gsub('\\[no day\\] May 1832', '01 May 1832', convict.sub[convict.sub$Convict.Name == 'Murphy, Matthew',2])
convict.sub[convict.sub$Convict.Name == 'Flannagan, George',2] <- gsub('15 Ap\\[ril 1844', '15 April 1844', convict.sub[convict.sub$Convict.Name == 'Flannagan, George',2])
convict.sub[convict.sub$Convict.Name == 'Bradley, John',2] <- gsub('014 July 1834', '04 July 1834', convict.sub[convict.sub$Convict.Name == 'Bradley, John',2])
convict.sub[convict.sub$Convict.Name == 'Plunkett, James',2] <- gsub('158 May 1848', '18 May 1848', convict.sub[convict.sub$Convict.Name == 'Plunkett, James',2])
convict.sub[convict.sub$Convict.Name == 'Turner, Thomas William',2] <- gsub('140 June 1867', '14 June 1867', convict.sub[convict.sub$Convict.Name == 'Turner, Thomas William',2])
convict.sub[convict.sub$Convict.Name == 'Hartley, Joseph',2] <- gsub('074 April 1856', '07 April 1856', convict.sub[convict.sub$Convict.Name == 'Hartley, Joseph',2])
convict.sub[convict.sub$Convict.Name == 'Ralph, James',2] <- gsub('22 Mach 1833', '22 March 1833', convict.sub[convict.sub$Convict.Name == 'Ralph, James',2])
convict.sub[convict.sub$Convict.Name == 'Shaw, Thomas',2] <- gsub('087 August 1827', '08 August 1827', convict.sub[convict.sub$Convict.Name == 'Shaw, Thomas',2])
convict.sub[convict.sub$Convict.Name == 'Lord, William',2] <- gsub('06\\+ January 1834', '06 January 1834', convict.sub[convict.sub$Convict.Name == 'Lord, William',2])
convict.sub[convict.sub$Convict.Name == 'Board, John',2] <- gsub('on October 1840', 'on 01 October 1840', convict.sub[convict.sub$Convict.Name == 'Board, John',2])
convict.sub[convict.sub$Convict.Name == 'Morrison, James',2] <- gsub('years 24 May 1830', 'years      24 May 1830', convict.sub[convict.sub$Convict.Name == 'Morrison, James',2])
convict.sub[convict.sub$Convict.Name == 'Braid, Mary',2] <- gsub('in January 1834', 'on 01 January 1834', convict.sub[convict.sub$Convict.Name == 'Braid, Mary',2])
convict.sub[convict.sub$Convict.Name == 'McKinlay, William',2] <- gsub('life November 1829', 'life on 01 November 1829', convict.sub[convict.sub$Convict.Name == 'McKinlay, William',2])
convict.sub[convict.sub$Convict.Name == 'Clark, Samuel',2] <- gsub('31 November 1833', '30 November 1833', convict.sub[convict.sub$Convict.Name == 'Clark, Samuel',2])
convict.sub[convict.sub$Convict.Name == 'Cadman, Robert',2] <- gsub('s 10 May 1842', 's       10 May 1842', convict.sub[convict.sub$Convict.Name == 'Cadman, Robert',2])
convict.sub[convict.sub$Convict.Name == 'Matheson, Charles',2] <- gsub('years 03 May 1834', 'years       03 May 1834', convict.sub[convict.sub$Convict.Name == 'Matheson, Charles',2])
convict.sub[convict.sub$Convict.Name == 'Downes, John',2] <- gsub('89714', '', convict.sub[convict.sub$Convict.Name == 'Downes, John',2])
convict.sub[convict.sub$Convict.Name == 'Pierre',2] <- gsub('in December 1833', 'on 01 December 1833', convict.sub[convict.sub$Convict.Name == 'Pierre',2])
convict.sub[convict.sub$Convict.Name == 'Rickett, William',2] <- gsub('on July 1827', 'on 01 July 1827', convict.sub[convict.sub$Convict.Name == 'Rickett, William',2])
convict.sub[convict.sub$Convict.Name == 'Maria (A Slave)',2] <- gsub('life September 1825', 'life on 01 September 1825', convict.sub[convict.sub$Convict.Name == 'Maria (A Slave)',2])
convict.sub[convict.sub$Convict.Name == 'Ayres, Robert (The younger)',2] <- gsub('12 A\\[pril 1833', '12 April 1833', convict.sub[convict.sub$Convict.Name == 'Ayres, Robert (The younger)',2])
convict.sub[convict.sub$Convict.Name == 'Robertson, John',2] <- gsub('22 A\\[pril 1840', '22 April 1840', convict.sub[convict.sub$Convict.Name == 'Robertson, John',2])
convict.sub[convict.sub$Convict.Name == 'Kennedy, T F',2] <- gsub('07 Hanuary 1848', '07 January 1848', convict.sub[convict.sub$Convict.Name == 'Kennedy, T F',2])
convict.sub[convict.sub$Convict.Name == 'Buchanan, William',2] <- gsub('life in May 1833', 'life on 01 May 1833', convict.sub[convict.sub$Convict.Name == 'Buchanan, William',2])
convict.sub[convict.sub$Convict.Name == 'Pickett, Edward',2] <- gsub('on January 1835', 'on 01 January 1835', convict.sub[convict.sub$Convict.Name == 'Pickett, Edward',2])
convict.sub[convict.sub$Convict.Name == 'Andrews, Henry',2] <- gsub('26 Nov ember 1830', '26 November 1830', convict.sub[convict.sub$Convict.Name == 'Andrews, Henry',2])
convict.sub[convict.sub$Convict.Name == 'Jones, John',2] <- gsub('147 October 1846', '17 October 1846', convict.sub[convict.sub$Convict.Name == 'Jones, John',2])
convict.sub[convict.sub$Convict.Name == 'Carr, John',2] <- gsub('7 years 10 May 1830', '7 years     10 May 1830', convict.sub[convict.sub$Convict.Name == 'Carr, John',2])
convict.sub[convict.sub$Convict.Name == 'Greenhalgh, John',2] <- gsub('06 on January 1836', '06 January 1836', convict.sub[convict.sub$Convict.Name == 'Greenhalgh, John',2])
convict.sub[convict.sub$Convict.Name == 'Walker, Henry',2] <- gsub('29 February 1837', '28 February 1837', convict.sub[convict.sub$Convict.Name == 'Walker, Henry',2])
convict.sub[convict.sub$Convict.Name == 'Brown, Mary',2] <- gsub('26 Mat 1834', '26 March 1834', convict.sub[convict.sub$Convict.Name == 'Brown, Mary',2])
convict.sub[convict.sub$Convict.Name == 'Booth, Stephen',2] <- gsub('02 Mach 1846', '02 March 1846', convict.sub[convict.sub$Convict.Name == 'Booth, Stephen',2])
convict.sub[convict.sub$Convict.Name == 'Pearnell, Charles',2] <- gsub('17 Juner 1850', '17 June 1850', convict.sub[convict.sub$Convict.Name == 'Pearnell, Charles',2])
convict.sub[convict.sub$Convict.Name == 'Scott, Edward',2] <- gsub('20ctober 1831', '20 October 1831', convict.sub[convict.sub$Convict.Name == 'Scott, Edward',2])
convict.sub[convict.sub$Convict.Name == 'Page, Ann',2] <- gsub('in    October 1829', 'on 01 October 1829', convict.sub[convict.sub$Convict.Name == 'Page, Ann',2])
convict.sub[convict.sub$Convict.Name == 'Priestley, Mary Ann',2] <- gsub('in October 1829', 'on 01 October 1829', convict.sub[convict.sub$Convict.Name == 'Priestley, Mary Ann',2])
convict.sub[convict.sub$Convict.Name == 'Signall, Philip',2] <- gsub('05 1832', '01 May 1832', convict.sub[convict.sub$Convict.Name == 'Signall, Philip',2])
convict.sub[convict.sub$Convict.Name == 'Askin, John',2] <- gsub('31 Mat 1827', '31 March 1827', convict.sub[convict.sub$Convict.Name == 'Askin, John',2])
convict.sub[convict.sub$Convict.Name == 'Callanan, Ann',2] <- gsub('27 Ju 1835', '27 July 1835', convict.sub[convict.sub$Convict.Name == 'Callanan, Ann',2])
convict.sub[convict.sub$Convict.Name == 'Williams, Robert',2] <- gsub('on September 1833', 'on 01 September 1833', convict.sub[convict.sub$Convict.Name == 'Williams, Robert',2])
convict.sub[convict.sub$Convict.Name == 'Hooper, Joseph',2] <- gsub('on November 1844', 'on 01 November 1844', convict.sub[convict.sub$Convict.Name == 'Hooper, Joseph',2])
convict.sub[convict.sub$Convict.Name == 'Newman, Martin',2] <- gsub('during 1841', '', convict.sub[convict.sub$Convict.Name == 'Newman, Martin',2])
convict.sub[convict.sub$Convict.Name == 'Roberts, William',2] <- gsub('during 1841', '', convict.sub[convict.sub$Convict.Name == 'Roberts, William',2])
convict.sub[convict.sub$Convict.Name == 'Beckett, Robert',2] <- gsub('50 March 1850', '05 March 1850', convict.sub[convict.sub$Convict.Name == 'Beckett, Robert',2])
convict.sub[convict.sub$Convict.Name == 'Sculley, Darby',2] <- gsub('232 February 1849', '23 February 1849', convict.sub[convict.sub$Convict.Name == 'Sculley, Darby',2])
convict.sub[convict.sub$Convict.Name == 'Smith, William',2] <- gsub('243 July 1835', '24 July 1835', convict.sub[convict.sub$Convict.Name == 'Smith, William',2])
convict.sub[convict.sub$Convict.Name == 'Sullivan, Michael',2] <- gsub('9 - 10 August 1858', '10 August 1858', convict.sub[convict.sub$Convict.Name == 'Sullivan, Michael',2])
convict.sub[convict.sub$Convict.Name == 'Lowe, Thomas',2] <- gsub('234 July 1828', '24 July 1828', convict.sub[convict.sub$Convict.Name == 'Lowe, Thomas',2])
convict.sub[convict.sub$Convict.Name == 'Collett, Thomas',2] <- gsub('Aoril', 'April', convict.sub[convict.sub$Convict.Name == 'Collett, Thomas',2])
convict.sub[convict.sub$Convict.Name == 'Sandys, Horatio',2] <- gsub('day] June 1928', '01 June 1928', convict.sub[convict.sub$Convict.Name == 'Sandys, Horatio',2])
convict.sub[convict.sub$Convict.Name == 'Williamson, Edward',2] <- gsub('26\\+ March 1835', '26 March 1835', convict.sub[convict.sub$Convict.Name == 'Williamson, Edward',2])
convict.sub[convict.sub$Convict.Name == 'Cole, Joseph',2] <- gsub('29 October \\[year', '29 October 1848 [year', convict.sub[convict.sub$Convict.Name == 'Cole, Joseph',2])
convict.sub[1,2] <- gsub('on  August 1837', 'on 01 August 1837', convict.sub[1,2]) #Downes, Thomas

#this finds the first 4 digit number (year) and trims 15 places left of it
#then it look for the first digits and strips right of it
convict.sub["Date.of.Sentence"] <- substring(convict.sub$Sentence.details,
                                             regexpr("[[:digit:]]{4}", convict.sub$Sentence.details) - 15,
                                             regexpr("[[:digit:]]{4}", convict.sub$Sentence.details) + 4)
convict.sub["Date.of.Sentence"] <- substring(convict.sub$Date.of.Sentence,
                                             regexpr("[[:digit:]]{1}", convict.sub$Date.of.Sentence))

#these 11 records have no date of sentence, or only the year. set all to NA
convict.sub[13220,"Date.of.Sentence"] <- NA #Williams, John
convict.sub[13221,"Date.of.Sentence"] <- NA #Thompson, William
convict.sub[13222,"Date.of.Sentence"] <- NA #Dickinson, Samuel
convict.sub[13247,"Date.of.Sentence"] <- NA #Willams, John
convict.sub[21103,"Date.of.Sentence"] <- NA #Dickford, John
convict.sub[27732,"Date.of.Sentence"] <- NA #Bennett, Samuel
convict.sub[33396,"Date.of.Sentence"] <- NA #Monkton, Henry
convict.sub[35891,"Date.of.Sentence"] <- NA #Hughes, William
convict.sub[62406,"Date.of.Sentence"] <- NA #Wilson, Joseph Manning
convict.sub[74897,"Date.of.Sentence"] <- NA #Cook, William
convict.sub[84221,"Date.of.Sentence"] <- NA #Eastwood, Joshua

#convert remaining failed dates to NA. data had no year at all.
convict.sub$Date.of.Sentence <- gsub('Con', NA, convict.sub$Date.of.Sentence)

#remove all messy ., th 1st rd sept
convict.sub$Date.of.Sentence <- gsub('[.,]', '', convict.sub$Date.of.Sentence)
convict.sub$Date.of.Sentence <- gsub('th|rd|nd', '', convict.sub$Date.of.Sentence)
convict.sub$Date.of.Sentence <- gsub(' Sept ', ' September ', convict.sub$Date.of.Sentence)
convict.sub$Date.of.Sentence <- gsub('1st', '1', convict.sub$Date.of.Sentence)

#testing only... do not run!
#convict.sub$Date.of.Sentence <- ifelse(is.na(as.Date(convict.sub$Date.of.Sentence, "%d %B %Y")),
#                                       convict.sub$Date.of.Sentence,
#                                       NA)

#real run... convert all values to date. This creates no NAs now that all data is fixed
convict.sub$Date.of.Sentence <- as.Date(convict.sub$Date.of.Sentence, "%d %B %Y")

#run this to see details of NA Date.of.Sentence records 
convict.sub[is.na(convict.sub$Date.of.Sentence),c(1,2,3,7)]
convict.sub[(is.na(convict.sub$Date.of.Sentence)==FALSE),c(1,2,3,7)]

##########################################################
##       6. extract (Ticket.of.Leave)                   ##
##########################################################
#add code to strip Ticket of leave
convict.sub["Ticket.of.Leave"] <- ifelse(regexpr("[Tt]icket", convict.sub$Sentence.details) == "-1", "N", "Y")


##########################################################
##       7. Analysis of data                            ##
##########################################################
#quality measure information
unique(convict.sub$Place.of.Sentence)
table(convict.sub$County.Colony)
unique(convict.sub$Sentence.Duration)
unique(convict.sub$Place.of.Arrival)
sum(convict.sub$Ticket.of.Leave == "Y")/nrow(convict.sub)
                                    
sum(is.na(convict.sub$Convict.Name))
sum(is.na(convict.sub$Sentence.details))
sum(is.na(convict.sub$Date.of.Departure))
sum(is.na(convict.sub$Place.of.Arrival))
sum(is.na(convict.sub$Place.of.Sentence))
sum(is.na(convict.sub$Sentence.Duration))
sum(is.na(convict.sub$Date.of.Sentence))
sum(is.na(convict.sub$Ticket.of.Leave))


##########################################################
##   8. Create dataset of no NA for use with tasks      ##
##########################################################

#re-order attributes
#1 = Convict.Name, 5 = County.Colony, 6 = Town, 7 = Place.of.Sentence, 3 = Date.of.Sentence
#4 = Date.of.Departure, 10 = Ticket.of.Leave, 8 = Sentence.Duration
convicts <- convict.sub[,c(1,5,3,4,10,8)] 

#include only complete cases
convicts <- convicts[complete.cases(convicts),]

#remove unused data
rm(dataraw, convict, convict.sub, counties, towns, i, rows, rows1)

#write data to csv so you don't have to do everything above when crashing/reloading
write.table(convicts, file = "convicts.csv", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

####################################################################################################################
##                                                                                                                ##
##   9. Predicting (start here)                                                                                   ##
##                                                                                                                ##
####################################################################################################################

#read csv of convicts data
convicts <- read.csv("convicts.csv", sep = " ")
#reset row index for our subset
rownames(convicts) <- 1:nrow(convicts)

#define a colour scale for SOMs (credit Shane Lynn)
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

#convert sentence duration to a scale of 1(0:7) - 2(8:10) - 3(11:15) - 4(16:life sentence)
convicts[,6] <- ifelse(convicts[,6] <= 7,
                                1, ifelse(convicts[,6] <= 10,
                                            2, ifelse(convicts[,6] <= 15,
                                                        3, 4)))

#convert date of departure to a scale of year by year
convicts[,3] <- as.Date(convicts[,3])
convicts[,3] <- format(convicts[,3], format="%Y")

#display table of our our new factors (we are looking for uniform distribution)
#This is good compared to the table for convicts without categorical grouping
table(convicts$County.Colony)
table(convicts$Date.of.Departure)

#convert all attributes to numeric values
convicts$County.Colony <- as.numeric(convicts$County.Colony)
convicts$Date.of.Departure <- as.numeric(convicts$Date.of.Departure)
convicts$Place.of.Arrival <- as.numeric(convicts$Place.of.Arrival)
convicts$Ticket.of.Leave <- as.numeric(convicts$Ticket.of.Leave)
convicts$Sentence.Duration <- as.numeric(convicts$Sentence.Duration)

##########################################################
##   10. SOM                                            ##
##########################################################

#SOM
data_train <- convicts[,c(2,3,4,5,6)] #relevant attributes

#now train the SOM using the Kohonen method
data_train_matrix <- as.matrix(scale(data_train))
names(data_train_matrix) <- names(data_train)
require(kohonen)
som_grid <- somgrid(xdim = 14, ydim =14, topo = "hexagonal")  

# Train the SOM model
system.time(som_model <- som(data_train_matrix, 
                             grid=som_grid, 
                             rlen=500, 
                             alpha=c(0.05,0.01),
                             keep.data = TRUE ))

#remove grid, data train
rm(som_grid, data_train, data_train_matrix)
#par(mfrow=c(2,2)) #split plot window 2*2
#par(mfrow=c(1,1)) #restore plot window

#changes v iteration
plot(som_model, type = "changes")
#counts within nodes
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)
#map quality
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)
#neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)
#codes
plot(som_model, type = "codes")

#Plot the original scale heatmap for a variable from the training set:
var <- 5 #define the variable to plot
var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2]
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], palette.name=coolBlueHotRed)
rm(var_unscaled, var)


##########################################################
##   11. MLP  (very bad at predicting!!!)               ##
##########################################################

#set Target atribute and training values
trainTargets <- decodeClassLabels(convicts[,6])
trainValues <- convicts[,2:5]
#standard split for training and test, plus normalizing of data
trainset <- splitForTrainingAndTest(trainValues, trainTargets, ratio=0.65)
trainset <- normTrainingAndTestSet(trainset, type = "norm")

#train the MLP
model <- mlp(trainset$inputsTrain, trainset$targetsTrain, size = c(25), maxit = 500,
             initFunc = "Randomize Weights", initFuncParams = c(-0.4, 0.4),
             learnFunc = "Std_Backpropagation", learnFuncParams = c(0.001, .0001),
             updateFunc = "Topological_Order", updateFuncParams = c(0.001),
             inputsTest = trainset$inputsTest, targetsTest = trainset$targetsTest)

#remove train targets and values
rm(trainTargets, trainValues)
#use predict() on the model, using test inputs
predictTestSet <- predict(model,trainset$inputsTest)
#test the model against test targets
confusionMatrix(trainset$targetsTest,predictTestSet)


##########################################################
##   12. Trees                                          ##
##########################################################

#rpart
DT.rpart <- rpart(Sentence.Duration ~ Place.of.Arrival + Date.of.Departure + Ticket.of.Leave, convicts)
print(DT.rpart)
plot(DT.rpart)
text(DT.rpart)

#ctree
DT.ctree <- ctree(Sentence.Duration ~  Place.of.Arrival + Date.of.Departure + Ticket.of.Leave,
                  convicts, controls = ctree_control(maxdepth = 3))
print(DT.ctree)
plot(DT.ctree)
##########################################################
##   13. support vector machines                       ##
##########################################################
req_convicts <- convicts[,c(2,3,4,5,6)]
#converting into matrix as vector is too large in dataframe
req_convicts.matrix <- as.matrix(scale(req_convicts))
norm.convicts <- normalizeData(req_convicts.matrix, type="0_1")
norm.convicts_1 <- normalizeData(req_convicts.matrix, type="center")
norm.convicts_2 <- normalizeData(req_convicts.matrix, type = "norm")

library(kernlab)
set.seed(1)
data(req_convicts.matrix)
n <- nrow(req_convicts.matrix)
traindata <- sample(1:n, 55500, replace = FALSE)
testdata <- setdiff(1:n, traindata)
convictstrain <- req_convicts.matrix[traindata,]
convictstest <- req_convicts.matrix[testdata,]
svm.kernal <- ksvm(Ticket.of.Leave ~ ., 
               data = convictstrain, 
               kernel = "rbfdot",
               kpar = list(sigma = 0.05), 
               C = 5, 
               cross = 3) 
svm.kernal
granting <- predict(svm.kernal,convictstest[,-1])


table(truth=convictstest[,4], prediction=granting)
plot(svm.kernal, data = traindata, grid = 50 , slice = list(County.Colony = 3, Place.of.Arrival = 6))
is.recursive(c("Ticket.of.Leave"))
is.atomic(c("Ticket.of.Leave"))
is.matrix(req_convicts.matrix)


##linear svm
library(e1071)
library(ggplot2)
install.packages("ggplot2")
svm.linear <- svm(V4 ~ V2+V3, data = norm.convicts, kernal = "linear")
svm.linear
plot(svm.linear, data = norm.convicts)

