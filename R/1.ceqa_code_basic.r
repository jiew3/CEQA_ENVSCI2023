# FIN = final EIR
# MND = mitigated negative declaration
# NOE = notice of exemption
# NEG = negative declaration
# SCH = the State Clearinghouse (SCH) -> at the center of state agency involvement in the CEQA environmental review process
# NOC = Notice of Completion -> After preparation of the draft EIR, a NOC must be submitted to the Office of Planning and Research which includes project location, location of review copies, and public comment review period information.
# NOD = notice of determination -> the lead agency may decide whether or how to approve or carry out the project at which time a NOD must be filed within five days of approval


library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(lubridate)
library(epiR)
library(tidyr)

#############################################SECTION 1: CLEANING ORIGINAL DATASETS CAPTURED FROM CEQAnet ##########################################################


# Step 1. Data Input
doc_files <- list.files('CEQAnet dataset/',pattern = 'rds',full.names = T)
doc_list <- lapply(doc_files,readRDS)
doc_dt <- rbindlist(doc_list,use.names = T,fill = T)
doc_dt$`SCH Number`<- as.integer(doc_dt$`SCH Number`)

local_action <-read.csv("CEQAnet dataset/sch_local_action.csv")
print(ls(local_action))
colnames(local_action) <- c("SCH Number", "Local Action")

project_issue <-fread("CEQAnet dataset/sch_project_issues.csv")
print(ls(project_issue))
colnames(project_issue) <- c("SCH Number", "Project Issue")

review_agency <-read.csv("CEQAnet dataset/sch_reviewing_agencies.csv")
print(ls(review_agency))
colnames(review_agency) <- c("SCH Number", "Reviewing Agency")


#Step 5. Describe NOE File

doc_dt$Received <- as.Date(doc_dt$Received,format = "%m/%d/%Y")# change the format of Received Time
doc_dt <- doc_dt[order(Received,decreasing = F)]# ordered by the decreasing received data

type_data_all <- doc_dt 
#type_data_all <- rbind(EIR, MND, NEG, NOE) #merge four types of files into one
#type_data_all$Received <- as.Date(type_data_all$Received, format = "%m/%d/%Y") # change the format of Received Time
range (type_data_all$Received) # (2000-01-03" - "2021-10-01)
dim (type_data_all)  # [1] 170991    51
   # create a Year Variable: used when matching 
type_data_all$year <-year(type_data_all$Received)
  # see SCH Number
range(type_data_all$"SCH Number")  #200404814 2109088043

#replace "" to NA
names(type_data_all[, sapply(type_data_all, class) == 'character']) # find character variables


fillNA <- function(x){ifelse(x=="",NA,x)}
type_data_all <- type_data_all %>% mutate_if(is.character,fillNA)


########################################################################
# Step 10. recode all variables in the document file (no deletion, N = 170991)

# find the number of missing values
miss <- colSums(is.na(type_data_all))
miss <- data.frame(miss)

    #!!!(1). Recode Lead Agencies!!!!!!!!!!!!

# describe orginial lead agency from CEQAnet
lead_agency <-table(type_data_all$"Lead Agency")
lead_agency <- data.frame(lead_agency)
summary(lead_agency)
colnames(lead_agency) <- c('Lead Agency', 'Freq')
lead_agency <- lead_agency[order(lead_agency$Freq, decreasing = TRUE),]
print(head (lead_agency,10))
# write.csv(lead_agency, "/share/crsp/lab/ulibarri/jiew35/CEQA/recode_folder/lead_agency_re.csv", row.names = FALSE)

# import new recoded lead agencies
lead_agency_re <- read_csv("recode_folder/lead_agency_re.csv") 

        #to merge recoded data:
            # Remove Leading/Trailing Whitespace of "lead agency" variable 
type_data_all$"Lead Agency" <- trimws(type_data_all$"Lead Agency")
           # merge the recoded lead_agency dataset to type dataset
type_data_all <- merge(type_data_all,lead_agency_re,by="Lead Agency",all.x=TRUE)
dim(type_data_all)   #170991     56

        #describe the recoded lead agency
dim(table(type_data_all$"Lead Agency")) #2892 original groups of lead agencies
dim(table(type_data_all$"la_rename"))  #2819  renamed groups of lead agencies
dim(table(type_data_all$"la_topic_2"))  #40 types of topics
table(type_data_all$"la_level")  # 10 kinds of administrative levels

# colSums(is.na(type_data_all))   
      
    #!!!(2). Recode location coordinates 

        #check the validity of coordinate variables and see how they look like
lc <- type_data_all %>% select("SCH Number", "Location Coordinates")
lc <- lc[order(lc$"Location Coordinates", decreasing = TRUE),]
lc <- na.omit(lc) 
# write.csv(lc, "/share/crsp/lab/ulibarri/jiew35/CEQA/recode_folder/coordinate.csv", row.names = FALSE)



        # change the format of projects' coordinates
         # NOTES: the format changes from dms to dd (Degrees (°), minutes ('), seconds ('') to decimal degrees) 
         # latitudes in the southern hemisphere are negative while longitudes in the western hemisphere are negative. 
          # '\xb0' means °

type_data_all$coordinates <- type_data_all$"Location Coordinates"
type_data_all$coordinates <- str_replace_all(type_data_all$coordinates,'\\xb0',"°")
ew <- str_extract(type_data_all$coordinates,'[EW]*$')
ew[is.na(ew)|ew==""]<-NA
ns <- str_remove_all(str_extract(type_data_all$coordinates,'[NS]\\s'),'\\s')
ns[is.na(ns)|ns==""]<-NA
type_data_all$coordinates <- str_extract_all(type_data_all$coordinates,'[0-9\\.]{1,}')

dms_sets <- lapply(seq_along(type_data_all$coordinates),function(i){
  #print(i) 
  x <- type_data_all$coordinates[[i]]
  if(is.na(x[1])|length(x)!=6){matrix(NA,nrow = 1,ncol = 2)}else{
tempmat <- matrix(as.numeric(x),byrow = T,nrow = 2)
t(epi.dms(tempmat))}})

dms_dt <- data.table(do.call(rbind,dms_sets))
colnames(dms_dt)<-c('latitude','longitude')
type_data_all<-cbind(type_data_all,dms_dt)
type_data_all$latitude <- ifelse(ns=='N',type_data_all$latitude ,-1*type_data_all$latitude )
type_data_all$longitude <- ifelse(ew=='E',type_data_all$longitude,-1*type_data_all$longitude )


        #!!!(3).recode the variable of "Cities": 

           # == see the combination of cities captured from CEQAnet
# table (type_data_all$Cities)  
city_type <- data.frame(table(type_data_all$Cities))  #the variable of "Cities" is a list of cities for projects
colnames (city_type) <- c("Cities", "Freq")
city_type <- city_type[order(city_type$Freq, decreasing = TRUE),] 
dim(data.frame(city_type)) # city_type: the combination of cities

           # == see each city (seperate cities)
type_data_all$city_list <- str_split(type_data_all$Cities,  ", ")

city_list <- type_data_all %>%   #city_list: the distribution of each city in the dataset,not a combination
      unnest(col = city_list) %>%
      count(city_list)

#saveRDS(city_list,"recode_folder/city_list_re.rds")

        # == count the number of cities for each project 

type_data_all$city_list_n <- sapply(lapply(type_data_all$city_list,na.omit),length)
type_data_all$city_list_n[type_data_all$city_list_n == 0] <- NA

        # == recode the number of cities for each project 
        #   [city_list_dummy = 1 (multiple cities), 0 (one city) ]

type_data_all$city_list_dummy <- (type_data_all$city_list_n>1)+0


         #!!!(4).recode the variable of "Counties": 

        # == see the combination of counties 
table (type_data_all$Counties)
county_type <- data.frame(table(type_data_all$Counties))
colnames (county_type) <- c("Counties", "Freq")
county_type <- county_type[order(county_type$Freq, decreasing = TRUE),] 
dim(data.frame(county_type)) # county_type: the combination of counties


         # == see each county
type_data_all$county_list <- str_split(type_data_all$Counties,  ", ")


county_list <- type_data_all %>%   #county_list: the distribution of each city in the dataset,not a combination
      unnest(col = county_list) %>%
      count(county_list)

      # == count the number of counties for each project 

type_data_all$county_list_n <- sapply(lapply(type_data_all$county_list,na.omit),length)
type_data_all$county_list_n[type_data_all$county_list_n == 0] <- NA

        # == recode the number of counties for each project 
        #   [county_list_dummy = 1 (multiple counties), 0 (one county) ]

type_data_all$county_list_dummy <- (type_data_all$county_list_n>1)+0


dim(type_data_all)  # 170991     65

# export dataset, except coordinate, city_list, county_list, which are lists.
type_data_all_copy <- type_data_all[,sapply(type_data_all,class)!='list',with = F]
saveRDS(type_data_all_copy,"new_dataset/type_data_all_copy.rds")

#############################################SECTION 2: SELECT THREE TYPES ##########################################################



        # !!!(1) if we want to select certain document types


type_data_three <- filter(type_data_all, type_data_all$"Document Type" != "NOE")       
dim(type_data_three)  #54944    65

        #  !!!(2) filter data in 2011:2020 (current research)
   
type_data_three_2011 <- subset(type_data_three, str_extract(type_data_three$"SCH Number", "^\\d{4}") >=2011 & str_extract(type_data_three$"SCH Number", "^\\d{4}") <=2020)       # str_extract(type_data_three$"SCH Number", "^\\d{4}")   # extract the first 4 digits of SCH number


dim(type_data_three_2011)   # 18931


                # received year before 2021

type_data_three_2011 <- subset(type_data_three_2011, type_data_three_2011$year <=2020)         
dim(type_data_three_2011)         # 18701


          #!!!(3) drop duplicated SCHs and keep the earliest one

       # sort the received data
type_data_three_2011 <- type_data_three_2011[order(type_data_three_2011$Received, decreasing = FALSE),] # ordered by the increasing received data
summary(duplicated(type_data_three_2011$"SCH Number")) #summary the duplicate data: 17225 are false; 1476  true


type_data_three_unique <- type_data_three_2011[!duplicated(type_data_three_2011$"SCH Number"),]  #dataset without duplicate data


# #DOUBLE CHECK EXAMPLE: used to check whether it is the earliest data that is kept for the same SCH
# y <- type_data_three_unique[type_data_three_unique$"SCH Number" == "2018112008", ]
# y$Received
# x<- (type_data_all[type_data_all$"SCH Number" == "2018112008", ])
# x$Received


# After deleting duplicates：
# EIR	2593			MND	11037			NEG	3595		
table(type_data_three_unique$"Document Type")

# Describe type_data_unique (combined three document types)
dim(type_data_three_unique)  #N = 17225 
# names(type_data_three_unique)

# Know the number of missing values for each variable
colSums(is.na(type_data_three_unique))


# export dataset, except coordinate, city_list, county_list, which are lists.
type_data_three_unique_copy <- type_data_three_unique[,sapply(type_data_three_unique_copy,class)!='list',with = F]
saveRDS(type_data_three_unique_copy,"new_dataset/type_data_three_unique_copy.rds")
dim(type_data_three_unique_copy)

################################################SECTION 4: THREE FEATURES #################################################

# Step 1. Describe Project Issues

colSums(is.na(project_issue)) # count the number of missing values

project_issue <- na.omit(project_issue) # remove missing values of SCH Number
dim (project_issue) # 1057488 

issue_type <- table(project_issue$"Project Issue") # summarize the project issue by categories
issue_type <- 100 * prop.table(issue_type) #unit is %)
issue_type <- data.frame(issue_type) # change the table to dataframe

colnames(issue_type) <- c('Project Issue', 'Freq') # change the name of the dataframe
issue_type <- issue_type[order(issue_type$Freq, decreasing = TRUE),]  # sort the data by frequencies
# summary(issue_type)

print(issue_type)


## group by SCH number 
issue_sch <- data.frame (table(project_issue$"SCH Number"))
# issue_sch <- issue_sch [order(issue_sch$Freq, decreasing = TRUE),]
# colnames (issue_sch) <- c ('SCH Number', 'Freq')
summary(issue_sch$Freq)  #the biggest is 49, the mean is 20.12
table(issue_sch$Freq)


 # to merge recoded data

issue_type_re <- read_csv("recode_folder/issue_type_re.csv")
project_issue <- merge(project_issue,issue_type_re,by="Project Issue", all.x = TRUE)

colSums(is.na(project_issue))

dim(project_issue) # 1057488  ;     3
dim(table(project_issue$"Project Issue")) # 53 old types
dim(table(project_issue$project_issue_recoded)) # 29 new types


        # delete SCH numbers with duplicated project_issue_Recoded
project_issue_unique <- project_issue

project_issue_unique <- project_issue_unique[!duplicated(project_issue_unique[,.(`SCH Number`,project_issue_recoded)]),]

dim(project_issue)       # 1057488       3
dim(project_issue_unique)  #672209      3
saveRDS(project_issue_unique,"new_dataset/project_issue_unique.rds")

#Step 2. Describe Local Actions

## group by local action
colSums(is.na(local_action)) # count the number of missing values
local_action <- na.omit(local_action) # remove 0 missing values of SCH Number
dim(local_action)  # 84431


action_type <-table(local_action$"Local Action")
action_type <- 100 * prop.table(action_type)
action_type <- data.frame(action_type)
colnames(action_type) <- c('Local Action', 'Freq')
action_type <- action_type[order(action_type$Freq, decreasing = TRUE),]
summary(action_type)
print(action_type)

#saveRDS(action_type,"new_dataset/action_type_re.rds")

## recode local action
local_action$action_recoded <- local_action$"Local Action"
local_action$action_recoded <- gsub("Community Plan", "Other Plan", local_action$action_recoded)
local_action$action_recoded <- gsub("Specific Plan", "Other Plan", local_action$action_recoded)
local_action$action_recoded <- gsub("Waste Management Plan", "Other Plan", local_action$action_recoded)
local_action$action_recoded <- gsub("Master Plan", "Other Plan", local_action$action_recoded)

local_action$action_recoded <- gsub("General Plan Amendment", "General Plan", local_action$action_recoded)
local_action$action_recoded <- gsub("General Plan Element", "General Plan", local_action$action_recoded)
local_action$action_recoded <- gsub("General Plan Update", "General Plan", local_action$action_recoded)

local_action$action_recoded <- gsub("Prezone", "Zoning", local_action$action_recoded)
local_action$action_recoded <- gsub("Rezone", "Zoning", local_action$action_recoded)
local_action$action_recoded <- gsub("Variance", "Zoning", local_action$action_recoded)

local_action$action_recoded <- gsub(" \\(Subdivision, etc.)", "", local_action$action_recoded)
local_action$action_recoded <- gsub("Subdivision", "Land Division", local_action$action_recoded)
local_action$action_recoded <- gsub("Planned Unit Development", "Land Division", local_action$action_recoded)


local_action$action_recoded <- gsub("Local Coastal Permit", "Coastal Permit", local_action$action_recoded)
local_action$action_recoded <- gsub("Williamson Act Cancellation", "Other Action", local_action$action_recoded)


table(local_action$action_recoded) #12 types

        # delete SCH numbers with duplicated action_recoded

local_action_unique <- local_action
local_action_unique <- data.frame(local_action_unique)
local_action_unique <- local_action_unique[!duplicated(local_action_unique[c("SCH.Number","action_recoded")]),]
colnames(local_action_unique) <- c('SCH Number', 'Local Action', "action_recoded")

dim(local_action) # 84431     3
dim(local_action_unique)  #  73684     3


saveRDS(local_action_unique,"new_dataset/local_action_unique.rds")

#Step 3. Describe Reviewing Agencies
colSums(is.na(review_agency)) 
review_agency <- na.omit(review_agency) #remove missing values of SCH Number
dim(review_agency) # n = 1100523

review_type <- table(review_agency$"Reviewing Agency")
review_type <- 100* prop.table(review_type)
review_type <- data.frame(review_type)

colnames(review_type) <- c('Reviewing Agency', 'Freq')
review_type <- review_type[order(review_type$Freq, decreasing = TRUE),] 
summary(review_type)

        #!!!recode reviewing agencies!!!!!!!!!!!!

        # to merge recoded data
#### current data has 224 rows, 7 columns
review_agency_re <- read_csv("recode_folder/review_agency_re.csv")

review_agency <- merge(review_agency,review_agency_re,by="Reviewing Agency", all.x = TRUE)

### THIS IS OLD NUMBER, NOT SURE OF DIFFERENCE
#dim(review_agency) # 1100559; 6
#### THIS IS NEW NUMBER, WHICH REFLECTS DIMENSION OF CURRENT DATA
dim(review_agency) # 1100523; 8

### OLD VALUES
#dim(table(review_agency$"Reviewing Agency")) # 224 old types
#dim(table(review_agency$ra_recoded)) # 59 new types
#dim(table(review_agency$ra_recoded_abb)) 
### NEW VALUES
dim(table(review_agency$"Reviewing Agency")) # 196 old types
dim(table(review_agency$ra_recoded)) # 79 new types
dim(table(review_agency$ra_recoded_abb)) 

      # delete SCH numbers with duplicated ra_recoded

review_agency_unique <- review_agency
review_agency_unique <- data.frame(review_agency_unique)
review_agency_unique <- review_agency_unique[!duplicated(review_agency_unique[c("SCH.Number","ra_recoded")]),]
colnames(review_agency_unique) <- c("Reviewing Agency",'SCH Number', 'ra_rename', "ra_recoded", "ra_recoded_abb",   "ra_recoded_level","ra_topic","ra_topicemployee")

### AGAIN, OLD VALUES
#dim(review_agency) # 1100559     6
#dim(review_agency_unique)  #  670568     8

### NEW VALUES
dim(review_agency) # 1100523     8
### weirdly this one is the same
dim(review_agency_unique)  #  670568     8

saveRDS(review_agency_unique,"new_dataset/review_agency_unique.rds")

# !!! Step 4. Combine Project Issue 
require(data.table)
project_issue_comb <- setDT(project_issue_unique)[,.(`project_issue_recoded`= paste(`project_issue_recoded`, collapse = ",")), by=`SCH Number`] 
# issue_type_comb <- data.frame (table(project_issue_comb$"Project_Issue"))
# summary(issue_type_comb$Freq)

project_issue_n <- project_issue_unique %>% group_by(`SCH Number`) %>% count(n())  # calculate the number of issues for each SCH

project_issue_n <- project_issue_n %>% select(c("SCH Number","n"))
                                                  
project_issue_n$issue.type <- (project_issue_n$n>1)+0 #create a new variable, if N of issues >1, coded as 1, otherwise 0
table(project_issue_n$issue.type)

project_issue_comb <- merge(project_issue_comb,project_issue_n, by = "SCH Number", all.x = TRUE) # merge two datasets
colnames(project_issue_comb) <-c('SCH Number','issues combined', 'N of issues', 'more than 1 issue')

# Step 5.Combine Local Action 
require(data.table)
local_action_comb <- setDT(local_action_unique)[,.(`action_recoded`= paste(`action_recoded`, collapse = ",")), by=`SCH Number`] 
# action_type_comb <- data.frame (table(local_action_comb$"Local Action"))
# summary(action_type_comb$Freq)



local_action_n <- local_action_unique %>%
    group_by(`SCH Number`) %>%
  summarise(n())  # calculate the number of actions for each SCH


names(local_action_n)[2] <- "n"

local_action_n$Local.Action <- ifelse(local_action_n$n > 1, 1, 0) #create a new variable, if N of actions >1, coded as 1, otherwise 0
table(local_action_n$Local.Action)

local_action_comb <- merge(local_action_comb,local_action_n, by = "SCH Number", all.x = TRUE) # merge two datasets
colnames(local_action_comb) <-c('SCH Number','actions combined', 'N of actions', 'more than 1 action')




# Step 6.Combine Reviewing Agency
require(data.table)

review_agency_comb <- setDT(review_agency_unique)[,.(`ra_recoded_abb`= paste(`ra_recoded_abb`, collapse = ",")), by=`SCH Number`] 
# review_type_comb <- data.frame (table(review_agency_comb$"Reviewing Agency"))
# summary(review_type_comb$Freq)



review_agency_n <- review_agency_unique %>%
    group_by(`SCH Number`) %>%
  summarise(n())  # calculate the number of agencies for each SCH

names(review_agency_n)[2] <- "n"

review_agency_n$Reviewing.Agency <- (review_agency_n$n > 1)+0 #create a new variable, if N of agencies >1, coded as 1, otherwise 0
table(review_agency_n$Reviewing.Agency)

review_agency_comb <- merge(review_agency_comb,review_agency_n, by = "SCH Number") # merge two datasets
colnames(review_agency_comb) <-c('SCH Number','agencies combined', 'N of agencies', 'more than 1 review')


saveRDS(local_action_comb,"new_dataset/local_action_comb.rds")
saveRDS(project_issue_comb,"new_dataset/project_issue_comb.rds")
saveRDS(review_agency_comb,"new_dataset/review_agency_comb.rds")


colSums(is.na(local_action_comb))
colSums(is.na(project_issue_comb))
colSums(is.na(review_agency_comb))

### these still match exactly
dim(local_action_comb)   # 47103
dim(project_issue_comb)  # 52553
dim(review_agency_comb)  # 56896


# Step 7. Merge all into One Dataset

   #merge three feature file
total_feature <- merge(review_agency_comb,project_issue_comb,by="SCH Number")
dim(total_feature)  # 52461
total_feature <- merge(total_feature,local_action_comb,by="SCH Number")
dim(total_feature)  # 43894    10
saveRDS(total_feature,"new_dataset/total_feature.rds")





################################################SECTION 5: COMBINE THREE TYPES WITH THREE FEATURES #################################################

total <- merge(type_data_three_unique, total_feature, by="SCH Number", all.X=TRUE)

# total is the dataset that includes three types, 2011-2020, and feature data

dim (total)  # 13371    74


total_copy <- total[,sapply(total,class)!='list',with = F]
saveRDS(total_copy,"new_dataset/total_copy.rds")






