library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(rstatix)
library(ggpubr)


total <- readRDS("new_dataset/total_copy.rds")
names (total)
dim(total) #13371    74

# STEP1. see year trend (received year)

plot_year_type <- total %>%
            group_by(`year`, `Document Type`) %>%
              summarise(
                n = n()
              )
write.csv(plot_year_type,"result/plot_year_type.csv", row.names = FALSE)


#STEP2: see issue distribution
plot_issue_n <- total %>%
                group_by(`Document Type`, `N of issues`) %>%
                  summarise(
                    n = n()
                  ) 


total$issues <- str_split(total$"issues combined",  ",")
plot_issue_type <- total %>% group_by(`Document Type`) %>% unnest(col = issues) %>% count(issues)
                  

plot_issue_type <- plot_issue_type %>% group_by(`Document Type`) %>% mutate(percent = 100 * prop.table(n))

write.csv(plot_issue_type,"result/plot_issue_type.csv", row.names = FALSE)




#STEP3: calculate energy ratio and greenhouse gas ratio
 
#calculte energy ratio
test <- subset(total, total$year >=2019)
test$issues <- str_split(test$"issues combined",  ",")
issue_test <- test %>% unnest(col = issues) %>% count(issues)
issue_test <- issue_test %>% mutate(percent = 100 * prop.table(n))
#ratio in the manucript: 14/13371*100=0.1047042%

# #calculte Greenhouse Gas ratio
test_ghg <- total
test_ghg$issues <- str_split(test_ghg$"issues combined",  ",")
issue_test_ghg <- test_ghg %>% unnest(col = issues) %>% count(issues)
issue_test_ghg <- issue_test_ghg %>% mutate(percent = 100 * prop.table(n))
#ratio in the manucript: 135/13371*100=1.009648%


#STEP4: see action distribution

total$actions <- str_split(total$"actions combined",  ",")
plot_action_type <- total %>% group_by(`Document Type`) %>% unnest(col = actions) %>%count(actions)
plot_action_type <- subset(plot_action_type, actions!="NA")

write.csv(plot_action_type,"result/plot_action_type.csv", row.names = FALSE)


#STEP5: summary n
total %>%
  group_by(`Document Type`) %>%
  get_summary_stats(`N of issues`, type = "mean_sd")
total %>%
  group_by(`Document Type`) %>%
  get_summary_stats(`N of actions`, type = "mean_sd")





#STEP6: count word frequencies in document title (N=13371)

library(tidytext)

total[10]
#6.1 Total
project_name <- data.frame(Project.Title = total$`Project Title`) # tibble aka neater data frame

project_name_clean <- project_name %>%
    mutate(Project.Title = str_remove_all(Project.Title, " a | be | had | it | only | she | was | about | because | has | its | of | some | we | after | been | have | last | on | such | were | all | but | he | more | one | than | when | also | by | her | most | or | that | which | an | can | his | mr | other | the | who | any | co | if | mrs | out | their | will | and | corp | in | ms | over | there | with | are | could | inc | mz | s | they | would | as | for | into | no | so | this | up | at | from | is | not | says | to | No.|A |If |The |Is | No |At |1|2|3|4|5|6|7|8|9|0| Alameda| Alpine| Amador| Butte| Calaveras| Colusa| Contra Costa| Del Norte| El Dorado| Fresno| Glenn| Humboldt| Imperial| Inyo| Kern| Kings| Lake| Lassen| Los Angeles| Madera| Marin| Mariposa| Mendocino| Merced| Modoc| Mono| Monterey| Napa| Nevada| Orange| Placer| Plumas| Riverside| Sacramento| San Benito| San Bernardino| San Diego| San Francisco| San Joaquin| San Luis Obispo| San Mateo| Santa Barbara| Santa Clara| Santa Cruz| Shasta| Sierra| Siskiyou| Solano| Sonoma| Stanislaus| Sutter| Tehama| Trinity| Tulare| Tuolumne| Ventura| Yolo| Yuba| County| CA| Annexation| Coastal Permit| Community Plan| Design Review| General Plan Amendment| General Plan Element| General Plan Update| Grading Permit| Land Division| Local Coastal Permit| Master Plan| Other Action| Planned Unit Development| Prezone| Redevelopment| Rezone| Site Plan| Specific Plan| Subdivision| Use Permit| Variance| Waste Management Plan| Williamson Act Cancellation"))


project_name_words <- project_name_clean %>% unnest_tokens(output = word, input = Project.Title) 
project_name_wordcounts <- project_name_words %>% count(word, sort = TRUE)
head(project_name_wordcounts,100)


write.csv(project_name_wordcounts,"result/project_name_wordcounts.csv") 

#6.2 Three Types

#total_eir <- subset (total, total$"Document Type"=="EIR")
#total_mnd <- subset (total, total$"Document Type"=="MND")
#total_neg <- subset (total, total$"Document Type"=="NEG")

regex_list<-"\\ba\\b|\\bbe\\b|\\bhad\\b|\\bit\\b|\\bonly\\b|\\bshe\\b|\\bwas\\b|\\babout\\b|\\bbecause\\b|\\bhas\\b|\\bits\\b|\\bof\\b|\\bsome\\b|\\bwe\\b|\\bafter\\b|\\bbeen\\b|\\bhave\\b|\\blast\\b|\\bon\\b|\\bsuch\\b|\\bwere\\b|\\ball\\b|\\bbut\\b|\\bhe\\b|\\bmore\\b|\\bone\\b|\\bthan\\b|\\bwhen\\b|\\balso\\b|\\bby\\b|\\bher\\b|\\bmost\\b|\\bor\\b|\\bthat\\b|\\bwhich\\b|\\ban\\b|\\bcan\\b|\\bhis\\b|\\bmr\\b|\\bother\\b|\\bthe\\b|\\bwho\\b|\\bany\\b|\\bco\\b|\\bif\\b|\\bmrs\\b|\\bout\\b|\\btheir\\b|\\bwill\\b|\\band\\b|\\bcorp\\b|\\bin\\b|\\bms\\b|\\bover\\b|\\bthere\\b|\\bwith\\b|\\bare\\b|\\bcould\\b|\\binc\\b|\\bmz\\b|\\bs\\b|\\bthey\\b|\\bwould\\b|\\bas\\b|\\bfor\\b|\\binto\\b|\\bno\\b|\\bso\\b|\\bthis\\b|\\bup\\b|\\bat\\b|\\bfrom\\b|\\bis\\b|\\bnot\\b|\\bsays\\b|\\bto\\b|\\bNo.|A\\b|If\\b|The\\b|Is\\b|\\bNo\\b|At\\b|1|2|3|4|5|6|7|8|9|0|\\bAlameda|\\bAlpine|\\bAmador|\\bButte|\\bCalaveras|\\bColusa|\\bContra\\bCosta|\\bDel\\bNorte|\\bEl\\bDorado|\\bFresno|\\bGlenn|\\bHumboldt|\\bImperial|\\bInyo|\\bKern|\\bKings|\\bLake|\\bLassen|\\bLos\\bAngeles|\\bMadera|\\bMarin|\\bMariposa|\\bMendocino|\\bMerced|\\bModoc|\\bMono|\\bMonterey|\\bNapa|\\bNevada|\\bOrange|\\bPlacer|\\bPlumas|\\bRiverside|\\bSacramento|\\bSan\\bBenito|\\bSan\\bBernardino|\\bSan\\bDiego|\\bSan\\bFrancisco|\\bSan\\bJoaquin|\\bSan\\bLuis\\bObispo|\\bSan\\bMateo|\\bSanta\\bBarbara|\\bSanta\\bClara|\\bSanta\\bCruz|\\bShasta|\\bSierra|\\bSiskiyou|\\bSolano|\\bSonoma|\\bStanislaus|\\bSutter|\\bTehama|\\bTrinity|\\bTulare|\\bTuolumne|\\bVentura|\\bYolo|\\bYuba|\\bCounty|\\bCA|\\bAnnexation|\\bCoastal\\bPermit|\\bCommunity\\bPlan|\\bDesign\\bReview|\\bGeneral\\bPlan\\bAmendment|\\bGeneral\\bPlan\\bElement|\\bGeneral\\bPlan\\bUpdate|\\bGrading\\bPermit|\\bLand\\bDivision|\\bLocal\\bCoastal\\bPermit|\\bMaster\\bPlan|\\bOther\\bAction|\\bPlanned\\bUnit\\bDevelopment|\\bPrezone|\\bRedevelopment|\\bRezone|\\bSite\\bPlan|\\bSpecific\\bPlan|\\bSubdivision|\\bUse\\bPermit|\\bVariance|\\bWaste\\bManagement\\bPlan|\\bWilliamson\\bAct\\bCancellation"
total$Project.Title <- str_remove_all(total$`Project Title`,regex_list)
total_list <- split(total,f = total$`Document Type`)
project_word_counts <- lapply(total_list,function(x) {
  x %>% unnest_tokens(output = word, input = Project.Title) %>% count(word, sort = TRUE)
})
lapply(seq_along(project_word_counts),function(x){
  write.csv(project_word_counts[[x]],paste0("result/project_name_wordcounts_",tolower(names(project_word_counts)[[x]]),".csv") )
  })





