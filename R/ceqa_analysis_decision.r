# Table 4. Determinants of CEQA Level of Review: Results from Multinomial Logistic Regression (Project Level)

library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(texreg)
library(effects)

#input data
all_demo_envir <- readRDS("new_dataset/all_demo_envir_copy.rds") 
dim(all_demo_envir)   #7148


###############################SECTION 1. TRANSFORM VARIABLES  ###############################
#============================ tract level


all_demo_envir$pop_den <- (all_demo_envir$"Population Density (Per Sq. Mile)")/1000
all_demo_envir$ln_pop_den <- log(all_demo_envir$pop_den+1)
all_demo_envir$white_ratio <- (all_demo_envir$"% Total Population: Not Hispanic or Latino: White Alone") / 100

all_demo_envir$pop_char_Score <- all_demo_envir$"Pop. Char. Score" 

all_demo_envir$PM <- all_demo_envir$"PM2.5"
all_demo_envir$Cleanup_Sites <- all_demo_envir$"Cleanup Sites"
all_demo_envir$Haz_Waste <- all_demo_envir$"Haz. Waste"
all_demo_envir$Imp_Water_Bodies <- all_demo_envir$"Imp. Water Bodies"
all_demo_envir$Pollution_Burden_Score <- all_demo_envir$"Pollution Burden Score"

all_demo_envir$N_actions <- all_demo_envir$"N of actions" 
all_demo_envir$N_issues <- all_demo_envir$"N of issues"

######################### SECTION 3.  REGRESSION (TRACT) #########################
#  Table 4. Determinants of CEQA Level of Review: Results from Multinomial Logistic Regression (Project Level)
require(nnet)
#setting the reference level
all_demo_envir$Type <- as.factor(all_demo_envir$"Document Type")
levels(all_demo_envir$Type)
all_demo_envir$Type <- relevel(all_demo_envir$Type, ref = "NEG")



relabs <- c('ln(pop. density)','pop. vulnerability',
            'white %','pollution burden',"PM2.5",
            "cleanup sites",
            "hazardous waste",
            "impaired water bodies",'N_issues')

terms <- c('ln_pop_den','pop_char_Score','white_ratio','Pollution_Burden_Score','N_issues')
subterms <- c('PM','Cleanup_Sites','Haz_Waste','Imp_Water_Bodies')


sumtable(all_demo_envir,vars =  c(terms,subterms),
         labels = relabs,
         summ = c('mean(x)','sd(x)','min(x)','max(x)','notNA(x)'),
         file = 'result/table2b.html')


subd_scale <- all_demo_envir %>% mutate_if(is.numeric,scale)


#Table 4
type_tract_m1 <- multinom(Type ~  N_issues 
                          + ln_pop_den + white_ratio + pop_char_Score 
                          + Pollution_Burden_Score
                          , data = subd_scale)

type_tract_m2 <- multinom(Type ~  N_issues 
                          + ln_pop_den + white_ratio*pop_char_Score 
                          + Pollution_Burden_Score
                          , data = subd_scale)

htmlreg(list(type_tract_m1, type_tract_m2),
        file = "result/table4.html",
        single.row = T,
        stars = c(0.01, 0.05, 0.1),
        digits = 3,
        caption = "Determinants of CEQA Decisions",
        include.nobs = TRUE,
        caption.above = TRUE)





