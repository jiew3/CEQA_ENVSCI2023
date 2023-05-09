# Table 3. The Number of CEQA Proposed Projects by Census Tract: Results from Fixed Effects Negative Binomial Regression (Tract Level)

library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(texreg)
library(data.table)
library(effects)
library(pglm)
#devtools::install_github("benjaminguinaudeau/margins.pglm")
library(margins.pglm)
n_tract2 <- readRDS("new_dataset/n_tract2_copy.rds") 
rownames(n_tract2) <-NULL

######################### SECTION 1.   The distribution of project numbers #########################
table(n_tract2$tract_data_year)


table(n_tract2$n)



######################### SECTION 2.   Independent Variables #########################

n_tract2$pop_den <- (n_tract2$"Population Density (Per Sq. Mile)")/1000
n_tract2$ln_pop_den <- log(n_tract2$pop_den+1)
n_tract2$white_ratio <- (n_tract2$"% Total Population: Not Hispanic or Latino: White Alone") / 100

n_tract2$pop_char_Score <- n_tract2$"Pop. Char. Score" 

n_tract2$PM <- n_tract2$"PM2.5"
n_tract2$Cleanup_Sites <- n_tract2$"Cleanup Sites"
n_tract2$Haz_Waste <- n_tract2$"Haz. Waste"
n_tract2$Imp_Water_Bodies <- n_tract2$"Imp. Water Bodies"
n_tract2$Pollution_Burden_Score <- n_tract2$"Pollution Burden Score"


#### something about integer64 makes pglm mad 
n_tract2 <- n_tract2[,sapply(n_tract2,class)!='integer64',with = F]

# ######################### SECTION 3.   Correlation: at the tract level #########################

# ( Appendix A. Correlation between Independent Variables at the Census Tract Level)

library(Hmisc)
corr_tract <- n_tract2[, c("n", "ln_pop_den", "white_ratio","Pop. Char. Score", "Pollution Burden Score", "PM2.5", "Cleanup Sites", "Haz. Waste", "Imp. Water Bodies")]

corr_tract <- rcorr(as.matrix(corr_tract), type=c("pearson"))
corr_tract <- round (rbind (corr_tract$r,corr_tract$n,corr_tract$P),4)

write.csv(corr_tract,"result/corr_tract.csv", row.names = FALSE)  


# ######################### SECTION 4.  NB FIXED: Table 3 #########################
#Table 3. The Number of CEQA Proposed Projects by Census Tract: Results from Fixed Effects Negative Binomial Regression (Tract Level)

library(pglm)
### flagging for follow up might need fixest for interaction plots to work
#library(fixest)

###### this converts observations to 3 periods -- 1 per screen year ####
###### mean value for each period by FIPS ####
terms <- c('ln_pop_den','pop_char_Score','white_ratio','Pollution_Burden_Score')
subterms <- c('PM','Cleanup_Sites','Haz_Waste','Imp_Water_Bodies')

period_tract <- n_tract2[,lapply(.SD,mean,na.rm=T),by=.(FIPS,screen_year),.SDcols = c(terms,subterms)]
n_period <- n_tract2[,sum(n),by=.(FIPS,screen_year)]
setnames(n_period,'V1','n_period')

period_tract <- merge(period_tract,n_period)
subd <- period_tract
library(vtable)

relabs <- c('ln(pop. density)','pop. vulnerability',
            'white %','pollution burden',"PM2.5",
            "cleanup sites",
            "hazardous waste",
            "impaired water bodies",'# projects')

sumtable(subd,vars =  c(terms,subterms,'n_period'),
         labels = relabs,
         summ = c('mean(x)','sd(x)','min(x)','max(x)','notNA(x)'),
         file = 'result/table2.html')

subd$FIPS <- as.character(subd$FIPS)
subd$screen_year <- as.factor(subd$screen_year)

subd <- subd[FIPS %in% subd[,sum(!is.na(Pollution_Burden_Score)&!is.na(pop_char_Score)),by=.(FIPS)][V1==3,]$FIPS,]


round(table(subd$n_period)/nrow(subd),2)
subd_scale <- subd %>% mutate_if(is.numeric,scale)
subd_scale$n_period<-subd$n_period

formulas <- list(base_form = reformulate(termlabels = terms,response = 'n_period',intercept = T),
                 interact = reformulate(termlabels = c(terms,'white_ratio:pop_char_Score'),
                                        response = 'n_period',intercept = T))

base_mod <- pglm(formula = formulas$base_form,family="negbin", 
                 effect = "twoways", model="within",
                 data = subd_scale,index=c("FIPS","screen_year"),
                 method = 'nr')

interaction_mod <- pglm(formula = formulas$interact,
                        family="negbin", effect = "twoways",
                        model="within",
                        data = subd_scale,index=c("FIPS","screen_year"),method = 'nr')


coefnames <- c('(Intercept)',relabs,"vulnerability*white %")
htmlreg(list(base_mod,interaction_mod),
          #custom.coef.names = coefnames,
file = "result/table3.html",
single.row = TRUE,
stars = c(0.01, 0.05, 0.1),
digits = 3,
caption = "Fixed-Effect Negative Binomial Regression",
custom.gof.rows = list("tract fixed effects" = c("YES", "YES"),
                       'screen period fixed effects' = c('YES','YES'),
                       'n. obs' = c(length(base_mod$model$n_period),length(interaction_mod$model$n_period))),
include.nobs = TRUE,
caption.above = TRUE)


terms2 <- c('ln_pop_den','pop_char_Score','white_ratio')
subterms <- c('PM','Cleanup_Sites','Haz_Waste','Imp_Water_Bodies')
formulas2 <- lapply(subterms,function(f) reformulate(termlabels = c(terms2,f),response = 'n_period',intercept = T))

mods2 <- list(
pglm(formulas2[[1]],family="negbin", 
     effect = "twoways", model="within",
     data = subd_scale,index=c("FIPS","screen_year"),
     method = 'nr'),
pglm(formulas2[[2]],family="negbin", 
     effect = "twoways", model="within",
     data = subd_scale,index=c("FIPS","screen_year"),
     method = 'nr'),
pglm(formulas2[[3]],family="negbin", 
     effect = "twoways", model="within",
     data = subd_scale,index=c("FIPS","screen_year"),
     method = 'nr'),
pglm(formulas2[[4]],family="negbin", 
     effect = "twoways", model="within",
     data = subd_scale,index=c("FIPS","screen_year"),
     method = 'nr'))


#coefnames <- c('(Intercept)',relabs,"vulnerability*white %")
htmlreg(mods2,
        #custom.coef.names = coefnames,
        file = "result/tableA2.html",
        single.row = TRUE,
        stars = c(0.01, 0.05, 0.1),
        digits = 3,
        caption = "Fixed-Effect Negative Binomial Regression",
        custom.gof.rows = list("tract fixed effects" = rep("YES",length(mods2)),
                               'screen period fixed effects' = rep("YES",length(mods2)),
                               'n. obs' = sapply(mods2,function(x) length(x$model$n_period))),
        include.nobs = TRUE,
        caption.above = TRUE)








