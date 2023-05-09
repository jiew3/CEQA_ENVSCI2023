library(stringr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(data.table)
#####################################SECTION 1: CENSUS TRACT BASED ON COORDINATES###########################################


                #get the census tract
library(maps)
library(maptools)
total <- readRDS('new_dataset/total_copy.rds')
type_data_geo <- total[!is.na(total$latitude)&!is.na(total$longitude), ]
dim(type_data_geo)            # 7311    75  

library(tigris)
library(sf)
california <- tracts(state = 'CA',year = 2010,class = 'sf')
#california <- readShapePoly("/share/crsp/lab/ulibarri/jiew35/CEQA/data/map/tl_2019_06_tract.shp")


proj_points <- type_data_geo %>% 
  as.data.frame %>% 
  sf::st_as_sf(coords = c('longitude','latitude'),crs = st_crs(california))

which_tract <- st_within(proj_points,california)
which_tract[sapply(which_tract,length)==0] <- NA

type_data_geo$GEOID10 <- california$GEOID10[unlist(which_tract)]
type_data_geo$tractID <- type_data_geo$GEOID10 

               #describe tracts' information
length(which(!is.na (type_data_geo$tractID))) # 7126 coordinate values 
type_data_tract <- data.frame(table(type_data_geo$tractID)) 

# type_data_tract[2][type_data_tract[2]==0] <- NA
# type_data_tract <- type_data_tract[complete.cases(type_data_tract[2]),]
# type_data_tract <- type_data_tract[order(type_data_tract[2], decreasing = FALSE),] 
# dim(type_data_tract)  #2899

            # drop rows with missing values on tractID (tractID: id of census tracts)
dim(type_data_geo)  # 7311   77 
type_data_geo <- type_data_geo[!is.na(type_data_geo$tractID), ]
dim(type_data_geo)  # 7126   77 

type_data_geo$id <- paste(type_data_geo$year,type_data_geo$GEOID10,sep = '_')

#!!!!! export coordinate dataset, except tract (too large).


type_data_geo_copy <- type_data_geo[,sapply(type_data_geo,class)!='list',with = F]
saveRDS(type_data_geo_copy,"new_dataset/type_data_geo_copy.rds")

################################ SECTION 2. see the calenviroscreen data ############################# 
calscreens <- list.files('data/envir_data_folder/',full.names = T)
cal_list <- lapply(calscreens,fread)

drop_version_specific_name <- function(x) gsub('\\s[2-4]\\.0',"",x)

names(cal_list)<-calscreens

colnames(cal_list[[1]])<-drop_version_specific_name(colnames(cal_list[[1]]))
colnames(cal_list[[2]])<-drop_version_specific_name(colnames(cal_list[[2]]))
colnames(cal_list[[3]])<-drop_version_specific_name(colnames(cal_list[[3]]))

yrs <- c(2014,2017,2021)
for(i in seq_along(cal_list)){
  cal_list[[i]]$screen_year <- yrs[i]
}

enviroscreen <- rbindlist(cal_list,use.names = T,fill = T)




# 2011-2014 use calenviroscreen2014; 2015-2017 use calenviroscreen2017; 2018-2020 use calenviroscreen2021
enviroscreen$Census_Tract<-enviroscreen$`Census Tract`
## data.table rolling join

tract_year_combos <- data.table(expand.grid(Census_Tract=unique(enviroscreen$`Census Tract`),Year = 2011:2020))
tract_year_combos$join_time <- tract_year_combos$Year
enviroscreen$join_time <- enviroscreen$screen_year
setkey(enviroscreen, Census_Tract, join_time)
setkey(tract_year_combos, Census_Tract,join_time)

tract_year_combos <- enviroscreen[tract_year_combos, roll = -Inf]  

saveRDS(tract_year_combos,"new_dataset/envir_data_copy.rds")

tract_year_combos$tractID <-paste0('0',tract_year_combos$Census_Tract`)


#### here 
################################SECTION 3: DEMO DATA AT TRACT LEVEL ################################
census_files <- list.files('data/tract_data_folder/',full.names = T)

census_list <- lapply(seq_along(census_files),function(x) fread(census_files[x]) %>% .[,!duplicated(names(.)),with = F]%>%mutate(tract_data_year = str_extract(census_files[x],'[0-9]{4}')))
tract_data<-rbindlist(census_list ,use.names = T,fill = T)
dim(tract_data) # 80570    163

# drop variables that are all missing 
tract_data <- tract_data[ , colSums(is.na(tract_data)) < nrow(tract_data),with = F]  # Remove rows with NA only
dim(tract_data)  #  80570     120
# head(tract_data$FIPS,5)

#change the format of census tract(FIPS), used for data matching
tract_data$FIPS <- paste0("0", tract_data$FIPS)
tract_data$id<-paste(tract_data$tract_data_year,tract_data$FIPS,sep = '_')

dim(tract_data[duplicated(tract_data$id),])  # id does not have duplicates
tract_data_copy <- tract_data
saveRDS(tract_data_copy,"new_dataset/tract_data_copy.rds")
#tract_data is a combined census tract dataset (2010-2019)


################################SECTION 4: MERGE TYPE DATA GEO and CALSCREEN DATA, and then demog data ################################
#enviroscreen$id <- paste(enviroscreen$screen_year,paste0(0,enviroscreen$`Census Tract`),sep = '_')

### this sets up rolling join where tracts are joined by FIPS and enviroscreen year that most closely follows ACS year
tract_data$join_time <- as.numeric(tract_data$tract_data_year)
tract_data$id <- as.character(tract_data$FIPS)
setkey(tract_data,id,join_time)
enviroscreen$id <- as.character(paste0(0,enviroscreen$`Census Tract`))
enviroscreen$join_time<-as.numeric(enviroscreen$join_time)
setkey(enviroscreen,id,join_time)
demo_envir <- enviroscreen[tract_data, roll = -Inf]
##confirmed that dimension is same as prior approach
dim(demo_envir)  #80570   187

demo_envir_copy <- demo_envir
saveRDS(demo_envir_copy,"new_dataset/demo_envir_copy.csv")

### ovewrite id to make unique tract-year id
demo_envir$id <- paste(demo_envir$tract_data_year,paste0(0,demo_envir$`Census Tract`),sep = '_')

all_demo_envir <- merge(type_data_geo, demo_envir, by = "id", all.x = TRUE)
dim(all_demo_envir) #7126
all_demo_envir_copy <- all_demo_envir[,sapply(all_demo_envir,class)!='list',with = F]
saveRDS(all_demo_envir_copy,"new_dataset/all_demo_envir_copy.rds") 


dim(all_demo_envir_copy) # 7126  259

##########################SECTION 5: DV: N of projects by tracts #########################

#get the number of projects by year and census tract
n_tract <- type_data_geo %>%
                group_by(`tractID`, `year`) %>%
                  summarise(
                    n = n()
                  )   #5772
table(n_tract[3])

n_tract$id<-paste(n_tract$year,n_tract$tractID,sep = '_')

      # !!!!!!!Adding zero for census tracts without proposed projects to form a panel dataset
n_tract2 <- merge(demo_envir, n_tract, by = "id", all.x = TRUE)   
dim(n_tract2) # 80570   190

# names(n_tract2)  
# colSums(is.na(n_tract2))

n_tract2$n[is.na(n_tract2$n)] <- 0
table(n_tract2$n)

saveRDS(n_tract2,"new_dataset/n_tract2_copy.rds") 

