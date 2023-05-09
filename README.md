# CEQA-infra

This repository includes R codes, data, and other files for the paper "Environmental Justice, Infrastructure Provisioning, and Environmental Impact Assessment: Evidence from the California Environmental Quality Act”

The file "data cleaning notes" presents an overview of data cleaning process.



There are five R code files in the "R" folder.
  File "1.ceqa_code_basic.r" is used to clean raw data about reviewing agencies, local actions, project issues, and projects' information captured from CEQAnet website by Professor Tyler Scott.

  File "2.ceqa_des.r" is used to get descriptive statistics mentioned in this paper, such as the distribution of project issues and local actions, the distribution of different projects across years, the word frequency mentioned in project titles (N=13371; the research period = 2011-2020)

  File "3.ceqa_clean_tract.r" is used to pair our project data with Calenviroscreen and ACS data at the tract level. In this part, we only keep projects whose coordinates are known. 

  File "4.ceqa_analysis_number.r" is used for predict factors that are associated with the number of infrastructure proposed in each census tracts. In this part, the dependent variable is the number of projects proposed in one tract in a certain year. This is a panel dataset (N of observations = 80570; the research period = 2011-2020). Fixed Effects Negative Binomial Regression is employed.

  File "5.ceqa_analysis_decision.r" is used for predict factors that are associated with the review level for projects under CEQA (Project Level). In this part, the dependent variable is the review level (eir, nd, and mnd). Multinomial Logistic Regression is employed (N of observations = 7126).





There are three files in the "recode_folder" folder, which will be used when cleaning raw datasets and runing R codes.

  File "review_agency_re" is a list of recoded reviewing agencies, and will be paired with our orginial dataset.
  
  File "lead_agency_re" is a list of recoded lead agencies, and will be merged with our orginal dataset.
  
  File "issue_type_re" is a list of recoded project issues, and will be merged with our orginal dataset.
  
  
  
In CEQAnet dataset folder, you can find original datasets captured from the CEQAnet website.  
