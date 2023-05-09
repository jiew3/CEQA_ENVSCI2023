# CEQA-infra

This repository includes R codes, data, and other files for the paper "Environmental Justice, Infrastructure Provisioning, and Environmental Impact Assessment: Evidence from the California Environmental Quality Act”

For description:
  File "ceqa_des.r" is used to get descriptive statistics mentioned in this paper, such as the distribution of project issues and local actions, the distribution of different projects across years, the word frequency mentioned in project titles (N=13371; the research period = 2011-2020)

For model analysis:
  File "ceqa_analysis_number.r" is used for predict factors that are associated with the number of infrastructure proposed in each census tracts. In this part, the dependent variable is the number of projects proposed in one tract in a certain year. This is a panel dataset (N of observations = 80570; the research period = 2011-2020). Fixed Effects Negative Binomial Regression is employed.  For dataset, refer to https://github.com/jiew3/CEQA_ENVSCI2023/releases/tag/n_tract2_copy.rds

  File "ceqa_analysis_decision.r" is used for predict factors that are associated with the review level for projects under CEQA (Project Level). In this part, the dependent variable is the review level (eir, nd, and mnd). Multinomial Logistic Regression is employed (N of observations = 7126).

