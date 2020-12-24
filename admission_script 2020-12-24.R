# Install package if required

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidymodels)) install.packages("tidymodels", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(janitor)) install.packages("janitor", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
if(!require(gt)) install.packages("gt", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(DALEX)) install.packages("DALEX", repos = "http://cran.us.r-project.org")
if(!require(vip)) install.packages("vip", repos = "http://cran.us.r-project.org")
if(!require(auditor)) install.packages("auditor", repos = "http://cran.us.r-project.org")


#load packages
library(tidyverse)
library(tidymodels)
library(caret)
library(GGally)
library(janitor)
library(corrplot)
library(Matrix)
library(gt)
library(ggcorrplot)
library(gridExtra)
library(DALEX)
library(vip)
library(auditor)

# Import data from github
library(readr)
Admission_Predict <- read_csv("https://raw.githubusercontent.com/HTMorris/Capstone-Project-2-Predicting-Graduate-Admission/main/Admission_Predict_Ver1.1.csv")
View(Admission_Predict)

# clean variable names 
Admission_Predict <- Admission_Predict %>% 
  clean_names()  # clean names will remove space between column names and replace it with an underscore, as well as place column names in lower case

# find out the number of unique values of variables
Admission_Predict %>% 
  summarise(across(1:9, n_distinct)) # number of unique observations for each variable/column




# save Admission_Predict_Ver1_1 data file to be used in Rmarkdown
saveRDS(Admission_Predict, file = "Admission_Predict.Rds")

# skim dataset - discriptive statistics of dataset
skimr::skim(Admission_Predict)

# Serial No. 
# GRE Scores (out of 340) graduate record examination
#TOEFL Scores ( out of 120 ) Test of English as a Foreign Language
#University Rating ( out of 5 ) - ranking of applicant university
#Statement of Purpose (SOP) {out of 5}
# Letter of Recommendation Strength ( out of 5 )
#Undergraduate GPA ( out of 10 )
#Research Experience ( either 0 or 1 )
#Chance of Admit ( ranging from 0 to 1 )




# Data wrangling

adm_df <- Admission_Predict %>% 
   mutate(university_rating = as.factor(university_rating),    # transform university rating to categorical variable
         research = as.factor(research)                       # transform research to a categorical variable
                  ) %>% 
  select(-serial_no)                                          # remove serial number from dataset


# save adm_df data file to be used in Rmarkdown
saveRDS(adm_df, file = "adm_df.Rds")


#skim adm_df
skimr::skim(adm_df) 

normality <- shapiro.test(adm_df$chance_of_admit) # test for normality

round(normality$p.value, 6) # reject the null hypothesis of normality

## Chance of Admits density curve
adm_df %>% 
  ggplot(aes(x=chance_of_admit))+    # developing graph of chance of admit
  geom_density()+      # add density layer
  theme_bw()                  # theme black and white

# Split Data into training and testing set

set.seed(1)  # set seed of random numbers so results can be replicated.
test_index <- createDataPartition(y = adm_df$chance_of_admit, times = 1, p = 0.2, list = FALSE)
adm_train <- adm_df[-test_index,]   # training dataset
adm_test <- adm_df[test_index,]     # testing dataset

# save adm_train and adm_test data file to be used in Rmarkdown
saveRDS(adm_train, file = "adm_train.Rds")
saveRDS(adm_test, file = "adm_test.Rds")

# Data exploration

indicator <- c("min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max")  # recreate row name for Table 1

Table1 <- bind_cols(  data.frame(
                      Indicator = indicator,                                      # add indicator name
                      "chance_of_admit" = (summary(adm_train$chance_of_admit) %>% # presents summary statistics for chance of admit   
                        tibble() %>%                                              # convert to tibble
                        round(.,digits = 4) %>%                                   # set decimal place to 4 places
                        rename("chance of admit"=1)) ),                           # rename column name to chance of admit
            
            data.frame(
                      "cgpa" = (summary(adm_train$cgpa ) %>%                      #summary statistics for cgpa
                                     tibble() %>%                                 # convert to tibble
                                     round(.,digits = 3) %>%                      # round to 3 decimal places
                                     rename("cgpa"=1)) ),                         # rename column to cgpa
            data.frame(
              "gre_score" = (summary(adm_train$gre_score ) %>%                    # Summary Statistics for gre_score
                          tibble() %>%                                            # convert to tibble
                          round(.,digits = 1) %>%                                 # round to 1 decimal place
                          rename("gre_score"=1)) ),                               # rename column name to gre_score
  
            data.frame(
              "toefl_score" = (summary(adm_train$toefl_score) %>%                 # summary statistics for toefl_score
                               tibble() %>%                                       # convert to tibble
                               round(.,digits = 1) %>%                            # round to 1 decimal place
                               rename("toefl_score"=1)) ),                        # rename column name to toefl_score
            
            data.frame(
              "sop" = (summary(adm_train$sop) %>%                                 # summary statistics for sop
                                 tibble() %>%                                     # convert to tibble
                                 round(.,digits = 3) %>%                          # round to three decimal place
                                 rename("sop"=1)) ),                              # rename column name to sop
            data.frame(
              "lor" = (summary(adm_train$lor) %>%                                 # summary statistics for lor
                         tibble() %>%                                             # convert to tibble
                         round(.,digits = 2) %>%                                  # round to two decimal place
                         rename("lor"=1)) )                                       # rename column name to lor
    ) 
 

# save adm_train and adm_test data file to be used in Rmarkdown
saveRDS(Table1, file = "Table1.Rds")


  

fig1a<- adm_train %>% 
  group_by(research) %>%  #group by research experience
summarise(count=n()) %>%      # count the number of observation in each research category
ggplot(aes(x=research, y=count, label=count))+  # create graph
  geom_segment(aes(xend=research, yend=0))+     # add line from origin
  geom_point(size=6, colour="red")+                           # add point, set size of point to 4
  geom_text(nudge_y = 10)+                       # add label and add space between point and label (number)
  coord_flip()+                                 # flip coordinates
  theme_bw()                                    # add theme, black and white


fig1b <-adm_train %>% 
  group_by(university_rating) %>%   # group by university rating
  summarise(count=n()) %>%               # count the number of observation in each university rating
ggplot(aes(x=university_rating, y=count, label=count))+  # create graph
  geom_segment(aes(xend=university_rating, yend=0))+     # add line from origin
  geom_point(size=6, colour="red")+                           # add point, set size of point to 4
  geom_text(nudge_y = 6)+                       # add label and add space between point and label (number)
  coord_flip()+                                 # flip coordinates
  theme_bw()                                    # add theme, black and white



library(gridExtra)

fig1 <- grid.arrange(fig1a, fig1b, nrow=2, top="Figure 1: Frequency Distribution of Research and University Rating")

# save ggplot
ggsave(fig1, filename = "fig1.png", width = 7)
  

## boxplot of all variables
adm_train %>% 
  mutate(across(where(is.factor), as.numeric)) %>% # convert factors into numeric to facilitate the grouping of variables into two columns
  mutate(research = if_else(research==2,1,0)) %>%   # conversion to numeric recoded research into 2 and 1, this was recoded to original number
  pivot_longer(1:8, names_to = "indicator", values_to = "value") %>%   # reshape the eight columns into 2 columns (long format), two columns, one for the name of the indicator and one for the value
  ggplot(aes(x=indicator, y=value, colour=indicator))+  # create graph 
  geom_boxplot()+    # create box plot
  facet_wrap(vars(indicator), scales = "free") +  # create separate boxplot with different scale.
  labs(title = "Figure 1: Box Plot of Variables",   # create title of graph
       x="Academic Profile",                         # create title of x axis
       y="value")+   # create title for graph        # create title of y axis
  theme_bw()+ # theme to give a black and white background
  theme(legend.position = "none") 




# Chance of admission


## Create Histogram

adm_train %>% 
  ggplot(aes(x=chance_of_admit))+   #create graph
  geom_histogram( binwidth = 0.07, fill ="grey", colour="black") + # create histogram. binwidth chosen with the range of the data in mind
  theme_bw()+   # black and white background
  labs(title = "Figure 2: Histogram of Chance of Admit", # add title
       x= "Chance of admit (binwidth=0.07)"              # add x-axis
       
  )  


## Create histogram and density plot grouped by research experience
adm_train %>% 
  ggplot(aes(x=chance_of_admit))+   #create graph
  geom_histogram(  aes(y=..density..), binwidth = 0.07, fill ="grey", colour="black") + # create histogram. binwidth chosen with the range of the data in mind
  geom_density()+
  theme_bw()+
  labs(title = "Figure 2-1: Histogram of Chance of Admit by Research Experience", # add title
       x= "Chance of admit (binwidth=0.07)"              # add x-axis
        )   +
  facet_grid(vars(research))  # group by research experience


## Create histogram and density plot grouped by university ranking
adm_train %>% 
  ggplot(aes(x=chance_of_admit))+   #create graph
  geom_histogram(  aes(y=..density..), binwidth = 0.07, fill ="grey", colour="black") + # create histogram. binwidth chosen with the range of the data in mind
  geom_density()+
  theme_bw()+
  labs(title = "Figure 2-2: Histogram of Chance of Admit by University Rating", # add title
       x= "Chance of admit (binwidth=0.07)"              # add x-axis
       )   +
  facet_grid(vars(university_rating)) # group by university ratings

# University Ratings
adm_train %>% 
  select(chance_of_admit, cgpa, gre_score, toefl_score, sop, lor, research, university_rating) %>%   # reordering columns 
  mutate(research = as.numeric(research)) %>% # convert factors into numeric to facilitate the grouping of variables into two columns
  mutate(research = if_else(research==2,1,0)) %>%   # conversion to numeric recoded research into 2 and 1, this was recoded to original number
  pivot_longer(1:7, names_to = "indicator", values_to = "value") %>%   # reshape the eight columns into 2 columns (long format), two columns, one for the name of the indicator and one for the value
  ggplot(aes(x=indicator, y=value, colour=university_rating))+  # create graph 
  geom_boxplot(alpha=0.5) +    # create box plot and alpha refers to the opacity of a geom.
  facet_wrap(vars(indicator), scales = "free") +  # create separate graph for each indicator
  labs(title = "Figure 3: Academic Profile Disaggregated by Applicant University Ranking", # add title
       x = "Academic Profile"  )+                                        # add x axis title
  theme_bw() 


### The higher the university ratings, the higher the score under each indicator, suggesting that the quality of the university influences the academic profile of the person
## there are a few outliers, especially for university ranking 3 to 5, suggesting that some persons who applied to these university were below expectation

## Research
adm_train %>% 
  select(chance_of_admit, cgpa, gre_score, toefl_score, sop, lor, university_rating, research) %>%   # reordering columns
  mutate(university_rating = as.numeric(university_rating)) %>% # convert factors into numeric to facilitate the grouping of variables into two columns
  pivot_longer(1:7, names_to = "indicator", values_to = "value") %>%   # reshape the eight columns into 2 columns (long format), two columns, one for the name of the indicator and one for the value
  ggplot(aes(x=indicator, y=value, colour=research))+  # create graph 
  geom_boxplot(varwidth=TRUE) +    # create box plot
  facet_wrap(vars(indicator), scales = "free") +  # create separate graph for each indicator
  labs(title = "Figure 4: Academic Profile Disaggregated by Research Experience",
       # add title
       x = "Academic Profile"  )+   # add x axis title
  theme_bw()   # theme to give a black and white background  


# Bivariate Analysis

adm_train %>% 
  select(where(is.numeric)) %>%   # select only numeric variable
  pivot_longer(gre_score:cgpa, names_to = "indicator", values_to = "value") %>%   # reshape the six columns into 3 columns (long format)
  ggplot(aes(x=value, y=chance_of_admit, colour=indicator))+  # create graph, with y= chance of admit and the x axis for the other indicator value and colour is based on indicator
  geom_smooth(method = "lm")+   # add regression line
  geom_point(alpha=0.4)+    # add data point and alpha refers to the opacity of a geom. 
  facet_wrap(vars(indicator), scales = "free") +  # create separate graph for each indicator.
  labs(title = "Figure 5A: Chance of Admission relative to Academic Profile")+  # add title to graph
  theme_bw()+  # theme to give a black and white background
  theme(legend.position = "none") # remove legend 


## Bivariate Analysis disaggregated by University Ratings
adm_train %>% 
  pivot_longer(c(gre_score, toefl_score, cgpa, sop, lor), names_to = "indicator", values_to = "value") %>%   # reshape all numeric except chance of admit into one column for name and one column for value (long format)
  ggplot(aes(x=value, y=chance_of_admit, colour=university_rating))+  # create graph, with y= chance of admit and the x axis for the other indicator value and colour is based on university ratings 
  geom_smooth(method = "lm", se=FALSE)+   # add regression line
  geom_point(alpha=0.1)+    # add data point and alpha refers to the opacity of a geom.
  facet_wrap(vars(indicator), scales = "free") +  # create separate graph for each indicator.
  labs(title = "Figure 5B: Chance of Admission relative to Academic Profile by University Rating")+  # add title to graph
  theme_bw() + 
  theme(plot.title = element_text(size = 11)) #set title size to 11 to keep the title on page of rmarkdown file.


# Bivariate analysis disaggregated by research experience

adm_train %>% 
  pivot_longer(c(gre_score, toefl_score, cgpa, sop, lor), names_to = "indicator", values_to = "value") %>%   # reshape all numeric except chance of admit into one column for name and one column for value (long format)
  ggplot(aes(x=value, y=chance_of_admit, colour=research))+  # create graph, with y= chance of admit and the x axis for the other indicator value and colour is based on university ratings 
  geom_smooth(method = "lm", se=FALSE)+   # add regression line
  geom_point(alpha=0.1)+    # add data point and alpha refers to the opacity of a geom.
  facet_wrap(vars(indicator), scales = "free") +  # create separate graph for each indicator.
  labs(title = "Figure 5C: Chance of Admission relative to Academic Profile by Research Expereince")+  # add title to graph
  theme_bw()+ 
  theme(plot.title = element_text(size = 11)) #set title size to 11 to keep the title on page of rmarkdown file.




## Correlation Plot
adm_train %>% 
  select(chance_of_admit, cgpa, toefl_score, sop, lor) %>%   # select only numeric variable and reorder 
  cor() %>%   # calculate correlation matrix
  ggcorrplot(                                          # calculate correlation plot
    type = "lower",   # plot the lower portion of the correlation matrix
    lab = TRUE)+      # label the correlation plot
  labs(title="Figure 6: Correlation Plot")  # add title



### METHODOLOGY ###

## Feature engineering

 adm_recipe <-  recipe(chance_of_admit ~ .,    # identify that chance_of_adimit as outcome varible and the remainding variables as predictors
                                    data = adm_train) %>% 
  step_dummy(all_nominal()) %>%             # create dummy variable of non-numeric variables (research and university ratings)
  step_center(all_predictors()) %>%         # center predictors only, that is the average of a variable is subtracted from each observation 
  step_scale(all_predictors())              # scale predictors, that is, each variable is divided by its standard deviation.
  
  
summary(adm_recipe)

adm_train1 <- adm_recipe %>% prep() %>% bake(adm_train)  # apply transformation to training data set
adm_test1 <- adm_recipe %>% prep() %>% bake(adm_test)   # apply transformation to testing data set

# save adm_train1 and adm_test1 data file to be used in Rmarkdown
saveRDS(adm_train1, file = "adm_train1.Rds")
saveRDS(adm_test1, file = "adm_test1.Rds")



### Linear Regression Model


set.seed(1234)    # set seed for replication.
train_lm <- train(
  chance_of_admit ~ .,   # set dependent variable and the rest of variables are predictors
  adm_train1,             # dataset used in model
  method = "lm",  # linear regression 
  tuneGrid= data.frame(intercept = c(TRUE, FALSE)),  # tuning parameter - include or exclude intercept
  trControl = trainControl(   #establing controls
    method = "repeatedcv",     # resampling method
    number = 10,               # number of folds
    repeats = 10,               # The number of time repeated
    verboseIter = TRUE          # show the process
  )
) 

#save results for linear regression model
results_lm <- train_lm$results  # save results of the lm model
saveRDS(results_lm, file = "results_lm.Rds")  # save results of lm model





rmse_lm <- min(train_lm$results$RMSE)  # minimum RMSE for linear regression 
rmsesd_lm <- train_lm$results$RMSESD[which.min(train_lm$results$RMSE)]  # RMSE standard deviation associated with minimum RMSE for linear regression

bind_rows(data.frame(model = "Linear Regression",      # describe model type
                     RMSE = rmse_lm,                   # RMSE from the best fit model  (model with lowest RMSE)
                     RMSE_lower = rmse_lm-rmsesd_lm,   # RMSE less one standard deviation of RMSE - lower band of error band
                     RMSE_upper = rmse_lm+rmsesd_lm),  # RMSE plus one standard derivation of RMSE - upper band of error band  
          
)

# Plot of Model RMSE results vs Tuning Parameter (Intercept or No Intercept)

fig7a <- train_lm$results %>%  # results of the model
  ggplot(aes(x = intercept, y = RMSE, colour=intercept)) +    # produce ggplot of RMSE and the number of the variables to include
  geom_line() + # add line graph
  geom_point() + # add point
  scale_y_continuous(breaks = seq(0.05,0.8,0.05))+   #add scale to view graph
  geom_errorbar(aes(x = intercept,   # add error band
                    ymin = RMSE - RMSESD,   # lower bound of RMSE with one standard deviation
                    ymax = RMSE + RMSESD))+ # upper bound of RMSE with one standard deviation
  labs(title = "Figure 7a: Tuned RMSE") +  # add title to graph
  theme_bw()+  # black and white background
  theme(legend.position = "none")  # remove legend






# Plot of variable importance

pred_vip <- function(object, newdata) {   # create prediction wrapper for vip
  predict(object, newdata = newdata)
}

# Variable importance plot
set.seed(1234)  # for reproducibility
fig7b <- vip(train_lm, method = "firm", # method used variance based method
    nsim = 1000,   # number of simulation
    ice = TRUE, # individual conditional expectation
    train = adm_train1, # training data
    target = "chance_of_admit", # target is the depedent variable
    metric = "rmse",  # metric used to choose
    pred_wrapper = pred_vip,   # pred function created
    scale=TRUE)+  # scale the variable importance
  labs(title = "Figure 7b: Importance")+ # add title
  theme_bw()




# Dalex Package Variable explance and residual model
library(DALEX)
lm_exp <- DALEX::explain(train_lm, label = "lm", data = adm_train1, y = adm_train1$chance_of_admit)
library(auditor)
# model residual (mr)
lm_mr <- model_residual(lm_exp)

# Plot predicted value and target variable and residual
fig7c <- plot(lm_mr, type = "prediction", abline = TRUE)+ # plot of prediction and target variable 
  labs(title = "Figure 7c: Model Fit")+ # add title
  theme_bw()+  #black and white theme
  theme(legend.position = "none")  # remove legend

fig7d <- plot(lm_mr,type = "residual")+ # plot of residual
  labs(title = "Figure 7d:Model Residual")+ # add title
  theme_bw()+  #black and white theme 
  theme(legend.position = "none")  # remove legend

# lower values of chance_of_admit tend to be overestimated in the model

# combining plots of lm
fig7 <- grid.arrange(fig7a, fig7b, fig7c, fig7d, nrow=2, top="Figure 7: Linear Regression Diagnostics")
# save ggplot of fig7
ggsave(fig7, filename = "fig7.png", width = 7)


##STEP SELECTION


set.seed(1234)
train_lmseq <- train(    #train model
  chance_of_admit ~ .,   # dependent variable is chance of admit and the rest of varaibles are potential predictor
  adm_train1,            # dataset
  method = "leapSeq",  # regression with stepwise selection
  tuneGrid = data.frame(nvmax = seq(1, 11, 1)),  # tuning parameter - how many variables to include in model
  trControl = trainControl(
    method = "repeatedcv",     # resampling method
    number = 10,  # the number of folds
    repeats = 10,               # The number of time repeated
    verboseIter = TRUE   # show the process
  )
) 

#save results for linear regression model with stepwise selection
results_lmseq <- train_lmseq$results  # save results of the lm stepwise model
saveRDS(results_lmseq, file = "results_lmseq.Rds")  # save results of lm stepwise model


rmse_lmseq <- min(train_lmseq$results$RMSE)  # minimum RMSE for linear regression 
rmsesd_lmseq <- train_lmseq$results$RMSESD[which.min(train_lmseq$results$RMSE)]  # RMSE standard deviation associated with minimum RMSE for linear regression





#Display results
lmseq_selection <- summary(train_lmseq$finalModel)   # include the subset of the model up to the bestTune parameter

lmseq_selection_table <- lmseq_selection$which %>%   # extract variables selected in the first six round of selection
                            data.frame() %>%           # convert to dataframe
                            slice(6)  %>%              # select only the 6th model, best tuning parameter                       
                            mutate(model = paste("model", sep = "_", 6)) %>%  # create column that labels the row, model 1 to model 6
                            select(model, everything()) %>%   # place model column as the first column 
                            rename(Intercept = X.Intercept.) %>%     # rename x.Intercept. to Intercept for clarity.
                            #filter(model=="model_6") %>%  #filter out only the six model - choosen model
                           # mutate(across(where(is.logical), as.character)) %>%   # convert logical variables to character to facilitate creating a pivot long table
                            pivot_longer(cols = Intercept:research_X1, # pivot long, choose all the colunms
                                         names_to = "Variables", # name given to the column
                                         values_to = "Selection"  # name given to the row in column
                                         ) %>% 
                            mutate(Selection = if_else(Selection=="TRUE", "Included", "Excluded")) # rename Selection, if TRUE, rename to include, Otherwise Excluded

# save table of lmseq_selection                            
saveRDS(lmseq_selection_table, file = "lmseq_selection_table.Rds")  # save results of lm stepwise model

  


### Plot Diagnostics of Linear Stepwise Regression

fig8a <-train_lmseq$results %>%  # results of the model
  ggplot(aes(x = nvmax, y = RMSE), highlight=TRUE) +    # produce ggplot of RMSE and the number of the variables to include
  geom_line() + # add line graph
  geom_point() + # add point
  geom_errorbar(aes(x = nvmax,   # add error band
                    ymin = RMSE - RMSESD,   # lower bound of RMSE with one standard deviation
                    ymax = RMSE + RMSESD))+ # upper bound of RMSE with one standard deviation
  labs(title = "Figure 8a: RMSE")+  # add title to graph
  theme_bw()


# Variable importance plot
set.seed(1234)  # for reproducibility
fig8b <- vip(train_lmseq, method = "firm", # method used variance based method
    nsim = 1000,   # number of simulation
    ice = TRUE, # individual conditional expectation
    train = adm_train1, # training data
    target = "chance_of_admit", # target is the depedent variable
    metric = "rmse",  # metric used to choose
    pred_wrapper = pred_vip,   # pred function created
    scale=TRUE)+  # scale the variable importance
  labs(title = "Figure 8b: Importance")+ # add title
  theme_bw() 




# variable explain

lmseq_exp <- DALEX::explain(train_lmseq, label = "lmseq", data = adm_train1, y = adm_train1$chance_of_admit)

# model residual (mr)
lmseq_mr <- model_residual(lmseq_exp)

# Plot predicted value and target variable and residual
fig8c <- plot(lmseq_mr, type = "prediction", abline = TRUE)+ # plot of prediction and target variable 
  labs(title = "Figure 8c: Model Fit")+ # add title
  theme_bw()+  #black and white theme
  theme(legend.position = "none")  # remove legend

fig8d <- plot(lmseq_mr,type = "residual")+ # plot of residual
  labs(title = "Figure 8d:Model Residual")+ # add title
  theme_bw()+  #black and white theme 
  theme(legend.position = "none")  # remove legend

# lower values of chance_of_admit tend to be overestimated in the model

# combining plots of lm
fig8 <- grid.arrange(fig8a, fig8b, fig8c, fig8d, nrow=2, top="Figure 8: Linear Stepwise Regression Results and Diagnostics")
# save ggplot of fig7
ggsave(fig8, filename = "fig8.png", width = 7)

##################### Random Forest

train_rf <- train(            # training dataset
  chance_of_admit ~ .,        # dependent variable and the rest of variables are potential predictors
  adm_train1,                 # dataset
  method = "rf",              # method used, random forest
  tuneGrid = data.frame(mtry = seq(1, 15, 1)),     # tuning grid, from 1 to 15 by 1
  trControl = trainControl(                    #establish controls
    method = "repeatedcv",                    # resampling method repeates cross validation
    number = 10,                              # number of folds
    repeats = 10,                             # number of time repeated
    verboseIter = TRUE                        # show process
  )
) 

#save results for Random forest model
results_rf <- train_rf$results  # save results of the rf model
saveRDS(results_rf, file = "results_rf.Rds")  # save results of rf model

train_rf$bestTune   # best tune parameter

rmse_rf <- min(train_rf$results$RMSE)  # average minimum RMSE for random forest
rmsesd_rf <- train_rf$results$RMSESD[which.min(train_rf$results$RMSE)]  # RMSE standard deviation associated with minimum RMSE for random forest



# Table of Results

bind_rows(data.frame(model = "Linear Regression",      # describe model type
                     RMSE = rmse_lm,                   # RMSE from the best fit model  (model with lowest RMSE)
                     RMSE_lower = rmse_lm-rmsesd_lm,   # RMSE less one standard deviation of RMSE - lower band of error band
                     RMSE_upper = rmse_lm+rmsesd_lm),  # RMSE plus one standard deivation of RMSE - upper band of error band  
          
          data.frame(model = "Linear Regression with Stepwise Selection",  # describe model type
                     RMSE = rmse_lmseq,                                    # RMSE from the best fit model (model tuning parameters with lowest RMSE) for linear regression with stepwise selection
                     RMSE_lower = rmse_lmseq-rmsesd_lmseq,                 # RMSE less one standard deviation of RMSE - lower band of error band for linear regression with stepwise selection
                     RMSE_upper = rmse_lmseq+rmsesd_lmseq),                # RMSE plus one standard deviation of RMSE - upper band of error band for linear regression with stepwise selection
          
          data.frame(model = "Random Forest",                              # describe model type
                     RMSE = rmse_rf,                                       # RMSE from the best fit model (model tuning parameters with the lowest RMSE) for random forest model
                     RMSE_lower = rmse_rf-rmsesd_rf,                       # RMSE less one standard deviation of RMSE - lower band for error band for random forest model
                     RMSE_upper = rmse_rf+rmsesd_rf)                       # RMSE plus one standard deviation of RMSE - upper band for error band for random forest
          
          
)


### Plot Diagnostics of Random Forest

fig9a <-train_rf$results %>%  # results of the model
  ggplot(aes(x = mtry, y = RMSE), highlight=TRUE) +    # produce ggplot of RMSE and the number of the variables to include
  geom_line() + # add line graph
  geom_point() + # add point
  geom_errorbar(aes(x = mtry,   # add error band
                    ymin = RMSE - RMSESD,   # lower bound of RMSE with one standard deviation
                    ymax = RMSE + RMSESD))+ # upper bound of RMSE with one standard deviation
  labs(title = "Figure 9a: RMSE")+  # add title to graph
  theme_bw()

# Variable importance plot
set.seed(1234)  # for reproducibility
fig9b <- vip(train_rf, method = "firm", # method used variance based method
    nsim = 1000,   # number of simulation
    ice = TRUE, # individual conditional expectation
    train = adm_train1, # training data
    target = "chance_of_admit", # target is the depedent variable
    metric = "rmse",  # metric used to choose
    pred_wrapper = pred_vip,   # pred function created
    scale=TRUE)+  # scale the variable importance
  labs(title = "Figure 9b: Importance")+ # add title
  theme_bw() 
 
# variable explain

rf_exp <- DALEX::explain(train_rf, label = "rf", data = adm_train1, y = adm_train1$chance_of_admit)

# model residual (mr)
rf_mr <- model_residual(rf_exp)

# Plot predicted value and target variable and residual
fig9c <- plot(rf_mr, type = "prediction", abline = TRUE)+ # plot of prediction and target variable 
  labs(title = "Figure 9c: Model Fit")+ # add title
  theme_bw()+  #black and white theme
  theme(legend.position = "none")  # remove legend

fig9d <- plot(rf_mr,type = "residual")+ # plot of residual
  labs(title = "Figure 9d:Model Residual")+ # add title
  theme_bw()+  #black and white theme 
  theme(legend.position = "none")  # remove legend

# lower values of chance_of_admit tend to be overestimated in the model

# combining plots of random forest results and diagnostics
fig9 <- grid.arrange(fig9a, fig9b, fig9c, fig9d, nrow=2, top="Figure 9: Random Forest Results and Diagnostics")
# save ggplot of fig7
ggsave(fig9, filename = "fig9.png", width = 7)





#### Support Vector Machine - Support Vector Machines with Polynomial Kernel
### Grid_svmPoly1 represents the final tuning grid used after trying different combination

# create tuning grid
Grid_svmPoly1 <- expand.grid(degree=seq(1,3,1),  # the polynomial degree
                             scale=c(0.1,0.88,1),  # A scaling factor the the kernel
                             C=seq(0.5,1,0.5))   # the cost of predicting a sample within or on the wrong side of the margin

set.seed(1234)  # set seed for reproducibility
train_svmPoly <- train(  
  chance_of_admit ~ .,  # varaibles in model: dependent variale chance of admit, the rest of variable in dataset are predictors
  adm_train1,           # dataset
  method = 'svmPoly',   # method used, Support Vector Machine with Polynomial Kernerl
 tuneGrid = Grid_svmPoly1, # turning grid
  trControl = trainControl(
    method = "repeatedcv",   # method of resampling
    number = 10,             # number of folds
    repeats = 10,            # number of time repeated
    verboseIter = TRUE      # show process
  )
) 

#save results for Support Vector Machines with Polynomial Kernel
results_svmPoly <- train_svmPoly$results  # save results of the Support Vector Machines with Polynomial Kernel model
saveRDS(results_svmPoly, file = "results_svmPoly.Rds")  # save results of Support Vector Machines with Polynomial Kernelmodel


train_svmPoly$bestTune  # best tuning parameter
rmse_svmPoly <-min(train_svmPoly$results$RMSE) # smallest rmse
rmsesd_svmPoly <- train_svmPoly$results$RMSESD[which.min(train_svmPoly$results$RMSE)]  # RMSE standard deviation associated with minimum RMSE for random forest



train_svmPoly$results %>%  # results of the model
  pivot_longer(cols = c(degree, scale, C), names_to = "parameter", values_to = "value") %>%   # group tuning parameters 
  ggplot(aes(x = value, y = RMSE, colour=parameter), highlight=TRUE) +    # produce aesthetic mapping of RMSE and the number of the variables to include
  #geom_line() + # add line graph
  geom_point(alpha=0.2) + # add point
  #geom_errorbar(aes(x = value,   # add error to value
  #                  ymin = RMSE - RMSESD,   # lower bound of RMSE with one standard deviation
  #                  ymax = RMSE + RMSESD))+ # upper bound of RMSE with one standard deviation
  
  geom_text(x=1.5, y=0.062, label=paste("Best degree = ", train_svmPoly$bestTune$degree )) + # label Best degree
  geom_text(x=0.9, y=0.061, label=paste("Best scale = ", train_svmPoly$bestTune$scale )) +   # label best scale
  geom_text(x=0.4, y=0.06, label=paste("Best C = ", train_svmPoly$bestTune$C )) +        # label Best C
  facet_wrap(vars(parameter), scales = "free_x")+ 
  labs(title = "Figure 12: Boosted Tree")+  # add title to graph
  theme_bw() + theme(legend.position = "none")


# Table of Results

bind_rows(data.frame(model = "Linear Regression",      # describe model type
                     RMSE = rmse_lm,                   # RMSE from the best fit model  (model with lowest RMSE)
                     RMSE_lower = rmse_lm-rmsesd_lm,   # RMSE less one standard deviation of RMSE - lower band of error band
                     RMSE_upper = rmse_lm+rmsesd_lm),  # RMSE plus one standard derivation of RMSE - upper band of error band  
          
          data.frame(model = "Linear Regression with Stepwise Selection",  # describe model type
                     RMSE = rmse_lmseq,                                    # RMSE from the best fit model (model tuning parameters with lowest RMSE) for linear regression with stepwise selection
                     RMSE_lower = rmse_lmseq-rmsesd_lmseq,                 # RMSE less one standard deviation of RMSE - lower band of error band for linear regression with stepwise selection
                     RMSE_upper = rmse_lmseq+rmsesd_lmseq),                # RMSE plus one standard deviation of RMSE - upper band of error band for linear regression with stepwise selection
          
          data.frame(model = "Random Forest",                              # describe model type
                     RMSE = rmse_rf,                                       # RMSE from the best fit model (model tuning parameters with the lowest RMSE) for random forest model
                     RMSE_lower = rmse_rf-rmsesd_rf,                       # RMSE less one standard deviation of RMSE - lower band for error band for random forest model
                     RMSE_upper = rmse_rf+rmsesd_rf),                      # RMSE plus one standard deviation of RMSE - upper band for error band for random forest
          
        
          data.frame(model = "Support Vector Machines with Polynomial Kernel",  # describe model type
                     RMSE = rmse_svmPoly,                                       # RMSE from the best fit model (model tuning parameters with the lowest RMSE) for SVM
                     RMSE_lower = rmse_svmPoly-rmsesd_svmPoly,                  # RMSE less one standard deviation of RMSE - lower band for error band for SVM
                     RMSE_upper = rmse_svmPoly+rmsesd_svmPoly),                 # RMSE plus one standard deviation of RMSE - upper band for error band for SVM
          
          
          )


### Plot Diagnostics of Support Vector Machines with Polynomial Kernel

fig10a <-  train_svmPoly$results %>%  # results of the model
  pivot_longer(cols = c(degree, scale, C), names_to = "parameter", values_to = "value") %>%   # group tuning parameters 
  ggplot(aes(x = value, y = RMSE, colour=parameter), highlight=TRUE) +    # produce   # produce aesthetic mapping of RMSE and the number of the variables to include
  #geom_line() + # add line graph
  geom_point(alpha=0.4) + # add point and opacity
  facet_wrap(vars(parameter), scales = "free_x")+ 
  labs(title = "Figure 10a: RMSE")+  # add title to graph
  theme_bw() + theme(legend.position = "none")


# Variable Importance for Support Vector Machines with Polynomial Kernel


# Variable importance plot
set.seed(1234)  # for reproducibility
fig10b <- vip(train_svmPoly, method = "firm", # method used variance based method
    nsim = 1000,   # number of simulation
    ice = TRUE, # individual conditional expectation
    train = adm_train1, # training data
    target = "chance_of_admit", # target is the depedent variable
    metric = "rmse",  # metric used to choose
    pred_wrapper = pred_vip,   # pred function created
    scale=TRUE)+  # scale the variable importance
  labs(title = "Figure 10b: Importance")+ # add title
  theme_bw() 




# variable explain

svmPoly_exp <- DALEX::explain(train_svmPoly, label = "svmPoly", data = adm_train1, y = adm_train1$chance_of_admit)

# model residual (mr)
svmPoly_mr <- model_residual(svmPoly_exp)

# Plot predicted value and target variable and residual
fig10c <- plot(svmPoly_mr, type = "prediction", abline = TRUE)+ # plot of prediction and target variable 
  labs(title = "Figure 10c: Model Fit")+ # add title
  theme_bw()+  #black and white theme
  theme(legend.position = "none")  # remove legend

fig10d <- plot(svmPoly_mr,type = "residual")+ # plot of residual
  labs(title = "Figure 10d:Model Residual")+ # add title
  theme_bw()+  #black and white theme 
  theme(legend.position = "none")  # remove legend


# lower values of chance_of_admit tend to be overestimated in the model

# combining plots of Support Vector Machines with Polynomial Kernel
fig10 <- grid.arrange(fig10a, fig10b, fig10c, fig10d, nrow=2, top="Figure 10: Support Vector Machines with Polynomial Kernel Results and Diagnostics")
# save ggplot of fig10
ggsave(fig10, filename = "fig10.png", width = 7)



#### Stochastic Gradient boosting gbm
### Grid_gbm represents the final grid used after trying several other options


Grid_gbm <- expand.grid( n.trees = c(5000,10000),          # number of iteration
                        interaction.depth = c(1,2,3),   # maximum depth of tree, value higher than one suggest interaction
                         shrinkage =c(0.0001, 0.0005),        # how quickly the algorithm adapts
                          n.minobsinnode = c(3,10,20)            #minimum of observations in the terminal nodes to commence splitting
                                                )

set.seed(1234)
train_gbm<- train(
  chance_of_admit ~ ., # dependent variale chance_of_admit remaining variables are potential predictors
  adm_train1,             #dataset
  method = 'gbm',    # method use to train data - gradient boosting machine model
  tuneGrid = Grid_gbm , # tuning grid
  trControl = trainControl(
    method = "repeatedcv",  #resampling method repeated cross validation
    number = 10,            # number of folds
    repeats = 10,           # number of time repeated
    verboseIter = TRUE      # observing the process
  )
) 

#save results for Stochastic Gradient boosting gbm
results_gbm <- train_gbm$results  # save results of the Stochastic Gradient boosting model
saveRDS(results_gbm, file = "results_gbm.Rds")  # save results of Stochastic Gradient boosting model

train_gbm$results  %>%  # results of the model
  pivot_longer(cols = c(shrinkage, interaction.depth, n.minobsinnode, n.trees), names_to = "parameter", values_to = "value") %>%   # group tuning parameters 
  ggplot(aes(x = value, y = RMSE, colour=parameter), highlight=TRUE) +    # produce aesthetic mapping of RMSE and the number of the variables to include
  #geom_line() + # add line graph
  geom_point(alpha=0.4) + # add point
  facet_wrap(vars(parameter), scales = "free_x")+ 
  labs(title = "Figure 12: Gradient Boosted Model")+  # add title to graph
  theme_bw() + theme(legend.position = "none")

train_gbm$bestTune  #best tune parameter
rmse_gbm <-min(train_gbm$results$RMSE)  # minimum standard deviation
rmsesd_gbm <- train_gbm$results$RMSESD[which.min(train_gbm$results$RMSE)]  # RMSE standard deviation associated with minimum RMSE for random forest




### Plot Diagnostics of Stochastic Gradient boosting gbm

fig11a <-  train_gbm$result %>%  # results of the model
  pivot_longer(cols = c(shrinkage, interaction.depth, n.minobsinnode, n.trees), names_to = "parameter", values_to = "value") %>%   # group tuning parameters 
  ggplot(aes(x = value, y = RMSE, colour=parameter), highlight=TRUE) +    # produce aesthetic mapping of RMSE and the number of the variables to include
  #geom_line() + # add line graph
  geom_point(alpha=0.4) + # add point and opacity
  facet_wrap(vars(parameter), scales = "free_x")+ 
  labs(title = "Figure 11a: RMSE")+  # add title to graph
  theme_bw() + theme(legend.position = "none")


# Variable Importance for Stochastic Gradient boosting

  set.seed(1234)  # for reproducibility
  fig11b <- vip(train_gbm, method = "firm", # method used variance based method
    nsim = 1000,   # number of simulation
    ice = TRUE, # individual conditional expectation
    train = adm_train1, # training data
    target = "chance_of_admit", # target is the depedent variable
    metric = "rmse",  # metric used to choose
    pred_wrapper = pred_vip,   # pred function created
    scale=TRUE)+  # scale the variable importance
  labs(title = "Figure 11b: Importance")+ # add title
  theme_bw() 


# variable explain

gbm_exp <- DALEX::explain(train_gbm, label = "gbm", data = adm_train1, y = adm_train1$chance_of_admit)

# model residual (mr)
gbm_mr <- model_residual(gbm_exp)

# Plot predicted value and target variable and residual
fig11c <- plot(gbm_mr, type = "prediction", abline = TRUE)+ # plot of prediction and target variable 
  labs(title = "Figure 11c: Model Fit")+ # add title
  theme_bw()+  #black and white theme
  theme(legend.position = "none")  # remove legend

fig11d <- plot(gbm_mr,type = "residual")+ # plot of residual
  labs(title = "Figure 11d:Model Residual")+ # add title
  theme_bw()+  #black and white theme 
  theme(legend.position = "none")  # remove legend


 
# lower values of chance_of_admit tend to be overestimated in the model

# combining plots of Support Vector Machines with Polynomial Kernel
fig11 <- grid.arrange(fig11a, fig11b, fig11c, fig11d, nrow=2, top="Figure 11: Stochastic Gradient Boosting Results and Diagnostics")
# save ggplot of fig11
ggsave(fig11, filename = "fig11.png", width = 7)


### Assess all models



# create table of all results
results <- bind_rows(data.frame(model = "Linear Regression",      # describe model type
                     RMSE = rmse_lm,                   # RMSE from the best fit model  (model with lowest RMSE)
                     RMSE_lower = rmse_lm-rmsesd_lm,   # RMSE less one standard deviation of RMSE - lower band of error band
                     RMSE_upper = rmse_lm+rmsesd_lm),  # RMSE plus one standard derivation of RMSE - upper band of error band  
          
          data.frame(model = "Linear Regression with Stepwise Selection",  # describe model type
                     RMSE = rmse_lmseq,                                    # RMSE from the best fit model (model tuning parameters with lowest RMSE) for linear regression with stepwise selection
                     RMSE_lower = rmse_lmseq-rmsesd_lmseq,                 # RMSE less one standard deviation of RMSE - lower band of error band for linear regression with stepwise selection
                     RMSE_upper = rmse_lmseq+rmsesd_lmseq),                # RMSE plus one standard deviation of RMSE - upper band of error band for linear regression with stepwise selection
          
          data.frame(model = "Random Forest",                              # describe model type
                     RMSE = rmse_rf,                                       # RMSE from the best fit model (model tuning parameters with the lowest RMSE) for random forest model
                     RMSE_lower = rmse_rf-rmsesd_rf,                       # RMSE less one standard deviation of RMSE - lower band for error band for random forest model
                     RMSE_upper = rmse_rf+rmsesd_rf),                      # RMSE plus one standard deviation of RMSE - upper band for error band for random forest
          
          data.frame(model = "Support Vector Machines with Polynomial Kernel",  # describe model type
                     RMSE = rmse_svmPoly,                                       # RMSE from the best fit model (model tuning parameters with the lowest RMSE) for SVM
                     RMSE_lower = rmse_svmPoly-rmsesd_svmPoly,                  # RMSE less one standard deviation of RMSE - lower band for error band for SVM
                     RMSE_upper = rmse_svmPoly+rmsesd_svmPoly),                 # RMSE plus one standard deviation of RMSE - upper band for error band for SVM
          
          
          data.frame(model = "Stochastic Gradient boosting",  # describe model type
                     RMSE = rmse_gbm,                                       # RMSE from the best fit model (model tuning parameters with the lowest RMSE) for GMB
                     RMSE_lower = rmse_gbm-rmsesd_gbm,                  # RMSE less one standard deviation of RMSE - lower band for error band for GMB
                     RMSE_upper = rmse_gbm+rmsesd_gbm),                 # RMSE plus one standard deviation of RMSE - upper band for error band for GMB
          
                    ) 

# save results data file to be used in Rmarkdown
saveRDS(results, file = "results.Rds")

#create graph
  results %>% 
  ggplot(aes(x= fct_reorder(model, desc(RMSE)),  #create graph of results and reorder model based on RSME, from lowest to highest
             y=RMSE, colour=model))+
  geom_point()+
  geom_errorbar( aes(x=model,
                     ymin=RMSE_lower,   # add error bar, ymin represents the RMSE less one standard deviation of RMSE
                     ymax=RMSE_upper)   # add error bar max, ymax represents the RMSE plus one standard deivation of RMSE
  )+
  labs(title = "Figure 12: Results of 10 Fold Cross Validation Repeated 10 Times",
       x= "Models"
  )+  #title of graph
  coord_flip()+  #flip axis
  theme_bw() + 
  theme(axis.text = element_text(size = 10, 
                                 face = "bold"), legend.position = "none")+  #set axis face to 10 and bold text and remove legend.
  theme(plot.title = element_text(size = 11, hjust=-3)) #set title size to 11 and adjust to the left to keep the title on page of rmarkdown file.
  
 
  
  
  fig13 <- plot(lm_mr, lmseq_mr, rf_mr, svmPoly_mr, gbm_mr, type="prediction", abline=TRUE)+
    labs(title = "Figure 13: Predicted vs Target All Models")
 # save ggplot of fig10
 ggsave(fig13, filename = "fig13.png", width = 7) 
 
 #plot(lm_mr, lmseq_mr, rf_mr, svmPoly_mr, gbm_mr,  type = "residual_density") plot of residual density of all model - 
   
 #### Apply selected model to test dataset
 
 
 pred_test1 <- predict(train_lmseq, adm_test1)  # predict the value of chance of admit in the test set (hold out set)
 rmse_test1 <- RMSE(pred_test1, adm_test1$chance_of_admit)  # caculate RMSE for the test set
 rmse_test1

 # save results data file to be used in Rmarkdown
 saveRDS(rmse_test1, file = "rmse_test1.Rds")
 
 results$model[which.min(results$RMSE)]
 results$model[which.max(results$RMSE)]
 
  ### Aside examine all the model performance on training and test set.
 

### apply all model to test set

adm_test1 %>% 
  mutate(pred_lm = predict(train_lm, .),  # predict chance of admit using linear regression model
         pred_lmseq = predict(train_lmseq, .),  # predict chance of admit using linear regression with step wise selection 
         pred_rf = predict(train_rf, .),  # predict chance of admit using random forest model
         pred_svmPoly = predict(train_svmPoly, .),  # predict chance of admit using Support Vector Machines with Polynomial Kernel model
         pred_gbm = predict(train_gbm, .)  # predict chance of admit using Gradient Boost Model
  )%>% 
  rowwise() %>%  # transform series to perform row wise operation
  mutate(pred_mean = mean(pred_lm:pred_gbm),  #mean of all predicted valued for each row
          ) %>% 
  ungroup() %>% 
  summarise(RMSE_mean = RMSE(pred_mean, chance_of_admit),   # calculate RMSE for mean prediction
            RMSE_lm = RMSE(pred_lm, chance_of_admit),   # calculate RMSE for mean linear regression model
            RMSE_lmseq = RMSE(pred_lmseq, chance_of_admit), # calculate RMSE for linear regression with stepwise selection model
            RMSE_rf = RMSE(pred_rf, chance_of_admit),   # calculate RMSE for random forest model
            RMSE_svmPoly = RMSE(pred_svmPoly, chance_of_admit),   # calculate RMSE for Support Vector Machines with Polynomial Kernel Model
            RMSE_gbm = RMSE(pred_gbm, chance_of_admit) # calculate RMSE for Gradient Boost Model
          )

# all model showed similar results.



