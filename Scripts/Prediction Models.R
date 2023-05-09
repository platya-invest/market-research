
##### Objetive: Calculate and predict savings/invesment at a household level in Colombia

### Part 2: Predictive models

### Necessary Data

## ENPH 2016 - 2017:
# Caracteristicas generales personas
#Viviendas y hogares

###### Cleaning environment

rm(list=ls())

##### Upload packages

require(pacman)

p_load(tidyverse, ggplot2,
       ggExtra, tidymodels, themis,
       gridExtra, ranger, tidyr, vip,
       xgboost, finetune, lime, DALEX,
       yardstick) 

##### Functions

proper = function(x) paste0(toupper(substr(x, 1, 1)), 
                            tolower(substring(x, 2)))

##### Upload data #####

### Define data location

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Household Savings Platya Consultancy/Data")


Model_Data_ENPH <- read.csv2("Model Data.csv")

Model_Data_ECV <- read.csv2("ECV Model Data.csv")

##################################### Woodwork ######################################################

##### General Descriptive Statisttics #####

Model_Data_ENPH %>% 
  summarise(
    Total.Income = sum(It*Fex.HH, na.rm = TRUE),
    Total.Monetary.Available.Income = sum(Icmdug*Fex.HH, na.rm = TRUE),
    Total.Spending = sum(Gtug*Fex.HH, na.rm = TRUE),
    Total.Monetary.Spending = sum(Gcmug*Fex.HH, na.rm = TRUE)
  )



##### Define vector of variables to turn into strings

To_Factor <- c("Sex", "Ethnicity", "City", "Class",
               "Strata", "Healthcare.System",
               "Scholarship", "Study", "Max.Educ",
               "Labor.Contract", "Type.Contract", "Profession", "Position",
               "Service.Premium", "Type.Household", "Rent.Income", "Investments",
               "Owned.Rented", "Employed", "Under4", "Services",
               "Subsidies", "Savings.HH")

### Convertir el vector anterior a string

Model_Data_ENPH[To_Factor] <- lapply(Model_Data_ENPH[To_Factor], as.character)

##### Convertir variables de texto a string

Model_Data_ENPH_F <- Model_Data_ENPH %>% 
  mutate_if(
    is.character, factor)

str(Model_Data_ENPH_F)

##### Treatment of extreme values and Age Sqrt variable creation

Model_Data_ENPH_F <- Model_Data_ENPH_F %>% 
  mutate(
    No.Rooms = case_when(
      No.Rooms > 18 ~ median(No.Rooms),
      TRUE ~ No.Rooms
    ),
    Age2 = Age^2,
    People.Rooms = round(Size.HH/No.Rooms*100,1)
    ) %>% 
  filter(!is.na(Savings.HH))

##################################### Modelling with Tidymodels ######################################################
    
########## 1. Define training and test data sets, as well as the resampling method #####

set.seed(24)

ENPH_Split <- Model_Data_ENPH_F %>% 
  initial_split(strata = Savings.HH) # Se hace por estratificación para mantener la proporción de la variable Pobre entre submuestras.

ENPH_Training <- training(ENPH_Split)

ENPH_Test <- testing(ENPH_Split) # 74% de la muestra de entrenamiento inicial 

set.seed(2403)

ENPH_Folds <- vfold_cv(ENPH_Training, 
                         strata = Savings.HH) # 10 submuestras por default

########## 2. Definir recipes and specification #####

##### Asses data balance

ENPH_Training %>% 
  group_by(Savings.HH) %>% 
  summarise(Per.Households = round(sum(Fex.HH)/10722083*100))

summary(Model_Data_ENPH_F)

##### First Run #####

### ELCA's 1st Specification

Recipe_1 <- 
  recipe(Savings.HH ~ Sex + Age + Age2 +
           Max.Educ + Employed  + Services +
           People.Rooms + Subsidies + Icmdug +
           Class + City, 
         ENPH_Training,
         weights = Fex.HH) %>%
  step_impute_median(Icmdug) %>% 
  step_dummy(Sex, Class, City, Max.Educ, Employed, Services,
             Subsidies)

### ELCA's 2nd Specification

Recipe_2 <- 
  recipe(Savings.HH ~ Sex + Age + Age2 +
           Max.Educ + Employed  + Services +
           People.Rooms + Subsidies + Owned.Rented,
         ENPH_Training,
         weights = Fex.HH) %>%
  step_dummy(Sex, Max.Educ, Employed, Services,
             Subsidies, Owned.Rented)

### ELCA's 1st Specification with upsampling

Recipe_3 <- 
  recipe(Savings.HH ~ Sex + Age + Age2 +
           Max.Educ + Employed  + Services +
           People.Rooms + Subsidies + Icmdug +
           Class + City, 
         ENPH_Training,
         weights = Fex.HH) %>%
  step_impute_median(Icmdug) %>% 
  step_dummy(Sex, Class, City, Max.Educ, Employed, Services,
             Subsidies) %>% 
  step_smote(Savings.HH)


##### Revisar si la base de entrenamiento se está preparando bien

# Zoom <- juice(prep(Recipe_2))
# 
# summary(Zoom)

##### Second Run #####

### ELCA's 1st Specification modified

Recipe_P <- 
  recipe(Savings.HH ~ Sex + Age + Age2 +
           Max.Educ + Employed  + Services +
           People.Rooms + Subsidies + Icmdug,
         ENPH_Training,
         weights = Fex.HH) %>%
  step_impute_median(Icmdug) %>% 
  step_dummy(Sex, Max.Educ, Employed, Services,
             Subsidies)

Recipe_1 <- 
  recipe(Savings.HH ~ Sex + Age + Age2 + # Individual variables
           Max.Educ + Employed  + Healthcare.System + Labor.Contract +
           Type.Contract + Profession + Position + Size.Comp + Rent.Income +
           Investments + Income.Land.Properties +
           
           # Household variables
           Services + Strata + Tasa.Depen + Num.Employed + Num.Under4 +
           People.Rooms + Subsidies + Icmdug,
         ENPH_Training,
         weights = Fex.HH) %>%
  step_impute_mode(Sex, Max.Educ, Employed, Services, Strata,
                    Subsidies, Healthcare.System, Labor.Contract, Type.Contract,
                    Profession, Position, Rent.Income, Investments, Income.Land.Properties) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_dummy(Sex, Max.Educ, Employed, Services, Strata,
             Subsidies, Healthcare.System, Labor.Contract, Type.Contract,
             Profession, Position, Rent.Income, Investments, Income.Land.Properties
             )

##### Revisar si la base de entrenamiento se está preparando bien
# 
#Zoom <- juice(prep(Recipe_1))
# 
# summary(Zoom)

##### Third Run #####

### PCA analysis derived specification

Recipe_1 <- 
  recipe(Savings.HH ~ Sex + Age + Age2 + # Individual variables
           Max.Educ + Employed  + Healthcare.System + Labor.Contract +
           Profession + Position + Size.Comp +
           
           # Household variables
           Size.HH + Size.HH2 + Strata + Tasa.Depen + 
           Num.Employed + Num.Under4 +
           Class + City + Icmdug + Housing.Spending2 + Food.Spending +
           Healthcare.Cut + Pension.Cut,
         ENPH_Training,
         weights = Fex.HH) %>%
  step_impute_mode(Sex, Max.Educ, Employed, Strata,
                   Healthcare.System, Labor.Contract,
                   Profession, Position) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_dummy(Sex, Max.Educ, Employed, Strata,
             Healthcare.System, Labor.Contract,
             Profession, Position, Class, City) %>% 
  step_smote(Savings.HH)

Recipe_2 <- 
  recipe(Savings.HH ~ Sex + Age + Age2 + # Individual variables
           Max.Educ + Employed  + Healthcare.System + Labor.Contract +
           Profession + Position + Size.Comp +
           
           # Household variables
           Size.HH + Size.HH2 + Strata + Tasa.Depen + 
           Num.Employed + Num.Under4 +
           Class + City + Icmdug + Housing.Spending2 + Food.Spending +
           Healthcare.Cut + Pension.Cut,
         ENPH_Training,
         weights = Fex.HH) %>%
  step_impute_mode(Sex, Max.Educ, Employed, Strata,
                   Healthcare.System, Labor.Contract,
                   Profession, Position) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_dummy(Sex, Max.Educ, Employed, Strata,
             Healthcare.System, Labor.Contract,
             Profession, Position, Class, City) %>% 
  step_smote(Savings.HH) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_predictors(), 
           keep_original_cols = TRUE,
           threshold = .75)


Recipe_3 <- 
  recipe(Savings.HH ~  # Individual variables
           Max.Educ + Healthcare.System + Labor.Contract +
           
           # Household variables
           Size.HH + Size.HH2 + Strata + Tasa.Depen + 
           Num.Under4 + City + Icmdug + Housing.Spending2 + Food.Spending +
           Healthcare.Cut + Pension.Cut,
         ENPH_Training,
         weights = Fex.HH) %>%
  step_impute_mode(Max.Educ, Strata,
                   Healthcare.System, Labor.Contract) %>% 
  step_dummy(Max.Educ, Strata,
             Healthcare.System, Labor.Contract, City) %>% 
  step_smote(Savings.HH)

Recipe_4 <- 
  recipe(Savings.HH ~  # Individual variables
           Max.Educ + Healthcare.System + Labor.Contract +
         
         # Household variables
         Size.HH + Size.HH2 + Strata + Tasa.Depen + 
           Num.Under4 + City + Icmdug + Housing.Spending2 + Food.Spending +
           Healthcare.Cut + Pension.Cut,
         ENPH_Training,
         weights = Fex.HH) %>%
  step_impute_mode(Max.Educ, Strata,
                   Healthcare.System, Labor.Contract) %>% 
  step_dummy(Max.Educ, Strata,
             Healthcare.System, Labor.Contract, City)


##### Revisar si la base de entrenamiento se está preparando bien
# 
# Zoom <- juice(prep(Recipe_4))
# 
# summary(Zoom)

##### Last Run #####

Recipe_Last <- 
  recipe(Savings.HH ~  # Individual variables
           Max.Educ + Healthcare.System + Labor.Contract +
           
           # Household variables
           Size.HH + Size.HH2 + Strata + Tasa.Depen + 
           Num.Under4 + Icmdug + Housing.Spending2 + Food.Spending +
           Healthcare.Cut + Pension.Cut,
         ENPH_Training,
         weights = Fex.HH) %>%
  step_impute_mode(Max.Educ, Strata,
                   Healthcare.System, Labor.Contract) %>% 
  step_dummy(Max.Educ, Strata,
             Healthcare.System, Labor.Contract) %>% 
  step_smote(Savings.HH)

##### Revisar si la base de entrenamiento se está preparando bien
# 
# Zoom <- juice(prep(Recipe_4))
# 
# summary(Zoom)


########## 3. Definir modelos y el workflow #####

Elastic_Logit_Spec <- 
  logistic_reg(penalty = 0.0622,
               mixture = 0.0293) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

RF_Spec <-
  rand_forest(trees = 100) %>%
  set_engine("ranger",
             importance = "permutation") %>% 
  set_mode("classification") 

Xgb_Spec <-
  boost_tree(
    trees = 100) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

### Workflow


## Run 1 #####

ENPH_Wflow <- workflow_set(
  list("ELCA 1" = Recipe_1, 
       "ELCA 2" = Recipe_2, 
       "ELCA 1 SMOTE" = Recipe_3),
  list("Elastic Logit" = Elastic_Logit_Spec, 
       "Random Forest" = RF_Spec))

## Run 2 #####

ENPH_Wflow <- workflow_set(
  list("ELCA 1" = Recipe_P, 
       "ELCA 1 Modified" = Recipe_1),
  list("Elastic Logit" = Elastic_Logit_Spec, 
       "Random Forest" = RF_Spec))

## Run 3 #####

ENPH_Wflow <- workflow_set(
  list("Refence" = Recipe_1, 
       "PCA Spec" = Recipe_2,
       "Simple Spec" = Recipe_3,
       "Simplest Spec" = Recipe_4),
  list("Elastic Logit" = Elastic_Logit_Spec, 
       "Random Forest" = RF_Spec,
       "Xgboost" = Xgb_Spec))

## Last Run #####

ENPH_Wflow <- workflow_set(
  list("Final" = Recipe_Last),
  list("Xgboost" = Xgb_Spec))


######### 4. Run the models ######### 

ENPH_Metrics <- metric_set(accuracy, sensitivity, 
                             specificity, roc_auc)

doParallel::registerDoParallel()
set.seed(240394)

ENPH_RS_Basic <-
  workflow_map(
    ENPH_Wflow,
    "fit_resamples",
    resamples = ENPH_Folds,
    metrics = ENPH_Metrics
  )

########## 5. Select the best model based on the ROC metric #########

collect_metrics(ENPH_RS_Basic)

Zoom <- rank_results(ENPH_RS_Basic, 
                     rank_metric = "roc_auc")

Zoom$wflow_id[1]

write_excel_csv(Zoom, "Metrics Models K Fold Final.csv")

########## 6. Estimate predictors importance ########## 

Importance_Wf <- workflow(Recipe_Last,
                          Xgb_Spec)

Importance_Fit <- fit(Importance_Wf,
                      ENPH_Training)

Importance_Plot <- Importance_Fit %>%
  extract_fit_parsnip() %>% 
  vip(num_features = 15L,
      geom = "point",
  )

########## 7. Last fit in training and test data sets #####

Best_Fit <-
  ENPH_Wflow %>% 
  extract_workflow(Zoom$wflow_id[1]) %>%
  last_fit(ENPH_Split)

Best_Fit

########## 8. Out of sample metrics and confusion matrix #####

##### Confusion Matrix

collect_predictions(Best_Fit) %>%
  conf_mat(Savings.HH,
           .pred_class) %>% 
  autoplot()

###### Performance matrix

collect_metrics(Best_Fit)

########## 9. Tune the best model (XGBoost) #####

##### XGBoost #####

### Define hiperparameters to optimize#####

Xgb_Spec_Tuned <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    mtry = tune(),
    sample_size = tune(),
    learn_rate = tune()
  ) %>%
  set_engine("xgboost") %>%
  set_mode("classification")


### Define the best recipe for the tuning process #####

Xgb_Wflow <- workflow(Recipe_Last, 
                      Xgb_Spec_Tuned)

### Define the optimization grid #####

set.seed(2023)

Xgb_Grid <-
  grid_max_entropy(
    trees(c(100L, 500L)),
    tree_depth(c(5L, 11L)),
    min_n(c(10L, 700L)),
    mtry(c(5L, 10L)),
    sample_prop(c(0.3, 1.0)),
    learn_rate(c(-2, -1)), # Exponentes que corresponden a una tasa de aprendizaje de 0,01 y 0,1 respectivamente.
    size = 20
  )

Xgb_Grid

### Run the tuning of the model #####

doParallel::registerDoParallel()

set.seed(240394)

Xgb_Rs <-
  tune_race_anova(
    Xgb_Wflow,
    ENPH_Folds,
    grid = Xgb_Grid,
    metrics = metric_set(accuracy),
    control = control_race(verbose_elim = TRUE)
  )

Xgb_Rs

### Graph tuning process and selection of the best hiperparameter combination #####

plot_race(Xgb_Rs)

Tune <- Xgb_Rs$.metrics

show_best(Xgb_Rs)

### Last fit of the tuned model #####

Xgb_Last <-
  Xgb_Wflow %>%
  finalize_workflow(select_best(Xgb_Rs, 
                                "accuracy")) %>%
  last_fit(ENPH_Split)

## Confusion matrix

collect_predictions(Xgb_Last) %>%
  conf_mat(Savings.HH,
           .pred_class)

## Accuracy

collect_predictions(Xgb_Last) %>%
  accuracy(Savings.HH, .pred_class)


########## 10. ECV Household Savers Prediction #####

##### Data prep #####

Model_Data_ECV <- Model_Data_ECV %>% 
  ungroup() %>% 
  mutate(
    Healthcare.System = as.factor(Healthcare.System),
    Labor.Contract = as.factor(Labor.Contract),
    Max.Educ = as.numeric(Max.Educ),
    Max.Educ = ifelse(Max.Educ > 5, 
                      6, 
                      Max.Educ),
    Max.Educ = as.factor(Max.Educ),
    Housing.Spending2 = as.numeric(round(Housing.Spending2)),
    Pension.Cut = as.numeric(round(Pension.Cut))
  )

##### Model fit #####


ECV_Fit <-
  ENPH_Wflow %>% 
  extract_workflow(Zoom$wflow_id[1]) %>%
  fit(ENPH_Training)

##### Model Prediction #####

ECV_Pred <- predict(
  ECV_Fit, 
  Model_Data_ECV
  )

  

ECV_Final <- bind_cols(Model_Data_ECV,
                       ECV_Pred)


ECV_Final <- ECV_Final %>% 
  rename("Savings.HH" = .pred_class)

########## 11. Characterization of the savers population ##########

##### Percentage of households #####

ECV_Final %>% 
  group_by(Savings.HH) %>% 
  summarise(Per.Households = round(sum(FEX_C)/17068100*100))

### 11.776.989 of households aprox save in Colombia (69% of households).
# 45% of the savers are in the 3 upper deciles

##### By Age of the HH head #####

ECV_Final %>% 
  group_by(Savings.HH) %>% 
  summarise(Mean.Savings = weighted.mean(Age, 
                                         FEX_C,
                                         na.rm = TRUE))

##### By sex #####

prop.table(table(ECV_Final$Savings.HH, 
                 ECV_Final$Sex), 1)

##### Number of  Max educ #####

prop.table(table(ECV_Final$Savings.HH, 
                 ECV_Final$Max.Educ), 2)

##### By Household Size #####

prop.table(table(ECV_Final$Savings.HH, 
                 ECV_Final$Size.HH), 2)

##### By Num Under 4 #####

prop.table(table(ECV_Final$Savings.HH, 
                 ECV_Final$Num.Under4), 2)

##### By Num Dependent Rate #####


ECV_Final %>% 
  group_by(Savings.HH) %>% 
  summarise(Mean.Depen = weighted.mean(Tasa.Depen, 
                                         FEX_C,
                                         na.rm = TRUE))
