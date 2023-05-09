
##### Objetive: Calculate and predict savings/invesment at a household level in Colombia


### Necessary Data

## ECV 2021


###### Cleaning environment

rm(list=ls())

##### Upload packages

require(pacman)

p_load(ggplot2, 
       ggExtra, tidymodels, themis,
       gridExtra, ranger, tidyr, vip,
       xgboost, finetune, yardstick, 
       survey, srvyr, tidyverse) 

##### Functions

proper = function(x) paste0(toupper(substr(x, 1, 1)), 
                            tolower(substring(x, 2)))


##### Upload data #####

### Define data location

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/Work/Consultancy/Platya/Data/ECV 2021")

House_Data <- read.csv2("Datos de la vivienda.csv", 
                        sep = ";", 
                        dec = ",", 
                        fileEncoding = "UTF-8")


Household_Services <- read.csv2("Servicios del Hogar.csv", 
                                sep = ",", 
                                dec = ".", 
                                fileEncoding = "UTF-8")


Household_Life_Conditions <- read.csv2("Condiciones de vida del hogar y tenencia de bienes.csv", 
                                       sep = ",", 
                                       dec = ".", 
                                       fileEncoding = "UTF-8")


Household_Financing <- read.csv2("Tenencia y financiacion de la vivienda que ocupa el hogar.csv", 
                               sep = ";", 
                               dec = ".", 
                               fileEncoding = "UTF-8")


Household_Spending <- read.csv2("Gastos de los hogares.csv", 
                             sep = ",", 
                             dec = ".", 
                             fileEncoding = "UTF-8")

Characteristics_Household <- read.csv2("Caracteristicas y composicion del hogar.csv", 
                                       sep = ";", 
                                       dec = ".", 
                                       fileEncoding = "UTF-8")

Healthcare <- read.csv2("Salud.csv", 
                       sep = ",", 
                       dec = ".", 
                       fileEncoding = "UTF-8")

Childhood_Module <- read.csv2("Atencion integral de los niños y niñas menores de 5 años.csv", 
                              sep = ";", 
                              dec = ".", 
                              fileEncoding = "UTF-8")

Education <- read.csv2("Educación.csv", 
                       sep = ",", 
                       dec = ".", 
                       fileEncoding = "UTF-8")

Work_Force <- read.csv2("Fuerza de Trabajo.csv", 
                        sep = ",", 
                        dec = ".", 
                        fileEncoding = "UTF-8")

Child_Labor <- read.csv2("Trabajo infantil.csv", 
                 sep = ";", 
                 dec = ".", 
                 fileEncoding = "UTF-8")

ICT <- read.csv2("Tecnologias de información y comunicación.csv", 
                        sep = ",", 
                        dec = ".", 
                        fileEncoding = "UTF-8")




##### Quick review of the data

#Check total population

House_Data %>% 
  summarise(sum(FEX_C))

#Check total Households

Household_Spending %>% 
  summarise(sum(FEX_C))

#Check total houses

House_Data %>% 
  summarise(sum(FEX_C))

######################################## Woodwork ######################################################


########## Getting ready the spending data base ##########
##### Set Spending data to wide format #####

Household_Spending_Final <- Household_Spending %>% 
  arrange(P3204) %>% 
  select(-c(SECUENCIA_ENCUESTA, ORDEN,P3205S1:P3205S5, P3204S2)) %>% 
  pivot_wider(
    names_from = P3204,
    names_prefix = "Item",
    values_from = P3204S1)

##### Construct spending categories #####

### Initial Approach #####
# 
# Household_Spending_Final <- Household_Spending_Final %>% 
#   mutate(
#   Food = rowSums(across(contains("Item"), 
#                         .names = "spend{.col}")[, 1:25], 
#                  na.rm = TRUE),
#   Group.1 = rowSums(across(contains("Item"), 
#                         .names = "spend{.col}")[, c(26:35, 60:67, 77)], 
#                  na.rm = TRUE),
#   Group.2 = rowSums(across(contains("Item"), 
#                            .names = "spend{.col}")[, c(36:40, 58:64, 48, 49,104)], 
#                     na.rm = TRUE),
#   Group.3 = rowSums(across(contains("Item"), 
#                            .names = "spend{.col}")[, c(41:47, 55:57, 68:76, 82:84)], 
#                     na.rm = TRUE),
#   Group.4 = rowSums(across(contains("Item"), 
#                            .names = "spend{.col}")[, c(78:81, 85:98, 102, 103)], 
#                     na.rm = TRUE),
#   Services = rowSums(across(contains("Item"), 
#                             .names = "spend{.col}")[, c(50:54)], 
#                      na.rm = TRUE),
#   Durable.goods = rowSums(across(contains("Item"), 
#                                  .names = "spend{.col}")[, c(42, 47, 78:88, 92:95, 102, 103)], 
#                           na.rm = TRUE)
#   )

### Second Approach #####

Household_Spending_Final <- Household_Spending_Final %>% 
  mutate(
    Food = rowSums(across(contains("Item"), 
                          .names = "spend{.col}")[, 1:26], 
                   na.rm = TRUE),
    Group.1 = rowSums(across(contains("Item"), 
                             .names = "spend{.col}")[, c(27:35)], 
                      na.rm = TRUE),
    Monthly.Group.2 = rowSums(across(contains("Item"), 
                                     .names = "spend{.col}")[, c(36:67)], 
                              na.rm = TRUE),
    Group.3 = rowSums(across(contains("Item"), 
                             .names = "spend{.col}")[, c(68:77)], 
                      na.rm = TRUE),
    Group.4 = rowSums(across(contains("Item"), 
                             .names = "spend{.col}")[, c(78:104)], 
                      na.rm = TRUE), 
    Services = rowSums(across(contains("Item"), 
                              .names = "spend{.col}")[, c(50:54)], 
                      na.rm = TRUE),
    Housing.Services = rowSums(across(contains("Item"), 
                                      .names = "spend{.col}")[, 94:95], 
                               na.rm = TRUE)
    
  )

##### Calculate the monthly value

Household_Spending_Final <- Household_Spending_Final %>% 
  mutate(
    Monthly.Food = Food*4.24, # # Answered for the last 7 days
    Monthly.Group.1= Group.1*4,
    Monthly.Group.3 = Group.3/3, # Answered for the last 3 months
    Monthly.Group.4 = Group.4/12, # Answered for the last 12 months
    Monthly.Housing.Services = Housing.Services/12 # Answered for the last 12 months
    
  )


##### Select important variables for merge #####

Household_Spending_Merge <- Household_Spending_Final %>% 
  select(DIRECTORIO, SECUENCIA_P, Monthly.Food, Services,
         Monthly.Group.1, Monthly.Group.2, Monthly.Group.3, 
         Monthly.Group.4, Monthly.Housing.Services, FEX_C)

########## Merge data #####

### By hierarchy according to the ECV structure

##### Household level to household level  ##########

# Merging household services with household life conditions

Merge1 <- Household_Services %>%  
  merge(
    Household_Life_Conditions,
    by = c("DIRECTORIO", "SECUENCIA_ENCUESTA"),
    #all.x = TRUE
  )

Merge1 = Merge1[,!grepl(".x$",names(Merge1))]

colnames(Merge1) <- sub("\\.y", "", colnames(Merge1))

# Adding household ownership and financing information

Merge2 <- Merge1 %>% 
  merge(
    Household_Financing,
    by = c("DIRECTORIO", "SECUENCIA_ENCUESTA"),
    #all.x = TRUE
  )

Merge2 = Merge2[,!grepl(".x$",names(Merge2))]

colnames(Merge2) <- sub("\\.y", "", colnames(Merge2))

# Adding household spending information (Pending to check)

Household_Merge <- Merge2 %>%
  merge(
    Household_Spending_Merge,
    by.x = c("DIRECTORIO", "SECUENCIA_ENCUESTA"),
    by.y = c("DIRECTORIO", "SECUENCIA_P"),
    all.x = TRUE
  )

Household_Merge = Household_Merge[,!grepl(".y$",names(Household_Merge))]

colnames(Household_Merge) <- sub("\\.x", "", colnames(Household_Merge))

##### People level to people level  ##########

# Merging general characteristics with healthcare system information

Merge1 <- Characteristics_Household %>% 
  merge(
    Healthcare,
    by = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"),
    all.x = TRUE
  )

Merge1 = Merge1[,!grepl(".y$",names(Merge1))]

colnames(Merge1) <- sub("\\.x", "", colnames(Merge1))

# Adding childhood attention and care information (Check module)

Merge2 <- Merge1 %>%
  merge(
    Childhood_Module,
    by = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"),
    all.x = TRUE
  )

Merge2 = Merge2[,!grepl(".x$",names(Merge2))]

colnames(Merge2) <- sub("\\.y", "", colnames(Merge2))


# Adding education system information

Merge3 <- Merge2 %>% 
  merge(
    Education,
    by = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"),
    all.x = TRUE
  )

Merge3 = Merge3[,!grepl(".x$",names(Merge3))]

colnames(Merge3) <- sub("\\.y", "", colnames(Merge3))

# Adding work force information

Merge4 <- Merge3 %>% 
  merge(
    Work_Force,
    by = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"),
    all.x = TRUE
  )

Merge4 = Merge4[,!grepl(".x$",names(Merge4))]

colnames(Merge4) <- sub("\\.y", "", colnames(Merge4))

# Adding child labor information

Merge5 <- Merge4 %>% 
  merge(
    Child_Labor,
    by = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"),
    all.x = TRUE
  )

Merge5 = Merge5[,!grepl(".x$",names(Merge5))]

colnames(Merge5) <- sub("\\.y", "", colnames(Merge5))

# Adding ICTs information

People_Merge <- Merge5 %>% 
  merge(
    ICT,
    by = c("DIRECTORIO", "SECUENCIA_P", "SECUENCIA_ENCUESTA"),
    all.x = TRUE
  )

People_Merge = People_Merge[,!grepl(".x$",names(People_Merge))]

colnames(People_Merge) <- sub("\\.y", "", colnames(People_Merge))

People_Merge <- People_Merge %>% 
  rename(SECUENCIA_ENCUESTA_P = SECUENCIA_ENCUESTA)

##### Household level to people level  ##########

# Adding basic people information

Household_People_Merge <- Household_Merge %>% 
  merge(
    People_Merge,
    by.x = c("DIRECTORIO", "SECUENCIA_ENCUESTA"),
    by.y = c("DIRECTORIO", "SECUENCIA_P"),
    all.x = TRUE
  )

Household_People_Merge = Household_People_Merge[,!grepl(".x$",names(Household_People_Merge))]

colnames(Household_People_Merge) <- sub("\\.y", "", colnames(Household_People_Merge))


Household_People_Merge <- Household_People_Merge %>% 
  rename(SECUENCIA_ENCUESTA_H = SECUENCIA_ENCUESTA)

##### Dwelling level to People level ##########


ECV_2021 <- House_Data %>% 
  merge(Household_People_Merge,
    by = c("DIRECTORIO")
  )

# Delete variables with ".x" in the name and rename variables with ".y" in the name
# productc of the merge.

ECV_2021 = ECV_2021[,!grepl(".y$",names(ECV_2021))]

colnames(ECV_2021) <- sub("\\.x", "", colnames(ECV_2021))

##### Check Point #####

### Number of people in the data #####

ECV_2021 %>% 
  summarise(sum(FEX_C)) # Mine: 51,224
                        # DANE: 51,224

### Number of households in the data #####

Zoom <- ECV_2021 %>%
  group_by(DIRECTORIO, SECUENCIA_ENCUESTA_H) %>% 
  summarise(Mean.Fex = mean(FEX_C, 
                            na.rm = TRUE))

Zoom %>% 
  ungroup() %>% 
  summarise(sum(Mean.Fex)) # Mine: 17,068
                           # DANE: 17,068

##### Delete Merge process #####

rm(Merge1, Merge2, Merge3, 
   Merge4, Merge5)

########## Selecting relevant variables for the analysis ##########

ECV_2021_Final<- ECV_2021 %>% 
  mutate( # Calculate monthly service charge
    Sp.Electricty = round(P5018/P5018S1),
    Sp.Gas = round(P3163/P3163S1),
    Sp.Sewer = round(P5034/ P5034S1),
    Sp.Disposal = round(P5044/P5044S1),
    Sp.Water = round(P5067/P5067S1)
  ) %>% 
  select( ##### ID
    DIRECTORIO, SECUENCIA_ENCUESTA_H, SECUENCIA_P, ORDEN,
    ##### For Analysis
    "Sex" = P6020, "Age" = P6040, "Kin" = P6051, "City" = P756S2,
    CLASE, "Max.Educ.Father" = P6087, "Max.Educ" = P8587, "Healthcare.System" = P6100, 
    "Strata" = P8520S1A1, "Labor.Contract" = P6440, P6920,
    "Walls" = P4005, "Floors" = P4015, "Gas" = P5666,
    "Electricity" = P791, "Sewerage" = P5032, "Disposal" = P5038, 
    "Type.Sanitary" = P5022, "Phone.Service" = P5305,
    "No.Members.HH" = CANT_PERSONAS_HOGAR, 
    I_HOGAR, I_UGASTO, PERCAPITA, 
    
    ##### Spending
    # Services spending
    Sp.Electricty, Sp.Gas, Sp.Sewer, Sp.Disposal,
    Sp.Water, "Sp.Cooking.Fuel" = P8540, 
    "Sp.Cell" = P803S1, Services,
    # House ownership spending
    "Amortization.Fee" = P5100, "Rent.Fee" = P5140, "Admon" = P5650,
    # Healthcare spending
    "Cut.Healthcare" = P8551, "Cut.Other.Healthcare" = P3176, 
    "Cost.Hospitalization" = P3189S1A1,
    "Appointment.EPS" = P3178S1A1, "Appointment.Private" = P3178S2A1,
    "Appoinment.Prepaid" = P3178S3A1,
    # Childhood Attention 
    "Child.Tuition" = P6169S1, "Child.Uniforms" = P8564S1, 
    "Child.Equipment" = P8566S1, "Child.Equipment.Out" = P8568S1,
    "Child.Admon" = P6191S1, "Child.Transport" = P8572S1, 
    "Child.Food" = P8574S1, "Child.Others" = P8576S1,
    # Education
    "School.Tuition" = P3341S1, "School.Uniforms" = P3342S1,
    "School.Equipment" = P3343S1, "School.Admon" = P3344S1,
    "School.Transport" = P3345S1, "School.Food" = P3346S1, 
    "School.Equipment2" = P3347S1, "School.Others" = P3348S1,
    # Categories
    Monthly.Food, Monthly.Group.1, Monthly.Group.2, 
    Monthly.Group.3, Monthly.Group.4, Services, Monthly.Housing.Services,

    ##### Income
    "Salary" = P8624, "Scholarship" = P8610S1, "Salary.Food" = P6595S1, 
    "Salary.Housing" = P6605S1, "Salary.Other" = P6623S1, 
    "Subsidy.Food" = P8626S1, "Subsidy.Transport" = P8628S1, 
    "Subsidy.Family" = P8630S1,
    "Premiums" = P8631S1, "Premium.Services" = P1087S1A1, 
    "Premium.Vacations" = P1087S3A1, "Bonuses" = P1087S4A1,
    "Salary.Inde" = P6750, "Salary.No.Rem"  = P8640S1, 
    "Pension" = P8642S1, "Premium.Pension" = P8648S1,
    "Subsidy.Children" = P8644S1, "Rent.Income" = P8646S1, 
    "Other.Income" = P8654S1, "Income.Sell" = P8652S1, 
    "Income.Other.HH" = P8650S1A1,
    FEX_C
  )

summary(ECV_2021_Final)

########## Creating Income, Spending and Savings variables for analysis ##########


ECV_2021_Final <- ECV_2021_Final %>% 
  rowwise() %>% 
  mutate(Income.BR = sum(Salary, Scholarship, Salary.Food, Salary.Housing,
                         Salary.Other, Subsidy.Food, Subsidy.Transport,
                         Subsidy.Family, Premiums, Premium.Services,
                         Premium.Vacations, Bonuses,
                         Salary.Inde, Salary.No.Rem, Pension, Premium.Pension,
                         Subsidy.Children, Rent.Income, Other.Income, Income.Sell,
                         Income.Other.HH, 
                         na.rm = TRUE),
         Services.Spending.HH = sum(Sp.Electricty, Sp.Gas, Sp.Sewer,
                                 Sp.Disposal, Sp.Water, Sp.Cooking.Fuel, 
                                 Sp.Cell, Services,
                                 na.rm = TRUE),
         Housing.Spending.HH = sum(Amortization.Fee, Rent.Fee, Admon,
                                   Monthly.Housing.Services,
                               na.rm = TRUE),
         Healthcare.Spending = sum(Cut.Healthcare, Cut.Other.Healthcare, 
                                   Cost.Hospitalization, Appointment.EPS, 
                                   Appointment.Private, Appoinment.Prepaid,
                                   na.rm = TRUE),
         Childhood.Spending = sum(Child.Admon, Child.Equipment,
                                  Child.Equipment.Out, Child.Food,
                                  Child.Others, Child.Transport,
                                  Child.Tuition, Child.Uniforms,
                                  na.rm = TRUE),
         Education.Spending = sum(School.Tuition, School.Uniforms,
                                  School.Equipment, School.Equipment2,
                                  School.Admon, School.Transport,
                                  School.Food, School.Others,
                                  na.rm = TRUE)
         )

summary(ECV_2021_Final)

##### Calculate Spending per Household (HH) for the variables that were pulled from people's data sets #####

ECV_2021_Final <- ECV_2021_Final %>% 
  group_by(DIRECTORIO,SECUENCIA_ENCUESTA_H) %>% 
  mutate(
    
    Healthcare.Spending.HH = sum(Healthcare.Spending,
                                 na.rm = TRUE),
    Childhood.Spending.HH = sum(Childhood.Spending,
                                 na.rm = TRUE),
    Education.Spending.HH = sum(Education.Spending,
                                 na.rm = TRUE)
  )

##### Total Spending per Household #####

ECV_2021_Final <- ECV_2021_Final %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(
    Spending.BR.HH = sum(Services.Spending.HH, Housing.Spending.HH,
                         Healthcare.Spending.HH, Childhood.Spending.HH,
                         Education.Spending.HH, Monthly.Food, 
                         Monthly.Group.1, Monthly.Group.2,
                         Monthly.Group.3, Monthly.Group.4,
                         na.rm = TRUE
                         )
  )


##### Export data for prediction with ENPH #####

Export_data <- ECV_2021_Final %>% 
  select(DIRECTORIO, SECUENCIA_ENCUESTA_H, SECUENCIA_P, ORDEN,
         Sex, Age, Kin, Max.Educ, Strata, "Icmdug" = I_HOGAR,
        "Housing.Spending2" = Housing.Spending.HH, "Food.Spending" = Monthly.Food, 
        "Healthcare.Cut" =  Cut.Healthcare, City, Healthcare.System, Labor.Contract,
        FEX_C)

Export_data <- Export_data %>% 
  mutate(
    Pension.Cut = Healthcare.Cut + Healthcare.Cut*0.98,
    Under4 = ifelse(
      Age < 5, 
      1,
      0
    )
  )

Export_data <- Export_data %>% 
  group_by(DIRECTORIO, SECUENCIA_ENCUESTA_H) %>% 
  mutate(
    Num.Under4 = sum(
      Under4,
      na.rm = TRUE
    ),
    Size.HH = sum(SECUENCIA_P, 
                  na.rm = TRUE
    ),
    Size.HH2 = Size.HH^2,
    Per.Depen = ifelse(Age <= 14 | Age >= 65,
                       1,
                       0),
    Per.Product = ifelse(Age > 14 | Age < 65,
                         1,
                         0),
    Per.Depen.T = sum(Per.Depen,
                      na.rm = TRUE
    ),
    Per.Product.T = sum(Per.Product,
                        na.rm = TRUE
    ),
    Tasa.Depen = round(Per.Depen.T/Per.Product.T*100,1
    )
  )

Export_data <- Export_data %>% 
  filter(Kin == 1) %>% 
  select(-c(ORDEN, Kin, Per.Depen, Per.Depen.T, 
            Per.Product, Per.Product.T))

##### Income per household #####

ECV_2021_Final <- ECV_2021_Final %>% 
  group_by(DIRECTORIO,SECUENCIA_ENCUESTA_H) %>% 
  mutate(
    Income.BR.HH = sum(Income.BR, 
                       na.rm = TRUE)
  )

##### Income per capita #####

ECV_2021_Final <- ECV_2021_Final %>% 
  ungroup() %>% 
  mutate(
    Income.PerCapita.BR = round(Income.BR.HH/No.Members.HH)
  )

##### Check Point #####

ECV_2021_Final %>% 
  select(Income.BR, Income.BR.HH, I_HOGAR, Income.PerCapita.BR,
         PERCAPITA, Spending.BR.HH,FEX_C) %>% 
  summary()

# Difference Household income: 11% in the median
# Difference Per Capita income: 7,7% in the median


ECV_2021_Final %>% 
  ungroup() %>% 
  filter(Kin == 1) %>% 
  summarise(Total.Spending = sum(Spending.BR.HH*FEX_C),
            Total.Income.BR = sum(Income.BR.HH*FEX_C), 
            Total.Income.DANE = sum(I_HOGAR*FEX_C)
            ) 
# We have a 0,15 billion subestimation of the total spending vs DANE'S annexes
# The income, on the contrary, has a 10 billion overestimation. Is in need of review.

######################################## EDA ######################################################

########## Create Data set for analysis at a household level ##########

ECV_2021_HH <- ECV_2021_Final %>% 
  ungroup() %>% 
  filter(Kin == 1) 

##### Set data to survey to make more accurate calculations #####

ECV_2021_HH_S <- as_survey_design(ids = 1,
                                 weights = FEX_C,
                                 .data = ECV_2021_HH
)

##### Create deciles of income for analysis #####

Deciles_Ing_HH <- ECV_2021_HH_S %>% 
  summarise(Deciles.Ing = survey_quantile(I_HOGAR, 
                                             c(0.1,0.2,0.3, 0.4, 0.5,
                                               0.6,0.7,0.8,0.9)))


Deciles_Ing_HH <- Deciles_Ing_HH %>% 
  select(1:9)

Labels = c(paste0("Decile ", 1:9), "Decile 10")

ECV_2021_HH <- ECV_2021_HH %>% 
  mutate(Deciles.Income.HH = 
           cut(Icmdug,
               breaks = c(-Inf, Deciles_Ing_HH, Inf),
               labels = Labels))


### Check the decile conformation

ECV_2021_HH %>% 
  group_by(Deciles.Ing.HH) %>% 
  summarise(No.Households = sum(FEX_C))

### Total household income per decile #####

ECV_Income_Deciles <- ECV_2021_HH %>% 
  group_by(Deciles.Ing.HH) %>% 
  summarise(Total.Income.HH = round(sum(I_HOGAR*FEX_C)/1000000))

##### Create deciles of spending for analysis #####

Deciles_Spending_HH <- ECV_2021_HH_S %>% 
  summarise(Deciles.Spending = survey_quantile(Spending.BR.HH, 
                                          c(0.1,0.2,0.3, 0.4, 0.5,
                                            0.6,0.7,0.8,0.9)))


ECV_2021_HH <- ECV_2021_HH %>% 
  mutate(
    Deciles.Spending.HH = case_when(
      Spending.BR.HH <= Deciles_Spending_HH$Deciles.Spending_q10 ~ "Decile 1",
      Spending.BR.HH > Deciles_Spending_HH$Deciles.Spending_q10 & Spending.BR.HH <= Deciles_Spending_HH$Deciles.Spending_q20 ~ "Decile 2", 
      Spending.BR.HH > Deciles_Spending_HH$Deciles.Spending_q20 & Spending.BR.HH <= Deciles_Spending_HH$Deciles.Spending_q30 ~ "Decile 3", 
      Spending.BR.HH > Deciles_Spending_HH$Deciles.Spending_q30 & Spending.BR.HH <= Deciles_Spending_HH$Deciles.Spending_q40 ~ "Decile 4", 
      Spending.BR.HH > Deciles_Spending_HH$Deciles.Spending_q40 & Spending.BR.HH <= Deciles_Spending_HH$Deciles.Spending_q50 ~ "Decile 5", 
      Spending.BR.HH > Deciles_Spending_HH$Deciles.Spending_q50 & Spending.BR.HH <= Deciles_Spending_HH$Deciles.Spending_q60 ~ "Decile 6",
      Spending.BR.HH > Deciles_Spending_HH$Deciles.Spending_q60 & Spending.BR.HH <= Deciles_Spending_HH$Deciles.Spending_q70 ~ "Decile 7",
      Spending.BR.HH > Deciles_Spending_HH$Deciles.Spending_q70 & Spending.BR.HH <= Deciles_Spending_HH$Deciles.Spending_q80 ~ "Decile 8", 
      Spending.BR.HH > Deciles_Spending_HH$Deciles.Spending_q80 & Spending.BR.HH <= Deciles_Spending_HH$Deciles.Spending_q90 ~ "Decile 9", 
      Spending.BR.HH > Deciles_Spending_HH$Deciles.Spending_q90 ~ "Decile 10"
    )
  )

### Check the decile conformation

ECV_2021_HH %>% 
  group_by(Deciles.Spending.HH) %>% 
  summarise(No.Households = sum(FEX_C))

### Total household income per decile #####

ECV_Spending_Deciles <- ECV_2021_HH %>% 
  group_by(Deciles.Spending.HH) %>% 
  summarise(Total.Spending.HH = round(sum(Spending.BR.HH*FEX_C)/1000000))
