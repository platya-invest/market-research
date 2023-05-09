
##### Objetive: Calculate and predict savings/invesment at a household level in Colombia

### Part 1: Data merge and cleaning

### Necessary Data

## ENPH 2016 - 2017:
# Caracteristicas generales personas
#Viviendas y hogares

###### Cleaning environment

rm(list=ls())

##### Upload packages

require(pacman)

p_load(tidyverse, ggplot2, 
       ggExtra, themis,
       gridExtra, kableExtra,
       survey, srvyr, highcharter,
       FactoMineR, factoextra) 

##### Functions

proper = function(x) paste0(toupper(substr(x, 1, 1)), 
                            tolower(substring(x, 2)))

#####  Set highcharter options

options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))

#### Set font for graphs

Thm <- hc_theme(chart = list(
  style = list(
    fontFamily = "Helvetica")))

##### Upload data #####

### Define data location

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/Work/Consultancy/Platya/Data/ENPH 2016 - 2017")


People_ENPH <- read.csv2("Caracteristicas generales personas.csv", 
                              sep = ";", 
                              dec = ",", 
                              fileEncoding = "UTF-8")

Household_ENPH <- read.csv2("Viviendas y hogares.csv", 
                             sep = ";", 
                             dec = ",", 
                             fileEncoding = "UTF-8")

Expenses_ENPH <- read.csv2("Gastos menos frecuentes - Medio de pago.csv", 
                            sep = ";", 
                            dec = ",", 
                            fileEncoding = "UTF-8")


Food_Expenses_ENPH <- read.delim("Gastos diarios Urbano - Capitulo C.txt")


######################################## Woodwork ######################################################

##########  Capital letter to variables #####

names(People_ENPH) <- proper(names(People_ENPH)) 


names(Household_ENPH) <- proper(names(Household_ENPH)) 


names(Expenses_ENPH) <- proper(names(Expenses_ENPH)) 


names(Food_Expenses_ENPH) <- proper(names(Food_Expenses_ENPH)) 

########## Food Data set preparation for merge ########## 

##### Calculation of monthly spending in food #####

Food_Wide <- Food_Expenses_ENPH %>% 
  mutate(
    Monthly.Value = case_when(
      Nc2_cc_p2 %in% c(2, 3, 4) ~ Nc2_cc_p3_s1*2.14,
      Nc2_cc_p2 == 5 ~ Nc2_cc_p3_s1*2,
      Nc2_cc_p2 == 7 ~ Nc2_cc_p3_s1/2,
      Nc2_cc_p2 == 8 ~ Nc2_cc_p3_s1/3,
      TRUE ~ Nc2_cc_p3_s1
    )
  )

##### Long to wide for merge #####

Food_Expenses_ENPH_Final <- Food_Wide %>% 
  #arrange(P3204) %>% 
  select(-c(Secuencia_encuesta, Orden, Nc2_cc_p3_s2, Nc2_cc_p2, Nc2_cc_p3_s1)) %>% 
  pivot_wider(
    names_from = Nc2_cc_p1,
    names_prefix = "Item",
    values_from = Monthly.Value
    )


########## Merge data #####

Household_ENPH <- Household_ENPH %>% 
  rename("Fex.HH" = Fex_c)

### Household to household

Merge_Household1 <- Household_ENPH %>% 
  merge(
      Expenses_ENPH, 
      by = c("Directorio", "Secuencia_p")
    
  )


# Delete variables with ".y" in the name and rename variables with ".x" in the name
# productc of the merge.

Merge_Household1 = Merge_Household1[,!grepl(".y$",names(Merge_Household1))]

colnames(Merge_Household1) <- sub("\\.x", "", colnames(Merge_Household1))

Merge_Household2 <- Merge_Household1 %>% 
  merge(
    Food_Expenses_ENPH_Final, 
    by = c("Directorio", "Secuencia_p"),
    all.x = TRUE
  )

# Delete variables with ".y" in the name and rename variables with ".x" in the name
# productc of the merge.

Merge_Household2 = Merge_Household2[,!grepl(".y$",names(Merge_Household2))]

colnames(Merge_Household2) <- sub("\\.x", "", colnames(Merge_Household2))


### People and household data

ENPH_Raw <- Merge_Household2 %>% 
  merge(
    People_ENPH,
    by.x = c("Directorio", "Secuencia_encuesta"),
    by.y = c("Directorio", "Secuencia_p")
  )

# Delete variables with ".y" in the name and rename variables with ".x" in the name
# productc of the merge.

ENPH_Raw = ENPH_Raw[,!grepl(".y$",names(ENPH_Raw))]

colnames(ENPH_Raw) <- sub("\\.x", "", colnames(ENPH_Raw))

##### Check point #####

### Total People

ENPH_Raw %>% 
  summarise(Total = sum(Fex_c))

### Total Households

Zoom <- ENPH_Raw %>% 
  group_by(Directorio, Secuencia_p) %>% 
  summarise(Mean.Fex = mean(Fex_c))

Zoom %>% 
  ungroup() %>% 
  summarise(Total = sum(Mean.Fex)) # Mine: 14,35 millions
                                   # DANE's: 14,35 millions

########## Filter data by relevant variables for the model #####

ENPH_Final <- ENPH_Raw %>% 
  select(
    ##### ID
    Directorio, Secuencia_p, Orden,
    
    ##### General
    "Sex" = P6020, "Age" = P6040, "Ethnicity" = P6080, "Kin" = P6050,
    "City" = Dominio, "Class" =  P3, 
    ### Household
    "Strata" = P8520s1a1, "Electricity"= P8520s1, "Gas" = P8520s2,
    "Sewer" = P8520s3, "Disposal" = P8520s4, "Water" = P8520s5,
    "No.Rooms" = P1647,
    ##### Human Capital
    "Healthcare.System" = P6100, 
    "Healthcare.Cut" = P6120, "Pension.Cut" = P6920s1,
    "Scholarship" = P8610, "Study" = P6170, "Max.Educ" = P6210,
    
    ##### Labor market
    
    P6240, P6250, P6260, P6270, P6280, P6300, P6310, P6320, P6330, P6340, P6350, 
    "Labor.Contract" = P6440, "Type.Contract" = P6450, "Time.Working" = P6426,
    "Salary" = P6500, "Service.Premium" = P6630s1, "Profession" = P6370s1, 
    "CIIU4" = P6390s1, "Position" = P6430, "Size.Comp" = P6870,
    ##### Capital
    
    "Social.Security.Cut" = P6120,
    "Type.Household" = P5747, "Rent.Income" = P7500s1,
    "Investments" = P7510s5, "Value.Investments" = P7510s5a1, "Owned.Rented" = P5090,
    
    ##### Housing Spending
    
    "Rent.Sp" = P5140, "Amortization.Fee" = P5100s4, "Hypo.Rent.Sp" = P5110,
    
    #### Food Spending 
    
    Item1:Item25,
    
    ##### Services
    
    P10272s1a1, P10272s1a2, P10272s2a1, P10272s2a2, P10272s3a1, P10272s3a2, 
    P10272s4a1, P10272s4a2, P10272s6a1, P10272s6a2, P10272s7a1, P10272s7a2, 
    P10272s8a1, P10272s8a2, P10272s9a1, P10272s9a2,
    
    
    ##### Subsidies
    P1668s1, P1668s2, P1668s3, P1668s4, P1668s5, P1668s6,
    
    ##### Outcome variables
    It,  Icgu, Icmug, Icmdug, Gtug, Gcug, Gcmug,  
    Fex_c, Fex.HH
     )


########## Create relevant variables for the model ##########

##### People #####

ENPH_Final <- ENPH_Final %>% 
  mutate(
         Employed = case_when(
           P6240 == 1 ~ 1, 
           P6250 == 1 ~ 1, 
           P6260 == 1 ~ 1,
           Position == 7 & P6270 == 1 ~ 1,
           TRUE ~ 0
         ),
         Under4 = ifelse(
           Age < 5, 
           1,
           0
         ), 
         Services = ifelse(
           Electricity == 1 & Gas == 1 & Sewer == 1 & Disposal == 1 & Water == 1, 
           1, 
           0
         ),
         Subsidies = ifelse(
           P1668s1 == 1 | P1668s2 == 1 | P1668s3 == 1 | 
           P1668s4 == 1 | P1668s5 == 1 | P1668s6 == 1, 
           1,
           0
         ),
         "Water.Sp" = round(P10272s1a1/P10272s1a2),
         "Disposal.Sp" = round(P10272s2a1/P10272s2a2),
         "Sewer.Sp" = round(P10272s3a1/P10272s3a2),
         "Electricity.Sp" = round(P10272s4a1/P10272s4a2),
         "Gas.Sp" = round(P10272s6a1/P10272s6a2),
         "Phone.Sp" = round(P10272s7a1/P10272s7a2),
         "Internet.Sp" = round(P10272s8a1/P10272s8a2),
         "Tv.Sp" = round(P10272s9a1/P10272s9a2)
         )


##### Household #####

ENPH_Final <- ENPH_Final %>% 
  group_by(Directorio, Secuencia_p) %>% 
  mutate(Size.HH = sum(Secuencia_p, 
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
  ),
  Num.Employed = sum(Employed,
                     na.rm = TRUE
  ),
  Num.Under4 = sum(
    Under4,
    na.rm = TRUE
  )
  )

ENPH_Final <- ENPH_Final %>% 
  rowwise() %>% 
  mutate(
    Housing.Spending = sum(Water.Sp, Disposal.Sp, Sewer.Sp, Electricity.Sp,
                           Gas.Sp, Phone.Sp, Internet.Sp, Tv.Sp, Rent.Sp, Amortization.Fee,
                           na.rm = TRUE
                           ),
    Housing.Spending2 = sum(Water.Sp, Disposal.Sp, Sewer.Sp, Electricity.Sp,
                           Gas.Sp, Phone.Sp, Internet.Sp, Tv.Sp, Rent.Sp, Amortization.Fee,
                           Hypo.Rent.Sp,
                           na.rm = TRUE
    ),
    Food.Spending = sum(Item1, Item2, Item3, Item4, Item5, 
                        Item6, Item7, Item8, Item9, Item10, 
                        Item11, Item12, Item13, Item13, Item13,
                        Item14, Item15, Item16, Item17, Item18,
                        Item19, Item20, Item21, Item22, Item23,
                        Item24, Item25, 
                        na.rm = TRUE
                        )
  ) 


##### NAs Analysis #####

ENPH_Final <- ENPH_Final %>% 
  mutate(
    Gcmug = case_when(
      is.na(Gcmug) ~ Gcug, 
      TRUE ~ Gcmug
    )
  )


########## Replicating DANE Annexes #####

##### Setting data #####

Household_ENPH_Final <- ENPH_Final %>% 
  filter(Kin == 1)

### Check number of households

Household_ENPH_Final %>% 
  ungroup() %>% 
  summarise(Total.Households = sum(Fex.HH))

### Setting the household data set as survey data to account for the expansion factor

Household_ENPH_S <- as_survey_design(ids = 1,
                                weights = Fex.HH,
                                .data = Household_ENPH_Final)


# Turns off the scientific notation for large numbers

options(scipen=999) 

##### National available monthly income per household member #####

Household_ENPH %>% 
  summarise(sum(Icmdug*Fex.HH)) # 25,4 billions

  
##### Mean available monthly income per household member

Household_ENPH_S %>% 
  summarise(survey_mean(Icmdug)) # 1,773 millions


##### National available monthly spending per household member #####

Household_ENPH %>% 
  summarise(sum(Gcmug*Fex.HH, 
                na.rm = TRUE)) # 20,8 billions

##### Mean available monthly cost per household member

Household_ENPH_S %>% 
  summarise(survey_mean(Gcmug, 
                        na.rm = TRUE)) # 1,453 millions


##### Create deciles of income for analysis #####

Deciles_Income_HH <- Household_ENPH_S %>% 
  summarise(Deciles.Income = survey_quantile(Icmdug, 
                                          c(0.1,0.2,0.3,0.4,0.5,
                                            0.6,0.7,0.8,0.9))) 

Deciles_Income_HH <- Deciles_Income_HH %>% 
  select(1:9)

Labels = c(paste0("Decile ", 1:9), "Decile 10")

Household_ENPH_Final <- Household_ENPH_Final %>% 
  mutate(Deciles.Income.HH = 
    cut(Icmdug,
        breaks = c(-Inf, Deciles_Income_HH, Inf),
                   labels = Labels))

### Check the decile conformation

Household_ENPH_Final %>% 
  group_by(Deciles.Income.HH) %>% 
  summarise(No.Households = sum(Fex.HH))


### Household income per decile #####

ENPH_Income_Deciles <- Household_ENPH_Final %>% 
  group_by(Deciles.Income.HH) %>% 
  summarise(Income.HH = round(sum(Icmdug*Fex.HH, na.rm = TRUE)/1000000),
            Spending.HH = round(sum(Gcmug*Fex.HH, na.rm = TRUE)/1000000)
            )

##### Create deciles of spending for analysis #####

# Deciles_Spending_HH <- Household_ENPH_S %>% 
#   summarise(Deciles.Spending = survey_quantile(Gcmug, 
#                                                c(0.1,0.2,0.3, 0.4, 0.5,
#                                                  0.6,0.7,0.8,0.9)))
# 
# 
# Household_ENPH_Final <- Household_ENPH_Final %>% 
#   mutate(
#     Deciles.Spending.HH = case_when(
#       Gcmug <= Deciles_Spending_HH$Deciles.Spending_q10 ~ "Decile 1",
#       Gcmug > Deciles_Spending_HH$Deciles.Spending_q10 & Gcmug <= Deciles_Spending_HH$Deciles.Spending_q20 ~ "Decile 2", 
#       Gcmug > Deciles_Spending_HH$Deciles.Spending_q20 & Gcmug <= Deciles_Spending_HH$Deciles.Spending_q30 ~ "Decile 3", 
#       Gcmug > Deciles_Spending_HH$Deciles.Spending_q30 & Gcmug <= Deciles_Spending_HH$Deciles.Spending_q40 ~ "Decile 4", 
#       Gcmug > Deciles_Spending_HH$Deciles.Spending_q40 & Gcmug <= Deciles_Spending_HH$Deciles.Spending_q50 ~ "Decile 5", 
#       Gcmug > Deciles_Spending_HH$Deciles.Spending_q50 & Gcmug <= Deciles_Spending_HH$Deciles.Spending_q60 ~ "Decile 6",
#       Gcmug > Deciles_Spending_HH$Deciles.Spending_q60 & Gcmug <= Deciles_Spending_HH$Deciles.Spending_q70 ~ "Decile 7",
#       Gcmug > Deciles_Spending_HH$Deciles.Spending_q70 & Gcmug <= Deciles_Spending_HH$Deciles.Spending_q80 ~ "Decile 8", 
#       Gcmug > Deciles_Spending_HH$Deciles.Spending_q80 & Gcmug <= Deciles_Spending_HH$Deciles.Spending_q90 ~ "Decile 9", 
#       Gcmug > Deciles_Spending_HH$Deciles.Spending_q90 ~ "Decile 10"
#     )
#   )

### Check the decile conformation

# Household_ENPH_Final %>% 
#   group_by(Deciles.Spending.HH) %>% 
#   summarise(No.Households = sum(Fex.HH))

### Household spending per decile #####

# ENPH_Spending_Deciles <- Household_ENPH_Final %>% 
#   group_by(Deciles.Spending.HH) %>% 
#   summarise(Spending.HH = round(sum(Gcmug*Fex.HH)/1000000)
#             )



########## Savings ########## 

##### Calculating total savings per decile #####

ENPH_Income_Deciles <- ENPH_Income_Deciles %>% 
  mutate(
    Savings.HH = Income.HH - Spending.HH
  )

##### Calculating mean savings per decile #####

### Setting the household data set as survey data to account for the expansion factor
### and the newly created decile variable

Household_ENPH_S <- as_survey_design(ids = 1,
                                     weights = Fex.HH,
                                     .data = Household_ENPH_Final)


Mean.Income.Deciles <- Household_ENPH_S %>% 
  group_by(Deciles.Income.HH) %>% 
  summarise(Mean.Income.HH = round(survey_mean(Icmdug)/1000),
            Mean.Spending.HH = round(survey_mean(Gcmug)/1000)) %>% 
  select(1,2,4)


##### Merging with total data 

ENPH_Savings_Deciles <- bind_cols(
  ENPH_Income_Deciles, 
  Mean.Income.Deciles
)

### Mean savings per decile #####

Table <- ENPH_Savings_Deciles %>% 
  mutate(
    Mean.Savings.HH = Mean.Income.HH - Mean.Spending.HH,
    Mean.Savings.HH.2022 = round(Mean.Savings.HH*(126.06/96.92)),
    
  ) %>% 
  arrange(Income.HH) %>% 
  select("Income Deciles" = Deciles.Income.HH...1, 2,3,4,6,7,8, 9,
         -Deciles.Income.HH...5)

kable(Table) %>% 
  kable_styling(bootstrap_options  = "striped", 
                full_width = F) %>% 
  row_spec(0, bold = T, 
           color = "white", 
           background = "#6558a6")

##### Creating savings variable for analysis #####

Household_ENPH_Final <- Household_ENPH_Final %>% 
  mutate(
    Amount.Savings.HH = Icmdug - Gcmug,
    Amount.Savings.HH.2021 = round(Amount.Savings.HH*(111.41/96.92)),
    Savings.HH = ifelse(
      Amount.Savings.HH*Fex.HH > 0, 
      1,
      0
    )
  )

##### Imputation of variables with NAs by income decile #####

Household_ENPH_Final <- Household_ENPH_Final %>% 
  group_by(Deciles.Income.HH) %>% 
  mutate(
    Healthcare.Cut = ifelse(
      is.na(Healthcare.Cut), 
      median(Healthcare.Cut, 
             na.rm = TRUE), 
      Healthcare.Cut),
    Pension.Cut = ifelse(
      is.na(Pension.Cut), 
      median(Pension.Cut, 
             na.rm = TRUE), 
      Pension.Cut),
    Water.Sp = ifelse(
      is.na(Water.Sp), 
      median(Water.Sp, 
             na.rm = TRUE), 
      Water.Sp),
    Disposal.Sp = ifelse(
      is.na(Disposal.Sp), 
      median(Disposal.Sp, 
             na.rm = TRUE), 
      Disposal.Sp),
    Sewer.Sp = ifelse(
      is.na(Sewer.Sp), 
      median(Sewer.Sp, 
             na.rm = TRUE), 
      Sewer.Sp),
    Electricity.Sp = ifelse(
      is.na(Electricity.Sp), 
      median(Electricity.Sp, 
             na.rm = TRUE), 
      Electricity.Sp),
    Gas.Sp = ifelse(
      is.na(Gas.Sp), 
      median(Gas.Sp, 
             na.rm = TRUE), 
      Gas.Sp),
    Phone.Sp = ifelse(
      is.na(Phone.Sp), 
      median(Phone.Sp, 
             na.rm = TRUE), 
      Phone.Sp),
    Internet.Sp = ifelse(
      is.na(Internet.Sp), 
      median(Internet.Sp, 
             na.rm = TRUE), 
      Internet.Sp),
    Tv.Sp = ifelse(
      is.na(Tv.Sp), 
      median(Tv.Sp, 
             na.rm = TRUE), 
      Tv.Sp),
    Strata = ifelse(
      is.na(Strata), 
      mode(Strata), 
      Strata),
    Healthcare.System = ifelse(
      is.na(Healthcare.System), 
      mode(Healthcare.System), 
      Healthcare.System)
  )

Household_ENPH_Final <- Household_ENPH_Final %>% 
  filter(!is.na(Amount.Savings.HH.2022))

######################################## ADE ######################################################


########## Generally describe the saver population #########

##### Percentage of households #####

Household_ENPH_Final %>% 
  group_by(Savings.HH) %>% 
  summarise(Per.Households = round(sum(Fex.HH)/14350388*100))

### 7.749.210 of households aprox save in Colombia (54% of households).
# 45% of the savers are in the 3 upper deciles

Zoom <- Household_ENPH_Final %>% 
  arrange(Icmdug) %>% 
  group_by(Savings.HH, Deciles.Income.HH) %>% 
  summarise(Total.HH = sum(Fex.HH), 
            Mean.Savings = weighted.mean(Amount.Savings.HH.2022, 
                                         Fex.HH)
            ) # 5.3 monthly mean savings for decile 10 households
              # 1,5 for decile 9
              # 0.9 for decile 8


Upper_Deciles = c("Decile 8","Decile 9", "Decile 10")

Zoom %>% 
  filter(Savings.HH == 1 & Deciles.Income.HH %in% Upper_Deciles) %>% 
  summarise(Total = sum(Total.HH)/7778612) # 45% of the savers are in the 3 upper deciles


# 54% of the households, but how much?

##### Total amount of money available for potential investment #####

Household_ENPH_Final %>% 
  summarise(Total.Income = sum(Icmdug*Fex.HH, 
                                  na.rm = TRUE),
            Total.Spending = sum(Gcmug*Fex.HH, 
                                 na.rm = TRUE),
            Total.Savings = sum(Amount.Savings.HH*Fex.HH, 
                                na.rm = TRUE),
            Total.Savings.2022 = sum(Amount.Savings.HH.2022*Fex.HH, 
                                     na.rm = TRUE)
              ) # 4,5 billions in savings
                # Equivalent to 5,9 billions in 2022 money

### Filtering just the households that manage to have positive savings #####

Household_ENPH_Final %>% 
  filter(Amount.Savings.HH.2022 > 0) %>% 
  summarise(Total.Available = sum(Amount.Savings.HH.2022*Fex.HH, 
                                  na.rm = TRUE))

# 8.3 billions in potential investment money
# 10.8 billions in potential investment in 2022 money

### Distribution of savings  #####

## Distribution

summary(Household_ENPH_Final$Amount.Savings.HH.2022)

## Density plot

Graph <- Household_ENPH_Final %>% 
  mutate(
    Amount.Savings.HH.Plot = round(Amount.Savings.HH.2022/1000, 1)
  )

hchart(density(round(Graph$Amount.Savings.HH.Plot)), 
  type = "area", 
  name = "Household savings",
  color = "#6658a6") %>% 
  hc_colors(c("#6658a6")) %>% 
  hc_chart(zoomType = "x") %>% 
  hc_tooltip(
    headerFormat = '<span style="color:{point.color}">\u25CF<span style="font-weight:bold"> {series.name}:</span>',
    pointFormat = 
    "<br /> Savings: {point.x:.2f}
    <br /> Density: {point.y:.4f}") %>% 
  hc_title(text = "Household savings density 2017",
           style = list(fontWeight = "bold",
                        fontSize = "18px"),
           align = "left") %>%
  hc_subtitle(text = "2022 values, adjusted by inflation",
              style = list(fontWeight = "bold",
                           fontSize = "16px"),
              align = "left") %>%
  hc_caption(text = "Source ENPH 2017",
             style = list(fontSize = "10px"),
             align = "left") %>%
  hc_yAxis(title = list(text = "")) %>%
  hc_xAxis(title = list(text = " ")) %>%
  hc_exporting(enabled = TRUE) %>% 
  hc_add_theme(Thm)
  
  
##### Rate of savings ######

### Var creation #####

Household_ENPH_Final <- Household_ENPH_Final %>% 
  mutate(
    Saving.Rate.HH = round(Amount.Savings.HH/Icmdug*100,1)
  )

### Explore Saving rate var #####

summary(Household_ENPH_Final$Saving.Rate.HH)

## Explore inf and NAs

Zoom <- Household_ENPH_Final %>% 
  filter(Saving.Rate.HH == -Inf | is.na(Saving.Rate.HH))

# Cases where the income and expenditure is zero 
# and when income is zero and expentiture is greater than zero
# We filter by that 1,1% of the data in the next step

Zoom <- Household_ENPH_Final %>% 
  filter(Saving.Rate.HH != -Inf & !is.na(Saving.Rate.HH))

summary(Zoom$Saving.Rate.HH)

# Median saving rate of 12%

## Just for positive savings?

Zoom <- Household_ENPH_Final %>% 
  filter(Savings.HH > 0)

summary(Zoom$Saving.Rate.HH)

# Goes up to 36%





########## Characterize the savers group ##########

##### By Age of the HH head #####
# 
# Household_ENPH_Final %>% 
#   hchart('scatter', 
#          hcaes(x = as.factor(Savings.HH), 
#                y = Age)) %>% 
#   hc_colors(c("#6658a6")) %>% 
#   hc_title(text = "Household savings by age 2017",
#            style = list(fontWeight = "bold",
#                         fontSize = "18px"),
#            align = "left") %>%
#   hc_subtitle(text = "2022 values, adjusted by inflation",
#               style = list(fontWeight = "bold",
#                            fontSize = "16px"),
#               align = "left") %>%
#   hc_caption(text = "Source ENPH 2017",
#              style = list(fontSize = "10px"),
#              align = "left") %>%
#   hc_yAxis(title = list(text = "")) %>%
#   hc_xAxis(title = list(text = " ")) %>%
#   hc_exporting(enabled = TRUE) %>% 
#   hc_chart(zoomType = "x") %>% 
#   hc_tooltip(
#     headerFormat = '<span style="color:{point.color}">\u25CF<span style="font-weight:bold"> {series.name}:</span>',
#     pointFormat = 
#       "<br /> Age: {point.x:.2f}
#     <br /> Savings: {point.y:.4f}") %>% 
#   hc_add_theme(Thm)


Household_ENPH_Final %>% 
  group_by(Savings.HH) %>% 
  summarise(Mean.Savings = weighted.mean(Age, 
                                         Fex.HH,
                                         na.rm = TRUE))

##### By sex #####

prop.table(table(Household_ENPH_Final$Savings.HH, 
                 Household_ENPH_Final$Sex), 1)


Household_ENPH_Final %>% 
  group_by(Sex) %>% 
  summarise(Mean.Savings = weighted.mean(Amount.Savings.HH.2022, 
                                         Fex.HH,
                                         na.rm = TRUE))

##### Number of  Max educ #####

prop.table(table(Household_ENPH_Final$Savings.HH, 
                 Household_ENPH_Final$Max.Educ), 2)


Household_ENPH_Final %>% 
  group_by(Max.Educ) %>% 
  filter(Icmdug != 0) %>% 
  summarise(Mean.Savings = weighted.mean(Amount.Savings.HH.2022, 
                                         Fex.HH,
                                         na.rm = TRUE))


### Box plot

hcboxplot(
  var = Household_ENPH_Final$Max.Educ,
  x = Household_ENPH_Final$Amount.Savings.HH.2022,
  weight = Household_ENPH_Final$Fex.HH,
  outliers = FALSE,
  color = "#2980b9"
) %>% 
  hc_chart(type = "column")

##### By Estrato #####

prop.table(table(Household_ENPH_Final$Savings.HH, 
                 Household_ENPH_Final$Estrato), 2)


##### By Tamaño del hogar #####

prop.table(table(Household_ENPH_Final$Savings.HH, 
                 Household_ENPH_Final$Size.HH), 2)


Household_ENPH_Final %>% 
  group_by(Size.HH) %>% 
  summarise(Mean.Savings = weighted.mean(Amount.Savings.HH.2022, 
                                         Fex.HH,
                                         na.rm = TRUE))

Household_ENPH_Final %>% 
  group_by(Size.HH) %>% 
  # ungroup() %>% 
  summarise(Mean.Savings = sum(Fex.HH,
                                         na.rm = TRUE))



##### By City #####

prop.table(table(Household_ENPH_Final$Savings.HH, 
                 Household_ENPH_Final$City), 2)


Zoom <- Household_ENPH_Final %>% 
  group_by(City) %>% 
  summarise(Mean.Savings = weighted.mean(Amount.Savings.HH.2022, 
                                         Fex.HH,
                                         na.rm = TRUE),
            Mean.Expenses = weighted.mean(Gcug, 
                                          Fex.HH,
                                          na.rm = TRUE),
            Median.Savings = median(Amount.Savings.HH.2022),
            Median.Income = median(Icmdug),
            Median.Expenses = median(Gcmug)
            )

Household_ENPH_Final %>% 
  group_by(Size.HH) %>% 
  # ungroup() %>% 
  summarise(Mean.Savings = sum(Fex.HH,
                               na.rm = TRUE))

##### Number of  rooms #####

prop.table(table(Household_ENPH_Final$Savings.HH, 
                 Household_ENPH_Final$No.Rooms), 2)


##### PCA Analysis #####

### Get ready the data set for the PCA analysis #####

Recipe_Data <- Household_ENPH_Final %>% 
  select(Icmdug, Strata, Size.HH, Size.HH2, 
         Age, Max.Educ, Healthcare.System, Num.Under4, 
         Housing.Spending2, Deciles.Income.HH, Tasa.Depen, Healthcare.Cut,
         Pension.Cut, Amount.Savings.HH, Amount.Savings.HH.2022, Savings.HH,
         Food.Spending)


summary(Recipe_Data)

## Define variables to turn into characters

To_Factor <- c("Strata", "Max.Educ", "Healthcare.System",
               "Deciles.Income.HH", "Savings.HH")

Recipe_Data[To_Factor] <- lapply(Recipe_Data[To_Factor], as.character)

## Turn character variables to factor

Recipe_Data <- Recipe_Data %>% 
  ungroup() %>% 
  mutate_if(
    is.character, factor) 

PCA_Recipe <- recipe(~., data = Recipe_Data) %>%
  step_dummy(Strata, Max.Educ, Healthcare.System)

### Create data with the categorical variables dummified #####

PCA_Data <- juice(prep(PCA_Recipe))


### Set variables to numeric and run the PCA analysis #####

PCA_Data <- sapply(PCA_Data, 
                     as.numeric)


PCA_ENPH  <- PCA(PCA_Data, 
                 scale.unit = TRUE, 
                 graph =  T)

## Cum variance of of the first components


PCA_Variance <- PCA_ENPH$eig[1, 2] + 
  PCA_ENPH$eig[2, 2] +
  PCA_ENPH$eig[2, 3]

## Graph components and households

# fviz_pca_ind(PCA_ENPH, 
#              axes = c(1, 2), 
#              geom = c("point", "text"), 
#              repel = TRUE , 
#              title = "Households - PCA")


######### Export Data ########## 

Export_Data <- Household_ENPH_Final %>% 
  select(-c(Kin, P6250:P6350, Electricity:Water, P1668s1:P1668s6, P6240,
            Water.Sp:Tv.Sp, Per.Depen:Per.Product.T, P10272s1a1:P10272s9a2,
            Type.Household, Item1:Item25, Scholarship, Social.Security.Cut,
            Service.Premium, 
            ))

##### Final Data ##### 

setwd("C:/Users/Juan/OneDrive - Universidad de los Andes/Juan/Documentos/GitHub/Household Savings Platya Consultancy/Data")

write.csv2(Export_Data, 
           "Model Data.csv", 
           row.names = FALSE)

