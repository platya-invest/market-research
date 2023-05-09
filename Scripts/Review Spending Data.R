
##### Objetive: Review Spending data #####

### Necessary Data

## ECV 2021

##########  Upload data #####

Household_Spending <- read.csv2("Gastos de los hogares.csv", 
                                sep = ",", 
                                dec = ".", 
                                fileEncoding = "UTF-8")

Household_Spending_Location <- read.csv2("Gastos de los hogares lc.csv", 
                                sep = ";", 
                                dec = ".", 
                                fileEncoding = "UTF-8")

##########  Set Spending data to wide format #####

Household_Spending_Final <- Household_Spending %>% 
  arrange(P3204) %>% 
  select(-c(SECUENCIA_ENCUESTA, ORDEN,P3205S1:P3205S5, P3204S2)) %>% 
  pivot_wider(
    names_from = P3204,
    names_prefix = "Item",
    values_from = P3204S1)


##########  Merge Spending Data #####

Spending_Data <- Household_Spending_Location %>%  
  merge(
    Household_Spending_Final,
    by.x = c("DIRECTORIO", "SECUENCIA_ENCUESTA"),
    by.y = c("DIRECTORIO", "SECUENCIA_P"),
    all.x = TRUE
  )

Spending_Data = Spending_Data[,!grepl(".x$",names(Spending_Data))]

colnames(Spending_Data) <- sub("\\.y", "", colnames(Spending_Data))


###################### Exploring the data ###########################

##########  Checking if the wide format changed the data #####

##### Original data set #####

Household_Spending %>% 
  summarise(Total.Spending = sum(P3204S1*FEX_C, 
                                 na.rm = TRUE))

# 1.78 billions of spending

##### Wide data set #####

### Total spending per household

Household_Spending_Final <- Household_Spending_Final %>% 
  mutate(
    Total.Spending.HH = rowSums(across(contains("Item"), 
                          .names = "spend{.col}")[, 1:104], 
                   na.rm = TRUE)
  )

## National spending
  
Household_Spending_Final %>% 
  summarise(Total.Spending = sum(Total.Spending.HH*FEX_C, 
                                 na.rm = TRUE))


# 1.78 billions, same as the original

  



##########  Replicating annexes spending categories #####


ECV_2021_Test <- ECV_2021_Final %>% 
  mutate(
    Food = rowSums(across(contains("Item"), 
                          .names = "spend{.col}")[, 1:26], 
                   na.rm = TRUE),
    Housing.Services = rowSums(across(contains("Item"), 
                                      .names = "spend{.col}")[, 94:95], 
                               na.rm = TRUE)
    
    )


##### Calculate the monthly value

Household_Spending_Final <- Household_Spending_Final %>% 
  mutate(
    Monthly.Food = Food*4, # # Answered for the last 7 days
    Monthly.Housing.Services = Housing.Services/12 # Answered for the last 12 months
  )



ECV_2021_Final %>%
  ungroup() %>% 
  filter(Kin == 1) %>% 
  summarise(Total = sum(Housing.Spending.HH*FEX_C,
                        Services.Spending.HH*FEX_C))
  




Household_Spending_Review %>% 
  summarise(Total.Housing = )
            )

