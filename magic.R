library(readr)
library(dplyr)
library(stringr)

# First rows are kak, so we ignoring those
csvData <- read_delim(
  "02_hr_data_v00.csv",
  delim = ";",
  skip = 2,
  locale = locale(encoding = "Latin1"),
  name_repair = "minimal",
  show_col_types = FALSE
)

# Drop the first column because it has no data
csvData <- csvData[, -1]

#col names in english
translations <- c(
  "Ikä" = "Age",
  "VasenYritys" = "PreviousCompany",
  "Liikematka" = "BusinessTravel",
  "Osasto" = "Department",
  "EtäisyysKotoa" = "DistanceFromHome",
  "Koulutus" = "Education",
  "Koulutusala" = "EducationField",
  "TyöntekijöidenMäärä" = "NumEmployees",
  "Henkilöstökortti" = "EmployeeCard",
  "Sukupuoli" = "Gender",
  "Työtaso" = "JobLevel",
  "TyöRooli" = "JobRole",
  "Siviilisääty" = "MaritalStatus",
  "Kuukausitulot" = "MonthlyIncome",
  "YritystenLukumääräJoissaTyöskenteli" = "NumCompaniesWorked",
  "Yli18" = "Over18",
  "ProsenttiPalkankorotuksesta" = "PercentSalaryHike",
  "Aukioloajat" = "WorkingHours",
  "Osakeoptiotaso" = "StockOptionLevel",
  "Kokonaistyövuodet" = "TotalWorkingYears",
  "HarjoitusajatViimeVuonna" = "TrainingTimesLastYear",
  "VuosiaYrityksessä" = "YearsAtCompany",
  "VuosiaEdellisestäPromootiosta" = "YearsSinceLastPromotion",
  "VuosiaNykyisenJohtajanKanssa" = "YearsWithCurrentManager"
)

# Apply translations 
colnames(csvData) <- translations[colnames(csvData)]

# BusinessTravel accepted values
# Is value rare accepted?
businessTravelVals <-c(
  "travel_frequently",
  "travel_rarely",
  "non-travel"
)

# Education accepted vals
educationVals <-c(
  "college",
  "master",
  "bachelor",
  "doctor"
)

# Marital values
maritalVals <- c("single", "married", "divorced", "widowed")

# Convert all data to lowercase, because some values are all caps
# I will do some comparisons and it will be easier
csvData <- csvData %>%
  mutate(across(where(is.character), tolower))

# Convert column data
csvData <- csvData %>%
mutate(
  PreviousCompany = ifelse(
    PreviousCompany %in% c("yes", "no"), PreviousCompany, NA
  ),
  Age = as.integer(Age),
  #Department = as.integer(Department),
  DistanceFromHome = as.integer(DistanceFromHome),
  Education = case_when(
    str_detect(Education, "below") ~ NA_character_, # becomes NA because data like that makes no sense
    str_detect(Education, "bachelor") ~ "bachelor",
    str_detect(Education, "master") ~ "master",
    str_detect(Education, "doctor") ~ "doctor",
    str_detect(Education, "college") ~ "college",
    str_detect(Education, "doctor") ~ "doctor",
    TRUE ~ NA_character_   # any other value becomes NA
  ),
  NumEmployees = as.integer(NumEmployees),
  EmployeeCard = as.integer(EmployeeCard),
  Gender = ifelse(
    Gender %in% c("male", "female"), Gender, NA
  ),
  JobLevel = as.integer(JobLevel),
  #converting shortened job roles into proper ones
  JobRole = case_when(
    str_detect(JobRole, "mngr") ~ "manager",
    str_detect(JobRole, "lab tech") ~ "laboratory technician",
    str_detect(JobRole, "sales e") ~ "sales executive",
    str_detect(JobRole, "healthcare rprs") ~ "healthcare representative",
    str_detect(JobRole, "rs") ~ "research scientist",
    str_detect(JobRole, "sr") ~ "sales representative",
    str_detect(JobRole, "rd") ~ "research director",
    TRUE ~ JobRole
  ),
  MaritalStatus = case_when(
    str_detect(MaritalStatus, "single") ~ "single",
    str_detect(MaritalStatus, "married") ~ "married",
    #str_detect(MaritalStatus, "divorced") ~ "divorced",
    #str_detect(MaritalStatus, "widowed") ~ "widowed",
    TRUE ~ NA_character_  # any unrecognized values
  ),
  MonthlyIncome = as.numeric(MonthlyIncome),
  NumCompaniesWorked = as.integer(NumCompaniesWorked),
  Over18 = ifelse(
    Over18 %in% c("y", "n"), Over18, NA
  ),
  PercentSalaryHike = str_remove_all(PercentSalaryHike, "%"),  # remove %
  PercentSalaryHike = as.numeric(PercentSalaryHike),
  WorkingHours = str_remove_all(WorkingHours, "hrs"),  # remove hrs
  WorkingHours = as.numeric(WorkingHours),
  StockOptionLevel = ifelse(StockOptionLevel == "no", 0, StockOptionLevel),
  StockOptionLevel = as.numeric(StockOptionLevel),
  TotalWorkingYears = ifelse(TotalWorkingYears == "one", 1, TotalWorkingYears),
  TotalWorkingYears = as.numeric(TotalWorkingYears),
  TrainingTimesLastYear = str_trim(TrainingTimesLastYear),
  TrainingTimesLastYear = as.integer(TrainingTimesLastYear),
  YearsAtCompany = as.numeric(YearsAtCompany),
  # I guess "no" counts as zero
  YearsSinceLastPromotion = ifelse(
    YearsSinceLastPromotion == "no",
    0,
    YearsSinceLastPromotion
  ),
  YearsSinceLastPromotion  = as.numeric(YearsSinceLastPromotion),
  YearsWithCurrentManager  = as.numeric(YearsWithCurrentManager)
)

# Drop rows with junk data or that contain NA
csvData <- csvData %>%
  filter(
    !is.na(PreviousCompany),
    BusinessTravel %in% businessTravelVals,
    !is.na(Age),
    !str_detect(Education, "^[0-9]+$"),
    !str_detect(EducationField, "^[0-9]+$"),
    !is.na(MonthlyIncome),
    !is.na(NumEmployees),
    !is.na(Gender),
    !is.na(JobLevel),
    !is.na(JobRole),
    !is.na(MaritalStatus),
    !is.na(NumCompaniesWorked),
    !is.na(Over18),
    !is.na(PercentSalaryHike),
    !is.na(WorkingHours),
    !is.na(StockOptionLevel),
    !is.na(TotalWorkingYears),
    !is.na(TrainingTimesLastYear),
    !is.na(YearsAtCompany),
    !is.na(YearsSinceLastPromotion),
    !is.na(YearsWithCurrentManager)
  )

# To view output
View(csvData)
