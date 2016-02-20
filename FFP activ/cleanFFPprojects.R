
# Intro -------------------------------------------------------------------
# This piece of code imports data from FFP's tracking of programs, sectors, and countries.
# Laura Hughes, lhughes@usaid.gov, 9 February 2016

# Import packages ---------------------------------------------------------
# Helper functions.

pkgs =  c('dplyr', 'tidyr', 'readxl', 'ggplot2', 'stringr', 'lubridate')

# Check if packages are installed
alreadyInstalled = installed.packages()[,'Package']

toInstall = pkgs[!pkgs %in% alreadyInstalled]

# Install anything that isn't already installed.
if(length(toInstall > 0)) { 
  install.packages(toInstall)
}


# Load packages
for (i in 1:length(pkgs)) {
  library(pkgs[i], character.only = TRUE)
}


# Import raw data ---------------------------------------------------------
# Note: assumes the data will be in an Excel file with header information in the first 10 rows,
# and columns: category, subcategory, categoryNumber, activity, and all the projects.

# Raw data is in 4 pieces: 
# 1) whether or not the programs claim to do activities.
# 2) metadata on the programs
# 3) recategorized categories and subcats

# 1) Full data
rawData = read_excel('~/Documents/USAID/Kyla - FFP reshaping/TecSectorsTracking_021916.xlsx', 
                     sheet = 1, skip = 1) 

# 2) Pull out just the date information (since it gets screwed up on import)
metadata = read_excel('~/Documents/USAID/Kyla - FFP reshaping/TecSectorsTracking_021916.xlsx',
                      sheet = 2, col_types = c('text', 'text', 'text','text', 'text', 'text',
                                               'numeric', 
                                               'text', 'text', 'text', 'blank', 'blank', 'text', 'text')) %>% 
  mutate(startDate = ymd(startDate),
            endDate = ymd(endDate)) %>% 
  rename(pgrmNum = `Program Number`,
         pgrmName = `Program Name`,
         countryCt = `Country Count`,
         pgrmType = `Program Type`,
         ARRyear = `ARR Year Submitted`
         )


# 3) Add in the corrected categories from Kyla
categ =  read.csv('~/Documents/USAID/Kyla - FFP reshaping/activity_categories.csv') %>% 
  transmute(category1 = str_trim(`category.1`),
            category2 = str_trim(`category.2`),
            subcategory2 = str_trim(subcategory),
            origCat = str_trim(origCat), 
            origSub = str_trim(origSub),
            activity = str_trim(activity))

# Fix unlabeled column names
colnames(rawData)[1:3] = c('category', 'subcategory', 'catNum')

# Preliminary clean of activities data ------------------------------------

# Remove program-level data
activities = rawData %>% 
  slice(-12:-1) %>% 
  rename(activity = id) %>% 
  mutate(activity = str_trim(activity, side = 'both'),
         category = str_trim(category),
         subcategory = str_trim(subcategory))

# Merge in new category names
activities = full_join(activities, categ, by = c('activity' = 'activity', 'category' = 'origCat', 'subcategory' = 'origSub')) %>% 
  mutate(category1 = ifelse(is.na(category1), category, category1),
         subcategory2 = ifelse(is.na(subcategory2), subcategory, subcategory2)) %>%
  select(-subcategory, -category) %>% 
  rename(subcategory = subcategory2, activID = catNum)

# Reshape activities into a single table
activities = activities %>% 
  gather(id, isActive, -category1, -category2, -subcategory, -activID, -activity)


# Convert and check that all values are either 0, 1, or NA
# Assumptions: throwing away any data with for specifying what "other" means; Select Y/N --> no response.
activities = activities %>% 
  mutate(isActive = str_trim(isActive),
         isActive = ifelse(isActive %in% c('N/A', 'Select Y/N'), NA,
                           ifelse(isActive %in% c('N', 'no', 'No', 'NO'), '0',
                                  ifelse(isActive %in% c('Y', 'yes', 'Yes', 'YES'), '1',
                                         ifelse(str_detect(activity, 'Other') & !(isActive %in% c('0', '1', NA)), # Convert other values to 1's.
                                                '1', isActive)))))

# Throw a warning if anything is unexpected
if (any(unique(activities$isActive) %in% c("0", "1", NA) == FALSE)) {
  warning('Activity data has some weird info! Check that all values for the activites are 0, 1, or NA')
}

# Lastly... convert to numbers. 
activities = activities %>% 
  mutate(isActive = as.numeric(isActive))

# Merge programmatic level data -------------------------------------------
activities = full_join(activities, metadata, by = 'id')


# Additional cleaning
activities = activities %>% 
  mutate(pgrmType = str_trim(pgrmType),
         Awardee = str_trim(Awardee),
         Awardee = ifelse(Awardee == 'CRS', 'Catholic Relief Services',
                          ifelse(Awardee %in% c('ACDI/VOCA', 'ACDI / VOCA'), 'ACDI/VOCA (PCI, JSI & MCI)',
                                 ifelse(Awardee == 'CARE Bangladesh', 'CARE',
                                        ifelse(Awardee == 'Save the Children International', 'Save the Children',
                                               ifelse(Awardee == 'Mercy Corps/Nepal', 'Mercy Corps',
                                                      ifelse(Awardee == 'FH', 'Food for the Hungry',
                                                             ifelse(Awardee == 'ADRA EST DR CONGO', 'ADRA',
                                                                    Awardee))))))),
         pgrmType = ifelse(pgrmType == 'Devleopment', 'Development', pgrmType))



# Eliminate redundant data ------------------------------------------------



# Explore data ------------------------------------------------------------
activ = activities %>% 
  filter(pgrmType == 'Development') %>% 
  filter(isActive == 1)

View(activ %>% group_by(Country, category1) %>% summarise(num = sum(isActive, na.rm = TRUE)) %>% filter(num > 0) %>% group_by(Country) %>% mutate(tot = sum(num), pct = round(num/sum(num),2)) %>% arrange(desc(num)))

View(activ %>% group_by(Country, subcategory) %>% summarise(num = sum(isActive, na.rm = TRUE)) %>% filter(num > 0) %>% group_by(Country) %>% mutate(tot = sum(num), pct = round(num/sum(num),2)) %>% arrange(desc(num)))

View(activ %>% group_by(Country, activity) %>% summarise(num = sum(isActive, na.rm = TRUE)) %>% filter(num > 0) %>% group_by(Country) %>% mutate(tot = sum(num), pct = round(num/sum(num),2)) %>% arrange(desc(num)))

activ %>% group_by(Awardee) %>% summarise(num = n()) %>% arrange(desc(num))

View(activ %>% group_by(Awardee, category) %>% summarise(num = n()) %>% mutate(total = sum(num), pct = num/total) %>% ungroup() %>% arrange(desc(num)))

# Export data -------------------------------------------------------------
write.csv(activ, '~/Documents/USAID/Kyla - FFP reshaping/Development_activities.csv')
write.csv(activities, '~/Documents/USAID/Kyla - FFP reshaping/allFFP_activities.csv')

# just categories and activities ------------------------------------------
uniqueActiv = activ %>% 
  select(category, subcategory, activity) %>% 
  distinct() %>% 
  arrange(category, subcategory, activity)

write.csv(uniqueActiv, '~/Documents/USAID/Kyla - FFP reshaping/activity_categories.csv')
