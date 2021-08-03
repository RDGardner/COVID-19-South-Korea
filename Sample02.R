# Ricca Callis
# EN 605.662 Data Visualization
# Project 4 - Data Exploration and Design
# 3 Required Data Sets:
#   Data Set 1: https://www.kaggle.com/spscientist/students-performance-in-exams 
#   Data Set 2: https://www.kaggle.com/kimjihoo/coronavirusdataset?select=Case.csv 
#   Data Set 3: https://www.kaggle.com/karangadiya/fifa19 

# Project Instructions:

# I. Purpose:
#   Recently, a number of libraries have been released to help organizations 
#   develop new visualizations and illustration tools. Some of the popular libraries include D3, 
#   Chart.js, Plot.ly, Highcharts, Bokeh, ggplot, matplotlib, ProtoVIS, R Shiny, NVD3, etc… 
#   As data scientists, it is important for us to have a basic understanding of those libraries 
#   and their capabilities. The purpose of this assignment is to get familiar with open source 
#   libraries by developing three sample visualizations using any libraries for JavaScript, R, or Python.

# II. Task
#   1. Analyze 3 data sets. The data should have 3 or more variables and 100 or more rows.
#   2. Develop 3 different visualizations by leveraging the library of your choice to illustrate
#      the datasets under consideration.
#   3. File structure: students should structure their project the following way:
#       o your_lastname_project04/
#           	Paper: your_lastname_project04.pdf
#                 • Introduction: What the project is about
#                 • Dataset: explain and provide links to the sources
#                 • Approach: explain which libraries you selected
#                 • Visualization #1: Explain, provide screenshot, and justify
#                 • Visualization #2: Explain, provide screenshot, and justify
#                 • Visualization #3: Explain, provide screenshot, and justify
#                 • Conclusion
#                 • References
#           	src/
#                 • Sample01/
#                     o Index1.html or Sample1.py or Sample1.R, etc…
#                     o Data1.csv
#                     o Screenshot_sample01.jpg
#                 • Sample02/
#                     o Index2.html or Sample2.py or Sample2.R, etc…
#                     o Data2.csv
#                     o Screenshot_sample02.jpg
#                 • Sample03/
#                     o Index3.html or Sample3.py or Sample3.R, etc…
#                     o Data3.csv
#                     o Screenshot_sample03.jpg
#          Requeriments.txt: List dependencies (e.g. R, Shiny R, Python, matplotlib, D3, etc…). 
# IV. What to submit
#     • A .zip file with the file structure shown above
#     • A paper describing the projects, the datasets that were chosen, the thee visualizations or
#       dashboards that were developed (including screenshot), and explanation of what was
#       updated from any sample code that was used.
#     • Submit document through Blackboard. Please use the following file format: your_lastname_project04.zip

# Load standard libraries
library(readr)
library(ggplot2)
library(readxl)
library (MPV)
library (olsrr)
library("ggpubr")
library("Hmisc")
library(caret)
library (lattice)
library(leaps)
library(MASS)
library(alr3)
library(readxl)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(ggthemes)
library(viridis)
library("plotly")
library(corrplot)
library(maps)
library(alr3)
library(rms)
library(ppcor)
library(ggthemes)
library(data.table)
library(ISLR)
library(tibble)
library(aod)
library(tidyverse)
library(psych)
library(pastecs)
library(summarytools)
library(magrittr)
library(scales)
library(sf)
library(lubridate)
library(ggraph)
library(igraph)
library(dplyr)
library(reshape)
library(tidygraph)
library(ggthemes)
library(ggExtra)
library(cowplot)
library(maps)
library(highcharter)
install.packages("imputeTS")
library(imputeTS)
library(corrplot)
# Set  working directory
#setwd("/Users/riccacallis/Desktop/JHU/Data Science/Data Visualization/Project 4")

# DATA SET 2
# Data on COVID-19 in South Korea
# https://www.kaggle.com/kimjihoo/coronavirusdataset?select=Case.csv
# Read csv file
Patients<- read_csv("/Users/riccacallis/Desktop/JHU/Data Science/Data Visualization/Project 4/patients.csv")
View(Patients)

# 7754 observations
# 18 variables

# Columns:
# global_id (num, double)
# local_id (logical)
# sex (char)
# birth_year (num, double)
# country (char)
# province (char)
# disease (num, double)
# group (char)
# exposure_start (char)
# exposure_end (char)
# infection_reason (char)
# infection_order (num, double)
# infected_by (num, double)
# contact_number (num, double)
# confirmed_date (char)
# released_date (char)
# deceased_date (char)
# state = (char)

# Look at the first six rows
head(Patients)
# Look at all data & attach it
View(Patients)
attach(Patients)
str(Patients)

# Check for missing data/null values
apply(is.na(Patients[,]),2,sum)
# Some missing values found
# 7754 total observations
# Missing values confirmed_date: 0
# Missing values released_date: 7699
#   Total Obs - Missing Values = 7754-7699 = 55 released patients
# Missing values deceased_date: 7718
#   Total Obs - Missing Values = 7754-7718 = 36 deceased patients
# Total Number of Patients Still Infected: 
#   Total Obs - Released - Deceased = 7754 - 55 - 36 = 7663

# Descriptive Statistics
# Summary Statistics
summary(Patients) # N, class, min, 1Q, Median, Mean, 3Q, Max each variable
describe(Patients) # N, mean, sd, median, min, max, range, skew, kurtosis
stat.desc(Patients) # null, min, max, rang, sum, median, mean, SE mean, CI mean, var, std, coef var

# Create Age Variable
Patients %<>% 
  mutate(
    age = 2020 - birth_year,
    age_range = cut(
      age,
      breaks = seq(0, 100, 10),
      labels = c('00-09', '10-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90-99'),
      right = F
    ) %>% 
      as.character
  )


# Age distributions
Patients %>%
  select(age) %>%
  na.omit() %>%
  group_by(age) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot() +
  geom_bar(aes(x = age, y = Count), stat = 'identity') +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(text = element_text(size = 15, face = "bold")) + 
  ggtitle('Patient Age Distribution')

# Age bin distributions
Patients %>%
  select(age_range) %>%
  na.omit() %>%
  group_by(age_range) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot() +
  geom_bar(aes(x = age_range, y = Count), stat = 'identity') +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(text = element_text(size = 15, face = "bold")) + 
  ggtitle('Patient Age Distribution')

# Age by gender
Patients %>%
  select(age_range, sex) %>%
  na.omit() %>%
  group_by(age_range,sex) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot() +
  geom_bar(aes(x = age_range, y = Count, fill=sex), stat = 'identity') +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(text = element_text(size = 15, face = "bold")) + 
  ggtitle('Patient Age Distribution')

# Gender differences in age range 
gender_age_range = Patients %>% select(age_range,sex) %>% 
  na.omit() %>% 
  group_by(age_range,sex) %>% 
  summarise(Count = n())
gender_age_range
# Groups:   age_range [10]
# age_range sex    Count
# <chr>     <chr>  <int>
#   1 00-09     female     2
# 2 00-09     male       2
# 3 10-19     female     6
# 4 10-19     male       6
# 5 20-29     female    82
# 6 20-29     male      65
# 7 30-39     female    46
# 8 30-39     male      50
# 9 40-49     female    80
# 10 40-49     male      33
# 11 50-59     female    91
# 12 50-59     male      50
# 13 60-69     female    52
# 14 60-69     male      44
# 15 70-79     female    19
# 16 70-79     male      16
# 17 80-89     female     5
# 18 80-89     male      13
# 19 90-99     female     1
# 20 90-99     male       1

# Vertical BarChart
# Gender by Math Grade
age_range_gender_plot <-ggplot(data = gender_age_range) +
  geom_bar(mapping = aes(x = age_range,Count,fill = sex), stat = 'Identity',position = 'dodge') + 
  ggtitle('Age Group Distribution by Gender')
age_range_gender_plot

#Vertical bar chart with gender filter
# Select filter
ageRangeGenderHighlight <- highlight_key(gender_age_range)
gg5 <- ggplot(ageRangeGenderHighlight) +
  geom_bar(mapping=aes(x=age_range,y=Count, fill = sex), stat = 'Identity',position = 'dodge')
pickGender <- bscols(
  filter_select("id", "Select a Gender", ageRangeGenderHighlight, ~sex),
  ggplotly(gg5, dynamicTicks=TRUE)
)
bscols(pickGender)

# 
# # CANT GET THIS TO WORK
# # # Create Age Butterfly Chart
# Patients %>%
#   filter(!is.na(age), !is.na(sex)) %>%
#   group_by(sex, age_range) %>%
#   dplyr::summarise(Count = n()) %>%
#   ggplot(aes(x = age_range, y = Count, fill = sex)) +
#   geom_blank(
#     # dummy data
#     data = data.frame(
#       age_range = c('00-09', '00-09'),
#       sex = c('male', 'female'),
#       count = c(a <- Patients %>%
#                   group_by(age_range, sex) %>%
#                   dplyr::summarise(Count = n()) %>%
#                   na.omit %>% ungroup %>%
#                   select(Count) %>%
#                   top_n(Count, n = 1) %>%
#                   first * 1.2
#                 , -a)
#     )
#   ) +
#   geom_col(data = . %>% subset(sex == 'male'), aes(y = Count), color = 'black') +
#   geom_col(data = . %>% subset(sex == 'female'), aes(y = -1 * Count), position = 'identity', color = 'black') +
#   coord_flip() +
#   facet_wrap(
#     ~sex,
#     scales = 'free'
#   ) +
#   theme(
#     strip.text.x = element_blank(),
#     axis.ticks.y = element_blank(),
#     text = element_text(size = 15, face = 'bold')
#   ) +
#   scale_y_continuous(
#     expand = c(0, 0),
#     labels = abs
#   ) +
#   labs(x = 'Ages', y = 'Patients')

# Province Distribution
Patients%>%
  select(province) %>%
  na.omit() %>%
  group_by(province) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot() +
  geom_bar(aes(x = province, y = Count), stat = 'identity') +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(text = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 90)) +
  ggtitle('Patient Province Distribution')

# Province Infection Reasons
Patients %>% 
  filter(!is.na(province), !is.na(infection_reason)) %>% 
  group_by(province, infection_reason) %>% 
  dplyr::summarise(Count = n()) %>% 
  union_all(
    Patients %>% 
      filter(!is.na(province), !is.na(infection_reason)) %>% 
      group_by(province) %>% 
      dplyr::summarise(Count = n()) %>%
      mutate(infection_reason = 'total')
  ) %>% 
  union_all(
    Patients %>% 
      filter(!is.na(province), !is.na(infection_reason)) %>% 
      group_by(infection_reason) %>% 
      dplyr::summarise(Count = n()) %>%
      mutate(province = 'total')
  ) %>% 
  union_all(
    Patients %>% 
      filter(!is.na(province), !is.na(infection_reason)) %>% 
      dplyr::summarise(Count = n()) %>%
      mutate(province = 'total', infection_reason = 'total')
  ) %>% 
  ggplot(
    aes(
      x = reorder(province, Count, function(x){return(max(x))}), 
      y = reorder(infection_reason, Count, function(x){return(max(x))})
    )
  ) +
  geom_tile(aes(fill = Count)) +
  geom_text(aes(label = Count, color = Count), size = 4, fontface = 'bold', show.legend = F) +
  geom_vline(xintercept = seq(1, n_distinct(Patients %>% select(province))) + .5, linetype = 'dashed') +
  geom_hline(yintercept = seq(1, n_distinct(Patients %>% select(infection_reason))) + .5, linetype = 'dashed') +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_distiller(
    direction = 1
  ) +
  scale_color_distiller(palette = 8) +
  theme(
    text = element_text(size = 15, face = 'bold'),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  labs(x = 'Province', y = 'Infection reason')

#Convert into Plotly
newplot <- newChartData%>%
  ggplot(mapping = 
           aes(
             x = reorder(province, Count, function(x){return(max(x))}), 
             y = reorder(infection_reason, Count, function(x){return(max(x))}),
             text = paste(
               "Province:", province,
               "\nInfection Reason:", infection_reason,
               "\nCount:", Count
             )
           )
  ) +
  geom_tile(aes(fill = Count)) +
  geom_text(aes(label = Count, color = Count), size = 4, fontface = 'bold', show.legend = F) +
  geom_vline(xintercept = seq(1, n_distinct(Patients %>% select(province))) + .5, linetype = 'dashed') +
  geom_hline(yintercept = seq(1, n_distinct(Patients %>% select(infection_reason))) + .5, linetype = 'dashed') +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_distiller(
    direction = 1
  ) +
  scale_color_distiller(palette = 8) +
  theme(
    text = element_text(size = 15, face = 'bold'),
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  labs(x = 'Province', y = 'Infection reason')+
  labs(title = "Province by Infection Reason Heatmap")
newplot
ggplotly(newplot, tooltip="text")

#Province Infection Reasons Heat Map
plottest <-ggplot(newChartData, aes(x=province, y=infection_reason, fill=Count))+
  theme(axis.text.x = element_text(angle = 90))
plottest
plottest+geom_tile() #Ugly

# Province by Infection Reasons Again
newChartData <-data.frame (Patients %>% 
                             filter(!is.na(province), !is.na(infection_reason)) %>% 
                             group_by(province, infection_reason) %>% 
                             dplyr::summarise(Count = n()) %>% 
                             union_all(
                               Patients %>% 
                                 filter(!is.na(province), !is.na(infection_reason)) %>% 
                                 group_by(province) %>% 
                                 dplyr::summarise(Count = n()) %>%
                                 mutate(infection_reason = 'total')
                             ) %>% 
                             union_all(
                               Patients %>% 
                                 filter(!is.na(province), !is.na(infection_reason)) %>% 
                                 group_by(infection_reason) %>% 
                                 dplyr::summarise(Count = n()) %>%
                                 mutate(province = 'total')
                             ) %>% 
                             union_all(
                               Patients %>% 
                                 filter(!is.na(province), !is.na(infection_reason)) %>% 
                                 dplyr::summarise(Count = n()) %>%
                                 mutate(province = 'total', infection_reason = 'total')
                             ))

newChartData
#Filter
reasonHighlight <- highlight_key(newChartData)
ggProvinceReason<-ggplot(reasonHighlight)+
  geom_bar(mapping=(aes(province, Count, fill = infection_reason )), stat = 'Identity',position = 'dodge')+
  #facet_wrap(~province)+
  theme(axis.text.x=element_text(angle =90))+
  labs(title="Province by Infection Reason")
filterProvinceReason<-bscols(
  filter_checkbox("id", "Select a Province", reasonHighlight, ~province, inline=TRUE),
  ggplotly(ggProvinceReason),
  widths=c(15,15)
)
bscols(filterProvinceReason)


# Age x Province
Patients %>%
  select(age, province) %>%
  na.omit() %>%
  group_by(age, province) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot() +
  geom_tile(aes(x = age, y = province, fill = Count)) +
  geom_vline(xintercept = seq(0.5,9.5,by = 1), linetype = 'dashed') +
  geom_hline(yintercept = seq(0.5,14.5,by = 1), linetype = 'dashed') +
  scale_fill_gradientn(colours = c("yellow","royalblue"),
                       values = c(0,0.1,1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_classic() +
  theme(text = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 90),
        legend.position = "top",
        legend.key.width = unit(3,"cm"))

# Gender by Age by Province
Patients %>%
  select(sex, age, province) %>%
  na.omit() %>%
  group_by(sex, age, province) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot() +
  geom_tile(aes(x = age, y = province, fill = Count)) +
  geom_vline(xintercept = seq(0.5,9.5,by = 1), linetype = 'dashed') +
  geom_hline(yintercept = seq(0.5,14.5,by = 1), linetype = 'dashed') +
  scale_fill_gradientn(colours = c("yellow","royalblue"),
                       values = c(0,0.1,1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_classic() +
  theme(text = element_text(size = 15, face = "bold"),
        axis.text.x = element_text(angle = 90),
        legend.position = "top",
        legend.key.width = unit(3,"cm"))   +
  facet_wrap(~sex)

#Re do as Horizontal Bar Chart with Select Features
Patients %>%
  select(sex, age_range, province) %>%
  na.omit() %>%
  group_by(sex, age_range, province) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot() +
  geom_tile(aes(x = age_range, y = province, fill = Count)) +
  geom_text(aes(x=age_range, y=province, label=Count))+
  geom_vline(xintercept = seq(0.5,9.5,by = 1), linetype = 'dashed') +
  geom_hline(yintercept = seq(0.5,14.5,by = 1), linetype = 'dashed') +
  scale_fill_gradientn(colours = c("yellow","royalblue"),
                       values = c(0,0.1,1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_classic()+
  #theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~sex)

chartData <-data.frame(Patients %>%
                         select(sex, age_range, province) %>%
                         na.omit() %>%
                         group_by(sex, age_range, province) %>%
                         dplyr::summarise(Count = n()))  
class(chartData)
chartData

#Filter
pthighlight <- highlight_key(chartData)
ggPTFilter <- ggplot(pthighlight)+
  geom_tile(aes(x = age_range, y = province, fill = Count)) +
  geom_text(aes(x=age_range, y=province, label=Count))+
  geom_vline(xintercept = seq(0.5,9.5,by = 1), linetype = 'dashed') +
  geom_hline(yintercept = seq(0.5,14.5,by = 1), linetype = 'dashed') +
  scale_fill_gradientn(colours = c("yellow","royalblue"),
                       values = c(0,0.1,1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme_classic()+
  #theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~sex)
ptFilter<- bscols(
  filter_select("id", "Select a Gender", pthighlight, ~sex),
  ggplotly(ggPTFilter)
)
bscols(ptFilter) # Doesnt look right

# Gender by Age by Province Histogram Faceting
provinceFacet <- Patients %>%
  select(sex, age_range, province) %>%
  na.omit() %>%
  group_by(sex, age_range, province) %>%
  dplyr::summarise(Count = n()) %>%
  ggplot() +
  geom_histogram(mapping=(aes(age_range, Count)), stat = 'Identity',position = 'dodge')+
  facet_grid(sex~province)+
  theme(axis.text.x=element_text=(angle =90))+
  labs(title="Age Range Histograms by Province and Gender")
provinceFacet
#Filter
provinceHighlight <- highlight_key(chartData)
ggAgeProvinceGender<-ggplot(provinceHighlight)+
  geom_histogram(mapping=(aes(age_range, Count, fill = sex)), stat = 'Identity',position = 'dodge')+
  facet_wrap(~province)+
  theme(axis.text.x=element_text(angle =90))+
  labs(title="Age Range Frequency by Province and Gender")
filterProvince<-bscols(
  filter_checkbox("id", "Select a Province", provinceHighlight, ~province, inline=TRUE),
  ggplotly(ggAgeProvinceGender, width = 1000, height = 500),
  widths=c(15,15)
)
bscols(filterProvince)

# Check for missing data/null values
apply(is.na(Patients[,]),2,sum)
# 7754 total observations
# Missing values confirmed_date: 0
# Missing values released_date: 7699
#   Total Obs - Missing Values = 7754-7699 = 55 released patients
# Missing values deceased_date: 7718
#   Total Obs - Missing Values = 7754-7718 = 36 deceased patients
# Total Number of Patients Still Infected: 
#   Total Obs - Released - Deceased = 7754 - 55 - 36 = 7663


#IGNORE, CANT FIGURE OUT
# Patients %>%
#   filter(!is.na(confirmed_date)) %>%
#   group_by(confirmed_date) %>%
#   ggplot() +
#   geom_line(
#     aes(x = confirmed_date, y = cumsum(confirmed_date), group = 1, color = '1'),
#     stat = 'count',
#     size = 3,
#     show.legend = FALSE
#   ) +
#   geom_point(
#     data = . %>%
#       group_by(confirmed_date) %>%
#       dplyr::summarise(Count = n()) %>%
#       arrange(desc(count)) %>%
#       slice(1),
#     aes(x = confirmed_date, y = count(confirmed_date)),
#     color = 'red',
#     shape = 21,
#     stroke = 3,
#     size = 6
#   )

# IGNORE SECTION - CANT GET THIS WORKING
# Confirmed Cases Over Time
# Patients %>%
#   filter(!is.na(confirmed_date)) %>%
#   group_by(confirmed_date) %>%
#   ggplot() +
#   geom_line(
#     aes(x = confirmed_date, y = ..count.., group = 1, color = '1'),
#     stat = 'count',
#     size = 3,
#     show.legend = FALSE
#   ) +
#   geom_line(
#     aes(x = confirmed_date, y = sum(..count..) / Patients %>% select(confirmed_date) %>% n_distinct(na.rm = TRUE), group = 1, color = '2'),
#     linetype = 'dashed',
#     stat = 'count',
#     size = 2,
#     show.legend = F
#   ) +
#   geom_point(
#     data = . %>%
#       group_by(confirmed_date) %>%
#       dplyr::summarise(Count = n()) %>%
#       arrange(desc(count)) %>%
#       slice(1),
#     aes(x = confirmed_date, y = count),
#     color = 'red',
#     shape = 21,
#     stroke = 3,
#     size = 6
#   ) +
#   geom_text(
#     data = . %>%
#       group_by(confirmed_date) %>%
#       dplyr::summarise(Count = n()) %>%
#       arrange(desc(count)) %>%
#       slice(1),
#     aes(x = confirmed_date, y = count, label = paste(confirmed_date, ': \n "', count, '"', sep = '')),
#     vjust = -.5,
#     size = 6,
#     fontface = 'bold'
#   ) +
#   scale_x_date(
#     expand = expansion(mult = c(0, 0.02)),
#     date_breaks = '1 week',
#     labels = function(x) format(x, '%b %d')
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, .2))) +
#   labs(x = 'Date', y = 'The number of confirmed patients') +
#   theme(text = element_text(size = 15, face = 'bold'))



# Time Series Analysis
# Number of Cases (Confirmed, Deceased, Released) Over Time
# For each patient, identify their dates (confirmed, released, deceased)
DATE = Patients %>%
  select(confirmed_date, released_date, deceased_date)
# Confirm variable was made appropriately
head(DATE)
# Make variable for confirmed dates
C_DATE = DATE %>% 
  select(confirmed_date) %>% 
  group_by(confirmed_date) %>% 
  summarise(Count = n()) %>%
  mutate(Group = "Confirmed")
# Assign new confirmed date variable as array within Date
colnames(C_DATE)[1] = "DATE"
# Make variable for released dates
R_DATE = DATE %>% 
  select(released_date) %>% 
  group_by(released_date) %>% 
  summarise(Count = n()) %>%
  mutate(Group = "Released")
# Assign new released date variable as array within Date
colnames(R_DATE)[1] = "DATE"
# Make variable for deceased date
D_DATE = DATE %>% 
  select(deceased_date) %>% 
  group_by(deceased_date) %>% 
  summarise(Count = n()) %>%
  mutate(Group = "Deceased")
# Assign new deceased date variable as array within Date
colnames(D_DATE)[1] = "DATE"
# Bind all date arrays to one variable & omit NAs
T_DATE = rbind(C_DATE, R_DATE, D_DATE) %>% na.omit()
# Confirm code
head(T_DATE)
# Plot time course analysis
ggplot(NULL) +
  geom_point(data = T_DATE, 
             aes(x = DATE, y = Count, col = Group, shape = Group,size = Count)) +
  geom_line(data = T_DATE,
            aes(x = DATE, y = Count, col = Group, group = Group, linetype = Group)) +
  geom_text(data = T_DATE %>% filter(Group == "Confirmed"),
            aes(x = DATE, y = Count + 20, label = Count)) + 
  guides(size = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2,0.8)) +
  theme(axis.text.x = element_text(angle = 90))

#Multiple interactive components
multipleHighlights <- highlight_key(T_DATE)
multipleHighlightPlot <- ggplot(multipleHighlights) +
  #ggplot(NULL) +
  geom_point(data = T_DATE, 
             aes(x = DATE, y = Count, col = Group, shape = Group,size = Count)) +
  geom_line(data = T_DATE,
            aes(x = DATE, y = Count, col = Group, group = Group, linetype = Group)) +
  #geom_text(data = T_DATE %>% filter(Group == "Confirmed"),
  #          aes(x = DATE, y = Count + 20, label = Count)) + 
  guides(size = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2,0.8)) +
  theme(axis.text.x = element_text(angle = 90))
widgets <- bscols(
  filter_select("id", "Select a Group", multipleHighlights, ~T_DATE$Group),
  filter_checkbox("id", "Select Dates", multipleHighlights, ~DATE, inline = TRUE),
  ggplotly(multipleHighlightPlot),
  widths=c(20,20)
)

bscols(widgets)

#Confirmed cases by province
ConfirmedProvince = Patients %>% select(confirmed_date, province) %>% 
  na.omit(Patients$province) %>%
  filter(!is.na(province)) %>% 
  group_by(confirmed_date, province) %>% 
  summarise(Count = n())
# Confirm variable was made appropriately
head(ConfirmedProvince)
# Check for missing data/null values
apply(is.na(ConfirmedProvince[,]),2,sum)
# Get rid of NAs
ConfirmedProvincenoNA <- na.omit(ConfirmedProvince)
apply(is.na(ConfirmedProvincenoNA[,]),2,sum)
# Filter by province
provinceConfirmedFilter <- highlight_key(ConfirmedProvincenoNA)
ggConfirmedProvince <- ggplot(provinceConfirmedFilter) +
  #ggplot(NULL) +
  geom_point(data = ConfirmedProvincenoNA, 
             aes(x = confirmed_date, y = Count, col = province, shape = province)) +
  geom_line(data = ConfirmedProvincenoNA,
            aes(x = confirmed_date, y = Count, col = province, group = province, linetype = province)) + 
  guides(size = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 15, face = "bold"),
        legend.position = c(0.2,0.8)) +
  theme(axis.text.x = element_text(angle = 90))
selectProvince <- bscols(
  filter_select("id", "Select a Province", provinceConfirmedFilter, ~province),
  ggplotly(ggConfirmedProvince),
  widths=c(12,12)
)
bscols(selectProvince)


# IGNORE, CANT GET THIS TO WORK
# Time Series Analysis
# Number of Cases (Confirmed, Deceased, Released) Over Time, Group by State
# State being either currently infected (i.e., confirmed) or no longer infected 
# (either released or deceased)
# For each patient, identify their dates (confirmed, released, deceased) & state
# Patients %>% 
#   gather(key = state, value = date, confirmed_date, released_date, deceased_date) %>% 
#   filter(!is.na(date)) %>% 
#   mutate(state = str_extract(state, '(released|deceased)') %>% coalesce('isolated')) %>% 
#   group_by(state, date) %>% 
#   dplyr::summarise(Count = n()) %>%
#   spread(state, count) %>% 
#   complete(date=seq(ymd(min(date)), ymd(max(date)), by = 'day'))%>% 
#   replace(is.na(.), 0) %>% 
#   mutate(
#     deceased = cumsum(deceased),
#     isolated = cumsum(isolated),
#     released = cumsum(released),
#     r = dense_rank(desc(date))
#   ) %>% 
#   gather(key = state, value = count, deceased, isolated, released) %>% 
#   filter(r <= 10) %>% 
#   ggplot(aes(x = date, y = count, fill = factor(state, level = c('isolated', 'released', 'deceased')))) +
#   geom_col(
#     position = 'stack',
#     width = .5,
#     color = 'black'
#   ) +
#   geom_text(
#     aes(label = ..y..),
#     position = 'stack',
#     vjust = -.5,
#     size = 5,
#     fontface = 'bold'
#   ) +
#   guides(fill = guide_legend(title = 'state')) +
#   scale_x_date(
#     date_breaks = '2 days', 
#     labels = function(x) format(x, '%b %d')
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#   theme(text = element_text(size = 15, face = 'bold')) +
#   labs(x = 'Date', y = 'Total Patients')


# NETWORKS
# A graph G = (V, E)
#   V: A set of vertices (or nodes)
#   E: A set of edges (or links) between those nodes
# Like a network: each person is a vertex & each relationship between people is an edge connecting them

# Converting raw data to an igraph network object:
#   d: Edges of network. 1st two columns are the IDs of the source and target
#      node for each edge
#   vertices: Starts with a column of node IDs. 
# Network layouts: Algorithms that return coordinates for each node in a network
#   Kamada Kawai ("kk"): Force-directed algorithm. Produces nice layout for connected graphs
# Available layouts in igraph
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
par(mfrow=c(3,3), mar=c(1,1,1,1)) 
for (layout in layouts) 
  print(layout)

# Network Analysis: A network of links between infected individuals
library(ggplot2)
library(dplyr)
library(reshape)
Patients_Network = Patients %>%
  select(infected_by, global_id, contact_number) %>%
  na.omit()
Patients_Network
Patients_Network$global_id[!Patients_Network$global_id %in% Patients_Network$infected_by]
sort(unique(c(Patients_Network$global_id,Patients_Network$infected_by)))
DF = data.frame(
  Total_Id = sort(unique(c(Patients_Network$global_id,Patients_Network$infected_by)))
)

DF = merge(DF, Patients_Network,
           by.x = "Total_Id", by.y = "global_id", all = TRUE)
DF$contact_number[is.na(DF$contact_number)] = 0
DF
# Set visual properties for network plot by using key function parameters:
# Nodes have color, fill, shape, size, & stroke
# Edges have color, width, and linetype
# alpha parameter controls transparency
# ggraph packake uses traditional ggplot2 mapping aesthetics using aes()
# aes() matches visual parameters with attribute names from the data
# Here, nodes are COVID-19+ patients.
# We add a layer with node labels using geom_node_label()
# Node labels are unique patient IDs (or patient "names")
# The number inside each node is the patient's number of contacts
# A color gradient is added to highlight individual differences in number of contacts
# geom_edge_link() is used for straight edges
Patients %>%
  select(infected_by, global_id, contact_number) %>%
  na.omit() %>%
  as_tbl_graph() %>%
  arrange(as.numeric(name)) %>%
  mutate(Contact_Number = DF$contact_number) %>%
  ggraph(layout="kk") +
  geom_edge_diagonal(alpha = 1) +
  geom_node_label(aes(label = name, fill = Contact_Number),repel = TRUE) +
  scale_fill_gradientn(colours = c("yellow","red")) + 
  xlab("") + ylab("") +
  theme_classic() +
  ggtitle("Infection Network Analysis & Number of Contacts") + #Added title
  theme(legend.position = "right",
        #legend.box.background = element_rect(),
        #legend.box.margin = margin(2, 2, 2, 2),
        legend.key.height = unit(2,'cm')) # Adjusted size so you can read legend title

net<-Patients %>%
  select(infected_by, global_id, contact_number) %>%
  na.omit() %>%
  as_tbl_graph() %>%
  arrange(as.numeric(name)) %>%
  mutate(Contact_Number = DF$contact_number)
# Different Version
ggraph(net, layout="tree") +
  geom_edge_parallel(alpha = 1)+
  geom_node_label(aes(label = name, fill = Contact_Number),repel = TRUE)+
  scale_fill_gradientn(colours = c("yellow","red")) +
  ggtitle("Infection Network Analysis & Number of Contacts")+
  theme_void() #minimal/empty theme

