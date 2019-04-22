library("plyr")
library(descr)
library("partykit")
library("tidyverse")
data <- read.csv("D:/Programs/Python/survey/input/Career.csv")
str(data)
data$stream <- revalue(data$stream,c(
  'Computer science and engineering,'='COMP','Electronics and communication engineering,'='ELEC',
  'Information technology,'='COMP','Mechanical engineering,'='MECH','Civil engineering,'='MECH',
  'Mechatronics engineering,'='MECH','Electrical and electronics and engineering'='ELEC',
  'Environmental engineering,'='CHEM','Architecture and construction engineering,'='MECH',
  'Highway engineering,'='MECH','Computer engineering,'='COMP','Marine engineering,'='CHEM',
  'Automobile engineering,'='MECH','Aeronautical engineering,'='MECH','Aerospace engineering,'='MECH',
  'Telecommunication engineering,'='ELEC','Electronics and communication engineering,'='ELEC',
  'Agricultural engineering,'='CHEM','Production and industrial engineering,'='MECH',
  'Chemical engineering,'='CHEM','Electrical engineering,'='ELEC','Instrumental engineering,'='ELEC',
  'Mining engineering,'='CHEM','Architectural'='MECH','Biological Science'='CHEM','Bio-Medical'='CHEM',
  'Biological engineering'='CHEM','Nuclear engineering'='CHEM','Systems engineering'='COMP','Others'=-1
))
data$state <- revalue(data$state,c(
  'Rajasthan'='W','Delhi'='N','Haryana'='N','Punjab'='N','Chandigarh'='N','Andhra Pradesh'='S',
  'Telangana'='S','Arunachal Pradesh'='E','Assam'='E','Bihar'='C','Chhattisgarh'='C','Goa'='S',
  'Gujarat'='W','Himachal Pradesh'='N','Jammu and Kashmir'='N','Jharkhand'='C','Karnataka'='S',
  'Kerala'='S','Madhya Pradesh'='C','Maharashtra'='C','Manipur'='E','Meghalaya'='E','Mizoram'='E',
  'Nagaland'='E','Odisha'='E','Sikkim'='E','Tamil Nadu'='S','Tripura'='E','Uttar Pradesh'='N',
  'Uttarakhand'='N','West Bengal'='E','Andaman and Nicobar Islands'='S',
  'Dadra and Nagar Haveli'='W','Daman and Diu'='W','Lakshadweep'='S','Pondicherry'='S',
  'Abroad'=-1
))
data$career <- revalue(data$career,c('Defence'='D','Entrepreneurship'='E','Family business'='B','Higher study'='H',
                                     'Private Sector job'='R','Public Sector Job'='P'))
data$parents<- revalue(data$parents,c('Defence'='DEF','Business'='BUS','Private Sector'='PRIVATE',
                                      'Public Sector'='PUBLIC','Self-Employed'='SELF'))
str(data)
ct <- ctree(
  career ~ ., 
  data = data,
  control = ctree_control(minsplit=2, minbucket=5, mincriterion=.5))

