library(gdata)
library(plyr)

source('~/Dropbox/HIV/CD4/fix_excel_dates.R')

demog_data = read.xls ("/Users/frances/Dropbox/HIV/demographics-scan-dates-hiv-controls-master-excel-Jan-2017.xlsx",sep=",")

# These are all the spreadsheets I know of that contain VL data
VL_data_2017a = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2017/baselinedata02022017.xls",header=TRUE,sep=",")
VL_data_2017b = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2017/CD4_CD8_VL_20170131_final.xlsx",header=TRUE,sep=",")
VL_data_2015 = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2015/Copy of R01_Neuro_7yr_Demographic_Clinical_20150820_rawdata.xlsx",header=TRUE,sep=",")
VL_data_2014a=read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2014/Viral Load.xlsx",header=TRUE,sep=",")
VL_data_2014b=read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2014/Baseline Viral Loads.xls",header=TRUE,sep=",")

# Give corresponding cols the same name
# SID
colnames(VL_data_2014a)[(names(VL_data_2014a) == "studyid")] <- "SID"
colnames(VL_data_2014b)[(names(VL_data_2014b) == "StudyID")] <- "SID"
colnames(VL_data_2017a)[(names(VL_data_2017a) == "studyid")] <- "SID"

#lab.date
colnames(VL_data_2014a)[(names(VL_data_2014a) == "Viral.Load.Visit_date")] <- "VL.date"
colnames(VL_data_2014b)[(names(VL_data_2014b) == "visit_date")] <- "VL.date"
colnames(VL_data_2015)[(names(VL_data_2015) == "LAB.REPORT.DATE")] <- "VL.date"
colnames(VL_data_2017a)[(names(VL_data_2017a) == "visit_date")] <- "VL.date"
colnames(VL_data_2017b)[(names(VL_data_2017b) == "Lab.result.Date")] <- "VL.date"

#Viral laod
colnames(VL_data_2014a)[(names(VL_data_2014a) == "Viral.Load")] <- "Viral.Load"
colnames(VL_data_2014b)[(names(VL_data_2014b) == "viral_load")] <- "Viral.Load"
colnames(VL_data_2015)[(names(VL_data_2015) == "VIRAL.LOAD.COPIES")] <- "Viral.Load"
colnames(VL_data_2017a)[(names(VL_data_2017a) == "Viral.Load")] <- "Viral.Load"
colnames(VL_data_2017b)[(names(VL_data_2017b) == "Viral.Load.Copies")] <- "Viral.Load"

# Get dates all in the same format
VL_data_2014a$VL.date.correct=as.Date(VL_data_2014a$VL.date,"%d%B%Y")
VL_data_2014b$VL.date.correct=as.Date(VL_data_2014b$VL.date,"%d%B%Y")
VL_data_2015$VL.date.correct=as.Date(VL_data_2015$VL.date,"%Y-%m-%d")#
VL_data_2017a$VL.date.correct=as.Date(VL_data_2017a$VL.date,"%Y-%m-%d")#
VL_data_2017b$VL.date.correct=as.Date(VL_data_2017b$VL.date,"%Y-%m-%d")

#Merge relevant portions of the dataframes using row concatenation
d1<-rbind(x=VL_data_2014a[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2014b[,c('SID','VL.date.correct','Viral.Load')]) 
d2<-rbind(x=d1[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2015[,c('SID','VL.date.correct','Viral.Load')]) 
d3<-rbind(x=d2[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2017a[,c('SID','VL.date.correct','Viral.Load')]) 
d4<-rbind(x=d3[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2017b[,c('SID','VL.date.correct','Viral.Load')]) 
d5=unique(d4) #remove duplicate rows
d5=na.omit(d5) #remove NAs
table(d5$SID) # How many VL measurements for each subject? btw 1 and 25...
length(unique(d5$SID)) #451 - seems like too many subjects? 

# Use Martha's demog spreadsheet for ages/scan dates etc - note the 4 year excel fix, always be aware when data comes from a Mac!
d6=demog_data[,c('ID','PID','Birth.date',"Scan.date.5yrs","Scan.date.7yrs","Scan.date.9yrs.Skyra",'status')]
d6$Birth.date = fix_excel_dates(as.Date(d6$Birth.date,"%B %d, %Y"))
d6$Scan.date.5yrs = fix_excel_dates(as.Date(d6$Scan.date.5yrs,"%B %d, %Y"))
d6$Scan.date.7yrs = fix_excel_dates(as.Date(d6$Scan.date.7yrs,"%B %d, %Y"))
d6$Scan.date.9yrs.Skyra = fix_excel_dates(as.Date(d6$Scan.date.9yrs.Skyra,"%B %d, %Y"))

count(demog_data$status) # 85 HIV children
cd4_data_all_HIV<-merge(x=d5, y=d6, by.x ="SID",by.y ="PID")
cd4_data_all_HIV=cd4_data_all_HIV[cd4_data_all_HIV$status=="HIV",] #I have some CD4 data for 81 of 85 subjects, but nothing early for 17 of them
cd4_data_all_HIV <- cd4_data_all_HIV[order(cd4_data_all_HIV$SID, cd4_data_all_HIV$VL.date.correct,decreasing=FALSE),]
table(cd4_data_all_HIV$ID)
length(unique(cd4_data_all_HIV$SID)) #85 
cd4_data_all_HIV$suppressed=cd4_data_all_HIV$Viral.Load<400
cd4_data_all_HIV_suppressed=cd4_data_all_HIV[cd4_data_all_HIV$Viral.Load<400,]
cd4_data_all_HIV_suppressed<-cd4_data_all_HIV_suppressed[!duplicated(cd4_data_all_HIV_suppressed$SID), ]
cd4_data_all_HIV_suppressed$age_at_VL_suppression=difftime(cd4_data_all_HIV_suppressed$VL.date.correct,cd4_data_all_HIV_suppressed$Birth.date,units = 'days')/30
num_VL_measures=count(cd4_data_all_HIV, vars = "ID")
cd4_data_all_HIV_suppressed=merge(cd4_data_all_HIV_suppressed,num_VL_measures,by="ID")

tt=ddply(cd4_data_all_HIV, c("SID","suppressed"), summarise, 
         date_suppressed = min(VL.date.correct))

new1=tt[tt$suppressed=="TRUE",]
new1$first_suppressed_date=tt$date_suppressed[tt$suppressed=="TRUE"]

new2=tt[tt$suppressed=="FALSE",]
new2$first_VL_date=tt$date_suppressed[tt$suppressed=="FALSE"]

test=merge(new1,new2,by="SID",all="TRUE")
test=merge(test,d6,by.x="SID",by.y="PID")

# Now have cd4_data_all_HIV to work with

# Ken's
# This is a spreadsheet that Ken made with nadir CD4, VL (and lots of other stuff) at age 5. Originals not known/unavailable
VL_data_2013=read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2013/FINAL_full_clinical_data_20131101.xlsx",skip=1,header=TRUE,sep=",")
VL_data_2013 <- VL_data_2013[,colSums(is.na(VL_data_2013))<nrow(VL_data_2013)]
#Filter(function(cd4_data_2013)!all(is.na(cd4_data_2013)), df)
VL_data_2013<-head(VL_data_2013,-3)
VL_data_2013=VL_data_2013[!is.na(VL_data_2013$age_at_PVL_supp_.wks.),] #remove NAs
VL_data_2013$PVL_suppressed_date=as.Date(VL_data_2013$PVL_suppressed_date,"%Y-%m-%d")#52

#Children who Ken had calculated a PVL suppression for at 5 years
ken_ids<-VL_data_2013$PID[!is.na(VL_data_2013$age_at_PVL_supp_.wks.)] # 52 unique children, 3 I don't have, 6 I don't have early data, 

test_ken=merge(test,VL_data_2013,by.x='SID',by.y='PID') #
difftime(test_ken$first_suppressed_date,test_ken$PVL_suppressed_date,unit="days")
