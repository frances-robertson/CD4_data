library(gdata)
library(plyr)
library(data.table)

source('~/Dropbox/HIV/CD4/fix_excel_dates.R')

demog_data = read.xls ("/Users/frances/Dropbox/HIV/demographics-scan-dates-hiv-controls-master-excel-Jan-2017.xlsx",sep=",")

# These are all the spreadsheets I know of that contain VL data
VL_data_2017a = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2017/baselinedata02022017.xls",header=TRUE,sep=",")
VL_data_2017b = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2017/CD4_CD8_VL_20170131_final.xlsx",header=TRUE,sep=",")
VL_data_2017c = read.csv("/Users/Frances/Dropbox/HIV/viral_load.csv",header=TRUE,sep=",")
VL_data_2017d = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2017/R01_Reservoir_CD4_VL_20170626_Final.xlsx",header=TRUE,sep=",")
VL_data_2015 = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2015/Copy of R01_Neuro_7yr_Demographic_Clinical_20150820_rawdata.xlsx",header=TRUE,sep=",")
VL_data_2014a=read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2014/Viral Load.xlsx",header=TRUE,sep=",")
VL_data_2014b=read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2014/Baseline Viral Loads.xls",header=TRUE,sep=",")

#tester<-rbind(x=VL_data_2017b[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2017d[,c('SID','VL.date.correct','Viral.Load')]) 
#tester=unique(tester)

# Give corresponding cols the same name
# SID
colnames(VL_data_2014a)[(names(VL_data_2014a) == "studyid")] <- "SID"
colnames(VL_data_2014b)[(names(VL_data_2014b) == "StudyID")] <- "SID"
colnames(VL_data_2017a)[(names(VL_data_2017a) == "studyid")] <- "SID"
colnames(VL_data_2017c)[(names(VL_data_2017c) == "studyid")] <- "SID"

#lab.date
colnames(VL_data_2014a)[(names(VL_data_2014a) == "Viral.Load.Visit_date")] <- "VL.date"
colnames(VL_data_2014b)[(names(VL_data_2014b) == "visit_date")] <- "VL.date"
colnames(VL_data_2015)[(names(VL_data_2015) == "LAB.REPORT.DATE")] <- "VL.date"
colnames(VL_data_2017a)[(names(VL_data_2017a) == "visit_date")] <- "VL.date"
colnames(VL_data_2017b)[(names(VL_data_2017b) == "Lab.result.Date")] <- "VL.date"
colnames(VL_data_2017c)[(names(VL_data_2017c) == "visit_date")] <- "VL.date"
colnames(VL_data_2017d)[(names(VL_data_2017d) == "Lab.Result.Date")] <- "VL.date"

#Viral laod
colnames(VL_data_2014a)[(names(VL_data_2014a) == "Viral.Load")] <- "Viral.Load"
colnames(VL_data_2014b)[(names(VL_data_2014b) == "viral_load")] <- "Viral.Load"
colnames(VL_data_2015)[(names(VL_data_2015) == "VIRAL.LOAD.COPIES")] <- "Viral.Load"
colnames(VL_data_2017a)[(names(VL_data_2017a) == "viral_load")] <- "Viral.Load"
colnames(VL_data_2017b)[(names(VL_data_2017b) == "Viral.Load.Copies")] <- "Viral.Load"
colnames(VL_data_2017c)[(names(VL_data_2017c) == "viral_load")] <- "Viral.Load"
colnames(VL_data_2017d)[(names(VL_data_2017d) == "Viral.Load.Copies")] <- "Viral.Load"

# Get dates all in the same format
VL_data_2014a$VL.date.correct=as.Date(VL_data_2014a$VL.date,"%d%B%Y")
VL_data_2014b$VL.date.correct=as.Date(VL_data_2014b$VL.date,"%d%B%Y")
VL_data_2015$VL.date.correct=as.Date(VL_data_2015$VL.date,"%Y-%m-%d")#
VL_data_2017a$VL.date.correct=as.Date(VL_data_2017a$VL.date,"%Y-%m-%d")#
VL_data_2017b$VL.date.correct=as.Date(VL_data_2017b$VL.date,"%Y-%m-%d")
VL_data_2017c$VL.date.correct=as.Date(VL_data_2017c$VL.date,"%d-%B-%y")
VL_data_2017d$VL.date.correct=as.Date(VL_data_2017d$VL.date,"%Y-%m-%d")

#Merge relevant portions of the dataframes using row concatenation

d1<-rbind(x=VL_data_2014a[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2014b[,c('SID','VL.date.correct','Viral.Load')]) 
d1=na.omit(d1)
d2<-rbind(x=d1[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2015[,c('SID','VL.date.correct','Viral.Load')]) 
d2=na.omit(d2)
d3<-rbind(x=d2[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2017a[,c('SID','VL.date.correct','Viral.Load')]) 
d3=na.omit(d3)
d4<-rbind(x=d3[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2017b[,c('SID','VL.date.correct','Viral.Load')]) 
d4=na.omit(d4)
d5<-rbind(x=d4[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2017c[,c('SID','VL.date.correct','Viral.Load')]) 
d5=na.omit(d5)
d6<-rbind(x=d5[,c('SID','VL.date.correct','Viral.Load')], y=VL_data_2017d[,c('SID','VL.date.correct','Viral.Load')]) 
d6=na.omit(d6)
d6=unique(d6) #remove duplicate rows
d6=na.omit(d6) #remove NAs
table(d6$SID) # How many VL measurements for each subject? btw 1 and 25...
length(unique(d6$SID)) #451 - seems like too many subjects? 

# Use Martha's demog spreadsheet for ages/scan dates etc - note the 4 year excel fix, always be aware when data comes from a Mac!
demog=demog_data[,c('ID','PID','Birth.date',"Scan.date.5yrs","Scan.date.7yrs","Scan.date.9yrs.Skyra",'status')]
demog$Birth.date = fix_excel_dates(as.Date(demog$Birth.date,"%B %d, %Y"))
demog$Scan.date.5yrs = fix_excel_dates(as.Date(demog$Scan.date.5yrs,"%B %d, %Y"))
demog$Scan.date.7yrs = fix_excel_dates(as.Date(demog$Scan.date.7yrs,"%B %d, %Y"))
demog$Scan.date.9yrs.Skyra = fix_excel_dates(as.Date(demog$Scan.date.9yrs.Skyra,"%B %d, %Y"))

count(demog_data$status) # 85 HIV children
VL_data_all_HIV<-merge(x=d6, y=demog, by.x ="SID",by.y ="PID")
VL_data_all_HIV=VL_data_all_HIV[VL_data_all_HIV$status=="HIV",] #I have some CD4 data for 81 of 85 subjects, but nothing early for 17 of them
VL_data_all_HIV <- VL_data_all_HIV[order(VL_data_all_HIV$SID, VL_data_all_HIV$VL.date.correct,decreasing=FALSE),]
table(VL_data_all_HIV$ID)
length(unique(VL_data_all_HIV$SID)) #85 
VL_data_all_HIV$suppressed=VL_data_all_HIV$Viral.Load<400 #true of false

tt=ddply(VL_data_all_HIV, c("SID","suppressed"), summarise, 
         date_suppressed = min(VL.date.correct))

new1=tt[tt$suppressed=="TRUE",]
new1$first_suppressed_date=tt$date_suppressed[tt$suppressed=="TRUE"]

new2=tt[tt$suppressed=="FALSE",]
new2$first_VL_date=tt$date_suppressed[tt$suppressed=="FALSE"]

test=merge(new1,new2,by="SID",all="TRUE")
test=merge(test,demog,by.x="SID",by.y="PID")


VL_data_all_HIV_suppressed=VL_data_all_HIV[VL_data_all_HIV$Viral.Load<400,]
VL_data_all_HIV_suppressed<-VL_data_all_HIV_suppressed[!duplicated(VL_data_all_HIV_suppressed$SID), ]
VL_data_all_HIV_suppressed$age_at_VL_suppression.wks=difftime(VL_data_all_HIV_suppressed$VL.date.correct,VL_data_all_HIV_suppressed$Birth.date,units = 'weeks')
num_VL_measures=count(VL_data_all_HIV, vars = "ID")
VL_data_all_HIV_suppressed=merge(VL_data_all_HIV_suppressed,num_VL_measures,by="ID")
colnames(VL_data_all_HIV_suppressed)[(names(VL_data_all_HIV_suppressed) == "freq")] <- "num_VL_measures"
colnames(VL_data_all_HIV_suppressed)[(names(VL_data_all_HIV_suppressed) == "VL.date.correct")] <- "VL.date"

IDs=VL_data_all_HIV_suppressed$ID[VL_data_all_HIV_suppressed$num_VL_measures<19]
#104  116  121  124  134  137  139  154  165  166  178  194  202  214  216  95   98   PHRI
VL_data_all_HIV_suppressed<-VL_data_all_HIV_suppressed[VL_data_all_HIV_suppressed$num_VL_measures>20,]
write.table(VL_data_all_HIV_suppressed, "/Users/frances/Dropbox/HIV/viral_suppression_latest.csv", sep="\t")

setDT(VL_data_all_HIV) #if df is already a data frame
setkey(VL_data_all_HIV,ID)
VL_data_all_HIV[,diff:=c(NA,diff(VL.date.correct)),by="ID"] 
setDF(VL_data_all_HIV)
# Now have VL_data_all_HIV to work with

########################################################################
# Compare with Ken's - there are 4 that don't match, think Ken swapped some dates in error
# This is a spreadsheet that Ken made with nadir CD4, VL (and lots of other stuff) at age 5. Originals not known/unavailable
VL_data_2013=read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2013/FINAL_full_clinical_data_20131101.xlsx",skip=1,header=TRUE,sep=",")
VL_data_2013 <- VL_data_2013[,colSums(is.na(VL_data_2013))<nrow(VL_data_2013)]
#Filter(function(cd4_data_2013)!all(is.na(cd4_data_2013)), df)
VL_data_2013<-head(VL_data_2013,-3)
VL_data_2013=VL_data_2013[!is.na(VL_data_2013$age_at_PVL_supp_.wks.),] #remove NAs
VL_data_2013$PVL_suppressed_date=as.Date(VL_data_2013$PVL_suppressed_date,"%Y-%m-%d")#52

#Children who Ken had calculated a PVL suppression for at 5 years
ken_ids<-test_ken$ID.x[!is.na(test_ken$age_at_PVL_supp_.wks.)] # 52 unique children, 3 I don't have, 6 I don't have early data, 

test_ken=merge(test,VL_data_2013,by.x='SID',by.y='PID') #
difftime(test_ken$first_suppressed_date,test_ken$PVL_suppressed_date,units="days")

####################################################################################
# Viral load at age 5units = 
age5_VL=VL_data_all_HIV[!is.na(VL_data_all_HIV$Scan.date.5yrs),]
age5_VL$VL.days.from.scan<- difftime(age5_VL$Scan.date.5yrs,age5_VL$VL.date.correct, units = c("days"))
# sort ascending
age5_VL <- age5_VL[age5_VL$VL.days.from.scan>=0,]#leave this out for closest dtae to scan which can be afterwards
age5_VL <- age5_VL[order(age5_VL$SID, abs(age5_VL$VL.days.from.scan), decreasing=FALSE),]
# Exclude duplicates, keeping only min lab.days.from.scan
age5_VL<-age5_VL[!duplicated(age5_VL$SID), c("ID","SID","Viral.Load", "VL.date.correct","Scan.date.5yrs","VL.days.from.scan") ]
write.table(age5_VL, "/Users/frances/Dropbox/HIV/VL_closest_age5_scan.csv", sep="\t")
write.table(age5_VL, "/Users/frances/Dropbox/HIV/VL_before_age5_scan.csv", sep="\t")

