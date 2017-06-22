library(gdata)
library(plyr)

source('~/Dropbox/HIV/CD4/fix_excel_dates.R')

demog_data = read.xls ("/Users/frances/Dropbox/HIV/demographics-scan-dates-hiv-controls-master-excel-Jan-2017.xlsx",sep=",")

# These are all the spreadsheets I know of that contain CD4 data
cd4_data_2017 = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2017/CD4_CD8_VL_20170131_final.xlsx",header=TRUE,sep=",")
cd4_data_2017b = read.csv("/Users/Frances/Dropbox/HIV/CD4_Per_CD4_Count.csv",header=TRUE,sep=",")
cd4_data_2015 = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2015/Copy of R01_Neuro_7yr_Demographic_Clinical_20150820_rawdata.xlsx",header=TRUE,sep=",")
cd4_data_2014a = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2014/CD4_CD8_CD4Percent_CD8Percent.xlsx",header=TRUE,sep=",")
cd4_data_2014b=read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2014/Neuroimaging_LabResults_20141204_final.xlsx",header=TRUE,sep=",")

# 2014a needs sorting as all parameters CD4 etc are in one column - cludgy but this seems to work
cd4_data_2014a_1=cd4_data_2014a[cd4_data_2014a$Parameter_Description=='CD4 Count',];
cd4_data_2014a_1=cd4_data_2014a_1[,-which(names(cd4_data_2014a_1) == 'Parameter_Description')]
colnames(cd4_data_2014a_1)[(names(cd4_data_2014a_1) == "Parameter_Descriptionss")] <- "CD4.count"

cd4_data_2014a_2=cd4_data_2014a[cd4_data_2014a$Parameter_Description == 'CD4%',];
cd4_data_2014a_2=cd4_data_2014a_2[,-which(names(cd4_data_2014a_2) == 'Parameter_Description')];
colnames(cd4_data_2014a_2)[(names(cd4_data_2014a_2) == "Parameter_Descriptionss")] <- "CD4.percent"

cd4_data_2014a_3=cd4_data_2014a[cd4_data_2014a$Parameter_Description=='CD8 Count',];
cd4_data_2014a_3=cd4_data_2014a_3[,-which(names(cd4_data_2014a_3) == 'Parameter_Description')]
colnames(cd4_data_2014a_3)[(names(cd4_data_2014a_3) == "Parameter_Descriptionss")] <- "CD8.count"

cd4_data_2014a_4=cd4_data_2014a[cd4_data_2014a$Parameter_Description == 'CD8%',];
cd4_data_2014a_4=cd4_data_2014a_4[,-which(names(cd4_data_2014a_4) == 'Parameter_Description')];
colnames(cd4_data_2014a_4)[(names(cd4_data_2014a_4) == "Parameter_Descriptionss")] <- "CD8.percent"

cd4_data_2014a=merge(cd4_data_2014a_1,cd4_data_2014a_2,all=TRUE)
cd4_data_2014a=merge(cd4_data_2014a,cd4_data_2014a_3,all=TRUE)
cd4_data_2014a=merge(cd4_data_2014a,cd4_data_2014a_4,all=TRUE)

# Give corresponding cols the same name
# SID
colnames(cd4_data_2014a)[(names(cd4_data_2014a) == "studyid")] <- "SID"
colnames(cd4_data_2014b)[(names(cd4_data_2014b) == "Subject.ID")] <- "SID"
colnames(cd4_data_2017b)[(names(cd4_data_2017b) == "studyid")] <- "SID"
#lab.date
colnames(cd4_data_2017)[(names(cd4_data_2017) == "Lab.result.Date")] <- "lab.date"
colnames(cd4_data_2015)[(names(cd4_data_2015) == "LAB.REPORT.DATE")] <- "lab.date"
colnames(cd4_data_2014a)[(names(cd4_data_2014a) == "Visit_Date")] <- "lab.date"
colnames(cd4_data_2014b)[(names(cd4_data_2014b) == "Lab.Result.Date")] <- "lab.date"
colnames(cd4_data_2017b)[(names(cd4_data_2017b) == "visit_date")] <- "lab.date"
#CD4.count
colnames(cd4_data_2017)[(names(cd4_data_2017) == "CD4.absolute")] <- "CD4.count"
colnames(cd4_data_2015)[(names(cd4_data_2015) == "CD4.ABS.COUNT")] <- "CD4.count"
colnames(cd4_data_2014b)[(names(cd4_data_2014b) == "CD4.abs.count")] <- "CD4.count"
colnames(cd4_data_2017b)[(names(cd4_data_2017b) == "cd4_counts")] <- "CD4.count"
#CD4.percent
colnames(cd4_data_2017)[(names(cd4_data_2017) == "CD4.percentage")] <- "CD4.percent"
colnames(cd4_data_2015)[(names(cd4_data_2015) == "CD4.PERCENTAGE")] <- "CD4.percent"
colnames(cd4_data_2014b)[(names(cd4_data_2014b) == "CD4.Percentage")] <- "CD4.percent"
colnames(cd4_data_2017b)[(names(cd4_data_2017b) == "cd4_per")] <- "CD4.percent"

#CD8.count
colnames(cd4_data_2017)[(names(cd4_data_2017) == "CD8.absolute")] <- "CD8.count"
#CD8.percent
colnames(cd4_data_2017)[(names(cd4_data_2017) == "CD8.percentage")] <- "CD8.percent"

# Get dates all in the same format
cd4_data_2014a$lab.date.correct=as.Date(cd4_data_2014a$lab.date,"%d%B%Y")
cd4_data_2014b$lab.date.correct=as.Date(cd4_data_2014b$lab.date,"%Y-%m-%d")#
cd4_data_2015$lab.date.correct=as.Date(cd4_data_2015$lab.date,"%Y-%m-%d")#
cd4_data_2017$lab.date.correct=as.Date(cd4_data_2017$lab.date,"%Y-%m-%d")
cd4_data_2017b$lab.date.correct=as.Date(cd4_data_2017b$lab.date,"%d-%B-%y")

#Merge relevant portions of the CD4 dataframes using row concatenation
d1<-rbind(x=cd4_data_2017[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4_data_2015[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d2<-rbind(x=d1[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4_data_2014a[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d3<-rbind(x=d2[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4_data_2014b[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d4<-rbind(x=d3[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4_data_2017b[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d4=unique(d4) #remove duplicate rows
d4=d4[!is.na(d4$CD4.count),] #remove NAs
table(d4$SID) # How many CD4 measurements for each subject?
length(unique(d4$SID)) #131 - seems like too many subjects? Because there are CD4 measures for some controls in 2015/2016!

# Use Martha's demog spreadsheet for ages/scan dates etc - note the 4 year excel fix, always be aware when data comes from a Mac!
d5=demog_data[,c('ID','PID','Birth.date',"Scan.date.5yrs","Scan.date.7yrs","Scan.date.9yrs.Skyra",'status')]
d5$Birth.date = fix_excel_dates(as.Date(d5$Birth.date,"%B %d, %Y"))
d5$Scan.date.5yrs = fix_excel_dates(as.Date(d5$Scan.date.5yrs,"%B %d, %Y"))
d5$Scan.date.7yrs = fix_excel_dates(as.Date(d5$Scan.date.7yrs,"%B %d, %Y"))
d5$Scan.date.9yrs.Skyra = fix_excel_dates(as.Date(d5$Scan.date.9yrs.Skyra,"%B %d, %Y"))

# Check whether CD4 avail for all listed HIV+ children
count(demog_data$status) # 85 HIV children
cd4_data_all_HIV<-merge(x=d4, y=d5, by.x ="SID",by.y ="PID")
cd4_data_all_HIV=cd4_data_all_HIV[cd4_data_all_HIV$status=="HIV",] #I have some CD4 data for 81 of 85 subjects, but nothing early for 17 of them
cd4_data_all_HIV <- cd4_data_all_HIV[order(cd4_data_all_HIV$SID, cd4_data_all_HIV$lab.date.correct,decreasing=FALSE),]
table(cd4_data_all_HIV$SID)
length(unique(cd4_data_all_HIV$SID)) #I have some CD4 data for 85 of 85 
#setdiff(HIV$ID,unique(cd4_data_all_HIV$ID)) #no CD4 values for 121, 139, 154 and PHRI?? First 3 not scanned again after 5 years and Ken has nadirs

# Now have cd4_data_all_HIV to work with

########################################################################
# For CD4 nadirs
# Find earliest and most recent lab dates for all HIV+ children and calculate child's age at these dates
tt=ddply(cd4_data_all_HIV, c("ID"), summarise, 
          earliest = min(lab.date.correct), 
          latest = max(lab.date.correct), 
          nadir_CD4=min(CD4.count), 
          nadir_CD4_percent=min(CD4.percent), 
          nadir_CD4_date=lab.date.correct[which.min(CD4.count)], 
          nadir_CD4percent_date=lab.date.correct[which.min(CD4.percent)])

#ddply(dd, ~group, subset, value==max(value), select=c('date2', 'value')) 
test=merge(tt,d5)
test$age_at_first_lab=difftime(test$earliest,as.Date(test$Birth.date,"%B %d, %Y"))/7
test$age_at_last_lab=difftime(test$latest,as.Date(test$Birth.date,"%B %d, %Y"))/365
test$age_at_nadir_CD4=difftime(test$nadir_CD4_date,as.Date(test$Birth.date,"%B %d, %Y"))/7
test$age_at_nadir_CD4_percent=difftime(test$nadir_CD4percent_date,as.Date(test$Birth.date,"%B %d, %Y"))/7

# Which ones am I missing data for?
# Identify 18 children who were older than 12 weeks at their first avail CD4 measure
missing_early=test[test$age_at_first_lab>12,c('ID', 'PID')] #none

# Identify children who were more than 6 moths younger at their last avail CD4 measure than their last scan age
age9=test[!is.na(test$Scan.date.9yrs.Skyra),]
missing_age9=age9[difftime(age9$Scan.date.9yrs.Skyra,age9$latest)>180,c('ID','PID')] #153, 183 PHRI
age7=test[!is.na(test$Scan.date.7yrs),]
missing_age7=age7[difftime(age7$Scan.date.7yrs,age7$latest)>180,c('ID','PID')] #214 

count(cd4_data_all_HIV$ID)$x[count(cd4_data_all_HIV$ID)$freq<25] #only 3 less than 20 though
#These all less than 25 visit dates: 121 (13 visits) 139 194 (19 visits) 199 214 (19 visits) 215 216 PHRI

need_data=rbind(missing_early,missing_age9) # 153, 183, 214, also add PHRI
########################################################################
# source(compare_nadirCD4_Ken.R) # Check Ken's nadir CD4 against mine to get ones that disagree
need_data=rbind(need_data,disagree)
need_data=unique(need_data)
# write out ones that I need CD4 data for
write.table(need_data, "/Users/frances/Dropbox/HIV/IDs_needing_CD4_history.csv", sep="\t")

########################################################################
# Check CD4 and CD8 closest to scan date for 5 year olds

# CD8
# Merge relevant portions of the CD8 dataframes using row concatenation
c1<-rbind(x=cd4_data_2017[,c('SID','lab.date.correct','CD8.count','CD8.percent')], y=cd4_data_2014a[,c('SID','lab.date.correct','CD8.count','CD8.percent')]) 
c1=unique(c1) #remove duplicate rows
c1=na.omit(c1)

cd8_data_all_HIV<-merge(x=c1, y=d5, by.x ="SID",by.y ="PID")
cd8_data_all_HIV=cd8_data_all_HIV[cd8_data_all_HIV$status=="HIV",]
age5_cd8=cd8_data_all_HIV[!is.na(cd8_data_all_HIV$Scan.date.5yrs),]
age5_cd8$lab.days.from.scan<- difftime(age5_cd8$Scan.date.5yrs,age5_cd8$lab.date.correct, units = c("days"))
age5_cd8 <- age5_cd8[order(age5_cd8$SID, abs(age5_cd8$lab.days.from.scan), decreasing=FALSE),]
age5_cd8<-age5_cd8[!duplicated(age5_cd8$SID), c("ID","SID","CD8.count","CD8.percent", "lab.date.correct","Scan.date.5yrs","lab.days.from.scan") ]
write.table(age5, "/Users/frances/Dropbox/HIV/CD8_lab_before_age5_scan.csv", sep="\t")

# CD4
age5_cd4=cd4_data_all_HIV[!is.na(cd4_data_all_HIV$Scan.date.5yrs),]
age5_cd4$lab.days.from.scan<- difftime(age5_cd4$Scan.date.5yrs,age5_cd4$lab.date.correct, units = c("days"))
# sort ascending
#age5_cd4 <- age5_cd4[age5$lab.days.from.scan>=0,]#leave this out for closest dtae to scan which can be afterwards
age5_cd4 <- age5_cd4[order(age5_cd4$SID, abs(age5_cd4$lab.days.from.scan), decreasing=FALSE),]
# Exclude duplicates, keeping only min lab.days.from.scan
age5_cd4<-age5_cd4[!duplicated(age5_cd4$SID), c("ID","SID","CD4.count","CD4.percent", "lab.date.correct","Scan.date.5yrs","lab.days.from.scan") ]
#write.table(age5, "/Users/frances/Dropbox/HIV/lab_before_age5_scan.csv", sep="\t")
age5=merge(age5_cd4,age5_cd8,by="ID")
write.table(age5, "/Users/frances/Dropbox/HIV/CD4CD8_closest_to_age5_scan.csv", sep="\t")

# Check CD4 closest to scan date for 7 year olds
age7=cd4_data_all_HIV[!is.na(cd4_data_all_HIV$Scan.date.7yrs),]
age7$lab.days.from.scan<- difftime(age7$Scan.date.7yrs,age7$lab.date.correct, units = c("days"))
# sort ascending
age7_before <- age7[age7$lab.days.from.scan>=0,]#leave this out for closest dtae to scan which can be afterwards
age7_before <- age7_before[order(age7_before$SID, abs(age7_before$lab.days.from.scan), decreasing=FALSE),]
age7 <- age7[order(age7$SID, abs(age7$lab.days.from.scan), decreasing=FALSE),]

# Exclude duplicates, keeping only min lab.days.from.scan
age7<-age7[!duplicated(age7$SID), c("ID","SID","CD4.count","CD4.percent", "lab.date.correct","Scan.date.7yrs","lab.days.from.scan") ]
age7_before<-age7_before[!duplicated(age7_before$SID), c("SID","CD4.count","CD4.percent", "lab.date.correct","lab.days.from.scan") ]
colnames(age7_before)[(names(age7_before) == "lab.date.correct")] <- "lab.date.prescan"
colnames(age7_before)[(names(age7_before) == "lab.days.from.scan")] <- "lab.days.before.scan"
age7_all=merge(age7,age7_before,by="SID",all.x = TRUE)
write.table(age7_all, "/Users/frances/Dropbox/HIV/lab_closest_to_age7_scan.csv", sep="\t")

# Check CD4 closest to scan date for 9 year olds