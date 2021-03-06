library(gdata)
library(plyr)

source('~/Dropbox/HIV/CD4_data/fix_excel_dates.R')

demog = read.xls ("/Users/frances/Dropbox/HIV/demographics-scan-dates-hiv-controls-master-excel-Jan-2017.xlsx",sep=",")

# These are all the spreadsheets I know of that contain CD4 data
cd4_data_2017 = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2017/CD4_CD8_VL_20170131_final.xlsx",header=TRUE,sep=",")
cd4_data_2017b = read.csv("/Users/Frances/Dropbox/HIV/CD4_Per_CD4_Count.csv",header=TRUE,sep=",")
cd4_data_2017c = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2017/R01_Reservoir_CD4_VL_20170626_Final.xlsx",header=TRUE,sep=",")
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
rm(cd4_data_2014a_1,cd4_data_2014a_2,cd4_data_2014a_3,cd4_data_2014a_4)

# Give corresponding cols the same name
# SID
colnames(cd4_data_2014a)[(names(cd4_data_2014a) == "studyid")] <- "SID"
colnames(cd4_data_2014b)[(names(cd4_data_2014b) == "Subject.ID")] <- "SID"
colnames(cd4_data_2017b)[(names(cd4_data_2017b) == "studyid")] <- "SID"

#lab.date
colnames(cd4_data_2017)[(names(cd4_data_2017) == "Lab.result.Date")] <- "lab.date"
colnames(cd4_data_2017c)[(names(cd4_data_2017c) == "Lab.Result.Date")] <- "lab.date"
colnames(cd4_data_2015)[(names(cd4_data_2015) == "LAB.REPORT.DATE")] <- "lab.date"
colnames(cd4_data_2014a)[(names(cd4_data_2014a) == "Visit_Date")] <- "lab.date"
colnames(cd4_data_2014b)[(names(cd4_data_2014b) == "Lab.Result.Date")] <- "lab.date"
colnames(cd4_data_2017b)[(names(cd4_data_2017b) == "visit_date")] <- "lab.date"

#CD4.count
colnames(cd4_data_2017)[(names(cd4_data_2017) == "CD4.absolute")] <- "CD4.count"
colnames(cd4_data_2017c)[(names(cd4_data_2017c) == "CD4.Absolute")] <- "CD4.count"
colnames(cd4_data_2015)[(names(cd4_data_2015) == "CD4.ABS.COUNT")] <- "CD4.count"
colnames(cd4_data_2014b)[(names(cd4_data_2014b) == "CD4.abs.count")] <- "CD4.count"
colnames(cd4_data_2017b)[(names(cd4_data_2017b) == "cd4_counts")] <- "CD4.count"

#CD4.percent
colnames(cd4_data_2017)[(names(cd4_data_2017) == "CD4.percentage")] <- "CD4.percent"
colnames(cd4_data_2017c)[(names(cd4_data_2017c) == "CD4.percentage")] <- "CD4.percent"
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
cd4_data_2017c$lab.date.correct=as.Date(cd4_data_2017c$lab.date,"%Y-%m-%d")

#Merge relevant portions of the CD4 dataframes using row concatenation
d1<-rbind(x=cd4_data_2017[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4_data_2015[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d2<-rbind(x=d1[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4_data_2014a[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d3<-rbind(x=d2[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4_data_2014b[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d4<-rbind(x=d3[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4_data_2017b[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d5<-rbind(x=d4[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4_data_2017c[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
rm(cd4_data_2017,cd4_data_2017b,cd4_data_2017c,cd4_data_2014a,cd4_data_2014b,cd4_data_2015)
rm(d1,d2,d3,d4)

d5=unique(d5) #remove duplicate rows
d5=d5[!is.na(d5$CD4.count),] #remove NAs but only CD4 cos don't care if there was no CD8
table(d5$SID) # How many CD4 measurements for each subject?
length(unique(d5$SID)) #178 - seems like too many subjects? Because there are CD4 measures for some controls in 2015/2016!

# Use Martha's demog spreadsheet for ages/scan dates etc - note the 4 year excel fix, always be aware when data comes from a Mac!
demog=demog[,c('ID','PID','Birth.date',"Scan.date.5yrs","Scan.date.7yrs","Scan.date.9yrs.Skyra",'status','cd4.count.enrol','cd4.percent.enrol')]
demog$Birth.date = fix_excel_dates(as.Date(demog$Birth.date,"%B %d, %Y"))
demog$Scan.date.5yrs = fix_excel_dates(as.Date(demog$Scan.date.5yrs,"%B %d, %Y"))
demog$Scan.date.7yrs = fix_excel_dates(as.Date(demog$Scan.date.7yrs,"%B %d, %Y"))
demog$Scan.date.9yrs.Skyra = fix_excel_dates(as.Date(demog$Scan.date.9yrs.Skyra,"%B %d, %Y"))

# Check whether CD4 avail for all listed HIV+ children
count(demog$status) # 85 HIV children
cd4_data_all_HIV<-merge(x=d5, y=demog, by.x ="SID",by.y ="PID") #all CD counts
cd4_data_all_HIV=cd4_data_all_HIV[cd4_data_all_HIV$status=="HIV",] # get only HIV+ children
cd4_data_all_HIV <- cd4_data_all_HIV[order(cd4_data_all_HIV$SID, cd4_data_all_HIV$lab.date.correct,decreasing=FALSE),]
table(cd4_data_all_HIV$SID)
length(unique(cd4_data_all_HIV$SID)) #I have some CD4 data for 85 of 85 

# Now have cd4_data_all_HIV to work with

cd4_data_all_HIV$lab.days.from.5yr.scan<- difftime(cd4_data_all_HIV$Scan.date.5yrs,cd4_data_all_HIV$lab.date.correct, units = c("days"))
cd4_data_all_HIV$lab.days.from.7yr.scan<- difftime(cd4_data_all_HIV$Scan.date.7yrs,cd4_data_all_HIV$lab.date.correct, units = c("days"))
cd4_data_all_HIV$lab.days.from.9yr.Skyra.scan<- difftime(cd4_data_all_HIV$Scan.date.9yrs.Skyra,cd4_data_all_HIV$lab.date.correct, units = c("days"))

########################################################################
# For CD4 nadirs and CD4s closest to each scan date
# Find earliest and most recent lab dates for all HIV+ children and calculate child's age at these dates
nadir=ddply(cd4_data_all_HIV, c("ID"), summarise, 
          earliest = min(lab.date.correct), 
          latest = max(lab.date.correct), 
          nadir.CD4=min(CD4.count), 
          nadir.CD4_percent=min(CD4.percent), 
          nadir.CD4.date=lab.date.correct[which.min(CD4.count)], 
          nadir.CD4.percent.date=lab.date.correct[which.min(CD4.percent)])

closest_to_5yr_scan=ddply(cd4_data_all_HIV, c("ID"), summarize, 
          CD4.lab.days.from.5yr.scan=lab.days.from.5yr.scan[which.min(abs(lab.days.from.5yr.scan))],
          CD4.count.5yr.scan=CD4.count[which.min(abs(lab.days.from.5yr.scan))],
          CD4.percent.5yr.scan=CD4.percent[which.min(abs(lab.days.from.5yr.scan))],
          CD4.lab.date.closest.5yr.scan=lab.date.correct[which.min(abs(lab.days.from.5yr.scan))])

closest_to_7yr_scan=ddply(cd4_data_all_HIV, c("ID"), summarize,          
          CD4.lab.days.from.7yr.scan=lab.days.from.7yr.scan[which.min(abs(lab.days.from.7yr.scan))],
          CD4.count.7yr.scan=CD4.count[which.min(abs(lab.days.from.7yr.scan))],
          CD4.percent.7yr.scan=CD4.percent[which.min(abs(lab.days.from.7yr.scan))],
          CD4.lab.date.closest.7yr.scan=lab.date.correct[which.min(abs(lab.days.from.7yr.scan))])

closest_to_9yr.Skyra_scan=ddply(cd4_data_all_HIV, c("ID"), summarize,          
          CD4.lab.days.from.9yr.Skyra.scan=lab.days.from.9yr.Skyra.scan[which.min(abs(lab.days.from.9yr.Skyra.scan))],
          CD4.count.9yr.Skyra.scan=CD4.count[which.min(abs(lab.days.from.9yr.Skyra.scan))],
          CD4.percent.9yr.Skyra.scan=CD4.percent[which.min(abs(lab.days.from.9yr.Skyra.scan))],
          CD4.lab.date.closest.9yr.Skyra.scan=lab.date.correct[which.min(abs(lab.days.from.9yr.Skyra.scan))])

# Alternative is this
#closest_to_5yr_scan=ddply(cd4_data_all_HIV, ~ID, subset, lab.days.from.5yr.scan==min(abs(lab.days.from.5yr.scan)),
#      select=c('CD4.count', 'CD4.percent','lab.date.correct','lab.days.from.5yr.scan'))

test=merge(demog,nadir,all = TRUE)
test$wks_at_first_lab=difftime(test$earliest,as.Date(test$Birth.date,"%B %d, %Y"),units=c("weeks"))
test$wks_at_last_lab=difftime(test$latest,as.Date(test$Birth.date,"%B %d, %Y"),units=c("weeks"))
test$wks_at_nadir_CD4=difftime(test$nadir.CD4.date,as.Date(test$Birth.date,"%B %d, %Y"),units=c("weeks"))
test$wks_at_nadir_CD4_percent=difftime(test$nadir.CD4.percent.date,as.Date(test$Birth.date,"%B %d, %Y"),units=c("weeks"))

test=merge(test,closest_to_5yr_scan,all = TRUE)
test=merge(test,closest_to_7yr_scan,all = TRUE)
test=merge(test,closest_to_9yr.Skyra_scan,all = TRUE)
write.csv(test, "/Users/frances/Dropbox/HIV/spreadsheets/CD4_data.csv")#, sep="\t")

#######################################################################################
# Which ones am I missing data for?
# Identify children who were older than 12 weeks at their first avail CD4 measure
missing_early=test[test$age_at_first_lab>12,c('ID', 'PID')] #none

# Identify children who were more than 6 moths younger at their last avail CD4 measure than their last scan age
age9=test[!is.na(test$Scan.date.9yrs.Skyra),]
missing_age9=age9[difftime(age9$Scan.date.9yrs.Skyra,age9$latest)>180,c('ID','PID')] #153, 183 PHRI
age7=test[!is.na(test$Scan.date.7yrs),]
missing_age7=age7[difftime(age7$Scan.date.7yrs,age7$latest)>180,c('ID','PID')] #none

count(cd4_data_all_HIV$ID)$x[count(cd4_data_all_HIV$ID)$freq<25] #only 2 less than 20 though
# These all less than 25 visit dates: 121 (13 visits) 139 194 (19 visits) 199 214 215 216

need_data=rbind(missing_early,missing_age9) # 153, 183, PHRI
########################################################################
#source(compare_nadirCD4_Ken.R) # Check Ken's nadir CD4 against mine to get ones that disagree
#need_data=rbind(need_data,disagree)
#need_data=unique(need_data)
## write out ones that I need CD4 data for
#write.table(need_data, "/Users/frances/Dropbox/HIV/IDs_needing_CD4_history.csv", sep="\t")

#
# Older way of getting at scan values
########################################################################
# Check CD4 and CD8 closest to scan date for 5 year olds

# CD8
# Merge relevant portions of the CD8 dataframes using row concatenation
c1<-rbind(x=cd4_data_2017[,c('SID','lab.date.correct','CD8.count','CD8.percent')], y=cd4_data_2014a[,c('SID','lab.date.correct','CD8.count','CD8.percent')]) 
c1=unique(c1) #remove duplicate rows
c1=na.omit(c1)

cd8_data_all_HIV<-merge(x=c1, y=demog, by.x ="SID",by.y ="PID")
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
# actually should not do this and should do calculations for all ages then there would be nas where there was no scan
age7<-age7[!duplicated(age7$SID), c("ID","SID","CD4.count","CD4.percent", "lab.date.correct","Scan.date.7yrs","lab.days.from.scan") ]
age7_before<-age7_before[!duplicated(age7_before$SID), c("SID","CD4.count","CD4.percent", "lab.date.correct","lab.days.from.scan") ]
colnames(age7_before)[(names(age7_before) == "lab.date.correct")] <- "lab.date.prescan"
colnames(age7_before)[(names(age7_before) == "lab.days.from.scan")] <- "lab.days.before.scan"
age7_all=merge(age7,age7_before,by="SID",all.x = TRUE)
write.table(age7_all, "/Users/frances/Dropbox/HIV/spreadsheets/lab_closest_to_age7_scan.csv", sep="\t")

# Check CD4 closest to scan date for 9 year olds##########
age9=cd4_data_all_HIV[!is.na(cd4_data_all_HIV$Scan.date.9yrs.Skyra),]
age9$lab.days.from.scan<- difftime(age9$Scan.date.9yrs.Skyra,age9$lab.date.correct, units = c("days"))
# sort ascending
age9_before <- age9[age9$lab.days.from.scan>=0,]#leave this out for closest dtae to scan which can be afterwards
age9_before <- age9_before[order(age9_before$SID, abs(age9_before$lab.days.from.scan), decreasing=FALSE),]
age9 <- age9[order(age9$SID, abs(age9$lab.days.from.scan), decreasing=FALSE),]

# Exclude duplicates, keeping only min lab.days.from.scan
age9<-age9[!duplicated(age9$SID), c("ID","SID","CD4.count","CD4.percent", "lab.date.correct","Scan.date.9yrs.Skyra","lab.days.from.scan") ]
age9_before<-age9_before[!duplicated(age9_before$SID), c("SID","CD4.count","CD4.percent", "lab.date.correct","lab.days.from.scan") ]
colnames(age9_before)[(names(age9_before) == "lab.date.correct")] <- "lab.date.prescan"
colnames(age9_before)[(names(age9_before) == "lab.days.from.scan")] <- "lab.days.before.scan"
age9_all=merge(age9,age9_before,by="SID",all.x = TRUE)
write.table(age9_all, "/Users/frances/Dropbox/HIV/spreadsheets/lab_closest_to_age9_scan.csv", sep="\t")
