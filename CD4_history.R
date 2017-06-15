library("gdata")
library(plyr)

demog_data = read.xls ("/Users/frances/Dropbox/HIV/demographics-scan-dates-hiv-controls-master-excel-Jan-2017.xlsx",sep=",")

cd4scan_data_2017 = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2017/CD4_CD8_VL_20170131_final.xlsx",header=TRUE,sep=",")
cd4scan_data_2015 = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2015/Copy of R01_Neuro_7yr_Demographic_Clinical_20150820_rawdata.xlsx",header=TRUE,sep=",")
cd4scan_data_2014a = read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2014/CD4_CD8_CD4Percent_CD8Percent.xlsx",header=TRUE,sep=",")
cd4scan_data_2014b=read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2014/Neuroimaging_LabResults_20141204_final.xlsx",header=TRUE,sep=",")
# cd4scan_data_2013=read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2013/FINAL_full_clinical_data_20131101.xlsx",skip=1,header=TRUE,sep=",")

# 2014a needs sorting as all parameters CD4 etc are in one column
cd4scan_data_2014a_1=cd4scan_data_2014a[cd4scan_data_2014a$Parameter_Description=='CD4 Count',];
cd4scan_data_2014a_1=cd4scan_data_2014a_1[,-which(names(cd4scan_data_2014a_1) == 'Parameter_Description')]
colnames(cd4scan_data_2014a_1)[(names(cd4scan_data_2014a_1) == "Parameter_Descriptionss")] <- "CD4.count"

cd4scan_data_2014a_2=cd4scan_data_2014a[cd4scan_data_2014a$Parameter_Description == 'CD4%',];
cd4scan_data_2014a_2=cd4scan_data_2014a_2[,-which(names(cd4scan_data_2014a_2) == 'Parameter_Description')];
colnames(cd4scan_data_2014a_2)[(names(cd4scan_data_2014a_2) == "Parameter_Descriptionss")] <- "CD4.percent"
cd4scan_data_2014a=merge(cd4scan_data_2014a_1,cd4scan_data_2014a_2,all=TRUE)

#Make all colnames the same
colnames(cd4scan_data_2014a)[(names(cd4scan_data_2014a) == "studyid")] <- "SID"
colnames(cd4scan_data_2014b)[(names(cd4scan_data_2014b) == "Subject.ID")] <- "SID"

colnames(cd4scan_data_2017)[(names(cd4scan_data_2017) == "Lab.result.Date")] <- "lab.date"
colnames(cd4scan_data_2015)[(names(cd4scan_data_2015) == "LAB.REPORT.DATE")] <- "lab.date"
colnames(cd4scan_data_2014a)[(names(cd4scan_data_2014a) == "Visit_Date")] <- "lab.date"
colnames(cd4scan_data_2014b)[(names(cd4scan_data_2014b) == "Lab.Result.Date")] <- "lab.date"

colnames(cd4scan_data_2017)[(names(cd4scan_data_2017) == "CD4.absolute")] <- "CD4.count"
colnames(cd4scan_data_2015)[(names(cd4scan_data_2015) == "CD4.ABS.COUNT")] <- "CD4.count"
colnames(cd4scan_data_2014b)[(names(cd4scan_data_2014b) == "CD4.abs.count")] <- "CD4.count"

colnames(cd4scan_data_2017)[(names(cd4scan_data_2017) == "CD4.percentage")] <- "CD4.percent"
colnames(cd4scan_data_2015)[(names(cd4scan_data_2015) == "CD4.PERCENTAGE")] <- "CD4.percent"
colnames(cd4scan_data_2014b)[(names(cd4scan_data_2014b) == "CD4.Percentage")] <- "CD4.percent"

#Get dates all in the same format
cd4scan_data_2014a$lab.date.correct=as.Date(cd4scan_data_2014a$lab.date,"%d%B%Y")
cd4scan_data_2014b$lab.date.correct=as.Date(cd4scan_data_2014b$lab.date,"%Y-%m-%d")#
cd4scan_data_2015$lab.date.correct=as.Date(cd4scan_data_2015$lab.date,"%Y-%m-%d")#
cd4scan_data_2017$lab.date.correct=as.Date(cd4scan_data_2017$lab.date,"%Y-%m-%d")

#Merge the dataframes using row concatenation
d1<-rbind(x=cd4scan_data_2017[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4scan_data_2015[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d2<-rbind(x=d1[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4scan_data_2014a[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d3<-rbind(x=d2[,c('SID','lab.date.correct','CD4.count','CD4.percent')], y=cd4scan_data_2014b[,c('SID','lab.date.correct','CD4.count','CD4.percent')]) 
d4=unique(d3) #remove duplicate rows

d4=na.omit(d4)
table(d4$SID) # How many CD4 measurements for each subject
length(unique(d4$SID)) #131 subjects - seems like too many? There are CD4 measures for some controls in 2015/2016?

d5=demog_data[,c('ID','PID','Birth.date',"Scan.date.5yrs","Scan.date.7yrs","Scan.date.9yrs.Skyra",'status')]
d5$Birth.date = fix_excel_dates(as.Date(d5$Birth.date,"%B %d, %Y"))
d5$Scan.date.5yrs = fix_excel_dates(as.Date(d5$Scan.date.5yrs,"%B %d, %Y"))
d5$Scan.date.7yrs = fix_excel_dates(as.Date(d5$Scan.date.7yrs,"%B %d, %Y"))
d5$Scan.date.9yrs.Skyra = fix_excel_dates(as.Date(d5$Scan.date.9yrs.Skyra,"%B %d, %Y"))

count(demog_data$status) #85 HIV
dd<-merge(x=d4, y=d5, by.x ="SID",by.y ="PID")
dd=dd[dd$status=="HIV",]
table(dd$SID)
length(unique(dd$SID))
setdiff(HIV$ID,unique(dd$ID)) #no values for 121, 139, 154 and PHRI?? First 3 not scanned again after 5 years
#count(dd$ID)$x[count(dd$ID)$freq<20] #These all less than 20 visit dates: 100 104 105 107 136 165 190 194 195 208 209 214 215 216 95  97  98

tt=ddply(dd, c("ID"), summarise, earliest = min(lab.date.correct), latest = max(lab.date.correct), nadir_CD4=min(CD4.count), nadir_CD4_percent=min(CD4.percent))
test=merge(tt,d5)
test$age_at_first_lab=difftime(test$earliest,as.Date(test$Birth.date,"%B %d, %Y"))/7
test$age_at_last_lab=difftime(test$latest,as.Date(test$Birth.date,"%B %d, %Y"))/365
test$ID[test$age_at_first_lab>20] #100 104 105 107 136 165 190 194 195 208 209 214 215 216 95  97  98 
test$ID[test$age_at_last_lab<9] #153 183 188 214 215 216

test$age_at_nadir=test
