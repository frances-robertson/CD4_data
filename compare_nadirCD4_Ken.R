
# Ken had calculated nadir CD4s and other stuff at age 5, but they don't always agree with what I get using all the avail spreadsheets with historical CD4 data
# Calculations in Ken's may be inaccuarate or it is possible that in some cases I am missing historical data
# need to run CD4_history.R first in order to run this comparison

# This is a spreadsheet that Ken made with nadir CD4 (and lots of other stuff) at age 5. Originals not known/unavailable
cd4_data_2013=read.xls("/Volumes/MRI/UserFolders/Frances/!HIV_spreadsheets/2013/FINAL_full_clinical_data_20131101.xlsx",skip=1,header=TRUE,sep=",")
cd4_data_2013 <- cd4_data_2013[,colSums(is.na(cd4_data_2013))<nrow(cd4_data_2013)]
#Filter(function(cd4_data_2013)!all(is.na(cd4_data_2013)), df)
cd4_data_2013<-head(cd4_data_2013,-3)
cd4_data_2013=cd4_data_2013[!is.na(cd4_data_2013$cd4_nadir),] #remove NAs

#Children who Ken had calculated a nadir CD4 for at 5 years
ken_ids<-cd4_data_2013$PID[!is.na(cd4_data_2013$cd4_nadir)] # 56 children, 3 I don't have, 6 I don't have early data, 
missing<-test$ID[test$age_at_first_lab>20]
intersect(ken_ids,missing) #Ken has nadir CD4 for 6 of the ones I don't have early measures for: 95 97 98 100 194 195 
length(unique(intersect(ken_ids,test$PID))) # 49 children in Ken's dataset also in mine including 6 I don't have aerly measures for

# Check Ken's nadir CD4 against mine
test_ken=merge(test,cd4_data_2013,by.x='PID',by.y='PID') #
test_ken=test_ken[!duplicated(test_ken$ID.x),]

# 22 subjects have same min count and date, IDs 185 and 201 have different date
IDs1=test_ken$ID.x[test_ken$nadir_CD4==test_ken$cd4_nadir]
length(unique(IDs1[!is.na(IDs1)]))
IDs2=test_ken$ID.x[as.Date(test_ken$nadir_cd4_count_date,"%Y-%m-%d")==test_ken$nadir_CD4_date]
length(unique(IDs2[!is.na(IDs2)]))

test_ken$nadir_cd4_percent_date=as.Date(test_ken$nadir_cd4_percent_date,"%Y-%m-%d")

# 36 have same min percent date, but 40 have same percent
IDs3=test_ken$ID.x[as.Date(test_ken$nadir_cd4_percent_date,"%Y-%m-%d")!=test_ken$nadir_CD4percent_date]
length(unique(IDs3[!is.na(IDs3)]))
IDs4=test_ken$ID.x[test_ken$nadir_CD4_percent==test_ken$cd4_percent_nadir]
length(unique(IDs4[!is.na(IDs4)]))

IDs=test_ken$ID.x[test_ken$nadir_CD4!=test_ken$cd4_nadir] #25 are different CD4 count, 
# These 25 are different (including 6 that I am clearly missing early data for)
#101 103 112 119 126 135 142 158 159 160 161 167 173 178 184 192 194* 195* 199 202 95*  97*  98*  100* 120
IDs=test_ken$ID.x[test_ken$nadir_CD4_percent!=test_ken$cd4_percent_nadir] 
# 9 are different in CD4%, so 13 for date
# They are 116 180 194* 195* 202 95*  97*  98*  100 and also 101 112 126 201 for date
#101 dont have as early as 2005-12-20
#126 dont have as early as 2006-03-28
#180 dont have as early as 2006-10-18 as well as 194, 195, 95, 97, 98, 100
# are 112 and 126 dates switced in kens??
#112, 116 - drops later, 201 - drops later (kens date was not lowest, though his value was), 202 - drops later
#chech 101 112 126 180 

# Decided to use nadir CD4 percent since count changes with age (decreases)
age_at_CD4_percent_nadir.ken=test_ken$age_at_CD4_percent_nadir.wks[test_ken$nadir_CD4percent_date!=test_ken$nadir_cd4_percent_date]/7
age_at_CD4_percent_nadir.me=round(test_ken$age_at_nadir_CD4_percent[test_ken$nadir_CD4percent_date!=test_ken$nadir_cd4_percent_date])/7
earliest=test_ken$earliest[test_ken$nadir_CD4percent_date!=test_ken$nadir_cd4_percent_date] #earliest
nadir_CD4_percent_date.me=test_ken$nadir_CD4percent_date[test_ken$nadir_CD4percent_date!=test_ken$nadir_cd4_percent_date] #me
nadir_CD4_percent_date.ken=test_ken$nadir_cd4_percent_date[test_ken$nadir_CD4percent_date!=test_ken$nadir_cd4_percent_date] #ken