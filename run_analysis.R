run_analysis <-function()
  ## load libraries
  library(dplyr)
  library(tidyr)
  ## reads raw test and train data and bind rows to create 1 table tabcom
  tabte<-read.table("./UCI HAR Dataset/test/X_test.txt")
  tabtr<-read.table("./UCI HAR Dataset/train/X_train.txt")
  tabcom<-rbind(tabte,tabtr)

  ## reads feature names and assign these names as column names for tabcom table
  feat<-read.table("./UCI HAR Dataset/features.txt")
  colnames(tabcom)<-feat$V2

  ## extract only columns with strings '-mean()' and '-std()' from tabcom table
  sub1<-grep("-mean()",feat[,2],fixed=TRUE)
  sub2<-grep("-std()",feat[,2],fixed=TRUE)
  res<-tabcom[,c(sub1,sub2)]

  ## reads activities for test and train data and bind rows to create 1 table actcom
  actte<-read.table("./UCI HAR Dataset/test/y_test.txt")
  acttr<-read.table("./UCI HAR Dataset/train/y_train.txt")
  actcom<-rbind(actte,acttr)
  colnames(actcom)<-"act_id"

  ## reads subjects for test and train data and bind rows to create 1 table labcom
  subte<-read.table("./UCI HAR Dataset/test/subject_test.txt")
  subtr<-read.table("./UCI HAR Dataset/train/subject_train.txt")
  subcom<-rbind(subte,subtr)
  colnames(subcom)<-"subject"

  ## add columns with activities and subjects to 'res' table
  rescom<-cbind(res,actcom,subcom)

  ## use activity labels to join data table with activity labels
  ## for each activity number also the activity label is stored as column
  lab<-read.table("./UCI HAR Dataset/activity_labels.txt")
  colnames(lab)<-c("act_id","activity")
  resjn<-join(rescom,lab,by="act_id")

  ## aggregate in order to get the mean for each variable grouped by subject and activity
  result<-aggregate(resjn[,1:66],list(Activity=resjn$activity,Suject=resjn$subject),mean)
  