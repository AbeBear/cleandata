run_analysis <- function() {
  x_te<-read.table("./UCI HAR Dataset/test/X_test.txt")
  y_te<-read.table("./UCI HAR Dataset/test/y_test.txt")
  s_te<-read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  x_tr<-read.table("./UCI HAR Dataset/train/X_train.txt")
  y_tr<-read.table("./UCI HAR Dataset/train/y_train.txt")
  s_tr<-read.table("./UCI HAR Dataset/train/subject_train.txt")
  
  features<-read.table("./UCI HAR Dataset/features.txt")
  
  testData<-clean_var_names(x_te,y_te,s_te,features)
  trainData<-clean_var_names(x_tr,y_tr,s_tr,features)
  
  fullData<-rbind(testData,trainData)
  
  stdmean_index<-c(1,2,grep("mean[^Freq]()|std()",colnames(fullData)))
  stdmeanData<-fullData[,stdmean_index]
  
  subj_group<-aggregate(stdmeanData[,3:68], list(stdmeanData$SubjectID), mean)
  subj_group<-cbind(rep("All Activities",30),subj_group)
  colnames(subj_group)[1:2]<-c("Activity","Subject")
  act_group<-aggregate(stdmeanData[,3:68], list(stdmeanData$Activity), mean)
  act_group<-cbind(rep("All Subjects",6),act_group)
  colnames(act_group)[1:2]<-c("Subject","Activity")
  #browser()
  newDF<-rbind(subj_group,act_group)

  write.table(newDF,"UCI HAR Averages.txt",row.name=FALSE)
}

clean_var_names<-function(xt,yt,st,feat){
  
  colnames(xt)<-feat[,2]

  #browser()
  yt[,1]<-gsub("\n","",yt[,1])
  yt[,1]<-gsub("1","Walking",yt[,1])
  yt[,1]<-gsub("2","Walking Upstairs",yt[,1])
  yt[,1]<-gsub("3","Walking Downstairs",yt[,1])
  yt[,1]<-gsub("4","Sitting",yt[,1])
  yt[,1]<-gsub("5","Standing",yt[,1])
  yt[,1]<-gsub("6","Laying",yt[,1])
  #browser()
  colnames(yt)<-"Activity"
  
  colnames(st)<-"SubjectID"
  
  return(cbind(st,yt,xt))
}