# Owner: Jen-Yin Chao
# Date: 10 Dec 2017
# Title: The Comparison of CPU Process Scheduling Algorithms
# Description: This is a final project of Operating System in MS CS Fall 2017.

library(ggplot2)
library(reshape2)
library(plyr)

task_no = 5
turn_no = 2000
max_arrtime = 15
mean_burtime = 5
task_initial <- function(task_no=5, max_arrtime=15, mean_burtime=5){
  repeat{
    #task_arrtime = runif(task_no-1, 1, max_arrtime) + rnorm(task_no-1, mean=0, sd=2)
    task_arrtime = runif(task_no-1, 0.5, max_arrtime+0.5)
    task_arrtime = round(task_arrtime, 0)
    if (min(task_arrtime)>0 & max(task_arrtime)<max_arrtime+1 & sum(duplicated(task_arrtime))==0 ){ break }
  }
  #return(task_arrtime)
  repeat{
    task_burtime = rnorm(task_no, mean = mean_burtime, sd=2)
    task_burtime = round(task_burtime, 0)
    if (min(task_burtime)>0){ break }
  }
  task_table = cbind(c(0,sort(task_arrtime)), task_burtime, task_burtime, rep(0,task_no))
  colnames(task_table) = c("ArriveTime","BurstTime","RemainTime","CompleteTime")
  rownames(task_table) = c(1:task_no)
  return(task_table)
}
FCFS <- function(task_table){
  task_cand = which(task_table[,"ArriveTime"]<=Real_Time & task_table[,"RemainTime"]>0)
  if (length(task_cand)==0){
    return(0)
  } else {
    return( min(task_cand) )
  }
}
SJF <- function(task_table){
  task_cand = which(task_table[,"ArriveTime"]<=Real_Time & task_table[,"RemainTime"]>0 )
  if (length(task_cand)==0){
    return(0)
  } else if (length(task_cand)==1) {
    return( task_cand )
  } else {
    return( names(which(task_table[task_cand,"BurstTime"] == min(task_table[task_cand,"BurstTime"])))[1] )
  }
}
RR <- function(task_table,Real_Time,task_cand_pre,task_no){
  task_cand = which(task_table[,"ArriveTime"]<=Real_Time & task_table[,"RemainTime"]>0 )
  if (length(task_cand)==0){
    return(0)
  } else if (length(task_cand)==1) {
    return( task_cand )
  } else {
    repeat{
      task_cand_pre = task_cand_pre%%task_no
      task_cand_pre = task_cand_pre + 1
      if( sum(task_cand%in%task_cand_pre) ){ break }
    }
    return( task_cand_pre )
  }
}
SRTF <- function(task_table){
  task_cand = which(task_table[,"ArriveTime"]<=Real_Time & task_table[,"RemainTime"]>0 )
  #print(names(which(task_table[task_cand,"RemainTime"] == min(task_table[task_cand,"RemainTime"]))))
  if (length(task_cand)==0){
    return(0)
  } else if (length(task_cand)==1) {
    return( task_cand )
  } else {
    return( names(which(task_table[task_cand,"RemainTime"] == min(task_table[task_cand,"RemainTime"])))[1] )
  }
}
HRRN <- function(task_table,Real_Time){
  task_cand = which(task_table[,"ArriveTime"]<=Real_Time & task_table[,"RemainTime"]>0 )
  if (length(task_cand)==0){
    return(0)
  } else if (length(task_cand)==1) {
    return( task_cand )
  } else {
    RRatio = (Real_Time-task_table[,"ArriveTime"]-task_table[,"BurstTime"]+task_table[,"RemainTime"]+task_table[,"BurstTime"])/task_table[,"BurstTime"]
    task_table = cbind(task_table,RRatio)
    #print(task_table)
    return( names(which(task_table[task_cand,"RRatio"] == max(task_table[task_cand,"RRatio"])))[1] )
  }
}

Algo_List <- c("FCFS","SJF","RR","SRTF","HRRN")
WaitTime_table <- data.frame(t(rep(0,length(Algo_List))))
colnames(WaitTime_table) <- Algo_List

for (turn in 1:turn_no){
  WaitTime_table_temp <- double(0)
  for (algo_no in 1:length(Algo_List)){
    task_table <- task_initial(task_no,max_arrtime,mean_burtime)
    Algo = algo_no
    Real_Time = 0
    task_cand = 0
    Time_quan = 3
    repeat{
      task_cand <- switch(Algo,
                          FCFS(task_table),
                          SJF(task_table),
                          RR(task_table,Real_Time,task_cand,task_no),
                          SRTF(task_table),
                          HRRN(task_table,Real_Time))
      #Idle or not
      if (task_cand==0){
        time_proc = 1
        #print(paste(Real_Time,"-",Real_Time+time_proc," ","Idle",sep = ""))
      } else {
        if (Algo!=3){ #Algo othen than RR
          time_proc = 1
          task_table[task_cand,"RemainTime"] <- task_table[task_cand,"RemainTime"]-1
        } else { #RR Algo
          if (task_table[task_cand,"RemainTime"]>=Time_quan){
            time_proc = Time_quan
            task_table[task_cand,"RemainTime"] <- task_table[task_cand,"RemainTime"]-Time_quan
          } else {
            time_proc = task_table[task_cand,"RemainTime"]
            task_table[task_cand,"RemainTime"] <- 0
          }
        }
        
        if (task_table[task_cand,"RemainTime"]==0){
          task_table[task_cand,"CompleteTime"] <- Real_Time + time_proc
        }
        #print(paste(Real_Time,"-",Real_Time+time_proc," ",task_cand,sep = ""))
      }
      Real_Time = Real_Time + time_proc 
      if(sum(task_table[,"RemainTime"])==0){ break }
    }
    WaitTime <- task_table[,"CompleteTime"]-task_table[,"ArriveTime"]-task_table[,"BurstTime"]
    WaitTime_table_temp <- c(WaitTime_table_temp,mean(WaitTime))
  }
  WaitTime_table <- rbind(WaitTime_table, (WaitTime_table_temp))
}

WaitTime_table <- WaitTime_table[-1,]
WaitTime_table <- melt(WaitTime_table)
colnames(WaitTime_table) <- c("Algo","AveWaitTime")
WaitTime_summary <- ddply(WaitTime_table, "Algo", summarise, Mean=mean(AveWaitTime), SD=sd(AveWaitTime))

ggplot(WaitTime_table, aes(x=AveWaitTime, colour=Algo)) + geom_density() +
  ggtitle(paste("PDF of Ave Waiting Time between Algos with",task_no,"Tasks,",mean_burtime,"Mean Burst Time,",max_arrtime,"Max ArrTime")) + xlab("Average Waiting Time") +
  geom_vline(data = WaitTime_summary,aes(xintercept=Mean,colour=Algo), linetype="dashed", size=0.7) +
  geom_text(data = WaitTime_summary, aes(x=Mean-0.2,label=paste("mean =",round(Mean,4)),y=0.03), angle=90)
