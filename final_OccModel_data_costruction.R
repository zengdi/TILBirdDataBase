# 该脚本用于生成进行千岛湖鸟类多度数据不完全探测校正的数据集
rm(list = ls())
# 加载所需要的包
library(openxlsx)
library(lubridate)
library(vegan)
library(tidyverse)

# 加载表格形式千岛湖鸟类群落数据集
final_bird_data = read.xlsx("./final_database/final_bird_data.xlsx")

# 加载矩阵形式千岛湖鸟类群落数据集
load("./final_database/TILbird_land_Month.Rdata")

# 加载功能函数集bird_database_function.R
source("./functions/data_processing_functions.R")

# 读取千岛湖鸟类调查协变量总表
covars = read.xlsx("./final_database/transect_covariate.xlsx")

# 读取千岛湖鸟类物种名录
TIL_bird_list = read.delim('./TILBirdinfo_unique.txt', sep = ',')

construct_OCCdata = function(data_type){
  
  # 获得协变量表中的所有年份
  years = unique(substr(unique(covars$month),1,4))
  
  # 获得繁殖季数据数据，并只保留从2009年开始的数据
  bird_data = get_Data_Abun(TIL_bird_list)
  if(data_type == "breeding"){
    bird_data_tmp = bird_data$breeding
  }else if(data_type == "winter"){
    bird_data_tmp = bird_data$winter
  }else{
    stop("数据类型选择有误，请核对！")
  }
  
  birds_comm = bird_data_tmp[,,,-c(1:2)]
  # birds_comm = bird_data_tmp
  
  # 最终生成用于占域模型的数据列表 ####
  final_data = list()
  
  # 繁殖季鸟类群落数据存入 ####
  final_data[["y"]] = birds_comm
  
  # 岛屿水平的参数构建 #####
  # 岛屿参数存入
  final_data[['island']] = IslandData
  # 岛屿上样线参数构建
  # 样线名称
  transects = dimnames(birds_comm)[[2]]
  # 岛屿名称
  island_name = sapply(transects,function(x){unlist(strsplit(x,"-"))[1]})
  # 岛屿样线信息表
  islandIsl = data.frame(IslandName = island_name,
                         TransectCode = names(island_name))
  # 岛屿面积
  islandIsl$IslandArea = NA
  # 到大陆距离
  islandIsl$IslandDm = NA
  # 到最近大岛距离
  islandIsl$IslandDnl = NA
  # 到最近岛屿距离
  islandIsl$IslandDnn = NA
  # 样线长
  islandIsl$length = NA
  # 岛屿水平随机因子
  islandIsl$IslandRF = NA
  # 数据构建循环
  for(i in 1:nrow(islandIsl)){
    mark = islandIsl[i,]$IslandName
    islandIsl[i,]$IslandArea = IslandData[IslandData$marks==mark,]$A
    islandIsl[i,]$IslandDm = IslandData[IslandData$marks==mark,]$Dm
    islandIsl[i,]$IslandDnl = IslandData[IslandData$marks==mark,]$Dnl
    islandIsl[i,]$IslandDnn = IslandData[IslandData$marks==mark,]$Dnn
    islandIsl[i,]$length = IslandData[IslandData$marks==mark,]$length/IslandData[IslandData$marks==mark,]$transects
    islandIsl[i,]$IslandRF = 9-IslandData[IslandData$marks==mark,]$transects
  }
  # 存入岛屿样线参数表
  final_data[['islandIsl']] = islandIsl
  
  # 2. 样线水平的参数构建 #####
  # 样线水平参数三维矩阵生成函数，用于基于鸟类群落数据生成对应维度的样线水平协变量矩阵
  construct_array = function(){
    array(NA,dim=dim(birds_comm)[-1],
          dimnames = dimnames(birds_comm)[c(2,3,4)])
  }
  
  # 2.1 样线调查水平的协变量）####
  # 调查日期为当年的天次，Julian day #####
  julian_day = construct_array()
  dimnames(julian_day)
  # 2.2 上午/下行time ####
  time = construct_array()
  dimnames(time)
  # 2.3 调查持续时间duration #####
  duration = construct_array()
  dimnames(duration)
  # 2.4 调查人surveyor #####
  surveyor = construct_array()
  dimnames(surveyor)
  
  # 对调查人进行排序 #####
  # 基于所有调查数据中鸟类物种和多度进行香农指数计算，对每个调查人进行赋值
  final_bird_data$Surveyor = trimws(final_bird_data$Surveyor,which="both")
  final_bird_data = final_bird_data %>%
    mutate(year = str_sub(Month,1,4),
           month = str_sub(Month,5,6)) %>%
    filter(year >= "2009") %>% 
    mutate(Surveyor = trimws(Surveyor,which="both"))
  
  final_bird_data %>% pull(year) %>% unique

  surveyors = unique(final_bird_data$Surveyor)
  surveyors = surveyors[!surveyors %in% c(NA,"无")]
  surveyor_order = data.frame()
  for(surveyor_name in surveyors){
    data_tmp = final_bird_data[final_bird_data$Surveyor==surveyor_name,]
    data_tmp = data_tmp[!data_tmp$Species == "无" & data_tmp$Transect %in% covars$transect,]
    div_val = diversity(data_tmp$Abun)
    record = data.frame(surveyor_name,div_val)
    surveyor_order = rbind(surveyor_order,record)
  }
  surveyor_order = surveyor_order[order(surveyor_order$div_val),]
  surveyor_order$order = c(1:nrow(surveyor_order))
  
  # 按年进行数据整理
  # 数据生成
  for(year in years){
    # 测试代码
    # year = "2013"
    data_year_tmp = covars[substr(covars$month,1,4)==year,]
    # 这里使用的是协变量总表中的month列，即正确列，克服了基于date列获得month
    # 可能存在的跨月匹配问题
    months = unique(data_year_tmp$month)
    for(month_i in 1:length(months)){
      # 测试代码
      # month_i = 1
      # 这里有个实现上的小处理，协变量数据中当年的01是上一年数据
      # 例如，200901实际上应该对应的是2008-01
      data_month_tmp = data_year_tmp[data_year_tmp$month==months[month_i],]
      for(i in 1:nrow(data_month_tmp)){
        # 测试代码
        # i = 1
        
        data_tmp = data_month_tmp[i,]
        tran = data_tmp$transect
        rep = data_tmp$rep
        month_name = substr(months[month_i],5,6)
        if(month_name=="01"){
          year_new = as.character(as.numeric(year)-1)
        }else{
          year_new = year
        }
        rep_new = paste(month_name,rep,sep="_")
        
        # Julian day
        # 1st day is the Jan 1st
        date = as.Date(data_tmp$date,format = '%Y%m%d')
        day = yday(date)
        if(rep_new %in% dimnames(julian_day)[[2]]){
          julian_day[tran, rep_new, year_new] = day
        }
        
        # time
        time_start = data_tmp$survey_start
        if(time_start>="1200"){
          time_final = "PM"
        }else if(time_start<="1159"){
          time_final = "AM"
        }else{
          print(paste(tran,rep_new,year_new,sep=","))
          stop("check start time of survey!")
        }
        if(rep_new %in% dimnames(time)[[2]]){
          time[tran, rep_new, year_new] = time_final
        }
        
        # duration
        if(rep_new %in% dimnames(duration)[[2]]){
          duration[tran, rep_new, year_new] = data_tmp$duration
        }
        
        # surveyor
        if(rep_new %in% dimnames(surveyor)[[2]]){
          surveyor[tran, rep_new, year_new] = surveyor_order[surveyor_order$surveyor_name==trimws(data_tmp$surveyor,"both"),]$order
        }
      }
    }
  }
  
  # 将所有协变量值存入final_covar_list中
  final_data[["julian_day"]] = julian_day
  final_data[["surveyor"]] = surveyor
  final_data[["time"]] = time
  final_data[["duration"]] = duration
  final_data[["surveyor_order"]] = surveyor_order
  
  # 返回最终数据集
  return(final_data)
}

TILOccData_breeding = construct_OCCdata("breeding")
TILOccData_winter = construct_OCCdata("winter")

# 测试代码
# 繁殖季数据核对
TILOccData_breeding$julian_day["B2-1", "05_1", "2015"]
TILOccData_breeding$surveyor["B2-1", "05_1", "2015"]
TILOccData_breeding$time["B2-1", "05_1", "2015"]
TILOccData_breeding$duration["B2-1", "05_1", "2015"]
covars[(covars$transect=="B2-1"&covars$month=="201505"&covars$rep=="1"),]

# 越冬季数据核对
TILOccData_winter$julian_day["B1-1", "01_1", "2012"]
TILOccData_winter$surveyor["B1-1", "01_1", "2012"]
TILOccData_winter$time["B1-1", "01_1", "2012"]
TILOccData_winter$duration["B1-1", "01_1", "2012"]
covars[(covars$transect=="B1-1"&covars$month=="201301"&covars$rep=="1"),]

date_stamp = today() %>% str_remove_all("-")
saveRDS(TILOccData_breeding,
        str_c("./export_data/TILOccData_breeding_",
              date_stamp,
              ".rds"))
saveRDS(TILOccData_winter,
        str_c("./export_data/TILOccData_winter_",
              date_stamp,
              ".rds"))

