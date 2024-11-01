# 该脚本用于处理已生成的千岛湖鸟类调查数据集
# 主要功能是按指定的方法提取多度数，据按多次调查最大多度提取多度数据
# 作者：曾頔
# 更新日期：2022年4月15日

# 设置基本路径
# 使用的时候只用修改这个变量到数据库所在目录路径即可
path_base = getwd()
# 数据库路径
path_database = paste(path_base,'database',sep='/')
# 功能函数加载
# 处理多维数组
library(abind)
library(tidyverse)

# 加载千岛湖鸟类数据库
# 此5个数据集加载到内存可以调用：
# (1) 按调查月份岛屿水平多度数据
# (2) 按调查月份岛屿水平0/1数据
# (3) 按调查月份样线水平多度数据
# (4) 按调查月份样线水平0/1数据
# (5) 岛屿参数：IslandData
load(paste(path_database,'TILbird_land_Month.Rdata',sep='/'))

# 读取鸟类信息表（只包含拉丁名的）
bird_info = read.csv(paste(path_database,'TILBirdinfo_unique.txt',sep='/'),h = T)


# 1. 基于最大多度算法整合数据 ####
# 最大值确定标准：
# (1) 每个月5次调查的最大多度（max_month）
# (2) 每3个月15次调查中的最大多度（max_season）
# 有两个参数可以进行按调查季（繁殖或越冬）和年份提取数据
# data_type获取数据类型，其值如下
# island: 按调查月份岛屿水平多度数据：SppIslMonthData
# trans: 按调查月份样线水平多度数据：SppTranMonthData
getData_abu_max = function(data_type) {
  # 选择函数，基于data_type参数来选择数据库中对应的数据，并建立数值类型标记
  # 用于后续的数据整合，如果flag=abu代表结果为多度数据，flag=pa代表结果为0/1数据
  switch(data_type,
         'island' = {data = SppIslMonthData},
         'trans' = {data = SppTranMonthData})
  # 调试用，正式代码请注释#####
  data = SppIslMonthData
  data_interval = 2
  
  # 获得200904之后的数据集
  data = data[,,dimnames(data)[[3]] %>%
                enframe() %>% 
                filter(value>"200901") %>%
                pull(value),]
  count = 0
  summer_data = data[,,seq(1,data_interval+1),]
  winter_data = data[,,seq(4,4+data_interval),]
  for(i in seq(data_interval*3+1, dim(data)[3],data_interval+1)){
    if(data_interval == 2){
      if (count %% 2 == 0){
        summer_data = abind(summer_data,data[,,seq(i,i+data_interval),],along=3)
        count = count + 1
      }else{
        winter_data = abind(winter_data,data[,,seq(i,i+data_interval),],along=3)
        count = count + 1
      }
    }
  }
  # 夏季数据 #####
  # month数据:
  summer_month_abun_max = summer_data %>%
    array_branch(c(3)) %>%
    map(function(array_input){
      array_input[is.na(array_input)]=0
      apply(array_input, MARGIN=c(1,2), FUN=function(x){max(x,na.rm = T)})
    }) %>%
    unlist() %>%
    array(dim=dim(summer_data)[1:3],
          dimnames=dimnames(summer_data)[1:3])
  
  # season: 将夏季三个月多次调查数据合并成一年9次或15次调查的数据
  summer_data_tmp = summer_data %>% 
    array_branch(c(3))
  summer_data_list = {}
  for(i in seq(1,length(summer_data_tmp),3)){
    years = names(summer_data_tmp) %>% str_sub(1,4)
    year = years[i]
    data_tmp = abind(summer_data_tmp[[i]],
                     summer_data_tmp[[i+1]],
                     summer_data_tmp[[i+2]],
                     along = 3)
    summer_data_list[[year]] = data_tmp
  }
  
  # 夏季数据：获同年每个物种在不同样线上多度的最大值形成新的array
  summer_abun_max = summer_data_list %>% 
    # 获得多条样线上物种最大多度功能函数
    map(function(array_input){
      array_input[is.na(array_input)]=0
      apply(array_input, MARGIN=c(1,2), FUN=function(x){max(x,na.rm = T)})
    }) %>%
    unlist() %>%
    array(dim = c(dim(summer_data_list[[1]])[1:2],length(summer_data_list)),
          dimnames = append(dimnames(summer_data_list[[1]])[c(1,2)],list(names(summer_data_list))))
  
  # 冬季数据####
  # month数据:
  winter_month_abun_max = winter_data %>%
    array_branch(c(3)) %>%
    map(function(array_input){
      array_input[is.na(array_input)]=0
      apply(array_input, MARGIN=c(1,2), FUN=function(x){max(x,na.rm = T)})
    }) %>%
    unlist() %>%
    array(dim=dim(winter_data)[1:3],
          dimnames=dimnames(winter_data)[1:3])
  
  # seaon数据：将冬季三个月多次调查数据合并成一年9次或15次调查的数据
  winter_data_tmp = winter_data %>% 
    array_branch(c(3))
  winter_data_list = {}
  for(i in seq(1,length(winter_data_tmp),3)){
    years = names(winter_data_tmp) %>% str_sub(1,4)
    year = years[i]
    data_tmp = abind(winter_data_tmp[[i]],
                     winter_data_tmp[[i+1]],
                     winter_data_tmp[[i+2]],
                     along = 3)
    winter_data_list[[year]] = data_tmp
  }
  
  # 冬季数据：获同年每个物种在不同样线上多度的最大值形成新的array
  winter_abun_max = winter_data_list %>% 
    # 获得多条样线上物种最大多度功能函数
    map(function(array_input){
      array_input[is.na(array_input)]=0
      apply(array_input, MARGIN=c(1,2), FUN=function(x){max(x,na.rm = T)})
    }) %>%
    unlist() %>%
    array(dim = c(dim(winter_data_list[[1]])[1:2],length(winter_data_list)),
          dimnames = append(dimnames(winter_data_list[[1]])[c(1,2)],
                            list(names(winter_data_list))))
  
  
  return(list(summer_max_season = summer_abun_max,
              winter_max_season = winter_abun_max,
              summer_max_month = summer_month_abun_max,
              winter_max_month = winter_month_abun_max))
}

# 2. 按指定留居类型进行鸟名提取 ####
# resident可以是RSW中的任意一个，也可以是组合，但要以&连接，例R&W
getInfo = function(resident){
  # 调试用，正式代码要注释####
  # resident = "R&S"
  # 判断是否存在&，如果存在按&分隔
  if(grepl('&',resident)){
    # 对resident条件进行分隔
    resident_list = unlist(strsplit(resident,'&'))
    # 建立起始resident条件
    resident_condition = bird_info$Resident == resident_list[1]
    # 由于是或，基于剩余条件进行resident条件求逻辑或
    for(i in 2:length(resident_list)){
      resident_condition = (resident_condition | bird_info$Resident == resident_list[i])&(bird_info$Landbird ==1)
    }
    # 否则直接确定resident条件
  }else{
    resident_condition = bird_info$Resident == resident & bird_info$Landbird == 1
  }
  # 基于resident条件匹配数据
  data_ex = bird_info[resident_condition,]
  # 获得最终匹配数据
  data_ex = data_ex[,c(1,2,4,5)]
  return(data_ex)
}

# 3. 数据提取主函数 ######
# 按留居类型以及生活类型进行数据匹配提取
# (1) data_type:island(岛屿SppIslMonthData)/trans(样线SppTranMonthData)
# (2) season: summer/winter
# (3) time_scale: month/season
# R、W、S（同getInfo函数）
data_ex_birdinfo = function(data_type,season,time_scale){
  # 调试用，正式代码中请注释#####
  # data_type = "island"
  # resident = "R&S"
  # 基于给定参数从数据库中调取数据,按每次调查多度最大值提取数据
  bird_community = getData_abu_max(data_type)
  if(season == "summer"){
    resident = "R&S"
    bird_target = getInfo(resident) %>% 
      tibble() %>% 
      filter(Chinese!="无") %>%
      select(LatinWJ) %>% 
      arrange(LatinWJ) %>%
      pull()
    if(time_scale == "month"){
      data = bird_community[["summer_max_month"]]
      data = data[dimnames(data)[[1]] %in% bird_target,,]
      return(data)
    }else if(time_scale == "season"){
      data = bird_community[["summer_max_season"]]
      data = data[dimnames(data)[[1]] %in% bird_target,,]
      return(data)
    }else{
      stop('参数组合有误，请检查！')
    }
  }else if(season == "winter"){
    resident = "R&W"
    bird_target = getInfo(resident) %>% 
      tibble() %>% 
      filter(Chinese!="无") %>%
      select(LatinWJ) %>% 
      arrange(LatinWJ) %>%
      pull()
    
    if(time_scale == "month"){
      data = bird_community[["winter_max_month"]]
      data = data[dimnames(data)[[1]] %in% bird_target,,]
      return(data)
    }else if(time_scale == "season"){
      data = bird_community[["winter_max_season"]]
      data = data[dimnames(data)[[1]] %in% bird_target,,]
      return(data)
    }else{
      stop('参数组合有误，请检查！')
    }
  }else{
    stop('参数组合有误，请检查！')
  }
}

# 生成最终多度数据集
data_abun_max_list = list(
  # 岛屿水平夏季季节尺度（15次取最大值）数据
  abun_summer_season = data_ex_birdinfo("island","summer","season"),
  # 岛屿水平夏季月份尺度（5次取最大值）数据
  abun_summer_month = data_ex_birdinfo("island","summer","month"),
  # 岛屿水平冬季季节尺度（15次取最大值）数据
  abun_winter_season = data_ex_birdinfo("island","winter","season"),
  # 岛屿水平冬季月份尺度（5次取最大值）数据
  abun_winter_month = data_ex_birdinfo("island","winter","month")
)

# 保存数据到本地
saveRDS(data_abun_max_list,"./export_data/data_abun_max_list.rds")

# 数据核对
data_abun_max_list[["abun_summer_season"]]["Abroscopus albogularis","B1",]
SppIslMonthData["Abroscopus albogularis","B1",c("202104","202105","202106"),]

