rm(list = ls())
library(lubridate)
date_stamp = today() %>% str_remove_all("-")
# 该脚本用于处理已生成的千岛湖鸟类调查数据集
# 主要功能是按指定的方法提取多度数
# 通过原始数据得到每月3次调查的最大值，作为当月物种的多度
# 之后岛屿水平的多度可以通过样线水平求和或者求最大值得到
# 作者：曾頔
# 更新日期：2024年11月11日
# updating comments: 
# (1) 将样线水平的月份数据转化为年份数据，多度的获得方法为三个月份的多度最大值
# (2) 加新的数据集为summer_year_tran_abun_max和winter_year_tran_abun_max
# 数据库路径
path_database = 'final_database'
# 功能函数加载
# 处理多维数组
library(abind)
library(tidyverse)
source("./functions/data_processing_functions.R")

# 加载千岛湖鸟类数据库
# 此5个数据集加载到内存可以调用：
# (1) 按调查月份岛屿水平多度数据
# (2) 按调查月份岛屿水平0/1数据
# (3) 按调查月份样线水平多度数据
# (4) 按调查月份样线水平0/1数据
# (5) 岛屿参数：IslandData
# 需要使用的为: 按调查月份样线水平多度数据_SppTranMonthData #####
load(paste(path_database,'TILbird_land_Month.Rdata',sep='/'))

# 读取鸟类信息表（只包含拉丁名的）
TIL_bird_info = read.csv('TILBirdinfo_unique.txt',h = T,sep=",")
# 整合每个物种每月每样线多度数据 ####
# 最大值确定标准：每月三或五次调查中每物种的最大多度
# 输出数据有四个：繁殖季1套和越冬季1套（各有0-1的PA数据和多度的AB数据）

data = SppTranMonthData
# 时间间隔用来分别获得繁殖季和越冬季的鸟类数据
data_interval = 2

# 获得200904之后的数据集
data = data[,,dimnames(data)[[3]] %>%
              enframe() %>% 
              filter(value>"200901") %>%
              pull(value),]
# 计数变量，用于确定当前数据是繁殖季还是越冬季
count = 0
# 繁殖季数据第一层数据
summer_data = data[,,seq(1,data_interval+1),]
# 越冬季数据第一层数据
winter_data = data[,,seq(4,4+data_interval),]
# 进行繁殖季和越冬季数据的构建
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

#### 夏季数据处理 ####
# 1. 夏季每月样线多度处理 #####
# $$最基本数据集，后续转化的基础$$ #####
summer_month_abun_max = summer_data %>%
  # 基于月份，将原始的多度数据转化一个大表列，元素名为月份名
  array_branch(c(3)) %>%
  # 对每个月份的多次调查数据进行比较，保留每个物种最大多度
  map(function(array_input){
    array_input[is.na(array_input)]=0
    apply(array_input, MARGIN=c(1,2), FUN=function(x){max(x,na.rm = T)})
  }) %>%
  # 将列表拆分并进行重新组合，形成新的array
  # 行为物种，列为样线，第三轴为月份，值为多次调查中的最大多度
  unlist() %>%
  array(dim=dim(summer_data)[1:3],
        dimnames=dimnames(summer_data)[1:3])

# 夏季数据：获同年每个物种在不同样线上多度的最大值形成新的array
# 2. 夏季每年-岛屿多度处理 #####
# 2.1 先所有样线求最大，再多月求最大  #####
# 2.1.1 多样线比较获得每个物种每月的最大多度 #####
# 形成岛屿水平的物种每月多度数据array 
# 创建多样线比较获得岛屿水平每月每物种最大多度数据
summer_month_isl_abun_max = summer_month_abun_max %>%
  # 基于月份，将原始的多度数据转化一个大表列，元素名为月份名
  array_branch(c(3)) %>%
  # 对每个元素所对应的鸟类群落数据进行样线合并
  # 以每个物种在岛屿上所有样线中最大多度值作为该物种在岛屿上的多度
  # 使用map函数进行函数的高效循环
  map(
    # 每个群落数据的处理函数体
    function(data_tmp){
      # 对列名进行重构，只获得岛屿编号，去掉样线名
      isl = colnames(data_tmp) %>% str_remove("-\\d") %>%
        unique()
      # 获得原始数据中的物种名
      sp = rownames(data_tmp)
      # 获得原始数据中的样线名
      trans =  colnames(data_tmp)
      # 构建物种-岛屿群落数据矩阵表，行为物种，列为岛屿名
      data_new = matrix(0,nrow = dim(data_tmp)[1],ncol=length(isl),
                        dimnames = list(dimnames(data_tmp)[[1]],
                                        isl))
      # 对每个岛屿名称进行遍历
      for(is in isl){
        # 确定每个岛屿的样线数
        select_cols = unlist(str_match_all(trans,paste(is,"-?\\d?",sep="")))
        # 如果岛屿为I1和I2需要手动进行岛屿指定，正则表达式无法区别
        if(is == "I1"){
          select_cols = c("I1")
        }else if(is == "I2"){
          select_cols = c("I2")
        }
        # 如果有多条样线则进行取最大后赋值
        if(length(select_cols)>1){
          data_new[,is] = data_tmp[,select_cols] %>% apply(MARGIN = 1,max)
          # 如果只有1条样线，则直接赋值
        }else{
          data_new[,is] = data_tmp[,select_cols]
        }
      }
      return(data_new)
    }
  )

# 创建新array的维度数
dims_summer_month_isl_abun_max = c(dim(summer_month_isl_abun_max[[1]]),
                                   length(summer_month_isl_abun_max))
# 创建新array的维度名称
dimnames_summer_month_isl_abun_max = dimnames(summer_month_isl_abun_max[[1]])
dimnames_summer_month_isl_abun_max[[3]] = names(summer_month_isl_abun_max)
# 将每月群落数据列表转化为新的array
# 行为原始的物种名，列由样线合并成岛屿名，第三轴为月份
# 值为多条样线比较后的最大多度值
summer_month_isl_abun_max = summer_month_isl_abun_max %>% 
  unlist() %>%
  array(dim=dims_summer_month_isl_abun_max,
        dimnames=dimnames_summer_month_isl_abun_max)

# 2.1.2 多月份比较获得每个物种每年繁殖季的最大多度 #####
# 形成岛屿水平的物种每年多度数据array 
# 提取所有的月份
months = dimnames(summer_month_isl_abun_max)[[3]]
# 将月份转化为独立的年份
years = months %>% str_sub(start=1,end=4) %>% unique
# 构建岛屿-年array的维度数
dim_summer_year_isl_abun_max = c(dim(summer_month_isl_abun_max[,,1]),length(years))
# 构建岛屿-年array的维度名称
dimnames_summer_year_isl_abun_max = dimnames(summer_month_isl_abun_max[,,1])
dimnames_summer_year_isl_abun_max[[3]] = years

# $$夏季最终数据(先样线取最大，后月份取最大)$$ #####
# map函数进行每年数据的比较
summer_year_isl_abun_max = map(years,function(year){
  # 获得每一年3次数，并进行三次数据的比较，取最大
  # 获得每物种、每年、每岛屿上调查到的最大多度
  data_tmp = summer_month_isl_abun_max[,,unlist(str_match_all(months,
                                                              paste(year,"0","[4-6]",sep="")))] %>%
    apply(MARGIN=c(1,2), FUN=function(x){max(x,na.rm = T)})
  return(data_tmp)
}) %>% 
  unlist() %>%
  array(dim=dim_summer_year_isl_abun_max,
        dimnames=dimnames_summer_year_isl_abun_max)

# 2.1.3 样线-年水平数据，每条样线三个月调查取最大值 ####
# 先将月份转化为年
# 先将样线-月水平数据复制
summer_tran_month_abun_max = summer_month_abun_max
# 获得所有的年份
year_tran = str_sub(dimnames(summer_tran_month_abun_max)[[3]],1,4)
# 将原始数据的第三轴（时间轴）修改为年
dimnames(summer_tran_month_abun_max)[[3]] = year_tran
# 建立样线-年水平数据，并将2009年数据作为第一层
summer_year_tran_abun_max = apply(summer_tran_month_abun_max[,,1:3],c(1,2),max)
# 循环每个年份，比较每个月中最大度作为该样线该年度的多度值

for(i in seq(4,(length(year_tran)-2),3)){
  s = i
  e = i+2
  summer_year_tran_tmp = apply(summer_tran_month_abun_max[,,s:e],c(1,2),max)
  summer_year_tran_abun_max = abind(summer_year_tran_abun_max,
                                    summer_year_tran_tmp, 
                                    along=3)
}

# 对样线-年数据进行维度命名
dimnames(summer_year_tran_abun_max) = list(dimnames(summer_tran_month_abun_max)[[1]],
                                           dimnames(summer_tran_month_abun_max)[[2]],
                                           unique(year_tran))

# 2.2 先样线求和，再多月求最大 ####
# 2.2.1 每物种多度求和，获得每个物种岛屿水平每月的多度 ####
summer_month_isl_abun_sum = summer_month_abun_max %>%
  # 基于月份，将原始的多度数据转化一个大表列，元素名为月份名
  array_branch(c(3)) %>%
  # 对每个元素所对应的鸟类群落数据进行样线合并
  # 以每个物种在岛屿上所有样线中最大多度值作为该物种在岛屿上的多度
  # 使用map函数进行函数的高效循环
  map(
    # 每个群落数据的处理函数体
    function(data_tmp){
      # 对列名进行重构，只获得岛屿编号，去掉样线名
      isl = colnames(data_tmp) %>% str_remove("-\\d") %>%
        unique()
      # 获得原始数据中的样线名
      trans =  colnames(data_tmp)
      # 构建物种-岛屿群落数据矩阵表，行为物种，列为岛屿名
      data_new = matrix(0,nrow = dim(data_tmp)[1],ncol=length(isl),
                        dimnames = list(dimnames(data_tmp)[[1]],
                                        isl))
      # 对每个岛屿的多条样线上的多度进行求和
      for(is in isl){
        # 确定每个岛屿的样线数
        select_cols = unlist(str_match_all(trans,paste(is,"-?\\d?",sep="")))
        # 如果岛屿为I1和I2需要手动进行岛屿指定，正则表达式无法区别
        if(is == "I1"){
          select_cols = c("I1")
        }else if(is == "I2"){
          select_cols = c("I2")
        }
        
        # 如果有多条样线求和之后再赋值
        if(length(select_cols) > 1){
          data_new[,is] = data_tmp[,select_cols] %>%
            rowSums
          # 如果只有一条样线则直接赋值
        }else{
          data_new[,is] = data_tmp[,select_cols]
        }
      }
      return(data_new)
    }
  )

# 创建新array的维度数
dims_summer_month_isl_abun_sum = c(dim(summer_month_isl_abun_sum[[1]]),
                                   length(summer_month_isl_abun_sum))
# 创建新array的维度名称
dimnames_summer_month_isl_abun_sum = dimnames(summer_month_isl_abun_sum[[1]])
dimnames_summer_month_isl_abun_sum[[3]] = names(summer_month_isl_abun_sum)
# 将每月群落数据列表转化为新的array
# 行为原始的物种名，列由样线合并成岛屿名，第三轴为月份
# 值为多条样线求和后的多度
summer_month_isl_abun_sum = summer_month_isl_abun_sum %>% 
  unlist() %>%
  array(dim=dims_summer_month_isl_abun_sum,
        dimnames=dimnames_summer_month_isl_abun_sum)


# $$夏季最终数据(先样线求和，后月份取最大)$$ #####
# map函数进行每年数据的比较
summer_year_isl_abun_sum = map(years,function(year){
  # 获得每一年3 次数，并进行三次数据的比较，取最大
  # 获得每物种、每年、每岛屿上调查到的最大多度
  data_tmp = summer_month_isl_abun_sum[,,unlist(str_match_all(months,
                                                              paste(year,"0","[4-6]",sep="")))] %>%
    apply(MARGIN=c(1,2), FUN=function(x){max(x,na.rm = T)})
  return(data_tmp)
}) %>% 
  unlist() %>%
  array(dim=dim_summer_year_isl_abun_max,
        dimnames=dimnames_summer_year_isl_abun_max)


#### 冬季数据处理 ####

# 3. 冬季每月样线多度处理 #####
# $$最基本数据集，后续转化的基础$$ #####
winter_month_abun_max = winter_data %>%
  # 基于月份，将原始的多度数据转化一个大表列，元素名为月份名
  array_branch(c(3)) %>%
  # 对每个月份的多次调查数据进行比较，保留每个物种最大多度
  map(function(array_input){
    array_input[is.na(array_input)]=0
    apply(array_input, MARGIN=c(1,2), FUN=function(x){max(x,na.rm = T)})
  }) %>%
  # 将列表拆分并进行重新组合，形成新的array
  # 行为物种，列为样线，第三轴为月份，值为多次调查中的最大多度
  unlist() %>%
  array(dim=dim(winter_data)[1:3],
        dimnames=dimnames(winter_data)[1:3])

# 冬季数据：获同年每个物种在不同样线上多度的最大值形成新的array
# 4. 冬季每年-岛屿多度处理 #####
# 4.1 先所有样线求最大，再多月求最大 ######
# 4.1.1 多样线比较获得每个物种每月的最大多度 #####
# 形成岛屿水平的物种每月多度数据array 

# 创建多样线比较获得岛屿水平每月每物种最大多度数据
winter_month_isl_abun_max = winter_month_abun_max %>%
  # 基于月份，将原始的多度数据转化一个大表列，元素名为月份名
  array_branch(c(3)) %>%
  # 对每个元素所对应的鸟类群落数据进行样线合并
  # 以每个物种在岛屿上所有样线中最大多度值作为该物种在岛屿上的多度
  # 使用map函数进行函数的高效循环
  map(
    # 每个群落数据的处理函数体
    function(data_tmp){
      # 对列名进行重构，只获得岛屿编号，去掉样线名
      isl = colnames(data_tmp) %>% str_remove("-\\d") %>%
        unique()
      # 获得原始数据中的物种名
      sp = rownames(data_tmp)
      # 获得原始数据中的样线名
      trans =  colnames(data_tmp)
      # 构建物种-岛屿群落数据矩阵表，行为物种，列为岛屿名
      data_new = matrix(0,nrow = dim(data_tmp)[1],ncol=length(isl),
                        dimnames = list(dimnames(data_tmp)[[1]],
                                        isl))
      # 对每个岛屿名称进行遍历
      for(is in isl){
        # 确定每个岛屿的样线数
        select_cols = unlist(str_match_all(trans,paste(is,"-?\\d?",sep="")))
        if(is == "I1"){
          select_cols = c("I1")
        }else if(is == "I2"){
          select_cols = c("I2")
        }
        # 如果有多条样线则进行取最大后赋值
        if(length(select_cols)>1){
          data_new[,is] = data_tmp[,select_cols] %>% apply(MARGIN = 1,max)
          # 如果只有1条样线，则直接赋值
        }else{
          data_new[,is] = data_tmp[,select_cols]
        }
      }
      return(data_new)
    }
  )

# 创建新array的维度数
dims_winter_month_isl_abun_max = c(dim(winter_month_isl_abun_max[[1]]),
                                   length(winter_month_isl_abun_max))
# 创建新array的维度名称
dimnames_winter_month_isl_abun_max = dimnames(winter_month_isl_abun_max[[1]])
dimnames_winter_month_isl_abun_max[[3]] = names(winter_month_isl_abun_max)
# 将每月群落数据列表转化为新的array
# 行为原始的物种名，列由样线合并成岛屿名，第三轴为月份
# 值为多条样线比较后的最大多度值
winter_month_isl_abun_max = winter_month_isl_abun_max %>% 
  unlist() %>%
  array(dim=dims_winter_month_isl_abun_max,
        dimnames=dimnames_winter_month_isl_abun_max)

# 4.1.2 多月份比较获得每个物种每年繁殖季的最大多度 #####
# 形成岛屿水平的物种每年多度数据array 
# 提取所有的月份
months = dimnames(winter_month_isl_abun_max)[[3]]
# 将月份转化为独立的年份
years = months %>% str_sub(start=1,end=4) %>% unique
years = years[-length(years)]
# 构建岛屿-年array的维度数
dim_winter_year_isl_abun_max = c(dim(winter_month_isl_abun_max[,,1]),length(years))
# 构建岛屿-年array的维度名称
dimnames_winter_year_isl_abun_max = dimnames(winter_month_isl_abun_max[,,1])
dimnames_winter_year_isl_abun_max[[3]] = years

# $$冬季最终数据(先样线取最大，后月份取最大)$$ #####
# map函数进行每年数据的比较
winter_year_isl_abun_max = map(years,function(year){
  # 获得每一年11，12和次年1月的数据
  year = year %>% as.numeric
  col_select = c(paste(year,c(11:12),sep=""),paste(year+1,"01",sep=""))
  # 获得每一年3 次数，并进行三次数据的比较，取最大
  # 获得每物种、每年、每岛屿上调查到的最大多度
  data_tmp = winter_month_isl_abun_max[,,col_select] %>%
    apply(MARGIN=c(1,2), FUN=function(x){max(x,na.rm = T)})
  return(data_tmp)
}) %>% 
  unlist() %>%
  array(dim=dim_winter_year_isl_abun_max,
        dimnames=dimnames_winter_year_isl_abun_max)

# 4.1.3 样线-年水平数据，每条样线三个月调查取最大值 ####
# 先将月份转化为年
# 先将样线-月水平数据复制
winter_tran_month_abun_max = winter_month_abun_max
# 获得所有的年份
year_tran = str_sub(dimnames(winter_tran_month_abun_max)[[3]],1,4)
year_winter = head(unique(year_tran),-1)
# 将原始数据的第三轴（时间轴）修改为年
dimnames(winter_tran_month_abun_max)[[3]] = year_tran
# 建立样线-年水平数据，并将2009年数据作为第一层
winter_year_tran_abun_max = apply(winter_tran_month_abun_max[,,1:3],c(1,2),max)
# 循环每个年份，比较每个月中最大度作为该样线该年度的多度值
for(y in seq(4,length(year_tran)-2,3)){
  s = y
  e = y+2
  winter_year_tran_tmp = apply(winter_tran_month_abun_max[,,s:e],c(1,2),max)
  winter_year_tran_abun_max = abind(winter_year_tran_abun_max,
                                    winter_year_tran_tmp, 
                                    along=3)
}

# 对样线-年数据进行维度命名
dimnames(winter_year_tran_abun_max) = list(dimnames(winter_tran_month_abun_max)[[1]],
                                           dimnames(winter_tran_month_abun_max)[[2]],
                                           year_winter)

# 4.2 先样线求和，再多月求最大 ####
# 4.2.1 每物种多样性多度求和，获得每个物种岛屿水平每月的多度 ####
winter_month_isl_abun_sum = winter_month_abun_max %>%
  # 基于月份，将原始的多度数据转化一个大表列，元素名为月份名
  array_branch(c(3)) %>%
  # 对每个元素所对应的鸟类群落数据进行样线合并
  # 以每个物种在岛屿上所有样线中最大多度值作为该物种在岛屿上的多度
  # 使用map函数进行函数的高效循环
  map(
    # 每个群落数据的处理函数体
    function(data_tmp){
      # 对列名进行重构，只获得岛屿编号，去掉样线名
      isl = colnames(data_tmp) %>% str_remove("-\\d") %>%
        unique()
      # 获得原始数据中的样线名
      trans =  colnames(data_tmp)
      # 构建物种-岛屿群落数据矩阵表，行为物种，列为岛屿名
      data_new = matrix(0,nrow = dim(data_tmp)[1],ncol=length(isl),
                        dimnames = list(dimnames(data_tmp)[[1]],
                                        isl))
      # 对每个岛屿的多条样线上的多度进行求和
      for(is in isl){
        # 确定每个岛屿的样线数
        select_cols = unlist(str_match_all(trans,paste(is,"-?\\d?",sep="")))
        # 如果岛屿为I1和I2需要手动进行岛屿指定，正则表达式无法区别
        if(is == "I1"){
          select_cols = c("I1")
        }else if(is == "I2"){
          select_cols = c("I2")
        }
        # 如果有多条样线求和之后再赋值
        if(length(select_cols) > 1){
          data_new[,is] = data_tmp[,select_cols] %>%
            rowSums
          # 如果只有一条样线则直接赋值
        }else{
          data_new[,is] = data_tmp[,select_cols]
        }
      }
      return(data_new)
    }
  )

# 创建新array的维度数
dims_winter_month_isl_abun_sum = c(dim(winter_month_isl_abun_sum[[1]]),
                                   length(winter_month_isl_abun_sum))
# 创建新array的维度名称
dimnames_winter_month_isl_abun_sum = dimnames(winter_month_isl_abun_sum[[1]])
dimnames_winter_month_isl_abun_sum[[3]] = names(winter_month_isl_abun_sum)

# 将每月群落数据列表转化为新的array
# 行为原始的物种名，列由样线合并成岛屿名，第三轴为月份
# 值为多条样线求和后的多度
winter_year_isl_abun_sum = winter_month_isl_abun_sum %>%
  unlist %>%
  array(dim=dims_winter_month_isl_abun_sum,
        dimnames=dimnames_winter_month_isl_abun_sum)

# $$冬季最终数据(先样线取最大，后月份取最大)$$ #####
# map函数进行每年数据的比较
winter_year_isl_abun_sum = map(years,function(year){
  # 获得每一年11，12和次年1月的数据
  year = year %>% as.numeric
  col_select = c(paste(year,c(11:12),sep=""),paste(year+1,"01",sep=""))
  # 获得每一年3 次数，并进行三次数据的比较，取最大
  # 获得每物种、每年、每岛屿上调查到的最大多度
  data_tmp = winter_year_isl_abun_sum[,,col_select] %>%
    apply(MARGIN=c(1,2), FUN=function(x){max(x,na.rm = T)})
  return(data_tmp)
}) %>% 
  unlist() %>%
  array(dim=dim_winter_year_isl_abun_max,
        dimnames=dimnames_winter_year_isl_abun_max)


bird_community = list(summer_month_tran_abun_max = summer_month_abun_max,
                      summer_year_isl_abun_max = summer_year_isl_abun_max,
                      summer_year_isl_abun_sum = summer_year_isl_abun_sum,
                      summer_year_tran_abun_max = summer_year_tran_abun_max,
                      winter_month_tran_abun_max = winter_month_abun_max,
                      winter_year_isl_abun_max = winter_year_isl_abun_max,
                      winter_year_isl_abun_sum = winter_year_isl_abun_sum,
                      winter_year_tran_abun_max = winter_year_tran_abun_max)

# 2. 数据提取 ######
# 基于繁殖季和越冬季进行鸟类群落多度提取
{
  # 基于给定参数从数据库中调取数据,按每次调查多度最大值提取数据
  # 繁殖季数据处理
  sp_summer = getInfo("summer",TIL_bird_info)$LatinWJ
  # 样线-月水平数据
  bird_summer_abun_tran_month_max = bird_community$summer_month_tran_abun_max
  bird_summer_abun_tran_month_max = bird_summer_abun_tran_month_max[
    dimnames(bird_summer_abun_tran_month_max)[[1]] %in% sp_summer,,]
  bird_summer_abun_tran_month_max["Aegithalos concinnus","M2",]
  # 样线-年水平数据
  bird_summer_abun_tran_year_max = bird_community$summer_year_tran_abun_max
  bird_summer_abun_tran_year_max = bird_summer_abun_tran_year_max[
    dimnames(bird_summer_abun_tran_year_max)[[1]] %in% sp_summer,,]
  bird_summer_abun_tran_year_max["Aegithalos concinnus","M2",]
  
  # 岛屿-年水平最大值
  bird_summer_abun_isl_year_max = bird_community$summer_year_isl_abun_max
  bird_summer_abun_isl_year_max = bird_summer_abun_isl_year_max[
    dimnames(bird_summer_abun_isl_year_max)[[1]] %in% sp_summer,,
  ]
  
  # 岛屿-年水平求和
  bird_summer_abun_isl_year_sum = bird_community$summer_year_isl_abun_sum
  bird_summer_abun_isl_year_sum = bird_summer_abun_isl_year_sum[
    dimnames(bird_summer_abun_isl_year_sum)[[1]] %in% sp_summer,,
  ]
  
  # 越冬季数据处理
  sp_winter = getInfo("winter",TIL_bird_info)$LatinWJ
  # 样线-月水平数据
  bird_winter_abun_tran_month_max = bird_community$winter_month_tran_abun_max
  bird_winter_abun_tran_month_max = bird_winter_abun_tran_month_max[
    dimnames(bird_winter_abun_tran_month_max)[[1]] %in% sp_winter,,]
  
  # 样线-年水平数据
  bird_winter_abun_tran_year_max = bird_community$winter_year_tran_abun_max
  bird_winter_abun_tran_year_max = bird_winter_abun_tran_year_max[
    dimnames(bird_winter_abun_tran_year_max)[[1]] %in% sp_winter,,]
  
  # 岛屿-年水平最大值数据
  bird_winter_abun_isl_year_max = bird_community$winter_year_isl_abun_max
  bird_winter_abun_isl_year_max = bird_winter_abun_isl_year_max[
    dimnames(bird_winter_abun_isl_year_max)[[1]] %in% sp_winter,,
  ]
  # 岛屿-年水平求和数据
  bird_winter_abun_isl_year_sum = bird_community$winter_year_isl_abun_sum
  bird_winter_abun_isl_year_sum = bird_winter_abun_isl_year_sum[
    dimnames(bird_winter_abun_isl_year_sum)[[1]] %in% sp_winter,,
  ]
  
  save(bird_summer_abun_tran_month_max,
       bird_summer_abun_tran_year_max,
       bird_summer_abun_isl_year_max,
       bird_summer_abun_isl_year_sum,
       bird_winter_abun_tran_month_max,
       bird_winter_abun_tran_year_max,
       bird_winter_abun_isl_year_max,
       bird_winter_abun_isl_year_sum,
       file = str_c("./export_data/final_uncorrected_bird_data_abun",
                    date_stamp,".Rdata",sep=""))
}

