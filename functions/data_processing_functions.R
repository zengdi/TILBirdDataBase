# 该脚本用于存放数据下游处理重要的功能函数 #####
# 作者：曾頔
# 最后修改日期：2024/11/11

library(tidyverse)

# 千岛湖鸟类留居型处理功能函数 #####
# 该函数基于物种的留居型添加繁殖季鸟类标记（summer）和越冬季鸟类（winter）标记
# 注：其中P类型的鸟不参与后续的数据分析
bird_resident = function(bird_list){
  bird_list = bird_list %>% 
    mutate(summer = ifelse(Resident == "R" | Resident == "S",1,0),
           winter = ifelse(Resident == "R" | Resident == "W",1,0))
  return(bird_list)
}


# 繁殖季和越冬节鸟类数据提取功能函数 ####
# 该函数用于提取指定季节内的鸟类名录
# resident参数有两个值可用：（1）summer，繁殖季（2）winter越冬季
getInfo = function(resident, bird_list){
  # 调试用，正式代码要注释####
  # resident = "summer"
  # 生成带有summer和winter标记的鸟类名录数据
  bird_info = bird_resident(bird_list)
  # 判断是否存在&，如果存在按&分隔
  if(resident == "summer"){
    resident_condition = bird_info$summer == 1
  }else if(resident == "winter"){
    resident_condition = bird_info$winter == 1
  }else{
    stop("条件错误请检查！")
  }
  # 基于resident条件匹配数据
  bird_ex = bird_info[resident_condition,]
  # 保留必要列
  bird_ex = bird_ex %>% select(Chinese,LatinWJ,Resident,Landbird)
  return(bird_ex)
}