# 该脚本用于存放数据下游处理重要的功能函数 #####
# 作者：曾頔
# 最后修改日期：2024/11/11

library(tidyverse)
library(abind)

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
  bird_ex = bird_ex %>% 
    select(Chinese,LatinWJ,Resident,Landbird) %>% 
    filter(Landbird == 1)
  return(bird_ex)
}

# array_summer/winter每层数据构建函数，被get_Data_Abun所调用 #####
# 按每个月提取数据，并排序为三维矩阵
data_each_stack = function(data_select,month_order,data_interval){
  # 测试代码
  # data_select = SppTranMonthData[dimnames(SppTranMonthData)[[1]] %in% getInfo("R&S")$LatinWJ,,,]
  # month_order = 1
  # month_order = 25
  # data_interval = 2
  
  array_tmp = array()
  data_tmp = data_select[,,seq(month_order,data_interval+month_order),]
  dimname = dimnames(data_tmp)
  month_names = dimname[[3]]
  reps = dimname[[4]]
  year = substr(dimnames(data_tmp)[[3]][1],1,4)
  array_tmp = data_tmp[,,month_names[1],]
  for(month_i in 2:length(month_names)){
    array_tmp = abind(array_tmp,data_tmp[,,month_names[month_i],],along = 3)
  }
  monthname = substr(month_names,5,6)
  rep_names = paste(rep(monthname,each=5),reps,sep="_")
  dimnames(array_tmp)[[3]] = rep_names
  
  # 测试代码 ###
  # dim(array_tmp)
  # dimnames(array_tmp)
  return(list(year=year,
              array=array_tmp))
}

# 2. 千岛湖鸟类数据提取函数######
# 按鸟的类型选择数据（breeding & winter）
# 返回list中的array元素即为所需要的鸟类群落四维矩阵，其索引含义如下
# [species,transect,replication,year]
get_Data_Abun = function(TIL_bird_list) {
  
  # 季节性数据的时间间隔
  data_interval = 2
  
  # 原始数据库
  data = SppTranMonthData
  
  # 繁殖季鸟类名录
  TIL_bird_info = bird_resident(TIL_bird_list)
  bird_sp_breeding = getInfo("summer",TIL_bird_info)$LatinWJ
  # 越冬季鸟类名录
  bird_sp_winter = getInfo("winter",TIL_bird_info)$LatinWJ
  
  # 从数据库内物种名属于上述名录的物种
  sp_retain_breeding = dimnames(data)[[1]] %in% bird_sp_breeding
  sp_retain_winter = dimnames(data)[[1]] %in% bird_sp_winter
  
  # 从数据库提取所需要的鸟类群落数据
  data_select_breeding = data[sp_retain_breeding,,,]
  data_select_winter = data[sp_retain_winter,,,]
  
  # 目前data_select的排序，所有的调查次序都是1-5排序
  # 第1位顺序条件：样线顺序
  # 第2位顺序条件：调查月份，保证所有数据都是04，05，06，11，12，01排序
  # 第3位顺序条件：调查次序，保证
  # 
  
  # 繁殖季数据完整性核验
  dims = dim(data_select_breeding)
  dimname = dimnames(data_select_breeding)
  months = unique(dimname[[3]])
  cond = substr(months[length(months)],5,6) == "06" | substr(months[length(months)],5,6) == "01"
  if(!cond){
    stop("繁殖季数据不完整，请补充完整再运行该数据提取代码！")
  }
  
  # 越冬季数据完整性核验
  dims = dim(data_select_winter)
  dimname = dimnames(data_select_winter)
  months = unique(dimname[[3]])
  cond = substr(months[length(months)],5,6) == "06" | substr(months[length(months)],5,6) == "01"
  if(!cond){
    stop("越冬季数据不完整，请补充完整再运行该数据提取代码！")
  }
  
  # 繁殖季年份序列
  year_breeding = c()
  # 越冬季年份序列
  year_winter = c()
  
  # 先进行第1年数据的构建
  # breeding第一层构建
  array_breeding = array()
  res_list = data_each_stack(data_select_breeding,1,data_interval)
  year_breeding = c(year_breeding,res_list[['year']])
  array_breeding = res_list[["array"]]
  
  # 3. 测试代码 #####
  # dim(array_breeding)
  # dimnames(array_breeding)
  # array_breeding[,,5]
  # data_select[,,"200704",5]
  
  # winter第一层构建
  array_winter = array()
  res_list = data_each_stack(data_select_winter,4,data_interval)
  year_winter = c(year_winter,res_list[['year']])
  array_winter = res_list[["array"]]
  
  # 4. 测试代码 #####
  # dim(array_winter)
  # dimnames(array_winter)
  # array_winter[,,5]
  # data_select[,,"200711",5]
  
  # 计数器，用于在季节数据中进行winter和summer后缀的判断
  count = 0
  # 按指定间隔取数据，进行合并，与结果array合并
  # 从7开始的原因是在构建第1年数据的时候已经把月份的前6层用完了
  for(i in seq(7, length(data_select_breeding[1,1,,1]),data_interval+1)){
    
    # 5. 测试代码 #####
    # i = 7
    
    # 当数据按繁殖季（4-6月数据）和越冬季（11-次年1月）取
    if(data_interval == 2){
      # breeding season
      if (count %% 2 == 0){
        # 将对应年份3个月的数据按调查次数堆叠
        res_list = data_each_stack(data_select_breeding,i,data_interval)
        # 获得该层数据的年份，并保存
        year_breeding = c(year_breeding,res_list[['year']])
        # 将该导数按第四维度合并到array_breeding数据集中
        array_breeding = abind(array_breeding,res_list[["array"]],along=4)
        
        # winter season
      }else{
        # 将对应年份3个月的数据按调查次数堆叠
        res_list = data_each_stack(data_select_winter,i,data_interval)
        # 获得该层数据的年份，并保存
        year_winter = c(year_winter,res_list[['year']])
        # 将该导数按第四维度合并到array_breeding数据集中
        array_winter = abind(array_winter,res_list[["array"]],along=4)
      }
      count = count + 1
    }
  }
  dimnames(array_winter)[[4]] = year_winter
  dimnames(array_breeding)[[4]] = year_breeding
  
  
  # 6. 测试代码 #####
  # dim(array_full_year)
  # dimnames(array_full_year)
  return(data = list(breeding = array_breeding,
                     winter = array_winter))
}
# 7. 测试代码 #####
# bird_data = get_Data_Abun()
# dim(bird_data$breeding)
# dim(bird_data$winter)

# 将调查时间转化为AM和PM的功能函数 #####
convert_AM_PM = function(time_start,time_end){
  if(time_start >= "12:00"){
    return("PM")
  }else if(time_start <= "1159"){
    if(time_end < "1159"){
      return("AM")
    }else{
    }
  }
}

