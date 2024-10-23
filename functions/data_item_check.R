# 建立数据检查基本矩阵
# monthName调查月份的向量，num_col代表除了monthName外的列数
create_check_matrix = function(monthName,num_col){
  matrix_pre = matrix(NA,nrow = length(monthName),ncol = num_col+1)
  rownames(matrix_pre) = monthName
  matrix_pre[,1] = monthName
  return(matrix_pre)
}

# 物种名校对函数
spp_check = function(spp){
  if(is.na(spp)){
    return(TRUE)
  }else{
    if(spp=='无'){
      return(TRUE)
    }else{
      if(spp %in% TILBirdList_raw$Chinese){
        return(TRUE)
      }else{
        return(FALSE)
      }
    }
  }
}


########数据核对代码区##########################################
# 该代码用于每条数据记录的核对主要包括以下核对：
# 调查日期核对
# 调查时间核对
# 每条样线每次调查持续时间核对
# 调查次数核对
# 至少1个主要调查人核对
# 样线及重复次数核对
# 物种名数据核对
# 每列空行核对,包括如下数据
# (1) 样线数据 transect
# (2) 调查日期 day
# (3) 调查起始时间 began_time
# (4) 调查结束时间 end_time
# (5) 天气 weather
# (6) 调查重复次数 repetition
# (7) 物种名 species
# (8) 多度 abundance
# (9) 主要调查人 surveyor

# 核对函数 ######
data_check = function(monthName,mysheets){
  # 创建日期核对表：day_check_table
  day_check_table =  create_check_matrix(monthName,3)
  colnames(day_check_table) = c('month','survey_date_min','survey_date_max','status')
  survey_date_table = cbind(year = names(monthName),month = monthName)
  
  # 创建调查时间核对表：time_check_table
  time_check_table =  create_check_matrix(monthName,5)
  colnames(time_check_table) = c('month','time_began_min','time_began_max',
                                 'time_end_min','time_end_max','status')
  
  # 创建每条样线每次调查持续时间核对表：time_duration_check_table
  time_duration_check_table = data.frame(matrix(NA,ncol = 7))
  colnames(time_duration_check_table) = c('month','transect','survey_order','time_began',
                                          'time_end','duration','status')
  # 创建调查次数核对表：rep_check_table
  rep_check_table =  create_check_matrix(monthName,9)
  
  # 创建调查人核对表：surveyor_check_table
  surveyor_check_table = create_check_matrix(monthName,15)
  
  # 创建样线及重复次数核对表
  tran_data_check_table = create_check_matrix(monthName,70)
  # 样线重复数核对表，若返回character(0)代表没有问题,
  
  # 若样线重复调查有问题，则返回调查月份、有问题的样线名和有问题的调查次序
  # 例："200801 J1 1" "200901 B1-1 2,3"
  tran_rep_check = data.frame()
  
  # 鸟类物种名核对
  # 物种名核对表：spp_check_table
  # 该表格是所有有问题的物种记录名，会有重复，因为不同的记录者
  # 会记录到相同有问题的物种名
  spp_check_table = data.frame()
  # 千岛湖鸟类调查中记录到的任何鸟名向量（包含错记、无法识别的鸟名）
  # 几乎没有重复
  spp_all = c()
  
  
  # 创建每列空行核对表：dim_check_table
  dim_check_table = create_check_matrix(monthName,11)
  colnames(dim_check_table) = c('month','nrow','ncol',
                                      'transect','day','began_time','end_time',
                                      'weather','repetition','species',
                                      'abundance','surveyor')
  # 遍历所有调查月份
  for(i in 1:length(monthName)){
    # 1. 测试代码,需要注释后运行 #####
    # i = 23
    # 获得年
    yy = names(monthName[i])
    # 获得月
    mm = monthName[i]
    
    #print(i)
    #print(mm)
    # 调查日期核对 ####
    # 统计当前月份的调查起始和结束日期，并赋值给日期核对表
    survey_range = range(names(table(mysheets[[yy]][[mm]][,2])))
    day_check_table[mm,]['survey_date_min'] = survey_range[1]
    day_check_table[mm,]['survey_date_max'] = survey_range[2]
    # 进行起始日期和结束日期月份判断，如果前后一致核对表状态填为ok
    # 否则显示recheck
    condition1 = substr(survey_range[1],5,6) == substr(mm,5,6)
    condition2 = substr(survey_range[2],5,6) == substr(mm,5,6)
    if (condition1 & condition2){
      day_check_table[mm,]['status'] = 'ok'
    }else{
      day_check_table[mm,]['status'] = 'recheck'
    }
    
    # 调查时间核对####
    # 获得当前月份数据调查时间的起始值范围
    survey_began_time_range = range(names(table(mysheets[[yy]][[mm]][,3])))
    # 获得当前月份数据调查时间的结束值范围
    survey_end_time_range = range(names(table(mysheets[[yy]][[mm]][,4])))
    # 进行调查时间起始与结束最大和最小值填表
    time_check_table[mm,]['time_began_min'] = survey_began_time_range[1]
    time_check_table[mm,]['time_began_max'] = survey_began_time_range[2]
    time_check_table[mm,]['time_end_min'] = survey_end_time_range[1]
    time_check_table[mm,]['time_end_max'] = survey_end_time_range[2]
    # 如果核对表中有无，在状态列显示recheck
    # 否则为ok
    if('无' %in% time_check_table[mm,]){
      time_check_table[mm,]['status'] = 'recheck'
    }else{
      time_check_table[mm,]['status'] = 'ok'
    }
    
    # 检查每次调查的起始时间与终止时间差 #####
    data_month = mysheets[[yy]][[mm]]
    # 获得当月调查次数
    survey_order = unique(data_month$调查次数)
    # 遍历每次调查的数据
    for(order in survey_order){
      # order = 1
      # 获得每次调查的数据
      data_tmp = data_month[data_month$调查次数==order,]
      # 获得每次调查中的样线名
      transect_names = unique(data_tmp$样线)
      # 遍历每条样线
      for(tra in transect_names){
        # 定义持续时间
        duration = "NA"
        # 定义检查状态
        check = "check"
        
        # 测试代码####
        # tra = "S100"
        # 获得起始时间，并转化为时间对象
        time_start = names(table(data_tmp[data_tmp$样线 == tra,]$调查起始时间))
        # print(data_tmp[data_tmp$样线 == tra,])
        if(is.null(time_start)){
          time_start = "无"
        }else if(length(time_start)>1){
          time_start = paste(time_start,collapse=',')
        }
        
        # 获得终止时间，并转化为时间对象
        time_end = names(table(data_tmp[data_tmp$样线 == tra,]$调查结束时间))
        if(is.null(time_end)){
          time_end = "无"
        }else if(length(time_end)>1){
          time_end = paste(time_end,collapse=',')
        }
        
        
        # 如果起始和终止时间都不是无
        # print(paste("mm",mm))
        # print(paste("i",i))
        # print(paste("order",order))
        # print(paste("tran",tra))
        # print(paste('start',paste(time_start,collapse=',')))
        # print(paste('end',time_end))
        # 只有当起始时间和终止时间皆不为无时，才可以计算调查持续时间
        if(grepl('^[0-2][0-9][0-5][0-9]$',time_start) & grepl('^[0-2][0-9][0-5][0-9]$',time_end)){
          time_start_num = strptime(time_start,format = "%H%M")
          time_end_num = strptime(time_end,format = "%H%M")
          # 数字化时间并计算差值
          duration = as.numeric(difftime(time_end_num,
                                         time_start_num,
                                         units = "min"))
          # 如果duration大于2，则设status为ok
          if(duration >=2){
            check = "ok"
          }
        }
    
        # 更新time_duration_check_table内容
        add_record = c(month = mm,
                       transect = tra,
                       survey_order = order,
                       time_began = time_start,
                       time_end = time_end,
                       duration = duration,
                       status = check)
        time_duration_check_table = rbind(time_duration_check_table,
                                          add_record)
      }
    }
    
    
    # 调查次数核对 ####
    # 统计当前月份中的调查次数
    rep_table = table(mysheets[[yy]][[mm]][,6])
    # 将其赋值给调查次数核对表
    rep_check_table[mm,seq(2,length(rep_table)+1)] = names(rep_table)
    
    # 每月调查次数核对#####
    # 获得每个调查月份的调查样线名称
    tran_info = table(mysheets[[yy]][[mm]][,1])
    # 将每月调查样线名称存放在样线数据核对表中
    tran_data_check_table[i,2:(length(tran_info)+1)] = names(tran_info)
    #按样线遍历数据
    for (j in 1:length(tran_info)) {
      # 2.测试代码，运行时注释#####
      # j = 1
      # 从总数据集中取得指定月份指定样线的调查次数
      tran_name = names(tran_info)[j]
      rep_tran = table(subset(mysheets[[yy]][[mm]],样线==tran_name)[,6])
      
      if ((mm<201104 & length(rep_tran)<5)) {
        order_total = c("1","2","3","4","5")
        tran_order = names(rep_tran)
        order_loss = order_total[!order_total %in% tran_order]
        order_loss = paste(order_loss,collapse=",")
        loss_record = data.frame(month = mm,
                                 transect = tran_name,
                                 order_loss = order_loss)
        tran_rep_check = rbind(tran_rep_check,loss_record)
      }
      if ((mm>201103 & length(rep_tran)<3)) {
        order_total = c("1","2","3")
        tran_order = names(rep_tran)
        order_loss = order_total[!order_total %in% tran_order]
        order_loss = paste(order_loss,collapse=",")
        loss_record = data.frame(month = mm,
                                 transect = tran_name,
                                 order_loss = order_loss)
        tran_rep_check = rbind(tran_rep_check,loss_record)
      }
    }
    
    # 物种名核对，在此核对中，mysheet进行过物种名修改，该修改只去掉了两边空格####
    # (1)物种名去前后空格####
    names_char = mysheets[[yy]][[mm]][,7]
    # 前有空格条件
    cond_space_front = grepl('^ ',names_char)
    # 后有空格条件
    cond_space_back = grepl(' $',names_char)
    cond_final = cond_space_front | cond_space_back
    spp_space = mysheets[[yy]][[mm]][cond_final,7]
    # 如果有满足条件的行提出，并去掉物种名前后的空格
    if(length(spp_space)>0){
      mysheets[[yy]][[mm]][cond_final,7] = trimws(names_char[cond_final],
                                                  which = 'both')
    }
    spp_all = c(spp_all,unique(mysheets[[yy]][[mm]][,7]))
    spp_all = unique(spp_all)
    
    # 进行数据中物种名与IOC物种中文名校对
    # 获得要进行校对的数据（按月）
    data_temp = mysheets[[yy]][[mm]][,c(1,2,7,13)]
    # 如果数据物种名不在IOC列表中则提取出，形成核对物种名核对表
    # 若spp_check_table为空代表物种名无误
    # spp_check函数中已经考虑了物种名为空和'无'的情况，可以令其保留，不予纠错
    spp_recheck_temp = data_temp[!sapply(data_temp$种类,spp_check),]
    spp_check_table = rbind(spp_check_table,spp_recheck_temp)
    
    # 主要要调查人核对 #######
    # 统计当月的调查人
    # print(paste("yy",yy))
    # print(paste("mm",mm))
    # mm = "202311"
    # yy = "2023"
    surveyor_table = table(mysheets[[yy]][[mm]][,13])
    # 赋值给调查人核对表
    surveyor_check_table[mm,seq(2,length(surveyor_table)+1)] = names(surveyor_table)

    # 每列空行核对 ####
    # 若为核对表中值为ok代表核对通过，正数代表数据缺，负数代表数据比month列多
    # 核对思路：获得原始数据的行数，如果后每一列的行数与原始数据中月份的数量一致
    # 代表数据无缺失，否则需要检查和核对缺失
    dim_temp = dim(mysheets[[yy]][[mm]])
    dim_check_table[mm,2:3] = dim_temp
    # (1)检查样线列是否有空值
    if(dim_temp[1] == sum(table(mysheets[[yy]][[mm]][,1]))){
      dim_check_table[mm,4] = 'ok'
    }else{
      dim_check_table[mm,4] = dim_temp[1] - sum(table(mysheets[[yy]][[mm]][,1]))
    }
    # (2)检查日期列是否有空
    if (dim_temp[1] == sum(table(mysheets[[yy]][[mm]][,2]))) {
      dim_check_table[mm,5] = 'ok'
    } else {
      dim_check_table[mm,5] = dim_temp[1]-sum(table(mysheets[[yy]][[mm]][,2]))
    }
    # (3)调查起始时间列是否有空
    if (dim_temp[1] == sum(table(mysheets[[yy]][[mm]][,3]))) {
      dim_check_table[mm,6] = 'ok'
    } else {
      dim_check_table[mm,6] = dim_temp[1]-sum(table(mysheets[[yy]][[mm]][,3]))
    }
    # (4)调查结束时间列是否有空
    if (dim_temp[1] == sum(table(mysheets[[yy]][[mm]][,4]))) {
      dim_check_table[mm,7] = 'ok'
    } else {
      dim_check_table[mm,7] = dim_temp[1]-sum(table(mysheets[[yy]][[mm]][,4]))
    }
    # (5)调查天气列是否有空
    if (dim_temp[1] == sum(table(mysheets[[yy]][[mm]][,5]))) {
      dim_check_table[mm,8] = 'ok'
    } else {
      dim_check_table[mm,8] = dim_temp[1]-sum(table(mysheets[[yy]][[mm]][,5]))
    }
    # (6)调查次数列是否有空
    if (dim_temp[1] == sum(table(mysheets[[yy]][[mm]][,6]))) {
      dim_check_table[mm,9] = 'ok'
    } else {
      dim_check_table[mm,9] = dim_temp[1]-sum(table(mysheets[[yy]][[mm]][,6]))
    }
    # (7)调查物种列是否有空
    if (dim_temp[1] == sum(table(mysheets[[yy]][[mm]][,7]))) {
      dim_check_table[mm,10] = 'ok'
    } else {
      dim_check_table[mm,10] = dim_temp[1]-sum(table(mysheets[[yy]][[mm]][,7]))
    }
    # (8)多度列是否有空
    if (dim_temp[1] == sum(table(mysheets[[yy]][[mm]][,8]))) {
      dim_check_table[mm,11] = 'ok'
    } else {
      ## 把物种为空的记录去掉，保留真实多度有问题的记录
      # 物种记录为"无"的记录数据，这些需要在多度记录中去掉
      if(is.na(table(mysheets[[yy]][[mm]][,7])["无"])){
        num_sp_none = 0
      }else{
        num_sp_none = as.numeric(table(mysheets[[yy]][[mm]][,7])["无"])
      }
      num_check_abun = dim_temp[1]-sum(table(mysheets[[yy]][[mm]][,8]))-num_sp_none
      # 如果多度为空的与物种记录为无的数量一致，则代表无需再次核对
      if(num_check_abun==0){
        dim_check_table[mm,11] = "ok"
        # 否则剩下的就是多度有问题的记录，需要再次核对
      }else{
        dim_check_table[mm,11] = num_check_abun
      }
    }
    # (9)主调查人列是否有空
    if (dim_temp[1] == sum(table(mysheets[[yy]][[mm]][,13]))) {
      dim_check_table[mm,12] = 'ok'
    } else {
      dim_check_table[mm,12] = dim_temp[1]-sum(table(mysheets[[yy]][[mm]][,13]))
    }
  }
  # 返回所有的校对列表，以便进行原始数据校对和修改
  return(list(day_check = day_check_table,
              time_check = time_check_table,
              time_duration_check_table = time_duration_check_table[-1,],
              rep_check = rep_check_table,
              surveyor_check = surveyor_check_table,
              dim_check = dim_check_table,
              tran_check = tran_data_check_table,
              tran_rep_check = tran_rep_check,
              spp_check = spp_check_table,
              spp_all_raw = spp_all,
              mysheets = mysheets
              ))
}

