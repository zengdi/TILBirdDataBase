# 功能函数必须包
library(stringr)
require(openxlsx)

# 判断excel文件名错误 #####
# 被本文件中combined_all_data函数所调用
# excel_dir代表调查原始excel所在目录的绝对路径
file_name_error = function(excel_dir){
  # 利用正则表达式匹配路径中的文件名，检查文件名是否符合
  # "yyyymm-yyyymm.xlsx"的命名规则
  files = list.files(excel_dir)
  res = unlist(lapply(files, function(X) grepl('^[0-9]{6}-[0-9]{6}.xlsx$',X)))
  # 如果有不符合命令规则的文件名，返回TRUE
  return(files[!res])
}


# 判断sheet文件名错误并返回调查日期向量 #####
# 被combined_all_data函数所调用
# each_excel_path:每个excel文件的绝对路径
sheet_name_error = function(each_excel_path){
  sheet_names = unlist(lapply(each_excel_path,function(X) openxlsx::getSheetNames(X)))
  # 利用正则表达式匹配sheet名，检查是否符合"yyyymm"的命名规则
  res = unlist(lapply(sheet_names, function(X) grepl('^[0-9]{6}$',X)))
  if(length(sheet_names[!res]) > 0){
    return(list(raw_data = each_excel_path, dates = sheet_names,date_errors = sheet_names[!res]))
  }else{
    return(list(dates = sheet_names,date_errors = NA))
  }
}

# 构建数据的调查日期 ####
# path_raw_data为原始excel表格所在目录路径
# 暂时未被调用
construct_survey_time_range = function(path_raw_data){
  # 进行文件名规范检查
  errors = file_name_error(path_raw_data)
  # 如果有错误，提示
  if(length(errors)>0){
    print('如下文件名有错误，请核对：')
    print(errors)
    # 如果没有，返回所有sheet的名字
  }else{
    # 获得所有的调查年月
    dates = obtain_all_sheet_names(path_raw_data)
    return(dates)
  }
}

# 统一主要调查者函数，取主要调查者第一位
# 处理对象为每次调查的数据表
main_surveyor_unify = function(X){
  for(i in 1:nrow(X)){
    if(X[i,]$`调查人(主要)` !="无" & !is.na(X[i,]$`调查人(主要)`)){
      surveyors_main = unlist(strsplit(X[i,]$`调查人(主要)`,"、"))
      if(length(surveyors_main) >= 2){
        X[i,]$`调查人(主要)` = surveyors_main[1]
        if(X[i,]$`调查人(其他)`=="无"){
          X[i,]$`调查人(其他)` = paste(surveyors_main[2])
        }else{
          X[i,]$`调查人(其他)` = paste(surveyors_main[2],X[i,]$`调查人(其他)`,
                              sep="、")
        }
      }
    }
  }
  return(X)
}

# 读取指定excel文件中的调查数据 #####
# 读取所有原始数据sheets的数据，
# 对结果list中的数据表按调查年月重命名(sheetname) 
# 被combined_all_data函数调用
# excel_path代表每个xlsx文件的绝对路径
read_data_in_allsheets = function(excel_path) {
  # excel_path = list.files("~/R/TILbird_database/bird_data_combined/20220207raw_data",
  #                         full.names = T)[1]
  # 获得所有sheet的名称
  sheet_names = openxlsx::getSheetNames(excel_path)
  # 统一后所有数据表的列名
  col_names = c("样线","调查日期","调查起始时间","调查结束时间",
                "天气","调查次数","种类","数量","记录数量类型",
                "距离","活动类型","小生境","调查人(主要)",
                "调查人(其他)","备注")
  # 获得所有数据并集合形成list对象
  list_survey_data = lapply(sheet_names, function(X) openxlsx::read.xlsx(excel_path, sheet = X))
  # 统一所有数据表的列名
  list_survey_data = lapply(list_survey_data, setNames, col_names)
  list_survey_data = lapply(list_survey_data,function(X) X[,col_names])
  # 统一主要调查人，取第一主要调查人
  list_survey_data = lapply(list_survey_data,main_surveyor_unify)
  # 对list中的元素按年月命名
  names(list_survey_data) = sheet_names
  # 返回结果list对象
  return(list_survey_data)
}


# 检查所有的excel文件名及sheet文件是否规范
# 如果没有问题，则进行数据合并
# 返回值为合并数据集和调查年月组成的列表
# 生成所有调查数据的集合，以年和调查日期为关键字
# 可以year，sheetname为关键字调取：
# 例1，mysheets[['2013']]调取2013年所有的调查数据
# 例2，mysheets[['2015']][['201501']]调取2015年1月调查数据
# TODO: 如何在显示有问题的Sheet同时也显示Excel文件名方便查找
# 被主代码直接调用
# excel_dir:调查原始excel所在目录的绝对路径
combined_all_data = function(excel_dir){
  # excel_dir = "~/R/TILbird_database/bird_data_combined/20220205raw_data"
  # 检测excel所在目录路径是否存在
  if (! file.exists(excel_dir)){
    stop("excel_dir目录不存在，请检查！")
  }
  # 建立所有数据存放list
  alldata = list()
  # 检查所有原始数据excel文件名是否符合规范
  excel_errors = file_name_error(excel_dir)
  # 获所有数据excel文件的路径
  excels_path = list.files(excel_dir,full.names = T)
  # 检查所有sheet的名称是符合规范，并返回调查日期（即sheet名）
  sheet_res = sheet_name_error(excels_path)
  # 如果有错误，返回有错误的原始数据excel表和sheet名
  if(length(excel_errors)>0){
    print('以下Excel文件名有问题：')
    print(excel_errors)
    stop("Excel文件名有问题，请检查错误！")
  }
  if(!is.na(sheet_res['date_errors'])){
    print("以下Sheet名有问题：")
    print(sheet_res['date_errors'])
    stop("Sheet名有问题，请检查错误！")
  }
  # 所有的调查年月
  monthName = unlist(sheet_res['dates'])
  # 所有调查年份
  yearNames = str_sub(monthName,1,4)
  yearName = unique(yearNames)
  # 对所有调查月份按年份命名
  names(monthName) = yearNames
  total_data_table = data.frame()
  # 数据表合并
  for (i in 1:length(excels_path)) {
    mysheets.temp = read_data_in_allsheets(excels_path[i])
    for(i in mysheets.temp){
      total_data_table = rbind(total_data_table,i)
    }
    alldata = c(alldata,mysheets.temp)
  }

  write.xlsx(total_data_table,
             "./check_tables/total_raw_data_table.xlsx")
  mysheets = list()
  for (year in yearName){
    data_focal_year = names(alldata)[grep(year,names(alldata))]
    for (date in data_focal_year){
      mysheets[[year]][[date]] = alldata[[date]]
    }
  }
  # 返回所有数据及调查年月组成的列表
  return (list(mysheets = mysheets, 
               monthName = monthName, 
               yearName = yearName))
}



