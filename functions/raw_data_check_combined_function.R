# 功能函数必须包
library(stringr)
require(openxlsx)
library(tidyverse)

# 判断excel文件名错误 #####
# 被本文件中combined_all_data函数所调用
# excel_dir代表调查原始excel所在目录的绝对路径
file_name_error = function(excel_dir) {
  # 利用正则表达式匹配路径中的文件名，检查文件名是否符合
  # "yyyymm-yyyymm.xlsx"的命名规则
  files = list.files(excel_dir)
  res = unlist(lapply(files, function(X)
    grepl('^[0-9]{6}-[0-9]{6}.xlsx$', X)))
  # 如果有不符合命令规则的文件名，返回TRUE
  return(files[!res])
}


# 判断sheet文件名错误并返回调查日期向量 #####
# 被combined_all_data函数所调用
# each_excel_path:每个excel文件的绝对路径
sheet_name_error = function(each_excel_path) {
  sheet_names = unlist(lapply(each_excel_path, function(X)
    openxlsx::getSheetNames(X)))
  # 利用正则表达式匹配sheet名，检查是否符合"yyyymm"的命名规则
  res = unlist(lapply(sheet_names, function(X)
    grepl('^[0-9]{6}$', X)))
  if (length(sheet_names[!res]) > 0) {
    return(list(
      raw_data = each_excel_path,
      dates = sheet_names,
      date_errors = sheet_names[!res]
    ))
  } else{
    return(list(dates = sheet_names, date_errors = NA))
  }
}

# 构建数据的调查日期 ####
# path_raw_data为原始excel表格所在目录路径
# 暂时未被调用
construct_survey_time_range = function(path_raw_data) {
  # 进行文件名规范检查
  errors = file_name_error(path_raw_data)
  # 如果有错误，提示
  if (length(errors) > 0) {
    print('如下文件名有错误，请核对：')
    print(errors)
    # 如果没有，返回所有sheet的名字
  } else{
    # 获得所有的调查年月
    dates = obtain_all_sheet_names(path_raw_data)
    return(dates)
  }
}

# 统一主要调查者函数，取主要调查者第一位
# 处理对象为每次调查的数据表
main_surveyor_unify = function(X) {
  for (i in 1:nrow(X)) {
    if (X[i, ]$`调查人(主要)` != "无" & !is.na(X[i, ]$`调查人(主要)`)) {
      surveyors_main = unlist(strsplit(X[i, ]$`调查人(主要)`, "、"))
      if (length(surveyors_main) >= 2) {
        X[i, ]$`调查人(主要)` = surveyors_main[1]
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
  # 测试代码
  # excel_path = list.files("./raw_data/20241010",
  #                         full.names = T)[1]
  # 获得所有sheet的名称
  sheet_names = openxlsx::getSheetNames(excel_path)
  # 统一后所有数据表的列名
  # 从2023年起添加了录入人列，故在此对数据列进行修改添加
  col_names = c(
    "样线",
    "调查日期",
    "调查起始时间",
    "调查结束时间",
    "天气",
    "调查次数",
    "种类",
    "数量",
    "记录数量类型",
    "距离",
    "活动类型",
    "小生境",
    "调查人(主要)",
    "调查人(其他)",
    "备注",
    "录入人"
  )
  # 获得所有数据并集合形成list对象
  # 为2023年之前的数据添加录入人列，并赋值为NA
  list_survey_data = lapply(sheet_names, function(X) {
    # 读取每个excel表的每个sheet
    # excel_path = '/home/zengdi/R/TILbird_database/TILBirdDataBase/raw_data/20241010/201704-201801.xlsx'
    # print(X)
    excel_tmp = openxlsx::read.xlsx(excel_path, sheet = X)
    # 如果没有录入人列，则添加该列并赋值为NA
    if (!("录入人" %in% colnames(excel_tmp))) {
      n_num = nrow(excel_tmp)
      excel_tmp = excel_tmp %>%
        mutate(录入人 = as.character(rep(NA, n_num)), 
               备注 = as.character(备注))
    }
    return(excel_tmp)
  })
  # 统一所有数据表的列名
  list_survey_data = lapply(list_survey_data, setNames, col_names)
  list_survey_data = lapply(list_survey_data, function(X)
    X[, col_names])
  # 统一主要调查人，取第一主要调查人
  list_survey_data = lapply(list_survey_data, main_surveyor_unify)
  # 对list中的元素按年月命名
  names(list_survey_data) = sheet_names
  list_survey_data = lapply(names(list_survey_data), function(name) {
    df = list_survey_data[[name]]
    df$month =  name  # 将元素的名字作为一列添加
    return(df)
  })
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
  # excel_dir = "./raw_data/20241010"
  # 检测excel所在目录路径是否存在
  if (!file.exists(excel_dir)) {
    stop("excel_dir目录不存在，请检查！")  # 如果路径不存在，停止运行并报错
  }
  
  # 建立所有数据存放list
  alldata = list()  # 创建一个空列表，用于存放所有的Excel数据
  
  # 检查所有原始数据excel文件名是否符合规范
  excel_errors = file_name_error(excel_dir)  # 调用函数检查Excel文件名是否符合规范
  
  # 获取所有数据excel文件的路径
  excels_path = list.files(excel_dir, full.names = TRUE)  # 获取excel_dir目录下所有Excel文件的完整路径
  
  # 检查所有sheet的名称是否符合规范，并返回调查日期（即sheet名）
  sheet_res = sheet_name_error(excels_path)  # 调用函数检查每个Excel文件中Sheet的名称是否符合规范
  
  # 如果有错误，返回有错误的原始数据excel表和sheet名
  if (length(excel_errors) > 0) {
    cat("以下Excel文件名有问题：\n", paste(excel_errors, collapse = "\n"), "\n")  # 打印出所有有问题的Excel文件名
    stop("Excel文件名有问题，请检查错误！")  # 停止运行并提示文件名有问题
  }
  
  if (!is.na(sheet_res[['date_errors']])) {
    date_errors = sheet_res[['date_errors']]  # 获取所有有问题的Sheet名
    date_errors_with_files = map_chr(date_errors, function(sheet_name) {
      # 查找包含有问题的Sheet名的Excel文件路径
      file_with_issue = excels_path[which(map_lgl(excels_path, ~ sheet_name %in% readxl::excel_sheets(.x)))]
      # 生成包含Sheet名和对应Excel文件名的字符串
      paste0("Sheet名: ", sheet_name, " (文件: ", basename(file_with_issue), ")")
    })
    cat("以下Sheet名有问题：\n", paste(date_errors_with_files, collapse = "\n"), "\n")  # 打印出有问题的Sheet名及其对应的文件名
    stop("Sheet名有问题，请检查错误！")  # 停止运行并提示Sheet名有问题
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
    # print(excels_path[i])
    mysheets.temp = read_data_in_allsheets(excels_path[i])
    for(j in mysheets.temp){
      total_data_table = rbind(total_data_table,j)
    }
    alldata = c(alldata,mysheets.temp)
  }

  write.xlsx(total_data_table,
             "./check_tables/total_raw_data_table.xlsx")
  
  # 按年份将数据表分类
  mysheets = map(yearName, function(year) {
    # 获取当前年份对应的所有调查月份
    dates_in_year = monthName[names(monthName) == year]
    # 根据调查月份的名称，将所有对应的数据表从alldata中提取出来，并使用调查月份名称作为列表的名称
    set_names(map(dates_in_year, ~ alldata[[.x]] %>% select(-month)), dates_in_year)
  }) %>% set_names(yearName)  # 最终将每年的数据列表命名为对应的年份
  
  # 返回所有数据及调查年月组成的列表
  return (list(mysheets = mysheets,
               monthName = monthName,
               yearName = yearName,
               raw_total_data_table = total_data_table))
}