# 功能函数必须包
library(stringr)
require(openxlsx)
library(tidyverse)
library(readxl)

# 判断excel文件名错误 #####
# 被本文件中combined_all_data函数所调用
# excel_dir代表调查原始excel所在目录的绝对路径
file_name_error <- function(excel_dir) {
  # 利用正则表达式匹配路径中的文件名，检查文件名是否符合
  # "yyyymm-yyyymm.xlsx"的命名规则
  files <- list.files(excel_dir)
  res <- grepl("^[0-9]{6}-[0-9]{6}.xlsx$", files)
  # 如果有不符合命令规则的文件名，返回TRUE
  return(files[!res])
}

# 判断sheet文件名错误并返回调查日期向量 #####
# 被combined_all_data函数所调用
# each_excel_path:每个excel文件的绝对路径
sheet_name_error <- function(each_excel_path) {
  # 构造一个命名列表，方便后续知道哪个文件出错了
  names(each_excel_path) <- basename(each_excel_path)
  # 获取所有 sheet 名
  all_sheets_list <- lapply(each_excel_path, readxl::excel_sheets)
  # 用于收集具体的错误信息（哪个文件的哪个 Sheet 错了）
  all_errors <- list()
  # 用于收集遍历过程中遇到的所有 Sheet 名（暂时叫 valid_dates，实际包含所有读取到的）
  valid_dates <- c()
  # 遍历所有文件的 Sheet 名列表进行检查
  for (fname in names(all_sheets_list)) {
    # 取出当前文件对应的 Sheet 名向量
    sheets <- all_sheets_list[[fname]]
    # 检查 Sheet 名是否严格由6位数字组成
    # （如 "202301" 是 TRUE，"Sheet1" 或 "2023-01" 是 FALSE）。
    is_valid <- grepl("^[0-9]{6}$", sheets)
    # 如果当前文件中存在任何一个不合规的 Sheet 名 (!is_valid 为 TRUE)
    if (any(!is_valid)) {
      # # 提取出那些不合规的 Sheet 名
      error_sheets <- sheets[!is_valid]
      # 将错误信息存入 all_errors 列表，以文件名为 key
      all_errors[[fname]] <- error_sheets
    }
    # 将当前文件的所有 Sheet 名追加到总列表中
    valid_dates <- c(valid_dates, sheets)
  }
  # 如果错误列表的长度大于0，说明发现了错误
  if (length(all_errors) > 0) {
    # 返回所有日期以及具体的错误列表
    return(list(dates = valid_dates, date_errors = all_errors))
  } else {
    # 如果没有错误，date_errors 返回 NA
    return(list(dates = valid_dates, date_errors = NA))
  }
}

# 构建数据的调查日期 ####
# path_raw_data为原始excel表格所在目录路径
# 暂时未被调用
construct_survey_time_range <- function(path_raw_data) {
  # 进行文件名规范检查
  errors <- file_name_error(path_raw_data)
  # 如果有错误，提示
  if (length(errors) > 0) {
    print("如下文件名有错误，请核对：")
    print(errors)
    # 如果没有，返回所有sheet的名字
  } else {
    # 获得所有的调查年月
    dates <- obtain_all_sheet_names(path_raw_data)
    return(dates)
  }
}

# 统一主要调查者函数，取主要调查者第一位
# 处理对象为每次调查的数据表
main_surveyor_unify <- function(X) {
  # 确保列存在
  if (!"调查人(主要)" %in% colnames(X)) {
    return(X)
  }
  target_col <- X$`调查人(主要)`

  # 向量化操作，替代 for 循环
  # 1. 将 NA 替换为 "无" 以便统一处理 (或者跳过 NA)
  # 2. 使用正则表达式直接提取第一个名字 (截取 "、" 之前的内容)
  # 逻辑：如果有顿号，取顿号前的；如果没有，保持原样

  # 创建一个临时向量处理
  processed_col <- target_col

  # 找出非空且非NA的值
  valid_idx <- !is.na(processed_col) & processed_col != "无"

  if (any(valid_idx)) {
    # 使用 sub 函数提取第一个名字 (比 strsplit 快)
    # 匹配规则：遇到第一个 "、" 及其后面的所有内容，替换为空字符串
    processed_col[valid_idx] <- sub("、.*$", "", processed_col[valid_idx])
  }
  X$`调查人(主要)` <- processed_col
  return(X)
}

# 读取指定excel文件中的调查数据 #####
# 读取所有原始数据sheets的数据，
# 对结果list中的数据表按调查年月重命名(sheetname)
# 被combined_all_data函数调用
# excel_path代表每个xlsx文件的绝对路径
read_data_in_allsheets <- function(excel_path) {
  # 测试代码
  # excel_path = list.files("./raw_data/20241010",
  #                         full.names = T)[1]
  # 获得所有sheet的名称
  sheet_names <- readxl::excel_sheets(excel_path)
  # 统一后所有数据表的列名
  # 从2023年起添加了录入人列，故在此对数据列进行修改添加
  col_names <- c(
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
  list_survey_data <- lapply(sheet_names, function(sh_name) {
    # 读取每个excel表的每个sheet
    # excel_path = '/home/zengdi/R/TILbird_database/TILBirdDataBase/raw_data/20241010/201704-201801.xlsx'
    # print(X)
    excel_tmp <- suppressMessages(readxl::read_xlsx(excel_path, sheet = sh_name, col_types = "text"))
    names(excel_tmp) <- chartr("（）", "()", names(excel_tmp))
    # 如果没有录入人列，则添加该列并赋值为NA
    # 补全"录入人"
    if (!("录入人" %in% colnames(excel_tmp))) {
      excel_tmp$录入人 <- NA
    }

    # 补全"备注" (防止读取为逻辑值或数字)
    if ("备注" %in% colnames(excel_tmp)) {
      excel_tmp$备注 <- as.character(excel_tmp$备注)
    } else {
      excel_tmp$备注 <- NA
    }

    # 确保列存在且顺序一致 (处理缺失列的情况，防止报错)
    missing_cols <- setdiff(col_names, colnames(excel_tmp))
    if (length(missing_cols) > 0) {
      excel_tmp[missing_cols] <- NA
    }

    # 截取所需列
    excel_tmp <- excel_tmp[, col_names]

    # 添加月份列
    excel_tmp$month <- sh_name

    return(excel_tmp)
  })

  # 统一所有数据表的列名
  names(list_survey_data) <- sheet_names
  # 统一主要调查人，取第一主要调查人
  list_survey_data <- lapply(list_survey_data, main_surveyor_unify)

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
combined_all_data <- function(excel_dir) {
  # excel_dir = "./raw_data/20241010"
  # 检测excel所在目录路径是否存在
  if (!file.exists(excel_dir)) {
    stop("excel_dir目录不存在，请检查！") # 如果路径不存在，停止运行并报错
  }

  # 建立所有数据存放list
  alldata <- list() # 创建一个空列表，用于存放所有的Excel数据

  # 检查所有原始数据excel文件名是否符合规范
  excel_errors <- file_name_error(excel_dir) # 调用函数检查Excel文件名是否符合规范
  if (length(excel_errors) > 0) {
    cat("以下Excel文件名有问题：\n", paste(excel_errors, collapse = "\n"), "\n")
    stop("Excel文件名有问题，请检查错误！")
  }

  # 获取所有数据excel文件的路径
  excels_path <- list.files(excel_dir, full.names = TRUE) # 获取excel_dir目录下所有Excel文件的完整路径

  # 检查所有sheet的名称是否符合规范，并返回调查日期（即sheet名）
  sheet_res <- sheet_name_error(excels_path) # 调用函数检查每个Excel文件中Sheet的名称是否符合规范
  # 如果有sheet名错的情况
  if (is.list(sheet_res[["date_errors"]])) {
    # 提取sheet名
    err_list <- sheet_res[["date_errors"]]
    # 进行错误sheet名提示
    err_msgs <- c()
    for (fname in names(err_list)) {
      err_msgs <- c(err_msgs, paste0("文件: ", fname, " -> Sheet错误: ", paste(err_list[[fname]], collapse = ",")))
    }
    cat("以下Sheet名有问题：\n", paste(err_msgs, collapse = "\n"), "\n")
    stop("Sheet名有问题，请检查错误！")
  }

  # 使用 lapply 遍历所有文件，返回一个嵌套列表 (List of Lists)
  # nested_data_list[[1]] 是第一个文件的所有 sheet 数据的 list
  nested_data_list <- lapply(excels_path, read_data_in_allsheets)

  # 将嵌套列表展平 (Flatten)，变成一个包含所有 sheet 数据的单层 list
  # alldata 现在的结构：list(sheet1_df, sheet2_df, sheet3_df, ...)
  alldata <- unlist(nested_data_list, recursive = FALSE)

  # 一次性合并所有数据框
  total_data_table <- bind_rows(alldata)

  # 写出原始记录整合后的总表
  if (!dir.exists("./check_tables")) dir.create("./check_tables")
  write.xlsx(total_data_table, "./check_tables/total_raw_data_table.xlsx")

  # 所有的调查年月
  monthName <- sheet_res[["dates"]]
  if (length(monthName) == 0) {
    stop("错误：未检测到任何有效的调查月份（Sheet名），请检查Excel文件内容是否为空！")
  }
  
  # 所有调查年份
  yearNames <- str_sub(monthName, 1, 4)
  yearName <- unique(yearNames)
  # 对所有调查月份按年份命名
  names(monthName) <- yearNames

  # 使用list提高效率
  mysheets <- list()

  # 数据表合并
  for (yr in yearName) {
    # 找到该年份所有的月份
    months_in_yr <- monthName[str_sub(monthName, 1, 4) == yr]
    months_in_yr <- unique(months_in_yr) # 确保唯一

    # 初始化该年的列表
    mysheets[[yr]] <- list()

    for (m in months_in_yr) {
      # 从扁平列表中直接通过名字提取
      # 假设 alldata 的 name 已经是 sheetname (即月份)
      # 如果有重复的月份在不同的excel里，unlist会自动重命名，所以最好通过名字匹配

      # 更安全的方法：直接从 alldata 中找 month 列等于 m 的表？
      # 为了保持您原本的结构 mysheets[[year]][[month]] 是个 dataframe

      # 因为我们在 read_data_in_allsheets 里给 list 命了名 (sheet名)
      # unlist 后名字通常是 "文件名.sheet名" 或 仅 "sheet名" (取决于结构)
      # 最稳健的方式是遍历 alldata

      # 这里采用一个更高效的查找方式：
      # alldata 的名字里包含了 month 信息
      target_idx <- grep(paste0("(^|\\.)", m, "$"), names(alldata))
      if (length(target_idx) > 0) {
        # 如果有多个文件包含同一个月的数据（理论上不应发生），这里取第一个或合并
        # 假设一个月只在一个文件里
        df_temp <- alldata[[target_idx[1]]]
        df_temp$month <- NULL
        mysheets[[yr]][[m]] <- df_temp
      }
    }
  }

  # 返回所有数据及调查年月组成的列表
  return(list(
    mysheets = mysheets,
    monthName = monthName,
    yearName = yearName,
    raw_total_data_table = total_data_table
  ))
}
