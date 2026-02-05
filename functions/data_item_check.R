# 基础功能库加载
library(stringr)
require(openxlsx)
library(tidyverse)

# 确保字符串不会自动转为因子，保障合并效率
options(stringsAsFactors = FALSE)

# 创建空的核对矩阵函数 #####
# 目的：预分配一个以月份为行名的矩阵，用于存放核对结果。
# 逻辑：
# 1. 创建全是 NA 的矩阵。
# 2. 将第一列设为月份名，方便后续查阅。
# monthName调查月份的向量，num_col代表除了monthName外的列数
create_check_matrix <- function(monthName, num_col) {
  matrix_pre <- matrix(NA, nrow = length(monthName), ncol = num_col + 1)
  rownames(matrix_pre) <- monthName
  matrix_pre[, 1] <- monthName
  return(matrix_pre)
}

######## 数据核对代码区##########################################
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
data_check <- function(monthName, mysheets) {
  if (!exists("TILBirdList_raw")) {
    stop("错误: 全局环境中未找到 'TILBirdList_raw' 数据框，无法进行物种名校验。请先加载该数据。")
  }
  # 创建日期核对表：day_check_table
  day_check_table <- create_check_matrix(monthName, 3)
  # 重命名列名
  colnames(day_check_table) <- c(
    "month",
    "survey_date_min",
    "survey_date_max",
    "status"
  )
  survey_date_table <- cbind(year = names(monthName), month = monthName)

  # 创建调查时间核对表：time_check_table
  time_check_table <- create_check_matrix(monthName, 5)
  # 重命名列名
  colnames(time_check_table) <- c(
    "month", "time_began_min", "time_began_max",
    "time_end_min", "time_end_max", "status"
  )

  # 创建每条样线每次调查持续时间核对表：time_duration_check_table
  # 这里为了提高内存效率使用list的方法进行
  duration_list <- list()

  # 创建调查次数核对表：rep_check_table
  rep_check_table <- create_check_matrix(monthName, 15)

  # 创建调查人核对表：surveyor_check_table
  surveyor_check_table <- create_check_matrix(monthName, 30)

  # 创建样线及重复次数核对表
  # 这里的列数是按最大的样线数设定的（有些年份会把植物岛的调查样线加进来）
  # 这里设定128是因为含有字母的样线约有64条，翻一倍以防止有些人
  # 录入时把大写字母写成小写字母
  tran_data_check_table <- create_check_matrix(monthName, 128)
  # 样线重复数核对表，若返回character(0)代表没有问题,

  # 若样线重复调查有问题，则返回调查月份、有问题的样线名和有问题的调查次序
  # 例："200801 J1 1" "200901 B1-1 2,3"
  # 为了提高内存效率使用list方法
  tran_rep_list <- list()

  # 鸟类物种名核对
  # 物种名核对表：spp_check_table
  # 该表格是所有有问题的物种记录名，会有重复，因为不同的记录者
  # 会记录到相同有问题的物种名
  # 为了提高内存效率使用list方法
  spp_check_list <- list()

  # 千岛湖鸟类调查中记录到的任何鸟名向量（包含错记、无法识别的鸟名）
  # 几乎没有重复
  spp_all_list <- list()


  # 创建每列空行核对表：dim_check_table
  dim_check_table <- create_check_matrix(monthName, 11)
  colnames(dim_check_table) <- c(
    "month", "nrow", "ncol",
    "transect", "day", "began_time", "end_time",
    "weather", "repetition", "species",
    "abundance", "surveyor"
  )
  # 遍历所有调查月份
  for (i in 1:length(monthName)) {
    # [调试专用代码块] #####
    # i <- 103 # 手动指定第几个月

    yy <- names(monthName[i])
    # 获得月
    mm <- monthName[i]

    # debug #####
    # print(paste("i",i))
    # print(paste("month",mm))

    # 提取当月数据到临时变量，以减少提取的重复提高效率
    data_curr <- as.data.frame(mysheets[[yy]][[mm]])

    # 调查日期核对 ####
    # 统计当前月份的调查起始和结束日期，并赋值给日期核对表
    # 提取调查日期列
    date_vec <- data_curr[, 2]
    # 进行格式检查，过滤掉NA和空字符，取唯一日期值
    date_valid <- unique(date_vec[!is.na(date_vec) & date_vec != ""])
    
    
    if (length(date_valid) == 0) {
      # 如果全是空，直接设为 "无"
      day_check_table[mm, "survey_date_min"] <- "无"
      day_check_table[mm, "survey_date_max"] <- "无"
      day_check_table[mm, "status"] <- "check"
    } else {
      # 获得日期范围
      survey_range <- range(date_valid)
      # 提取调查开始日期
      day_check_table[mm, ]["survey_date_min"] <- survey_range[1]
      # 提取调查结束日期
      day_check_table[mm, ]["survey_date_max"] <- survey_range[2]
    }
    # 进行起始日期和结束日期月份判断，如果前后一致核对表状态填为ok
    # 否则显示check
    condition1 <- substr(survey_range[1], 5, 6) == substr(mm, 5, 6)
    condition2 <- substr(survey_range[2], 5, 6) == substr(mm, 5, 6)
    if (condition1 & condition2) {
      day_check_table[mm, ]["status"] <- "ok"
    } else {
      day_check_table[mm, ]["status"] <- "check"
    }

    # 调查时间核对####
    # 获得当前月份数据调查时间的起始值范围
    time_began_vec <- data_curr[, 3]
    # 过滤掉NA和空字符，取唯一日期值，并获得范围
    time_began_valid <- unique(time_began_vec[!is.na(time_began_vec) & time_began_vec != ""])
    if (length(time_began_valid) == 0) {
      time_check_table[mm, "time_began_min"] <- "无"
      time_check_table[mm, "time_began_max"] <- "无"
    } else {
      began_range <- range(time_began_valid)
      time_check_table[mm, "time_began_min"] <- began_range[1]
      time_check_table[mm, "time_began_max"] <- began_range[2]
    }
    
    # 获得当前月份数据调查时间的结束值范围
    time_end_vec <- data_curr[, 4]
    # 过滤掉NA和空字符，取唯一日期值，并获得范围
    time_end_valid <- unique(time_end_vec[!is.na(time_end_vec) & time_end_vec != ""])
    if (length(time_end_valid) == 0) {
      time_check_table[mm, "time_end_min"] <- "无"
      time_check_table[mm, "time_end_max"] <- "无"
    } else {
      end_range <- range(time_end_valid)
      time_check_table[mm, "time_end_min"] <- end_range[1]
      time_check_table[mm, "time_end_max"] <- end_range[2]
    }
    
    # 如果核对表中有无，在状态列显示check
    # 否则为ok
    if ("无" %in% time_check_table[mm, ]) {
      time_check_table[mm, ]["status"] <- "check"
    } else {
      time_check_table[mm, ]["status"] <- "ok"
    }

    # 检查每次调查的起始时间与终止时间差 #####
    # 获得当月调查次数
    survey_order <- unique(data_curr$调查次数)
    # 遍历每次调查的数据
    for (order in survey_order) {
      # debug ####
      # print(paste("order",order))
      # order = 5
      # 获得每次调查的数据
      data_tmp <- data_curr[data_curr$调查次数 == order, ]
      # 获得每次调查中的样线名
      transect_names <- unique(data_tmp$样线)
      # 遍历每条样线
      for (tra in transect_names) {
        # 定义持续时间
        duration <- "NA"
        # 定义检查状态
        check <- "check"

        # 测试代码####
        # tra = "S100"
        # 提取当前样线的时间数据
        sub_data_tra <- data_tmp[data_tmp$样线 == tra, ]
        # 获得惟一的起始时间
        time_start_raw <- unique(sub_data_tra$调查起始时间)
        # 过滤掉可能的空值
        time_start_raw <- time_start_raw[!is.na(time_start_raw) & time_start_raw != ""]
        # 将起始调查时间转为时间对象格式
        if (length(time_start_raw) == 0) {
          time_start <- "无"
        } else {
          time_start <- paste(time_start_raw, collapse = ",")
        }

        # 获得终止时间，并转化为时间对象
        # 提取调查结束时间
        time_end_raw <- unique(sub_data_tra$调查结束时间)
        # 过滤掉可能的空值
        time_end_raw <- time_end_raw[!is.na(time_end_raw) & time_end_raw != ""]
        if (length(time_end_raw) == 0) {
          time_end <- "无"
        } else {
          time_end <- paste(time_end_raw, collapse = ",")
        }

        # 如果起始和终止时间都不是无
        # print(paste("mm",mm))
        # print(paste("i",i))
        # print(paste("order",order))
        # print(paste("tran",tra))
        # print(paste('start',paste(time_start,collapse=',')))
        # print(paste('end',time_end))
        # 只有当起始时间和终止时间皆不为无时，才可以计算调查持续时间
        if (grepl("^[0-2][0-9][0-5][0-9]$", time_start) & grepl("^[0-2][0-9][0-5][0-9]$", time_end)) {
          time_start_num <- strptime(time_start, format = "%H%M")
          time_end_num <- strptime(time_end, format = "%H%M")
          # 数字化时间并计算差值
          duration <- as.numeric(difftime(time_end_num, time_start_num, units = "min"))
          # 如果duration大于2，则设status为ok
          if (!is.na(duration) && duration >= 2) {
            check <- "ok"
          }
        }
        # 使用list的方式提高效率
        add_record <- data.frame(
          month = mm, 
          transect = tra, 
          survey_order = order,
          time_began = time_start, 
          time_end = time_end,
          duration = duration, 
          status = check, 
          stringsAsFactors = FALSE
        )
        # 将结果存入duration_list列表
        duration_list[[length(duration_list) + 1]] <- add_record
      }
    }

    # 调查次数核对 ####
    # 统计当前月份中的调查次数
    rep_vals <- sort(unique(data_curr[, 6]))
    # 将其赋值给调查次数核对表
    rep_check_table[mm, seq(2, length(rep_vals) + 1)] <- rep_vals

    # 每月调查次数核对#####
    # 获得每个调查月份的调查样线名称
    tran_vals <- sort(unique(data_curr[, 1]))
    # 将每月调查样线名称存放在样线数据核对表中
    # debug ####
    # print(paste("length of tran_info:",length(names(tran_info))))
    # print(paste("ncol of tran_info:",ncol(tran_data_check_table)))
    # print(paste("seq_col:",(length(tran_info)+1)))
    tran_data_check_table[i, 2:(length(tran_vals) + 1)] <- tran_vals

    # 区别调查次数的条件计算
    # 201104之前是每月5次调查，201104之后每月3
    is_old_period <- as.numeric(mm) < 201104
    threshold <- if (is_old_period) 5 else 3
    target_orders <- if (is_old_period) c("1", "2", "3", "4", "5") else c("1", "2", "3")

    # 按样线遍历数据
    for (tra in tran_vals) {
      # 我们只需要知道某个样线有多少个不同的调查次数
      # 提取该样线对应的所有调查次数
      curr_orders <- data_curr[data_curr[, 1] == tra, 6]
      # 计算去重后的数量
      rep_count <- length(unique(curr_orders))
      # 如果调查次数不对，就统计缺少的调查次编号
      if (rep_count < threshold) {
        # 已经有的调查次编号
        exist_orders <- unique(curr_orders)
        # 与目标调查次不同的调查次编号（需要核对的）
        order_loss <- setdiff(target_orders, exist_orders)
        # 将缺少的调查次编号写入tran_rep_list统计列表
        tran_rep_list[[length(tran_rep_list) + 1]] <- data.frame(
          month = mm,
          transect = tra,
          # 将向量写成字符的形式
          order_loss = paste(order_loss, collapse = ","),
          stringsAsFactors = FALSE
        )
      }
    }

    # 物种名核对，在此核对中，mysheet进行过物种名修改，该修改只去掉了两边空格####
    # 对物种名列去左右空格
    data_curr[, 7] <- trimws(data_curr[, 7], which = "both")
    # 获得所有调查中出现记录的物种名并存入spp_all_list列表中
    spp_all_list[[i]] <- unique(data_curr[, 7])
    # 进行物种名是否合理判断
    # 逻辑:
    # 有效 = (是NA) 或 (是"无") 或 (在名录中)
    # 无效 = (!is.na) 且 (!="无") 且 (!在名录中)
    spp_col <- data_curr[, 7]
    # 已有物种名
    is_valid <- is.na(spp_col) | spp_col == "无" | (spp_col %in% TILBirdList_raw$Chinese)
    # 不存在的物种名行号
    invalid_rows <- !is_valid
    # 如果有不存在的物种名，提取对应的信息存入spp_check_list列表中
    if (any(invalid_rows)) {
      spp_recheck_temp <- data_curr[invalid_rows, c(1, 2, 7, 13)]
      spp_check_list[[length(spp_check_list) + 1]] <- spp_recheck_temp
    }

    # 主要要调查人核对 #######
    # 统计当月的调查人
    surv_vals <- unique(data_curr[, 13])
    # 赋值给调查人核对表
    surveyor_check_table[mm, seq(2, length(surv_vals) + 1)] <- surv_vals

    # 每列空行核对 ####
    # 若为核对表中值为ok代表核对通过，正数代表数据缺，负数代表数据比month列多
    # 核对思路：获得原始数据的行数，如果后每一列的行数与原始数据中月份的数量一致
    # 代表数据无缺失，否则需要检查和核对缺失
    dim_temp <- dim(data_curr)
    dim_check_table[mm, 2:3] <- dim_temp
    # (1)检查样线列是否有空值
    valid_count_transect <- sum(!is.na(data_curr[, 1]) & data_curr[, 1] != "")
    if (dim_temp[1] == valid_count_transect) {
      dim_check_table[mm, 4] <- "ok"
    } else {
      dim_check_table[mm, 4] <- dim_temp[1] - valid_count_transect
    }

    # (2)检查日期列是否有空
    valid_count_date <- sum(!is.na(data_curr[, 2]) & data_curr[, 2] != "")
    if (dim_temp[1] == valid_count_date) {
      dim_check_table[mm, 5] <- "ok"
    } else {
      dim_check_table[mm, 5] <- dim_temp[1] - valid_count_date
    }

    # (3)调查起始时间列是否有空
    valid_count_start_time <- sum(!is.na(data_curr[, 3]) & data_curr[, 3] != "")
    if (dim_temp[1] == valid_count_start_time) {
      dim_check_table[mm, 6] <- "ok"
    } else {
      dim_check_table[mm, 6] <- dim_temp[1] - valid_count_start_time
    }

    # (4)调查结束时间列是否有空
    valid_count_end_time <- sum(!is.na(data_curr[, 4]) & data_curr[, 4] != "")
    if (dim_temp[1] == valid_count_end_time) {
      dim_check_table[mm, 7] <- "ok"
    } else {
      dim_check_table[mm, 7] <- dim_temp[1] - valid_count_end_time
    }
    # (5)调查天气列是否有空
    valid_count_weather <- sum(!is.na(data_curr[, 5]) & data_curr[, 5] != "")
    if (dim_temp[1] == valid_count_weather) {
      dim_check_table[mm, 8] <- "ok"
    } else {
      dim_check_table[mm, 8] <- dim_temp[1] - valid_count_weather
    }
    # (6)调查次数列是否有空
    valid_count_survey_rep <- sum(!is.na(data_curr[, 6]) & data_curr[, 6] != "")
    if (dim_temp[1] == valid_count_survey_rep) {
      dim_check_table[mm, 9] <- "ok"
    } else {
      dim_check_table[mm, 9] <- dim_temp[1] - valid_count_survey_rep
    }

    # (7)调查物种列是否有空
    valid_count_species <- sum(!is.na(data_curr[, 7]) & data_curr[, 7] != "")
    if (dim_temp[1] == valid_count_species) {
      dim_check_table[mm, 10] <- "ok"
    } else {
      dim_check_table[mm, 10] <- dim_temp[1] - valid_count_species
    }
    # (8)多度列是否有空
    # 特殊逻辑：多度列非空数 + 物种为"无"的行数应该等于总行数
    valid_count_abun <- sum(!is.na(data_curr[, 8]) & data_curr[, 8] != "")

    if (dim_temp[1] == valid_count_abun) {
      dim_check_table[mm, 11] <- "ok"
    } else {
      # 统计物种列(第7列)中为"无"的数量
      num_sp_none <- sum(data_curr[, 7] == "无", na.rm = TRUE)
      # 缺失数 = 总行数 - 有多度的行数 - 物种为无的行数
      num_check_abun <- dim_temp[1] - valid_count_abun - num_sp_none

      if (num_check_abun == 0) {
        dim_check_table[mm, 11] <- "ok"
      } else {
        dim_check_table[mm, 11] <- num_check_abun
      }
    }

    # (9)主调查人列是否有空
    valid_count_surveyor <- sum(!is.na(data_curr[, 13]) & data_curr[, 13] != "")
    if (dim_temp[1] == valid_count_surveyor) {
      dim_check_table[mm, 12] <- "ok"
    } else {
      dim_check_table[mm, 12] <- dim_temp[1] - valid_count_surveyor
    }
  }
  
  # 统一处理物种全集：将list合并并去重转为向量
  spp_all <- unique(unlist(spp_all_list))
  
  # 将duration_list，tran_rep_list和spp_check_list分别进行统一合并
  # 即将list转换为数据框，这样在循环中的效率比较高
  # 调查持续时间检查
  time_duration_check_table <- if (length(duration_list) > 0) do.call(rbind, duration_list) else data.frame()
  # 样线重复调查次数检查
  tran_rep_check <- if (length(tran_rep_list) > 0) do.call(rbind, tran_rep_list) else data.frame()
  # 物种名核对检查
  spp_check_table <- if (length(spp_check_list) > 0) do.call(rbind, spp_check_list) else data.frame()

  # 只输出需要检查的记录
  time_duration_check_table <- time_duration_check_table %>% 
    as.data.frame() %>%
    filter(status == "check")
  
  # 只输出需要核对的记录
  day_check_table <- day_check_table %>%
    as.data.frame() %>%
    filter(status == "check")

  # 只输出需要检查的记录
  time_check_table <- time_check_table %>%
    as.data.frame() %>%
    filter(status == "check")

  # 返回所有的校对列表，以便进行原始数据校对和修改
  return(list(
    day_check = day_check_table,
    time_check = time_check_table,
    time_duration_check_table = time_duration_check_table,
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
