# 1. 进行数据重构的函数#####
# 将原表中中文名转换为拉丁名，只保留原始的6列，即
# 样线名、日期、时间、调查次数、物种、多度和调查月份
# 重构
# mysheets_new = list()
data_reconstruct = function(dataset, end_year) {
  # 1. 测试代码，正式代码中应注释掉 #####
  # dataset = mysheets
  # end_year = 2021
  
  # 维度校对表dim_check_new，原变量名：breed_dim_check
  dim_check_new = data.frame(check_list[['dim_check']])[, 1:3]
  # 记录调查中所有的物种名
  spp_all = character()
  # 记录调查中所有的样线名
  transect_all = character()
  # 经过调查次数和调查月份调整的数据
  breed_dataframe = data.frame()
  # 调查重复次数
  rep_seq = c()
  
  
  # 确定调查次数
  # 2007-2010年的调查次数为1-5，2011-至今的调查次数为1-3
  start_year = 2007
  midpoint_year = 2011 # 4月起由5遍调查改为3遍调查
  # 每月最大重复次数
  rep_max = c()
  # 确认重复次数，并记录不符合5次或3次调查的月份
  rep_error_5 = c()
  rep_error_3 = c()
  #rep_max_now = 0
  
  for (j in 1:length(monthName)) {

    yy = names(monthName[j])
    mm = monthName[j]
    
    # 2.测试代码，正式代码中应注释掉  ####
    # yy = "2010"
    # mm="201006"

    # 获得当月调查数据集
    data_month_temp = mysheets[[yy]][[mm]]
    
    # 对当前调查月份的物种名进行操作
    # （0）获得为空或者为无的记录
    # （1）判断数量列值是否为空或0
    # （2）如果为0或空，则将物种填充无，若是'无'则保持不动
    # （3）如果不为0，则将空或'无'填充为"未知物种"
    # （4）再和物种名录信息表进行匹配
    # 【加一个操作，如果这次调查只有1条记录，且为无，则进行上述操作
    #   如果这次调查有多条记录，但某些记录出现无或NA，则删除这条记录】
    
    # 考虑有些岛的有些调查并没有调查到物种
    # 目前的bug是，没有调查到物种的数据会被去掉了，就会损失调查人、天气等信息（已解决）
    # 以202111的I10为例，其第1次调查没有记录到物种信息，
    # 就看final_dataframe中会不会出现该信息，以判断这段代码修改是否完善
    
    for (k in 1:nrow(data_month_temp)) {
      # 获得每条数据
      record_tmp = data_month_temp[k, ]
      # 判断物种信息是否为"空"，若为空则修改为无令其进入
      # record_sp_abun_check_table中，方便后续数据核对和补充
      if (is.na(record_tmp$种类) | is.null(record_tmp$种类) | record_tmp$种类=="无") {
        data_month_temp[k, ]$种类  = "无"
      }
    }
    
    # 获得当前调查月份下所有**原始记录中的物种名**,第7列为物种名
    spp_name_month_raw = data.frame(data_month_temp[, 7])[, 1]
    
    # 将**原始数据的中文名**与包括所有物种名的鸟类信息名录TILBirdList_raw匹配
    # 转换为对应的拉丁名,如果匹配不上，则用NA表示
    spp_name_month = as.character(TILBirdList_raw$LatinWJ[match(spp_name_month_raw,
                                                                   TILBirdList_raw$Chinese)])
    #cbind(name.month,latin.name.month)
    # 获得**原始数据中文名**与TILBirdList_raw中中文名匹配成功的物种名索引
    # 以便提取有效物种的数据
    spp_match_idx = spp_name_month_raw %in% TILBirdList_raw$Chinese
    
    # 基于当月调查数据集中的1,2,3,4,5,6,8,13列和千岛湖鸟类物种名录构建新的数据表
    # 1，2，3，4列分别为样线名称、调查日期、调查起始时间、调查终止时间;
    # 5为天气，6列为调查次数、8列为多度、13列为主要调查人;
    data_month_new = data.frame(
      data_month_temp[, c(1, 2, 3, 4)],
      data_month_temp[, c(5, 6)],
      spp_name_month,
      data_month_temp[, 8],
      data_month_temp[, 13]
    )
    data_month_new = as.data.frame(data_month_new, stringsAsFactors = F)
    colnames(data_month_new) = c(
      'Transect',
      'Day',
      'T_start',
      'T_end',
      'Weather',
      'Rep',
      'Species',
      'Abun',
      "Surveyor"
    )
    # 提取拉丁名匹配上的记录（有效物种数据）作为该月的最终数据
    data_month_new = data_month_new[spp_match_idx, ]
    
    # 提取201101及其之前每月前5次调查的数据；提取201104及其以后每月前3次调查的数据
    if (as.numeric(yy) <= 2010 || mm == "201101") {
      # 因为Rep中存在NA值，所以在as.numberic转化时会报warning
      if (length(unique(as.numeric(data_month_new$Rep))) != 5) {
        rep_error_5 = c(rep_error_5, mm)
      }
      # 因为Rep中存在NA值，所以在as.numberic转化时会报warning
      # 先将调查次序转化为numeric
      data_month_new = data_month_new[as.numeric(data_month_new$Rep) <= 5, ]
      # 去掉转化后成为NA值的记录
      data_month_new = data_month_new[!is.na(data_month_new$Rep), ]
      
    } else if (as.numeric(yy) >= 2011 && mm != "201101") {
      # 因为Rep中存在NA值，所以在as.numberic转化时会报warning
      if (length(unique(as.numeric(data_month_new$Rep))) != 3) {
        rep_error_3 = c(rep_error_3, mm)
      }
      # 因为Rep中存在NA值，所以在as.numberic转化时会报warning
      # 先将调查次序转化为numeric
      data_month_new = data_month_new[as.numeric(data_month_new$Rep) <= 3, ]
      # 去掉转化后成为NA值的记录
      data_month_new = data_month_new[!is.na(data_month_new$Rep), ]
    }
    # 按调查次序排序
    data_month_new = data_month_new[with(data_month_new, order(Rep)), ]
    
    #将每月的新建的数据表存入最终的新数据集mysheets_new中
    # 该数据集可能不会被调用
    # mysheets_new[[j]] = data_month_new
    # 获得新生成数据表的维度数据存入新的维度核对表中
    # 便于向数据中添加月份信息
    dim_check_new[j, 2:3] = dim(data_month_new)
    
    # 所有调查到鸟类的物种名，原变量：breed.name.temp
    month_spp_temp = as.character(unique(data_month_new$Species))
    if (any(is.na(month_spp_temp))) {
      print(mm)
      break
    }
    
    # 将该调查月中的所有物种名存入千岛湖鸟类名录中spp_all，并去重
    spp_all = unique(c(spp_all, month_spp_temp))
    
    # 所有调查样线名
    month_transect_temp = unique(data_month_new$Transect)
    transect_all = unique(c(transect_all, month_transect_temp))
    
    # 将处理好的数据表另存一个临时变量，可在不改变原始数据的基础上，
    # 进行新的处理
    month_dataframe_temp = data_month_new
    
 
    # 记录每个月重复调查的最大次数，用于后续核对，写入dim_check_new.csv文件中
    rep_max = c(rep_max, max(month_dataframe_temp$Rep))
    # 在每月的调查数据中添加月份
    month_dataframe_temp$Month = rep(mm, dim(data_month_new)[1])
    
    # 按月份将每月的数据按行整合到最终的数据集中
    breed_dataframe = rbind(breed_dataframe, month_dataframe_temp)
  }
  
  # 添加每月调查最大积累次数到维度校对表
  dim_check_new$rep_max = rep_max
  
  # 1.2 数据核对和重构 ######
  # 月份核对
  not_match = !(substr(breed_dataframe$Day, 1, 4) == substr(breed_dataframe$Month, 1, 4))
  month_not_match_table = breed_dataframe[not_match, ]
  cond1 = length(not_match_table[, 1]) == 0
  # 检验维度校对表中每月数据行数之和与经月份和调查次数转换的表行数是否一致
  cond2 = sum(as.numeric(dim_check_new[, 2])) == dim(breed_dataframe)[1]
  
  # 对重构后的总表进行重新存储，方便在不改变原有表内容的条件下进行后结的表操作
  final_dataframe = breed_dataframe
  
  # 写出结构重构后的数据总表到check_tables目录，方便整合进行数据核对和检查
  write.xlsx(
    final_dataframe,
    paste(path_check_table_dir,
          'raw_final_dataframe.xlsx', sep = '/')
  )
  
  # 目前的bug，有些记录出现了物种为无，但是多度有数据的情况
  # 如何解决：将相关的记录提取出来组成新的check表以便后续核对（已解决，去掉）
  # 物种与多度不匹配核对表record_sp_abun_check_table
  record_sp_abun_check_table = final_dataframe[final_dataframe$Species ==
                                                 "无" & !is.na(final_dataframe$Abun), ]

  # 写出最后需要核对的表record_sp_abun_check_table
  write.xlsx(
    record_sp_abun_check_table,
    paste(
      path_check_table_dir,
      'record_sp_abun_check_table.xlsx',
      sep = '/'
    )
  )
  
  # 去掉上述物种为无但是多度不为空的记录
  final_dataframe = final_dataframe[!(final_dataframe$Species == "无" &
                                        !is.na(final_dataframe$Abun)), ]
  
  # 将最终数据表中abundance无记录的行值替换为1
  final_dataframe[(final_dataframe$Species != "无" &
                     is.na(as.numeric(final_dataframe$Abun))),]$Abun = 1
  
  # 将Abun列数值化
  final_dataframe$Abun <- as.numeric(final_dataframe$Abun)
  rep_seq = c(rep_seq, unique(final_dataframe$Rep))
  
  
  return(
    list(
      month_not_match_table = month_not_match_table,
      dim_check_new = dim_check_new,
      sp_all = spp_all,
      transect_all = transect_all,
      final_bird_data = final_dataframe,
      rep_seq = rep_seq
    )
  )
}


# 3.岛屿参数导入 #####
# 岛屿参数表
IslandData = read.delim(paste(path_island_tran_dir, 'IslandData.txt', sep =
                                '/'),
                        strings = F)
# 需要提取数据的岛屿编号表
island_req = read.delim(
  paste(path_island_tran_dir,
        'island_req.txt', sep = '/'),
  h = F,
  strings = F
)
# 需要提取数据的样线名表
trans_req = read.delim(
  paste(path_island_tran_dir, 'trans_req.txt', sep = '/'),
  strings = F,
  h = F
)

# 2.每调查协变量表生成函数 ######
tran_cov_create = function(final_bird_data) {
  # 3.测试代码，运行中需要注释####
  #final_bird_data = large_bird_data_table
  # 指定提取的样线编号
  island36.tran = trans_req[, 1]
  # 基于指定样线获得岛屿编号，其中sapply可以直接应用于vector无需指定行或列
  # island_mark = sapply(island36.tran,FUN=function(x){
  #   unlist(strsplit(x,"-"))[1]
  # })
  # 返回的协变量表
  tran_cov_table = data.frame()
  
  # 鸟类数据表中样线的最大重复次数
  reps = unique(final_bird_data$Rep)
  # 鸟类数据表中调查月份
  months = unique(final_bird_data$Month)
  months_sel = months[months >= "200904"]
  
  # 用于调查时间开始的判断
  na_df = data.frame()
  # 用于调查时间结束的判断
  no_df = data.frame()
  
  for (tran in 1:length(island36.tran)) {
    # 4.测试代码，运行中需要注释#####
    # tran = 1
    for (month in 1:length(months_sel)) {
      # 5.测试代码，运行中需要注释 ####
      # month = 2
      for (rep in 1:length(reps)) {
        # rep = 1
        data_tmp = subset(final_bird_data,
                          Transect == island36.tran[tran] &
                            Month == months_sel[month] & Rep == reps[rep])
        if (nrow(data_tmp) == 0) {
          next
        }
        # 协变量表中每条记录所具有的值
        island = unlist(strsplit(island36.tran[tran], "-"))[1]
        date = unique(data_tmp$Day)
        surveyor = unique(data_tmp$Surveyor)
        weather = unique(data_tmp$Weather)
        survey_start = unique(data_tmp$T_start)
        survey_end = unique(data_tmp$T_end)
        duration = "need to check"
        
        # 调查开始时间判断
        na_df = rbind(na_df,
                      data.frame(
                        island36.tran[tran],
                        island,
                        months_sel[month],
                        reps[rep],
                        paste(!is.na(survey_start) &
                                !is.na(survey_end), collapse = ",")
                      ))
        # 调查结束时间判断
        no_df = rbind(no_df,
                      data.frame(
                        island36.tran[tran],
                        island,
                        months_sel[month],
                        reps[rep],
                        paste(survey_start != "无" &
                                survey_end != "无", collapse = ",")
                      ))
        
        # 计算协变量表中调查持续时间值
        if (!is.na(survey_start) & !is.na(survey_end)) {
          if (survey_start != "无" & survey_end != "无") {
            time_start_num = strptime(survey_start, format = "%H%M")
            time_end_num = strptime(survey_end, format = "%H%M")
            duration = as.numeric(difftime(time_end_num,
                                           time_start_num,
                                           units = "min"))
          }
        }
        # 协变量表中的记录生成
        cov_record = data.frame(
          island = island,
          transect = island36.tran[tran],
          month = months_sel[month],
          date = date,
          rep = reps[rep],
          weather = weather,
          survey_start = survey_start,
          survey_end = survey_end,
          duration = duration,
          surveyor = surveyor
        )
        # 将记录整合到协变量表中
        tran_cov_table = rbind(tran_cov_table, cov_record)
      }
    }
  }
  write.xlsx(na_df,
             paste(path_check_table_dir, "survey_time_na_check.xlsx", sep = "/"))
  write.xlsx(na_df,
             paste(path_check_table_dir, "survey_time_no_check.xlsx", sep = "/"))
  return(tran_cov_table)
}


# 4.按月、岛屿、样线名和调查次序进行多层（按月份）多度数据结构构建函数#####
abundance_month = function(large_bird_data_table,
                           transect_all,
                           spp_all,
                           rep_seq) {
  # 岛屿编号
  island36 = island_req[, 1]
  # 样线编号
  island36.tran = trans_req[, 1]
  
  # 6.测试代码，运行中需要注释 ####
  spp_all = sp_all
  transect_all = tran_all
  large_bird_data_table = large_bird_data_table
  rep_seq = rep_seq

  # 建立每月，各条样线,每次调查的数据表，列为样线名，行为物种 （样线水平数据集）######
  # bird_dataset代表一个大型四维矩阵：
  # x为物种名（每张表的行名）
  # y为样线名（每张表的列名）
  # z代表月份，每张表的调查月份：格式为yyyymm
  # k轴代表调查次序:1-5
  bird_dataset = array(
    data = NA,
    dim = c(
      length(spp_all),
      length(transect_all),
      length(monthName),
      length(rep_seq)
    ),
    dimnames = list(sort(spp_all),
                    sort(transect_all),
                    monthName,
                    rep_seq)
  )
  
  # x代表物种，y代表样线，t代表月份，k代表调查重复次数
  #dim(bird_dataset)

  for (t in 1:length(monthName)) {
    # 按月份取数据，为了便于向bird_dataset中存放数据
    mm = monthName[t]
    # 7.测试代码，运行中需要注释 ####
    # mm = "201905"
    # mm = '201001'
    # 取得指定月份的数据
    comm_month = subset(large_bird_data_table, Month == mm)
    # 将多度转化为数值
    comm_month$Abun = as.numeric(comm_month$Abun)
    # 将调查次数转化为数值
    comm_month$Rep = as.numeric(comm_month$Rep)
    # 当前月份中所有样线ID
    tran_month = sort(unique(comm_month$Transect))
    
    # 数据整合计算：相同物种的多度合并
    for (y in 1:length(tran_month)) {
      # 8.测试代码，运行中需要注释 ####
      # y = 50
      # 获得每条样线的数据
      comm_month_tran = subset(comm_month, Transect == tran_month[y])
      # 获得当前样线中所有的调查重复次数
      comm_month_tran_rep_id = unique(comm_month_tran$Rep)
      # 遍历每次调查
      for (k in 1:length(comm_month_tran_rep_id)) {
        # 9.测试代码，运行中需要注释 ####
        # k = 1
        # 获得每次调查的数据
        comm_month_tran_rep = subset(comm_month_tran, Rep == comm_month_tran_rep_id[k])
        # 统计该条样线本月本次调查每个物种被调查的到的多度
        comm_month_tran_rep_abun = aggregate(
          x = comm_month_tran_rep$Abun,
          by = list(comm_month_tran_rep$Species),
          FUN = sum
        )
        # 对列重命名
        colnames(comm_month_tran_rep_abun) = c("Species", "Abun")
        
        # 将所有物种名为"无"的记录多度修改为1
        # 对于物种名是"无"，但是多度值大于0的记录，已在data_reconstruct函数中删除
        # 所以不用在此处考虑这种情况
        # (1)先判断目前数据中是否存在有物种为"无"的记录
        # (2)如果有，则进行多度赋值操作，如果没有，则不操作
        no_abudance_replace = comm_month_tran_rep_abun[comm_month_tran_rep_abun$Species=="无",]
        if(length(no_abudance_replace$Species)!=0){
          comm_month_tran_rep_abun[comm_month_tran_rep_abun$Species=="无",]$Abun=1
        }
        
        # 将整合统计多度后的数据，按物种、调查样线、调查日期、调查次序
        # 整合到最终数据表bird_dataset中
        # 获得当前数据的月份
        month.idx = monthName[t]
        # 获得当前数据的调查次序
        rep_idx = comm_month_tran_rep_id[k]
        # 获得当前数据的样线ID
        tran.idx = tran_month[y]
        
        # 获得当前数据中所有物种名
        tran.sp = comm_month_tran_rep_abun$Species
        
        # 当物种只有1种且为"无"时，将对应样线、对应月份下对应调查下
        # 物种的多度由NA转为0，代表这次调查没有调查到任何物种
        # if(length(tran.sp) ==1){
        #   if(tran.sp =="无"){
        #     bird_dataset[, tran.idx, month.idx, rep_idx] = 0
        #     next
        #   }
        # }
        # 
        # 将当前数据按样线ID、调查月份和调查次序更新到data_dataset对应的位置中
        bird_dataset[tran.sp, tran.idx, month.idx, rep_idx] = comm_month_tran_rep_abun$Abun
        
        # 将该月下指定调查次序中指定样线上没有调查到的物种（NA）更新为0，代表未调查这些物种
        bird_dataset[, tran.idx, month.idx, rep_idx][is.na(bird_dataset[, tran.idx, month.idx, rep_idx])] = 0
        
        # 在最终的bird_dataset中会在某些样线上，所有物种存在NA值，代表这条样线上，这次调查
        # 的数据有问题，比如物种不准确导致的数据被剔除等
        
      }
    }
  }
  
  
  # 进行中间数据备份，方便在不修改原来数据基础上进行后续操作
  y2 = bird_dataset
  
  # 选择指定样线的数据，构建新的数据array
  y = y2[, island36.tran, , ]
  #dim(y)
  
  # ！！！测试代码，运行需要删除 ####
  # 存在数据有问题的情况，导致所有物种的多度为NA
  # y[,,"201001",5]
  # 
  # # 没有后两次调查，所有值应为NA的情况
  # y[,,"201901",5]
  # 
  # # 此次调查没有调查任何物种的情况
  # t1 = as.data.frame(y[,c("M3","M4"),"201905",1])
  # 
  # # 此次调查中调查到有物种，但是也有记录物种为"无"的情况
  # t2= as.data.frame(y[,"B3-1","200901",5])
  # 
  # # t1与t2的列和，用于判断到底是没有调查到任何物种，还是有"无"的记录
  # colSums(t1)
  # colSums(t2)
  
  # 去接掉物种名为"无"的行即可得到最终转化前的array
  y = y[-dim(y)[1],,,]

  # 最终大表验证代码，在进行函数测试时使用，运行时注释掉，不要删除，关键数据验证代码
  # 经验证可以和原始数据整合后对应
  # array_test = y[,"B1-1","201905","3"]
  # array_test = array_test[array_test>0]
  # array_test = data.frame(sp=names(array_test),abun=array_test)
  # array_test$sp= TILBirdList_unique$Chinese[match(array_test$sp,
  #                                                            TILBirdList_unique$LatinWJ)]
  # array_test = array_test[order(array_test$sp), ]
  
  ## 对array中所有物种进行所有调查月的多度加和
  # 由于是从所有样线中选取36个岛的样线，有些物种在其它岛或者大陆上调查到，
  # 但在选定的36个岛上并没有调查到，多度求和可以判断哪些物种在选定36个岛上
  # 从未被调查到，以便去除
  SppIsl36.idx = apply(y, 1, function(x)
    sum(x, na.rm = T))
  # 所选择36个岛上被调查到的物种拉丁名
  SppIsl36 = names(SppIsl36.idx)[SppIsl36.idx != 0]
  # 此处所有的物种名已经过核对转化为拉丁名，因此后续物种名转换可以参考
  # TILBirdList_unique.txt表
  # 进行物种名录判断，判断所有物种名是否都在TILBirdList_unique中
  # 如果显示character(0)，代表都在IOC名录
  SppIsl36[!SppIsl36 %in% TILBirdList_unique$LatinWJ]
  # 去除不在TILBirdList_unique中的物种名，character(0)也可以执行，不影响结果
  SppIsl36.df = TILBirdList_unique[match(SppIsl36, TILBirdList_unique$LatinWJ), ]
  # 基于TILBirdList_unique名录从中选择林鸟作为最终的名录
  land.bird = subset(SppIsl36.df, Landbird == 1)
  # 获得每月每条样线调查到的林鸟物种多度数据集（样线水平数据集）
  birdset_month_land_tran = y[as.character(land.bird$LatinWJ), , , ]
  #检查是否还有所有调查月份之和为0的，如果有需要在前边核对
  # 正确的是结果中每个物种对应的值大于等于1
  data_check_vector = sort(apply(birdset_month_land_tran, 1, function(x)
    sum(x, na.rm = T)))
  # 如果有物种的多度为0，代表有物种在36个岛上这么多次调查一次都没有出现过
  if (min(data_check_vector) == 0) {
    return(data_check_vector)
    # 提示错误信息
    stop('有物种在所有的调查月份在选择的36个岛屿上皆没有记录，请检查数据！')
  }
  
  # 基于样线水平数据集获得每月每个调查岛屿每次调查的林鸟多度数据集（岛屿水平数据集）####
  # 确定多条样线岛屿的样线数量
  tran_mult_num = c(table(substr(
    dimnames(birdset_month_land_tran)[[2]][1:24], 1, 2
  )))
  # 数据备份
  birdset_month_land_temp = birdset_month_land_tran
  # 第二个下标代表样线名称，令下标2为1，可以获得B1-1上每个物种调查是否调查到
  # 即：birdset_month_land_temp[,1,,]
  # birdset_month_land_temp[,1:8,,]就代表取1-8列，即取B1-1到B1-8共8条样线数据
  # 对多样线岛屿上的所有样线按物种进行求和，获得所有样线总数据，并赋值给
  # 对应岛屿的第一条样线，然后提取该样线数据，作为该多样线岛屿的最终数据
  # 样线记录变量
  record_num = 1
  # 提取样线记录
  col_island = c()
  # 遍历每个多样线岛屿的样线数量
  for (i in tran_mult_num) {
    # 将指定岛屿所有样线的多度数据求和
    birdset_month_land_temp[, record_num, , ] =
      apply(birdset_month_land_temp[, record_num:(record_num + i - 1), , ], c(1, 3, 4), 
            function(x) sum(x,na.rm = T))
    # 并将该岛屿的第1条样线所在索引取出作为提取标识
    col_island = c(col_island, record_num)
    # 每完成一个岛的数据合并，样线记录变量加1
    record_num = record_num + i
  }
  # 创建从第25条样线开始到最后一条样线的所有索引值，并与之前的多样线岛屿索引全并
  col_island = c(col_island,
                 record_num:dim(birdset_month_land_temp)[[2]])
  # 从总表中提出对应的样线数据
  birdset_month_land_island = birdset_month_land_temp[, col_island, , ]
  # 将样线名命名为岛屿名
  dimnames(birdset_month_land_island)[[2]] = island36
  # 按岛屿面积进行数据排序
  birdset_month_land_island = birdset_month_land_island[, IslandData$marks, , ]
  dimnames(birdset_month_land_island)[[2]] = IslandData$marks
  
  # 对不足5次调查的年份进行NA数据转化#####
  # 按样线
  # birdset_month_land_tran[, , monthName[monthName > "201101"], rep_seq[rep_seq > 3]] = NA
  # 按岛屿
  # birdset_month_land_island[, , monthName[monthName > "201101"], rep_seq[rep_seq > 3]] = NA
  
  # 10.测试代码，对样线水平第四维进行测试提取#####
  df_test_tran = data.frame(tran="B1-1",month= "202012",rep="1",abun=birdset_month_land_tran[,"B1-1","202012",1])
  rownames(df_test_tran)= TILBirdList_unique$Chinese[match(rownames(df_test_tran),
                                   TILBirdList_unique$LatinWJ)]
  df_test_tran = subset(df_test_tran, abun>0)
  write.xlsx(df_test_tran,rowNames=T,
             paste(path_check_table_dir,"final_tran_data_check.xlsx",sep="/"))
  
  # 11.测试代码，对样线水平第四维进行测试提取#####
  df_test_island = data.frame(isl="B1",month= "201012",rep="5",
                              abun=birdset_month_land_island[,"B1","201012",5])
  rownames(df_test_island)= TILBirdList_unique$Chinese[match(rownames(df_test_island),
                                                           TILBirdList_unique$LatinWJ)]
  df_test_island = subset(df_test_island, abun>0)
  write.xlsx(df_test_island,rowNames=T,
             paste(path_check_table_dir,"final_island_data_check.xlsx",sep="/"))
  
  # 要返回的数据集重命名
  SppTranMonthData = birdset_month_land_tran
  SppIslMonthData = birdset_month_land_island
  message("重要提示：请在使用数据库前，核对final_tran_data_check.xlsx和final_island_data_check.xlsx与原始数据是否相同")
  return(list(SppTranMonthData = SppTranMonthData,
              SppIslMonthData = SppIslMonthData))
}

# 将dataframe中的拉丁名转化为中文名
convert_chinese = function(bird_data_frame){
  bird_data_frame = data.frame(bird_data_frame)
  rownames(bird_data_frame) = TILBirdList_unique$Chinese[match(rownames(bird_data_frame),
                                                               TILBirdList_unique$LatinWJ)]
  return(bird_data_frame)
}
