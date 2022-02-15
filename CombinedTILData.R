###############脚本使用说明############
# The encoding of this script is "UTF-8", including all the comments in scripts
# as well as the exporting files. If any errors, please set the encoding of
# your text editor to 'UTF-8'
# 该脚本是主脚本，调用了该functions目录中的三个功能函数脚本：
# (1) raw_data_check_combined_function.R ：用于原始excel表格合并
# (2) data_item_check.R：进行原始数据核对
# (3) data_reconstruction.R：进行最终数据集的重构、多度向0/1数据的转化
# 执行该主脚本后，即可在final_database目录中找到最终数据库TILbird_land_Month.Rdata和
# 每次调查的协变量表transect_covariate.xlsx
# 该数据库包括5个list：
# (1) 按调查月份、样线和调查次序的多度数据：SppTranMonthData
# (2) 按调查月份、岛屿和调查次序的多度数据：SppIslMonthData
# (3) 按调查月份、样线和调查次序的0/1数据：SppTranMonthData.PA
# (4) 按调查月份、岛屿和调查次序的0/1数据：SppIslMonthData.PA
# (5) 36个岛屿参数，上述1-4个list的生成皆基于这36个岛屿名称
# 具体的使用规则及数据要求详见README.txt

##### directory path construction #####
# 清除上次结果命令，在debug和测试中不建议运行！！！
system("rm ./final_database/*")
system("rm ./check_tables/*")
# 获得工程所在目录
path_base = getwd()
# 建立原始文档所在目录变量，用于读取原始鸟调数据
path_raw_excel_dir = paste(path_base,'20220214raw_data',sep='/')
# 建立最终数据库所在目录变量，用于保存最终的数据库
path_database_dir = paste(path_base,'final_database',sep='/')
# 建立功能函数所在目录变量，用于调用多个数据处理功能函数
path_function_dir = paste(path_base,'functions',sep='/')
# 建立核对表存放目录变量，用于存放整合数据校验后需要再次核对的数据
path_check_table_dir = paste(path_base,'check_tables',sep='/')
# 岛屿参数存放目录变量，用于存放岛屿参数以及需要获得数据的岛屿和样线名称
path_island_tran_dir = paste(path_base,'island',sep='/')

# 导入物种名参考数据
# 未去重的千岛湖鸟类名录（包括所有书写错误的物种名）
TILBirdList_raw = read.delim(paste(path_base,'TILBirdinfo_raw.txt',sep='/'),
                         sep=',')
# 去重后的千岛湖鸟类名录（只包括正确的中文名、拉丁名的物种，且没有任何重复）
TILBirdList_unique = read.delim(paste(path_base,'TILBirdinfo_unique.txt',sep='/'),
                                sep=',')

# 加载文件名检查、sheet名检查和数据合并函数文件
source(paste(path_function_dir,'raw_data_check_combined_function.R',sep='/'))

# 整合所有的调查数据表，返回整合后的数据表以及调查年月组成的列表
# 整合数据名为mysheets,调查年月日列表名为monthName
# mysheets可以以year，sheetname为关键字进行数据调取
# files代表原始调查数据文件路径
# 例1，mysheets[['2013']]调取2013年所有的调查数据
# 例2，mysheets[['2015']][['201501']]调取2015年1月调查数据
combined_data = combined_all_data(path_raw_excel_dir)

# 调查年月组成的列表，以年份命名
monthName = combined_data[['monthName']]
# 合并后的所有鸟类调查数据集
mysheets = combined_data[['mysheets']]
# 所有调查的年份
yearName = combined_data[['yearName']]

# data exploring example 
# 列出2015年所有数据
head(mysheets[['2015']],n=1)
# 列出2015年6月数据前几行
head(mysheets[['2015']][['201506']])

# 各个数据行的校对#####
# 加载数据核对函数文件
source(paste(path_function_dir,'data_item_check.R',sep='/'))
# 调用数据核对函数，返回检查结果list
check_list = data_check(monthName,mysheets)
# 写出所有核对结果列表，以便进行原始数据recheck
for(i in 1:(length(names(check_list))-1)){
  write.xlsx(as.data.frame(check_list[[i]]),paste(path_check_table_dir,
                              paste(names(check_list)[i],'.xlsx',sep = ''),
                              sep='/'))
}


# 在完成数据核对的同时，基于spp_all.csv文件构建千岛湖鸟类信息名录（拉丁名、生活型、留居类型等）
# 相关操作写在./TILbird_database/bird_data_combined/README.txt中

# 按正确物种信息整理后的最新数据大集合 #####
mysheets = check_list[['mysheets']]
# 获得指定年月下，指定样线，指定调查次数的数据
# 其它类型的数据获得皆可以此为例扩展
# 以B4为例获得202004第一次调查的数据
#(subset(mysheets[['2020']][['202004']],(substr(样线,1,2)=='B4'&调查次数==1)))
#names_char = mysheets[['2007']][['200712']][,7]
#test_tmp = subset(mysheets[['2007']][['200705']])

# 加载数据重构函数和按月份多度数据集构建函数文件
source(paste(path_function_dir,'data_reconstruction.R',sep='/'))
# 最终数据生成
# 起始年份
# 数据截止年份，按次年1月当年最后一次调查
end_year = 2021
# 进行数据表重构
data_re = data_reconstruct(mysheets,end_year)
# data_re中的核对表及最终合并和过滤后的鸟类群落数据总表输入到硬盘
for(i in names(data_re)){
  write.xlsx(as.data.frame(data_re[[i]]),paste(path_check_table_dir,
                                             paste(i,'.xlsx',sep = ''),
                                             sep='/'))
}

# 进行数据库生成 #####
# 获得最终的鸟类群落数据大表构数据表
large_bird_data_table = data_re$final_bird_data

# 获得每次调查的协变量表
tran_cov_table = tran_cov_create(large_bird_data_table)
write.xlsx(tran_cov_table,paste(path_database_dir,"transect_covariate.xlsx",sep="/"))

# 去掉为保留没有调查到物种的调查次数据所设置的"无"
sp_all = data_re$sp_all
tran_all = data_re$transect_all
rep_seq = data_re$rep_seq

# 转换获得按月份堆层的多度数据集，行为物种名，列为岛屿或样线
# abundance_month(bird_table,transect_all,sp_all)
# bird_table:代表总体鸟类数据集表
# transect_all：代表所有的样线
# sp_all：调查中遇到的所有的鸟物种
# 上述三种数据皆可由data_reconstruct获得
# abundance_month函数执行完，要先完成message的核对再执行后续操作
final_data = abundance_month(large_bird_data_table,tran_all,sp_all,rep_seq)

flag = readline("请确认：final_tran_data_check.xlsx和final_island_data_check.xlsx与原始数据相同？Y/N \n")
if(flag == "Y"){
  # 注：两个数据集中可能会出现如下情况：某条样线的某次调查中所有物种的多度为NA
  # 就放假情况表明，原始数据中该次调查有数据，但由于物种名不准确，导致数据被剔除
  # 按月份、样线名的多度数据集
  SppTranMonthData = final_data$SppTranMonthData
  # 按月份、岛屿名的多度数据集
  SppIslMonthData = final_data$SppIslMonthData
  
  
  # 多度数据集的随机核对 not run ####
  # 经核验转化后的数据库与原始数据对应且一致
  # convert_chinese(SppTranMonthData[SppTranMonthData[, 'B3-1', '201712',3]>0, 'B3-1', '201712',3])
  # convert_chinese(SppTranMonthData[SppTranMonthData[, 'B4-1', '202006',3]>0, 'B4-1', '202006',3])
  # convert_chinese(SppIslMonthData[SppIslMonthData[, 'B4', '202006',3]>0, 'B4', '202006',3])
  
  
  # 将多度数据集转换为0/1数据集
  SppTranMonthData.PA = SppTranMonthData
  SppIslMonthData.PA = SppIslMonthData
  # 将多度转化为0和1
  SppTranMonthData.PA[SppTranMonthData.PA > 0] = 1
  SppIslMonthData.PA[SppIslMonthData.PA > 0] = 1
  
  
  # 将岛屿参数、按月和岛屿或者样线的0/1数据和多度数据存储到
  # 数据库TILbird_land_Month.Rdata
  save(SppTranMonthData, 
       SppIslMonthData, 
       SppTranMonthData.PA, 
       SppIslMonthData.PA, 
       IslandData, 
       file = paste(path_base,'final_database','TILbird_land_Month.Rdata',sep='/'))
}

# 获得数据库中36个岛屿上所调查到的物种的中文名
dim(SppIslMonthData.PA)
TILBirdList_unique$Chinese[match(rownames(SppTranMonthData),TILBirdList_unique$LatinWJ)]

