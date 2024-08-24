千岛湖鸟类群落数据库生成代码的使用方法

本文档的编码为UTF-8，所有代码的编写和测试基于Ubuntu 20.04LTS (X86_64)平台的Rstudio（ver 2021.09.1）和R语言（ver-4.1.2）完成。

代码最后一次校对于202408xx完成，基于Ubuntu 22.04.4 LTS (x86_64)平台的Rstudio（ver 2024.04.2）和R语言（ver-4.4.0）完成版本完成。

运行前请执行如下代码，检查软件包完整性:
require("openxlsx","stringr")

1. 目录结构（列举主要目录及文档目的）

TILBirdDataBase/ (代码根目录)

  |-- raw_data/yyyymmdd (所有原始数据的xlsx文件)
  |   |-- 200704-200801.xlsx
  |   |-- 200804-200901.xlsx
  |   |-- 200904-201001.xlsx
  |   |-- 201004-201101.xlsx
  |   |-- ... ...
      
  |-- check_tables/ (存放各种原始数据核对表，用于原始数据表的纠错和校对)
  |   |-- day_check.xlsx (每次调查的调查日期核对表)
  |   |-- dim_check.xlsx (每月调查信息缺失核对表，包括记录数、样线数、调查日期、开始时间、结束时间、天气、样线重复次数、物种、多度、调查人)
  |   |-- final_island_data_check.xslx(最终岛屿水平数据取样核对表，与原始数据比对)
  |   |-- final_tran_data_check.xlsx(最终样线水平数据取样核对表，与原始数据比对)
  |   |-- month_not_match_table.xlsx(调查日期提取月份与调查月份不匹配核对表)
  |   |-- record_sp_abun_check_table.xlsx(记录中物种为"无"，多度不为0核对表)
  |   |-- rep_check.xlsx(每调查月所有样线重复次数信息核对表)
  |   |-- sp_all.xlsx(千岛湖鸟类拉丁名录，无重复)
  |   |-- spp_all_raw.xlsx(所原始记录中所有输入的鸟类物种种，包括各种形式)
  |   |-- spp_check.xlsx(千岛湖鸟类物种中文名核对表，记录了无法识别的鸟类中文名)
  |   |-- survey_time_na_check.xlsx(原始调查开始和结束时间缺失的记录核对表)
  |   |-- survey_time_no_check.xlsx(原始调查开始和结束时间为"无"的记录核对表)
  |   |-- surveyor_check.xlsx(原始主要调查人信息核对表)
  |   |-- time_check.xlsx(原始调查开始和结束时间核对表，与调查持续时间核对表time_duration_check_table类似)
  |   |-- time_duration_check_table.xls（调查持续时间核对表）
  tran_check.xlsx(每个调查月份调查样线名信息表，用于核对是否所有样线都有数据)
  tran_rep_check.xlsx（每条调查样线重复缺失信息表，显示哪些样线哪些次重复数据缺失）
  
  |-- final_database/ (最终数据库目录)
  |   |-- TILbird_land_Month.Rdata (最终的数据库)
  |   |   |-- SppIslMonthData (岛屿水平调查月份尺度多度数据)
  |   |   |-- SppIslMonthData.PA (岛屿水平调查月份尺度0/1数据)
  |   |   |-- SppTranMonthData (样线水平调查月份尺度多度数据)
  |   |   |-- SppTranMonthData.PA (样线水平调查月份尺度0/1数据)
  |   |-- transect_covariate.xlsx (每次调查协变量表，包括岛屿名、样线名、调查月份、重复次序、天气、开始结束时间、持续时间、主要调查人)
  
  |-- functions/ (功能函数目录)
  |   |-- data_item_check.R (原始数据核对功能函数，用于生成check_tables目录中的部分文件)
  |   |-- data_reconstruction.R (最终数据重构功能函数，用于生成最终数据库和协变量表，同时也生成check_tables目录中的部分文件)
  |   |-- raw_data_check_combined_function.R (原始调查数据合并功能函数，用于将所有原始数据的xlsx表格合并成一个大表)
  
  |-- island/ (岛屿参数及所需岛屿记录文件)
  |   |-- island_req.txt 需要提取数据的岛屿名称
  |   |-- IslandData.txt 需要提取数据的岛屿地理参数
  |   |-- trans_req.txt 需要提取数据的岛屿对应的样线名称，值的前2-3位要与island_req.txt一致
  
  |-- 202004-202101_excel_sheet_errors_test.xlsx (原始数据xlsx文件命名格式错误测试文件)
  
  |-- README.txt (千岛湖鸟类数据库生成代码说明文件)
  
  |-- bird_data_combined.Rproj (R Project文件)
  
  |-- CombinedTILData.R (数据合并主脚本文件)
  
  |-- TILBirdinfo_raw.txt (原始记录中所有鸟名记录信息表，用于将记录到的鸟名转为正确的拉丁学名)
  
  |-- TILBirdinfo_unique.txt (千岛湖鸟名记录信息表，最终数据物种中文名的核对和拉丁学名转中文名)


2. 原始数据表的命名规则及存放位置
(1)每个xlsx文件的命名规则为yyyy04-(yyyy+1)01，例如：202004-202101.xlsx
(2)每个xlsx文件包含1-6个sheets,为当年所有繁殖季和冬季的调查数据，例如：202004，202005，202006，202011，202012，202101。上述sheet可以不全，但是命名规则必须为yyyymm
(3)所有的原始数据都放置在raw_data/yyyymmdd 目录中

3. 使用说明：
（1）当所有的原始数据准备好后，只需要执行CombinedTILData.R中所有代码即可完成所有的数据合并。
（2）最终生成的数据库文件和协变量表在final_database 目录中
（3）最终数据库的数据调用格式，databaseName[sp_name,island/transect_names,survey_month,survey_seq(1-5)]
例如：SppIslMonthData.PA[,,'201906',1] 代表取出2019年6月所有调查岛屿的第1次调查数据

4. 对其它岛屿鸟类数据提取并整合至最终数据库的实现
通过修如下三个文件的内容以实现对除目前36个岛屿以外其它岛屿数据的获得：
(1) IslandData.txt 需要岛屿的岛屿参数
(2) island_req.txt 需要岛屿的名称
(3) trans_req.txt 需要岛屿上对应的样线名称
以上三者的内容要一一对应，(1)和(2)中的岛屿数量及名称要一致

5. 最终数据库中只保留了林鸟物种，其它生活型鸟未包括其中

6. 注意！！！！：
请对所有的原始数据进行备份后再执行该代码

