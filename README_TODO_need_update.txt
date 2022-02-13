The encoding of this file is 'utf-8'
该目录内所有代码基于Ubuntu20.04上的Rstudio server（Version 1.4.1717）和R（version 4.1）编写。运行前请执行如下代码，检查软件包完整性:
require("openxlsx","stringr")


1. 目录结构

 (根目录)

  |-- check_tables/ (用于存放数据核对结果表，只列举主要的)
  |   |-- breed_dataframe.csv (所有数据表集合而成的大表，用于错误核对)
  |   |-- not_match_table.csv (所有调查日期与数据表记录调查日期不一致的数据)
  |   |-- sp_all.csv (最终数据库中所有鸟类的拉丁名数据)
  |   |-- spp_all.csv (所有原始调查数据中出现的鸟名记录)
  |   |-- spp_check.csv (需要核对鸟名的数据记录)
  |   |-- dim_check_new.csv (核对调查次数)
  |   |-- final_dataframe.xlsx （数据结构重构后的数据总表，方便进行整体的数据核对）
  |   |-- ......
  
  |-- final_database/ (最终数据库目录)
  |   |-- TILbird_land_Month.Rdata (最终的数据库)
  
  |-- functions/ (功能函数目录)
  |   |-- data_item_check.R (数据核对功能函数)
  |   |-- data_reconstruction.R (最终数据重构功能函数)
  |   |-- raw_data_check_combined_function.R (原始调查数据合并目录)
  
  |-- island/ (岛屿参数及所需岛屿记录文件)
  |   |-- island_req.txt 需要提取数据的岛屿名称
  |   |-- IslandData.txt 需要提取数据的岛屿地理参数
  |   |-- trans_req.txt 需要提取数据的岛屿对应的样线名称，值的前2-3位要与island_req.txt一致
  
  |-- raw_data/ (放置原始的鸟类调查excel表，xlsx文件)
  
  |-- 最原始数据集备份勿动/ (编写这套代码用的最原始数据及代码参考)
  
  |-- .RData (R元数据)
  
  |-- .Rhistory (R执行等历史记录文件)
  
  |-- 202004-202101_excel_sheet_errors_test.xlsx (功能函数raw_data_check_combined_function.R的测试文件)
  
  |-- bird_data_combined.Rproj (R Project文件)
  
  |-- CombinedTILData.R (数据合并主脚本文件)
  
  |-- IslandData.txt (岛屿参数文件)
  
  |-- TIL_bird_spp.txt (千岛湖鸟类物种名信息，源自IOC)
  
  |-- README.txt (说明文件)

2. 原始数据表的合并及命名规则
(1)每个xlsx文件的命名规则为yyyy04-(yyyy+1)01，例如：202004-202101.xlsx
(2)每个xlsx文件包含1-6个sheets,为当年所有繁殖季和冬季的调查数据，例如：202004，202005，202006，202011，202012，202101。上述sheet可以不全，但是命名规则必须为yyyymm
(3)所有的原始数据都放置在./raw_data 目录中

3. 使用说明：
当所有的原始数据准备好后，只需要执行CombinedTILData.R即可完成所有的数据合并，获得最终的数据库文件
数据调用格式，databaseName[sp_name,island/transect_names,survey_month,survey_seq(1-5)]
例如：SppIslMonthData.PA[,,'201906',1] 代表取出2019年6月所有调查岛屿的第1次调查数据

4. 该脚本所有需要额外加载的软件包：
library(stringr)
library(openxlsx)

5. 对其它岛屿鸟类数据的添加实现
可以通过修如下三个文件的内容以实现对除目前36个岛屿以外其它岛屿数据的获得：
(1) IslandData.txt 需要岛屿的岛屿参数
(2) island_req.txt 需要岛屿的名称
(3) trans_req.txt 需要岛屿上对应的样线名称
以上三者的内容要一一对应，(1)和(2)中的岛屿数量及名称要一致

6. 千岛湖鸟类名录及IOC信息更新
（1）千岛湖鸟类及其名录原始文档
    ./check_tables/spp_check.csv  # 存放原始记录中物种名无法与ITL_bird_spp.txt中信息匹配的原始记录信息
    ./check_tables/sp_all.csv #去重和去错后的千岛湖鸟类总名录（拉丁名） 
    ./check_tables/spp_all.csv # 原始记录中总的鸟类物种（包含写错物种名、带特殊符号等），用于整合建构TIL_bird_spp.txt文件
    ./TIL_bird_spp.txt # 千岛湖原始记录中可能出现的物种名、对应拉丁名及其留居和栖息地类型信息

（2）./千岛湖鸟类物种名参考校对/
该目录用于生成TIL_bird_spp.txt文件,最重要的文件为0号、1号，用于生成其它文件：2_TIL_bird_spp.txt（用于数据生成中的物种名核对）和3_TILBird_IOCinfo.txt(千岛湖鸟类总名录)

（3）TIL_bird_spp.txt和TILBird_IOCinfo.txt文件更新步骤：
需要的基本文件（需要把代码整体跑一遍，才能统计出原始数据中全部记录到的物种名，即spp.check.csv）：
        ./check_tables/spp_check.csv
        ./千岛湖鸟类物种名参考校对/0_BLIOCPhyloMasterTax.xlsx (下载地址：https://data.vertlife.org/birdtree/BLIOCPhyloMasterTax.csv)
        ./千岛湖鸟类物种名参考校对/1_TIL_bird_spp_raw_截止到202101.xlsx

A、查看spp_check.csv文件，看是否有鸟类物种名相对完整和部分正确，但是没有在TIL_bird_spp.txt中匹配到的（比如，黑鹎？，大山，红头长山雀，这种只要可以判断物种的都属于“鸟类物种名相对完整和部分正确”但是没有匹配的）。如果有，先去确认原始记录；
B、基于物种名中文名，在0_BLIOCPhyloMasterTax.xlsx中scientific列查找其拉丁名，将其补充到1_TIL_bird_spp_raw_截止到yyyymmdd.xlsx的“鸟类信息”sheet中，并完成其它信息查找和补充。将其惟一正确的中文名和所有原始记录中的中文名补充到“带匹配格式信息”sheet中
C、利用vlookup函数对“带匹配格式信息”添加的物种名进行鸟类信息填充，已设好代码，直接拖动句柄即可
D、去掉“带匹配格式信息”中的格式，生成新的“final_构建”sheet，删除记录有误的行，并将该sheet转化为txt格式，以生成TILBirdinfo_raw.txt，将TILBirdinfo_raw.txt放入bird_data_combined/目录下
E、基于鸟类信息sheet，删除有误的物种之后，创建TILBirdinfo_unique sheet，并保存为TILBirdinfo_unique.txt格式，用于在最终数据集创建时的物种名核对
F、在TILBirdinfo_raw.txt和TILBirdinfo_unique.txt中一定要保留“无”这个关键字作为物种名，因为有些岛屿上的有些调查中没有调查到物种，“无”关键字可以令程序将未调查到物种的记录保留，以便完成每次调查协变量表的创建！


7. 注意！！！！：
请对所有的原始数据进行备份后再执行该代码，即将原始数据复制2份，1份放bird_data_combined/入raw_data/目录下，1份放入bird_data_combined/最原始数据集勿动/目录下

