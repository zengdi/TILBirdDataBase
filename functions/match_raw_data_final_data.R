# 目的：用于检验生成数据与原始数据是否存在差异 ######
# 作者：曾頔
# 最后修改日期：2024-11-03
# 最后运行日期：2024-11-03

library(tidyverse)
library(data.table)

# 对原始数据进行重构的函数 #####
raw_dt_reconstruct = function(dt_raw){
  # 重构原始数据
  raw_dt = dt_raw %>% 
    # 选择需要的列
    select(种类,样线,month,调查次数,数量) %>% 
    # 将数量转换为数值
    mutate(数量 = as.numeric(数量)) %>% 
    # 重命名列名
    rename("sp" = "种类",
           "tran" = "样线",
           "rep" = "调查次数",
           "abun" = "数量") %>% 
    # 基于中文名添加拉丁名
    left_join(TILBirdList_raw %>% distinct(),
              by = c("sp" = "Chinese")) %>% 
    # 过滤掉未匹配到拉丁名的记录
    filter(!is.na(LatinWJ)) %>% 
    # 调整列的顺序
    select(LatinWJ,tran,month,rep,abun) %>% 
    # 对列名进行重命名
    rename("sp" = "LatinWJ") %>% 
    # 对同物种名的数据进行合并
    reframe(abun = sum(abun,na.rm = T),
            .by = c(sp,tran,rep,month)) %>% 
    # 转换为data.table对象
    as.data.table()
  return(raw_dt)
}



final_tran_data_check = function(dt_raw,dt_tran_final){
  # 使用 expand.grid 获取所有维度的标签组合
  final_dt = expand.grid(
    sp = dimnames(dt_tran_final)[[1]],
    tran = dimnames(dt_tran_final)[[2]],
    month = dimnames(dt_tran_final)[[3]],
    rep = dimnames(dt_tran_final)[[4]]
  )
  # 将四维数组展平为一维向量，并添加到数据框中
  final_dt$abun = as.vector(dt_tran_final)
  final_dt = data.table(final_dt)
  
  raw_dt = raw_dt_reconstruct(dt_raw)
  
  # 对样线水平的数据进行核对 ######
  # 对数据进行拼接
  df_comp = merge(final_dt,raw_dt,
                  by = c("sp","tran","rep","month"),
                  all.x=T) %>% 
    # 过滤掉未在raw_dt中匹配到数据的记录
    filter(!is.na(abun.y)) %>% 
    as.data.table
  
  # 返回有差异的数据
  return(df_comp[,diff := abun.x - abun.y] %>% 
            filter(diff!=0)
  )
}


final_island_data_check = function(dt_raw,dt_isl_final){
  # 使用 expand.grid 获取所有维度的标签组合
  final_dt = expand.grid(
    sp = dimnames(dt_isl_final)[[1]],
    island = dimnames(dt_isl_final)[[2]],
    month = dimnames(dt_isl_final)[[3]],
    rep = dimnames(dt_isl_final)[[4]]
  )
  # 将四维数组展平为一维向量，并添加到数据框中
  final_dt$abun = as.vector(dt_isl_final)
  final_dt = as.data.table(final_dt)
  
  # 重构原始数据
  raw_dt = raw_dt_reconstruct(dt_raw)
  raw_dt_isl = raw_dt %>%
    separate(tran, 
             into = c("island", "code"), 
             sep = "-", 
             fill = "right", 
             remove = FALSE) %>%
    select(-code,-tran)
  
  # 对原始数据按岛屿合并
  raw_dt_isl = raw_dt_isl %>% 
    reframe(abun = sum(abun),
            .by = c(sp,island,rep,month)) %>% 
    as.data.table

  # 对数据进行拼接
  df_comp = merge(final_dt,raw_dt_isl,
                  by = c("sp","island","rep","month"),
                  all.x=T) %>% # 过滤掉未在raw_dt中匹配到数据的记录
    filter(!is.na(abun.y)) %>% 
    as.data.table
  
  # 返回有差异的数据
  return(df_comp[,diff := abun.x - abun.y] %>% 
           filter(diff!=0))
}

