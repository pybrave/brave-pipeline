#!/usr/local/bin/python
import os
import pandas as pd
import re
import sys,time
inputfile1 = sys.argv[1]
inputfile2 = sys.argv[2]
output = sys.argv[3]
#set path
#os.chdir("C:/Users/11111/Desktop/zyd/")
#os.listdir()
#load data
df1 = pd.read_table(inputfile1,header=None)
df2 = pd.read_table(inputfile2)
#分割数据将空值用0填充
df3 = df1.iloc[:,2].str.split(",",expand=True)
#print(df3)
df3 = df3.set_index(df1[0])
#print(df3)
df3 = df3.fillna(0)
#print(df3)
#根据df2对应关系创建字典
d = {}
for i in df2.itertuples():
    _ = {i[1]:i[2]}
    d.update(_)
d.update({"0":0})
#print(d)
#定义替换函数，0用0替换，不存在的值用0替换
def f(x):
    if x in d.keys():
        return d[x]
    else:
        return 0
#应用函数
result = df3.applymap(f)
#print(result)
#求和
DF = result.sum(1)
#设置输出格式，加\t避免出现科学计数法
def deal_str(data):
    data = str(data)+'\t'
    return data
#应用函数并输出
DF.map(deal_str).to_csv(output)
