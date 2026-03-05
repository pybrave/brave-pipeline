#!/usr/bin/env python
# coding: utf-8

# In[31]:


import os
import pandas as pd
import json
from functools import reduce
import subprocess

# 1. 命令字符串
# cmd_str = "lefse_format_input.py matrix.tsv matrix.in -c 2 -s 2 -u 1 -o 1000000"

# 2. 调用命令

# In[7]:


# os.chdir(os.getenv("OUTPUT_DIR"))
params_path = "./params.json"

print(os.getcwd())
# In[10]:


with open(params_path,"r") as f:
    params = json.load(f)

# data['treatment'] + data['control'] 


# In[11]:


def get_metadata_group(db_dict,group,group_name):
    return pd.DataFrame([(item['sample_name'],group_name) for item in db_dict[group] ], columns=['sample_name','group'])

def get_metadata(params):
    df_list = [get_metadata_group(params,group,group_name) for group,group_name in params["groups_name"].items()]
    metadata = pd.concat(df_list, ignore_index=True)
    metadata = metadata.set_index("sample_name")
    return metadata


# In[12]:


def get_one_df(file,sample_key):
    df = pd.read_csv(file,sep="\t",comment="#",header=None)
    df.columns = ["clade_name","ncbi_tax_id","relative_abundance","additional_species"]
    df = df.rename({"relative_abundance":sample_key},axis=1)
    df = df.drop(["additional_species","ncbi_tax_id"],axis=1)
    df = df.set_index(["clade_name"])
    return df


# In[13]:


samples = sum([params[group] for group in params["groups"]],[])
df_list = [get_one_df(item['profile'],item['sample_name']) for item in samples]


# In[14]:


df = reduce(lambda x,y:pd.merge(x,y,left_index=True,right_index=True, how="outer"),df_list)
# df = df.reset_index()
df = df.fillna(0)


# In[15]:


metadata = get_metadata(params)


# In[17]:


df = df.reset_index().query("not clade_name.str.contains('t__')").set_index("clade_name")


# In[19]:


df_merge = pd.merge(df.T,metadata,left_index=True,right_index=True, how="inner" )


# In[20]:


df_merge.shape


# In[21]:


df_res = df_merge.iloc[:, [-1] + list(range(df_merge.shape[1] - 1))].T
df_res = df_res.reset_index().rename(columns={"index":"sample_name"})


# In[22]:


df_res.to_csv("matrix.tsv",sep="\t",index=False)
df_res


# In[23]:

subprocess.run("lefse_format_input.py matrix.tsv matrix.in -c 2 -s 2 -u 1 -o 1000000", shell=True, check=True)



# In[34]:

subprocess.run("lefse_run.py matrix.in output/matrix.tsv", shell=True, check=True)




# In[27]:

subprocess.run("lefse_plot_cladogram.py output/matrix.tsv output/matrix.cladogram.pdf   --dpi 300", shell=True, check=True)

# get_ipython().system('MPLBACKEND=Agg  lefse_plot_cladogram.py matrix.res output/matrix.cladogram.pdf   --dpi 300')
# \
#   --format pdf \
#   --dpi 600 \
#   --label_font_size 14 \
#   --title_font_size 16 \
#   --max_point_size 40 \
#   --colored_labels 1 \
#   --class_legend_vis 0 \
#   --class_legend_font_size 12


# In[30]:

subprocess.run(" lefse_plot_res.py output/matrix.tsv output/matrix.pdf   --dpi 300", shell=True, check=True)

# get_ipython().system('MPLBACKEND=Agg lefse_plot_res.py matrix.res output/matrix.pdf   --dpi 300')


# In[28]:


# from IPython.display import Image, display
# # 显示图片
# display(Image(filename="./matrix.cladogram.png"))


# In[ ]:




