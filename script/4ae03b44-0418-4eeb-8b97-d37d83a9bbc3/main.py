#!/usr/bin/env python
# coding: utf-8

# In[22]:


import os
import json
import sys
import pandas as pd 
from functools import reduce


os.chdir(os.getenv("OUTPUT_DIR"))
params_path = sys.argv[1]
output_path = sys.argv[2]
params_path="params.json"
output_path="output"
common_dir =os.getenv("COMMON_SCRIPT_DIR")
sys.path.insert(0, common_dir)
from metaphlan import get_abundance,get_metadata


# In[23]:


with open(params_path,"r") as f:
    params = json.load(f)

def get_last_num(clade_name,num):
    clade_name_list = clade_name.split('|')
    return clade_name_list[len(clade_name_list)-num]

def get_last(clade_name):
    clade_name_list = clade_name.split('|')
    return clade_name_list[len(clade_name_list)-1]
def set_rank(taxonomy):
    if taxonomy.startswith("t__"):
        return "SGB"
    if taxonomy.startswith("s__"):
        return "SPECIES"
    elif  taxonomy.startswith("g__"):
        return "GENUS"
    elif  taxonomy.startswith("f__"):
        return "FAMILY"
    elif  taxonomy.startswith("o__"):
        return "ORDER"
    elif  taxonomy.startswith("c__"):
        return "CLASS"
    elif  taxonomy.startswith("p__"):
        return "PHYLUM"
    elif  taxonomy.startswith("k__"):
        return "KINGDOM"
def rename_taxonomy(taxonomy):
    if taxonomy.startswith("t__"):
        return taxonomy.replace("t__","").replace("_"," ")
    if taxonomy.startswith("s__"):
        return taxonomy.replace("s__","").replace("_"," ")
    elif  taxonomy.startswith("g__"):
        return taxonomy.replace("g__","").replace("_"," ")
    elif  taxonomy.startswith("f__"):
        return taxonomy.replace("f__","").replace("_"," ")
    elif  taxonomy.startswith("o__"):
        return taxonomy.replace("o__","").replace("_"," ")
    elif  taxonomy.startswith("c__"):
        return taxonomy.replace("c__","").replace("_"," ")
    elif  taxonomy.startswith("p__"):
        return taxonomy.replace("p__","").replace("_"," ")
    elif  taxonomy.startswith("k__"):
        return taxonomy.replace("k__","").replace("_"," ")
def get_one_df(file,sample_key):
    df = pd.read_csv(file,sep="\t",comment="#",header=None)
    df.columns = ["clade_name","ncbi_tax_id","relative_abundance","additional_species"]
    df = df.rename({"relative_abundance":sample_key},axis=1)
    df = df.drop("additional_species",axis=1)
    df = df.set_index(["clade_name","ncbi_tax_id"])
    return df    


metaphlan_sam_abundance=  params['metaphlan_sam_abundance']
df_list = [get_one_df(item['profile'],item['sample_name']) for item in metaphlan_sam_abundance]
df = reduce(lambda x,y:pd.merge(x,y,left_index=True,right_index=True, how="outer"),df_list)



df = df.reset_index()
df['taxonomy'] = df.apply(lambda x : get_last(x['clade_name']) ,axis=1)
df['tax_id'] = df.apply(lambda x : get_last(x['ncbi_tax_id']) ,axis=1)
df['rank'] = df.apply(lambda x : set_rank(x['taxonomy']) ,axis=1)
df['taxonomy'] = df.apply(lambda x : rename_taxonomy(x['taxonomy']) ,axis=1)
df = df.set_index(["clade_name","ncbi_tax_id","taxonomy","tax_id","rank"])
df = df.fillna(0)
df = df.reset_index()


# In[31]:


# metadata = get_metadata(params)


# In[30]:


# sorted(metadata.index)


# In[32]:


# df_sorted = df[]
# df_sorted


# In[17]:


output_file = f"{output_path}/matrix.tsv"
df.to_csv(output_file, sep="\t",index=False)
print(f"写入文件 {output_file}")
# rank_list = list(set(df["rank"]))
# for item in rank_list:
#     print(f"save: {output_path}/{item}.tsv")
#     df.query("rank==@item").to_csv(f"{output_path}/{item}.tsv",sep="\t")


# In[17]:


# rank_list = list(set(df["rank"]))
# item = rank_list[0]


# In[4]:


# for item in rank_list:
#     print(f"save: {output_path}/{item}.tsv")
#     df.query("rank==@item").to_csv(f"{output_path}/{item}.tsv",sep="\t")


# In[ ]:





# In[5]:


# params = sys.argv[1]
# output_path = sys.argv[2]


# In[6]:


# params = "/ssd1/wy/workspace2/nextflow_workspace/9a6af4fb-0af6-4752-af79-9adec161e7c2/4ae03b44-0418-4eeb-8b97-d37d83a9bbc3/86d2ce8c-9d23-42f2-a647-44838fc4a3c2/params.json"
# output_path ="/ssd1/wy/workspace2/nextflow_workspace/9a6af4fb-0af6-4752-af79-9adec161e7c2/4ae03b44-0418-4eeb-8b97-d37d83a9bbc3/86d2ce8c-9d23-42f2-a647-44838fc4a3c2/output"


# In[7]:


# with open(params,"r") as f:
#     params = json.load(f)


# In[9]:


# def get_last_num(clade_name,num):
#     clade_name_list = clade_name.split('|')
#     return clade_name_list[len(clade_name_list)-num]

# def get_last(clade_name):
#     clade_name_list = clade_name.split('|')
#     return clade_name_list[len(clade_name_list)-1]
# def set_rank(taxonomy):
#     if taxonomy.startswith("t__"):
#         return "SGB"
#     if taxonomy.startswith("s__"):
#         return "SPECIES"
#     elif  taxonomy.startswith("g__"):
#         return "GENUS"
#     elif  taxonomy.startswith("f__"):
#         return "FAMILY"
#     elif  taxonomy.startswith("o__"):
#         return "ORDER"
#     elif  taxonomy.startswith("c__"):
#         return "CLASS"
#     elif  taxonomy.startswith("p__"):
#         return "PHYLUM"
#     elif  taxonomy.startswith("k__"):
#         return "KINGDOM"
# def rename_taxonomy(taxonomy):
#     if taxonomy.startswith("t__"):
#         return taxonomy.replace("t__","").replace("_"," ")
#     if taxonomy.startswith("s__"):
#         return taxonomy.replace("s__","").replace("_"," ")
#     elif  taxonomy.startswith("g__"):
#         return taxonomy.replace("g__","").replace("_"," ")
#     elif  taxonomy.startswith("f__"):
#         return taxonomy.replace("f__","").replace("_"," ")
#     elif  taxonomy.startswith("o__"):
#         return taxonomy.replace("o__","").replace("_"," ")
#     elif  taxonomy.startswith("c__"):
#         return taxonomy.replace("c__","").replace("_"," ")
#     elif  taxonomy.startswith("p__"):
#         return taxonomy.replace("p__","").replace("_"," ")
#     elif  taxonomy.startswith("k__"):
#         return taxonomy.replace("k__","").replace("_"," ")
# def get_one_df(file,sample_key):
#     df = pd.read_csv(file,sep="\t",comment="#",header=None)
#     df.columns = ["clade_name","ncbi_tax_id","relative_abundance","additional_species"]
#     df = df.rename({"relative_abundance":sample_key},axis=1)
#     df = df.drop("additional_species",axis=1)
#     df = df.set_index(["clade_name","ncbi_tax_id"])
#     return df    


# In[10]:


# metaphlan_sam_abundance=  params['metaphlan_sam_abundance']
# df_list = [get_one_df(item['profile'],item['sample_name']) for item in metaphlan_sam_abundance]
# df = reduce(lambda x,y:pd.merge(x,y,left_index=True,right_index=True, how="outer"),df_list)
# df = df.reset_index()
# df['taxonomy'] = df.apply(lambda x : get_last(x['clade_name']) ,axis=1)
# df['tax_id'] = df.apply(lambda x : get_last(x['ncbi_tax_id']) ,axis=1)
# df['rank'] = df.apply(lambda x : set_rank(x['taxonomy']) ,axis=1)
# df['taxonomy'] = df.apply(lambda x : rename_taxonomy(x['taxonomy']) ,axis=1)
# df = df.set_index(["clade_name","ncbi_tax_id","taxonomy","tax_id","rank"])
# df = df.fillna(0)
# df = df.reset_index()
# df


# In[11]:


# df.to_csv(f"{output_path}/matrix.tsv",sep="\t",index=False)

