#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os

os.chdir(os.getenv("OUTPUT_DIR"))
params = "params.json"
output = "output"


# In[7]:


import sys
import os

# print(os.getcwd())
# sys.argv= ['a', '--input', 'hmp_pathabund.pcl', '--focal-metadata', 'STSite', '--last-metadata', 'STSite', '--output', 'output/plot1.png', '--focal-feature', 'METSYN-PWY']
# main()
# params = sys.argv[1]
# output = sys.argv[2]


import pandas as pd
import json
from  functools import reduce

with open(params,"r") as f:
    params_json = json.load(f)
humann_profile = params_json['humann_profile']
term_list = params_json['term']


def get_df(item,term,sample_name):

    # file_name = os.path.basename(item)
    # term = term.replace("_",".")
    # file_name = file_name.replace(f"_{term}.tsv","")
    
    # file_name = file_name.replace(f".Pathway.txt","")
    # file_name = file_name.replace(f".Module.txt","")
    # file_name = file_name.replace(f".{term}.txt","")
    df = pd.read_csv(item,sep="\t")
    if term == "GBM" or term == "GMM":
        df = df.drop("Coverage",axis=1)
    df.columns = ["term",sample_name]
    df = df.query("not term.str.contains('\\|')")
    return df 



# In[8]:


for term in term_list:
    print(term)
    # humann_profile_path = [item[term] for item in humann_profile]
    humann_profile_df = [get_df(item[term],term,item["sample_name"]) for item in humann_profile]
    humann_profile_merge_df = reduce(lambda x,y: pd.merge(x,y,on="term",how="outer"),humann_profile_df ).fillna(0)
    humann_profile_merge_df.to_csv(f"{output}/{term}.tsv", index=False,sep="\t")

