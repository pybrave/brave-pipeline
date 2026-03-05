import sys
import os
script_dir = os.path.dirname(os.path.abspath(__file__))
common_dir = os.path.abspath(os.path.join(script_dir, "../../script/common"))
sys.path.insert(0, common_dir)
from humann_barplot import main
print(os.getcwd())
# sys.argv= ['a', '--input', 'hmp_pathabund.pcl', '--focal-metadata', 'STSite', '--last-metadata', 'STSite', '--output', 'output/plot1.png', '--focal-feature', 'METSYN-PWY']
# main()
params = sys.argv[1]
output = sys.argv[2]
print(params)
print(output)
def delete_files_in_dir(folder):
    for entry in os.scandir(folder):
        if entry.is_file():
            os.remove(entry.path)
delete_files_in_dir(output)

import pandas as pd
import json
from  functools import reduce

with open(params,"r") as f:
    params_json = json.load(f)
humann_profile = params_json['humann_profile']
term = params_json['term']

humann_profile_path = [item[term] for item in humann_profile]

def get_df(item,term):
    file_name = os.path.basename(item)
    term = term.replace("_",".")
    file_name = file_name.replace(f"_{term}.tsv","")
    
    file_name = file_name.replace(f".Pathway.txt","")
    file_name = file_name.replace(f".Module.txt","")
    file_name = file_name.replace(f".{term}.txt","")
    df = pd.read_csv(item,sep="\t")
    df.columns = ["term",file_name]
    df = df.query("not term.str.contains('\\|')")
    return df 
humann_profile_df = [get_df(item,term) for item in humann_profile_path]
humann_profile_merge_df = reduce(lambda x,y: pd.merge(x,y,on="term",how="outer"),humann_profile_df ).fillna(0)

humann_profile_merge_df.to_csv(f"{output}/{term}.tsv", index=False,sep="\t")