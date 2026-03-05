import sys
import os
script_dir = os.path.dirname(os.path.abspath(__file__))
common_dir = os.path.abspath(os.path.join(script_dir, "../../script/common"))
sys.path.insert(0, common_dir)
from humann_barplot import main
print(os.getcwd())

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
query_term = params_json['query_term']

humann_profile_path = [item[term] for item in humann_profile]

def get_df(item,term,sample_name):
    file_name = os.path.basename(item)
    term = term.replace("_",".")
    # file_name = file_name.replace(f"_{term}.tsv","")
    
    # file_name = file_name.replace(f".Pathway.txt","")
    # file_name = file_name.replace(f".Module.txt","")
    # file_name = file_name.replace(f".{term}.txt","")
    df = pd.read_csv(item,sep="\t")
    df.columns = ["term",sample_name]
    # df = df.query("term.str.contains(@query_term)")
    return df 
# humann_profile_df = [get_df(item,term) for item in humann_profile_path]
humann_profile_df = [get_df(item[term],term,item["sample_name"])for item in humann_profile]
humann_profile_merge_df = reduce(lambda x,y: pd.merge(x,y,on="term",how="outer"),humann_profile_df ).fillna(0)

metadata = [(item['sample_name'],item['group'])for item in humann_profile]
metadata = pd.DataFrame(metadata, columns=["sample_name","group"]).set_index("sample_name").T
humann_profile_merge_df = humann_profile_merge_df[["term",*metadata.columns]]
metadata = metadata.reset_index().rename({"index":"term"},axis=1)
result_df = pd.concat([metadata,humann_profile_merge_df])


output_file= f"{output}/{term}.tsv"
result_df.to_csv(output_file, index=False,sep="\t")


# sys.argv= ['a', '--input', 'hmp_pathabund.pcl', '--focal-metadata', 'STSite', '--last-metadata', 'STSite', '--output', 'output/plot1.png', '--focal-feature', 'METSYN-PWY']
# sys.argv= ['a', '--input', output_file, '--output', f"{output}/plot.png", '--focal-feature',query_term ,"--sort","sum"]
# sys.argv= ['a', '--input', output_file, '--output', f"{output}/plot.png", '--focal-feature',query_term ,"--sort","braycurtis",'--focal-metadata', 'group',"--scaling","logstack", "--as-genera","--remove-zeros"]
# main()

query_term_list = query_term.split(",")
for query_term_item in query_term_list:
    sys.argv= ['a', '--input', output_file, '--output', f"{output}/{query_term_item}.png", '--focal-feature',query_term_item ,"--sort","braycurtis",'--focal-metadata', 'group',"--scaling","logstack", "--as-genera","--remove-zeros"]
    main()