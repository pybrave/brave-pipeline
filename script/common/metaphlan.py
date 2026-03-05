import json
import pandas as pd 
from functools import reduce



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

def get_abundance0(metaphlan_sam_abundance,rename=True):
    df_list = [get_one_df(item['profile'],item['sample_name']) for item in metaphlan_sam_abundance]
    df = reduce(lambda x,y:pd.merge(x,y,left_index=True,right_index=True, how="outer"),df_list)
    df = df.reset_index()
    df['taxonomy'] = df.apply(lambda x : get_last(x['clade_name']) ,axis=1)
    df['tax_id'] = df.apply(lambda x : get_last(x['ncbi_tax_id']) ,axis=1)
    df['rank'] = df.apply(lambda x : set_rank(x['taxonomy']) ,axis=1)
    if rename:
        df['taxonomy'] = df.apply(lambda x : rename_taxonomy(x['taxonomy']) ,axis=1)
    df = df.set_index(["clade_name","ncbi_tax_id","taxonomy","tax_id","rank"])
    df = df.fillna(0)
    return df

def get_abundance(params,rename=True,filter=None):
    if filter is None:
        samples = sum([params[group] for group in params["groups"]],[])
    else :
        samples = sum([params[group] for group in filter],[])
    # samples = sum([params[group] for group in params["groups"]],[])
    return get_abundance0(samples,rename=rename)

def get_metadata_group(db_dict,group,group_name,group_field,use_group_field):
    if use_group_field:
        return pd.DataFrame([(item['sample_name'],item[group_field]) for item in db_dict[group] ], columns=['sample_name','group'])
    else:
        return pd.DataFrame([(item['sample_name'],group_name) for item in db_dict[group] ], columns=['sample_name','group'])
def get_metadata(params,use_group_field=False,filter=None):
    group_field = params["group_field"]
    if filter is None:
        df_list = [get_metadata_group(params,group,group_name,group_field,use_group_field) for group,group_name in params["groups_name"].items()]
    else :
        df_list = [get_metadata_group(params,group,group_name,group_field,use_group_field) for group,group_name in params["groups_name"].items()
               if group in filter 
               ]
    metadata = pd.concat(df_list, ignore_index=True)
    metadata = metadata.set_index("sample_name")
    return metadata