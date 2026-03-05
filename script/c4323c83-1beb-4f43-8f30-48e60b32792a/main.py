#!/usr/bin/env python
# coding: utf-8

# In[ ]:


# test 123


# In[1]:


import sys
import os
import json
import pandas as pd 
import matplotlib.pyplot as plt
# script_dir = os.path.dirname(os.path.abspath(__file__))
common_dir =os.getenv("COMMON_SCRIPT_DIR")
sys.path.insert(0, common_dir)
from metaphlan import get_abundance,get_metadata
os.chdir(os.getenv("OUTPUT_DIR"))



params_path = sys.argv[1]
output_path = sys.argv[2]
params_path="params.json"
output_path="output"

with open(params_path) as f:
    data = json.load(f)

abundance0 = get_abundance(data)

metadata = get_metadata(data)


rank = data["rank"]
top_n = data["top_num"]


# In[18]:


# abundance_rank = abundance0.reset_index(['taxonomy','rank']).reset_index(drop=True).query("rank==@rank").drop("rank",axis=1)
# 
# abundance_rank.shape


# In[2]:


abundance_rank = abundance0.reset_index(['taxonomy','rank']).reset_index(drop=True).query("rank==@rank").drop("rank",axis=1)

abundance_rank= abundance_rank[~abundance_rank["taxonomy"].str.contains("GGB|SGB", regex=True)]
abundance_cols = abundance_rank.columns.drop("taxonomy")
abundance_rank[abundance_cols] = abundance_rank[abundance_cols].div(abundance_rank[abundance_cols].sum(axis=0), axis=1) * 100

abundance_rank


# In[3]:


# metadata


# In[20]:


# abundance_rank.iloc[:5,:5]


# In[21]:


# metadata.iloc[:5,:5]


# In[ ]:





# In[4]:


abundance_rank_df = abundance_rank.set_index("taxonomy")
grouped = {}
for g, samples in metadata.groupby("group").groups.items():
    sub = abundance_rank_df[samples]  # 取该 group 的样本
    grouped[g] = sub.sum(axis=1)  # 按物种求和
group_df = pd.DataFrame(grouped)

# top_taxa = group_df.sum(axis=1).sort_values(ascending=False).head(10).index
# group_df = group_df.loc[top_taxa]


# In[5]:


metadata


# In[51]:


# metadata.groupby("group").groups[data["groups_name"]["treatment"]]


# In[6]:


list_group = [data["groups_name"]["treatment"],data["groups_name"]["control"]]
list_group


# In[7]:


n_groups = metadata.groupby("group").ngroups
fig, axes = plt.subplots(1, n_groups, figsize=(5*n_groups, 12), sharey=True)

if n_groups == 1:
    axes = [axes]
for ax, (g) in zip(axes, list_group):
    samples = metadata.groupby("group").groups[g]
    taxonomy = group_df[g].sort_values(ascending=False).index
    abundance_sorted = abundance_rank_df.loc[taxonomy,samples]
    
    top_df = abundance_sorted.head(top_n).copy()
    other = abundance_sorted.iloc[top_n:, :].sum()  # 不算 row_sum 列
    other.name = "Other"
    group_data = pd.concat([other.to_frame().T, top_df.iloc[:, :]]).T
    group_data = group_data[group_data.sum().sort_values(ascending=False).index]

    n_cols = group_data.shape[1]
    colors = plt.cm.tab20.colors * ((n_cols // 20) + 1)  # 自动扩展颜色数
    
    # print(colors)
    group_data.plot(kind='bar', stacked=True, ax=ax, legend=False, color=colors[:n_cols])
        # 获取原始图例句柄和标签
    handles, labels = ax.get_legend_handles_labels()
    
    # 反转顺序
    handles, labels = handles[::-1], labels[::-1]
    # 添加全局 legend：图下方居中
    ax.legend(
        handles, labels,
        loc='upper center',
        bbox_to_anchor=(0.5,  -0.1),  # 调整为下方居中
        ncol=1,                       # 根据列数调整横排数量
        frameon=True
    )
    ax.set_title(g)
    ax.set_xticklabels(group_data.index, rotation=45)
    ax.set_xlabel('')
    # ax.set_xticklabels([]) 
    # ax.set_xticks([])
# handles, labels = axes[0].get_legend_handles_labels()
# handles, labels = handles[::-1], labels[::-1]
# group_data
plt.tight_layout(rect=[0, 0, 1, 1])  # 预留右边空白
plt.savefig(f"{output_path}/stacked_diagram.each.top{top_n}.pdf", bbox_inches="tight")
# group_data


# In[28]:


# abundance_rank_df.T.loc[["F-30-156"],["Methanobrevibacter arboriphilus"]]


# In[34]:


# abundance = abundance.query("not taxonomy.str.contains('GGB')")
abundance = df_final = pd.concat([
    abundance_rank.loc[abundance_rank["taxonomy"] != "Bacteria unclassified", ["taxonomy"]],
    abundance_rank.loc[abundance_rank["taxonomy"] != "Bacteria unclassified"].drop(columns="taxonomy")
      .apply(lambda x: x / x.sum() * 100, axis=0)
], axis=1).set_index("taxonomy")

# abundance_rank.set_index("taxonomy")



# abundance["row_sum"] = abundance.sum(axis=1)
abundance["row_sum"] = abundance.sum(axis=1)
abundance_sorted = abundance.sort_values("row_sum", ascending=False)
abundance_sorted = abundance_sorted.drop("row_sum",axis=1)

# top_n = 10
top_df = abundance_sorted.head(top_n).copy()
other = abundance_sorted.iloc[top_n:, :].sum()  # 不算 row_sum 列
other.name = "Other"
top_df = pd.concat([top_df.iloc[:, :], other.to_frame().T])
top_df


abundance = top_df.T
abundance = abundance.reset_index().rename({"index":"sample_name"},axis=1)
abundance


df_merge = pd.merge(abundance,metadata.reset_index(),left_on="sample_name",right_on="sample_name")
df_merge = df_merge.set_index(['sample_name','group'])
df_merge = df_merge[df_merge.sum().sort_values(ascending=False).index]

# top_df.iloc[:, :-1]


# In[35]:


# abundance_rank.loc[abundance_rank["taxonomy"] != "Bacteria unclassified", ["taxonomy"]]


# In[9]:


# df_merge


# In[29]:


# df


# In[28]:


# grouped.get_group('ACC')


# In[23]:


# abundance_rank["OCC1"].sum()


# In[10]:


df = df_merge.reset_index("group")
grouped = df.groupby('group')
n_groups = grouped.ngroups
fig, axes = plt.subplots(1, n_groups, figsize=(5*n_groups, 8), sharey=True)

if n_groups == 1:
    axes = [axes]

for ax, (g) in zip(axes, list_group):
    # group_data = grouped.groups[g]
    group_data = grouped.get_group(g).drop(columns='group')
    # group_data = group.drop(columns='group')
    n_cols = group_data.shape[1]
    colors = plt.cm.tab20.colors * ((n_cols // 20) + 1)  # 自动扩展颜色数

    # print(colors)
    group_data.plot(kind='bar', stacked=True, ax=ax, legend=False, color=colors[:n_cols])
    ax.set_title(g)
    ax.set_xticklabels(group_data.index, rotation=45)
    ax.set_xlabel('')
    # ax.set_xticklabels([]) 
    # ax.set_xticks([])

handles, labels = axes[0].get_legend_handles_labels()
handles, labels = handles[::-1], labels[::-1]
# 添加全局 legend：图下方居中
fig.legend(
    handles, labels,
    loc='center left',
    bbox_to_anchor=(1, 0.5),   # 根据需要可调整偏移
    ncol=1,             # 横排
    frameon=False                 # 可选：去掉图例边框
)

# 让出下方空间以容纳 legend
# plt.tight_layout(rect=[0, 0.05, 1, 1])  # 下边留出 5% 空间
plt.tight_layout(rect=[0, 0, 1, 1])  # 预留右边空白
plt.savefig(f"{output_path}/stacked_diagram.pdf", bbox_inches="tight")
# group_data.shape[1]


# In[124]:


# df_sum.shape[1]


# In[123]:


# df_sum


# In[122]:


# df_sum = df_merge.groupby("group").sum()
# df_sum.plot(kind='bar', stacked=True,  legend=True, color=colors[:df_sum.shape[1]])


# In[60]:


# 分组求和
df_sum = df_merge.groupby("group").mean()

# 绘制 stacked bar
fig, ax = plt.subplots(figsize=(8,6))
colors = plt.cm.tab20.colors * ((df_sum.shape[1] // 20) + 1)  # 自动扩展颜色数

df_sum.plot(kind='bar', stacked=True, ax=ax, color=colors[:df_sum.shape[1]], legend=False)

# 获取句柄和标签
handles, labels = ax.get_legend_handles_labels()

# 反转顺序（可选，根据需要）
handles, labels = handles[::-1], labels[::-1]
 
       # 根据需要可调整偏移
              # 横排
       
# 添加图例
ax.legend(handles, labels, loc='center left',bbox_to_anchor=(1, 0.5), ncol=1 , frameon=False)

ax.set_xlabel('')
ax.set_ylabel('abundance')
# ax.set_title('Stacked Bar Plot by Group')

plt.tight_layout()
# plt.show()
plt.savefig(f"{output_path}/stacked_diagram_mean.pdf", bbox_inches="tight")


# In[126]:


# df_merge[["Muribaculaceae bacterium","Bacteroidales bacterium"]]


# In[ ]:





# In[61]:


from skbio.diversity import alpha_diversity
from scipy.stats import mannwhitneyu  
import seaborn as sns


# In[62]:


abundance = abundance_rank.set_index("taxonomy").T


# In[63]:


shannon = alpha_diversity('shannon', abundance.values, ids=abundance.index)
simpson = alpha_diversity('simpson', abundance.values, ids=abundance.index)
chao1 = alpha_diversity('chao1', abundance.values, ids=abundance.index)
# faith_pd = alpha_diversity('faith_pd', abundance.values, ids=abundance.index)
observed_otus = alpha_diversity('observed_otus', abundance.values, ids=abundance.index)
pielou_e = alpha_diversity('pielou_e', abundance.values, ids=abundance.index)


# In[21]:


# metadata_diversity = metadata.set_index("sample_name")


# In[64]:


alpha_df = pd.DataFrame({
    'shannon': shannon,
    'species_richness': observed_otus,
    "simpson":simpson,
    "chao1":chao1,
    "pielou_e":pielou_e
})
alpha_df = alpha_df.merge(metadata, left_index=True, right_index=True).reset_index().rename({"index":"sample_name"},axis=1)


# In[65]:


alpha_df.to_csv(f"{output_path}/diversity.tsv",sep="\t",index=False)


# In[71]:


# data["groups_name"]["control"]


# In[72]:


# metric="shannon"
def plot_diversity(metric):
    control_group = data["groups_name"]['control']
    treatment_group = data["groups_name"]['treatment']
    
    control_alpha = alpha_df.query("group == @control_group")[metric].to_list()
    treatment_alpha = alpha_df.query("group == @treatment_group")[metric].to_list()
    stat, p_value = mannwhitneyu(control_alpha, treatment_alpha, alternative='two-sided')  
    
    plt.figure(figsize=(6, 6))
    # sns.boxplot(x='group', y=metric, data=alpha_df)
    # sns.swarmplot(x='group', y=metric, data=alpha_df, color='black')
    
    ax = sns.boxplot(x='group', y=metric, data=alpha_df, 
                palette="Set2",  # 配色方案
                width=0.5,       # 箱体宽度
                linewidth=2.5,
                showfliers=False,
                order=[data["groups_name"]["control"],data["groups_name"]["treatment"]]
            )    # 
    sns.stripplot(x='group', y=metric, data=alpha_df,  # 叠加散点图
                color='black', size=4, alpha=0.3)
    
    y_min,y_max = ax.get_ylim()
    plt.text(x=0.5, y=y_max*0.9, s=f"p = {round(p_value,4)}", ha="center", fontsize=12)
    
    plt.title(f'Alpha Diversity ({metric})')
    plt.savefig(f"{output_path}/{metric}_diversity.pdf", bbox_inches="tight")
plot_diversity("shannon")
plot_diversity("species_richness")
plot_diversity("simpson")
plot_diversity("chao1")
plot_diversity("pielou_e")


# In[39]:


metadata


# In[92]:


# abundance


# In[73]:


from skbio.diversity import beta_diversity
from skbio.stats.distance import permanova
from skbio.stats.ordination import pcoa
import numpy as np
from matplotlib.patches import Ellipse


# In[83]:


data_pcoa = pd.merge(metadata,abundance,left_index=True, right_index=True).reset_index().set_index(['index','group'])
bc_dm = beta_diversity("braycurtis", data_pcoa.values, ids=data_pcoa.index)
pcoa_result = pcoa(bc_dm)
pc_df = pcoa_result.samples.loc[:, ['PC1', 'PC2']].reset_index().rename(columns={"level_1":"group"})
permanova_result = permanova(distance_matrix=bc_dm, 
                            grouping=data_pcoa.reset_index().set_index(['index','group'],drop=False)['group'],
                            permutations=500)


# In[84]:


import numpy as np
import matplotlib.pyplot as plt
from matplotlib.patches import Ellipse

def draw_confidence_ellipse(x, y, ax, edgecolor='black', facecolor='none', alpha=0.3, n_std=2):
    if len(x) < 2:
        return
    cov = np.cov(x, y)
    vals, vecs = np.linalg.eigh(cov)
    order = vals.argsort()[::-1]
    vals = vals[order]
    vecs = vecs[:, order]

    theta = np.degrees(np.arctan2(*vecs[:, 0][::-1]))
    width, height = 2 * n_std * np.sqrt(vals)  # n_std σ 对应置信区间

    ell = Ellipse(
        (np.mean(x), np.mean(y)),
        width=width,
        height=height,
        angle=theta,
        edgecolor=edgecolor,
        facecolor=facecolor,
        linewidth=1.5,
        alpha=alpha
    )
    ax.add_patch(ell)


# In[87]:


# data


# In[88]:


data["groups_name"]["control"]


# In[89]:


# order=[data["groups_name"]["control"],data["groups_name"]["treatment"]]

custom_palette = {
    data["groups_name"]["control"]: "#66c2a5",
   data["groups_name"]["treatment"]: "#fc8d62"
}


# In[93]:


plt.figure(figsize=(7, 6))
ax = plt.gca()
sns.scatterplot(data=pc_df, x='PC1', y='PC2', hue='group', s=100, hue_order=[data["groups_name"]["control"],data["groups_name"]["treatment"]],
                palette=custom_palette, ax=ax)

for g, df_g in pc_df.groupby('group'):
    draw_confidence_ellipse(df_g['PC1'], df_g['PC2'], ax)

plt.xlabel(f"PC1 ({pcoa_result.proportion_explained[0]*100:.2f}%)")
plt.ylabel(f"PC2 ({pcoa_result.proportion_explained[1]*100:.2f}%)")
plt.title(f"Beta Diversity - PCoA (Bray-Curtis) PERMANOVA:{permanova_result['p-value']:.3f}")
plt.axis('equal')  # 保证x/y比例
plt.tight_layout()
# plt.savefig(f"{output_path}/beta_diversity.pdf")
plt.savefig(f"{output_path}/beta_diversity.pdf", bbox_inches="tight")


# In[ ]:





# In[ ]:





# In[ ]:




