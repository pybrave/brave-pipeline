#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os 
import json
OUTPUT_DIR = os.getenv("OUTPUT_DIR")
os.chdir(OUTPUT_DIR)


# In[2]:


print(f"Usage Dir: {OUTPUT_DIR}")


# In[3]:


import subprocess
import shlex

def run_command(cmd: str,shell=False):
    """
    执行系统命令（支持实时输出），支持直接输入空格分割的字符串。
    """
    # 自动按 shell 语法分割字符串，例如：
    # "ping -c 4 google.com" -> ["ping", "-c", "4", "google.com"]
    cmd_list = shlex.split(cmd)

    process = subprocess.Popen(
        cmd_list,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        shell=shell,   
        text=True
    )

    for line in process.stdout:
        print(line, end="")  # 实时输出

    process.wait()
    print(f"\nExit code: {process.returncode}")
    return process.returncode


# In[4]:


# !ncbi-genome-download -h


# In[5]:


with open("params.json") as f:
    params = json.load(f)


# In[64]:


# params


# In[9]:


assembly_accessions = params["assembly_accessions"]
formats = params["formats"]
formats = ",".join(formats)


# In[8]:





# In[66]:


# !ncbi-genome-download -v --taxids 511145  --assembly-levels complete --formats fasta bacteria


# In[75]:


cmd = f"ncbi-genome-download -v  --assembly-accessions {assembly_accessions} bacteria --formats {formats} --progress-bar --flat-output  --output-folder output"
run_command(cmd)


# In[80]:


os.system(f"gunzip output/*.gz")

