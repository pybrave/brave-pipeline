## install

```
git clone git@github.com:pybrave/pipeline-metagenomics.git ~/.brave/pipeline/7530139e-8985-423f-9fb6-32650828ca40
```
or
```
git clone https://github.com/pybrave/pipeline-metagenomics.git ~/.brave/pipeline/7530139e-8985-423f-9fb6-32650828ca40
```

## heatmap
<script/d31f5abb-6ef2-43cd-aeba-14fea48f4c8c/main.R>



params.json 是根据用户输入 io_schema.json 生成的表单生成的，文件输入配置在 inputs/workflow(只有当scatter=each才使用workflow)，一些其它配置请放在 io_schema.json 的 params 下，配置格式请参考如下
```
{
            "name": "mind",
            "label": "mind",
            "type": "BaseInputNumber",
            "initialValue":0.02,
            "tooltip":"missing individual data"
}
```
其中type的类型包括：BaseSelect,BaseSwitch,BaseInput,BaseInputNumber,BaseTextAreaNum

实现读取params.json 中的 seuratObject 的path，