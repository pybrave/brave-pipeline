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
{
            "name": "normalize_method",
            "label": "normalize_method",
            "type": "BaseSelect",
            "initialValue": "lognormalize",
            "data": [
                {
                    "label": "lognormalize",
                    "value": "lognormalize"
                },
                {
                    "label": "rc",
                    "value": "rc"
                }
            ],
            "tooltip": "Normalization method for NormalizeData"
}
```
其中type的类型包括：BaseSelect,BaseSwitch,BaseInput,BaseInputNumber,BaseTextAreaNum

目前也可以支持depends，用于表单项的条件显示或隐藏,参考如下,当method="lasso"时才显示alpha_en
```
  {
      "type": "BaseSelect",
      "name": "method",
      "label": "Method",
      "initialValue":"lasso",
      "required": true,
      "rules": [
        {
          "required": true
        }
      ],
      "data": [
        {
          "label": "lasso",
          "value": "lasso"
        },
        {
          "label": "ridge",
          "value": "ridge"
        },
        {
          "label": "elasticnet",
          "value": "elasticnet"
        }
      ]
    },{
      "type": "BaseInput",
      "name": "alpha_en",
      "depends": [
        {
          "name": "method",
          "value": "elasticnet"
        }
      ],
      "label": "Elastic Net alpha",
      "initialValue": 0.5,
      "required": true,
      "rules": [
        {
          "required": true
        }
      ]
    }
```
    

实现读取params.json 中的 seuratObject 的path，