
params.json 是根据用户输入 io_schema.json 生成的表单生成的，外部文件输入配置在 inputs/workflow(只有当scatter=each才使用workflow),文件输入配置格式如下:

```
{
            "label": "Ped File",
            "name": "ped",
            "type": "SelectSample",
            "input_type": "file",
            "component_id": "DEFAULT",
            "resolver": {
                "accept_formats": [
                    "DEFAULT"
                ]
            },
            "db": true,
            "rules": [
                {
                    "required": true,
                    "message": "\u8be5\u5b57\u6bb5\u4e0d\u80fd\u4e3a\u7a7a!"
                }
            ]
        }
```
一些其它配置请放在 io_schema.json 的 params 下，配置格式请参考如下
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
生成 bash的模板语法使用的pongo2

参考
/data2/brave_analysis_workspace/pipeline/script/05fa094c-7121-48c8-a05e-122f7d71f12d/main.sh
实现 
vcfCooker --in-bfile <bim file> --ref <reference.fasta>  --out <output-vcf> --write-vcf
bgzip <output-vcf>
