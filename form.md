## form 表单json
```json
{
  "formJson": [
    {
      "name": "group_field",
      "label": "Group Field",
      "rules": [
        {
          "required": true,
          "message": "This field cannot be empty!"
        }
      ],
      "type": "GroupFieldSelect"
    },
    {
      "name": "logistic",
      "label": "Logistic File",
      "component_id": "75087620-2ff8-4045-8694-a0c19aac12fc",
      "db": true,
      "rules": [
        {
          "required": true,
          "message": "This field cannot be empty!"
        }
      ],
      "columns": [
        "sample",
        "independent_variable",
        "outcome",
        "interaction_1",
        "interaction_2"
      ],
      "modes":[0,1,0,0,0],
      "columns_rules":[1,1,0,0,0],
      "type": "CollectedSampleSelect",
      "group": "group_field"
    },{
        "name": "risk_factor_value",
        "label": "Risk factor value",
        "required": true,
        "type": "BaseInput",
        "col": 24
    },{
      "name": "filter_sample",
      "label": "Filter Sample",
      "required": true,
      "type": "BaseTextAreaNum",
      "col":24
    },{
      "type": "BaseSelect",
      "name": "family",
      "label": "Family",
      "initialValue": "binomial",
      "required": true,
      "rules": [
        {
          "required": true
        }
      ],
      "data": [
        {
          "label": "binomial",
          "value": "binomial"
        },
        {
          "label": "gaussian",
          "value": "gaussian"
        }
      ]
    },
    {
        "type": "BaseSelect",
        "name": "numeric_missing",
        "label": "Numeric Variables Missing",
        "initialValue": "median",
        "data": [
            {
                "label": "Median imputation",
                "value": "median"
            },
            {
                "label": "Mean imputation",
                "value": "mean"
            },
            {
                "label": "Complete case",
                "value": "complete_case"
            }
        ]
    }
  ]
}

```
## 经过react组件渲染后，用户在界面上选择了相应的参数，最终生成的json如下所示：
```json
{
  "group_field": "group",
  "logistic": {
    "id": 6994,
    "analysis_result_id": "96965ea4-1bb4-450e-8410-2b74b9bbef70",
    "sample_id": null,
    "file_name": "v13_male",
    "component_id": "75087620-2ff8-4045-8694-a0c19aac12fc",
    "file_type": "collected",
    "content": "/opt/brave_prod/workspace/analysis/959dd087-c948-408b-8557-22fb4a5a81ef/99ffdb15-fa10-4a31-a12f-34e8d7656789/6a6cff4b-f5c1-43aa-ab7a-d4b7733ef77d/output/v13_male.tsv",
    "form_type": "CollectedSampleSelect",
    "groups": [
      "sample",
      "independent_variable",
      "outcome",
      "interaction_1",
      "interaction_2"
    ],
    "sample": {
      "id": 6994,
      "sample_name": "Code",
      "analysis_result_id": "96965ea4-1bb4-450e-8410-2b74b9bbef70",
      "columns_name": "Code"
    },
    "independent_variable": [
      {
        "id": 6994,
        "sample_name": "drink",
        "analysis_result_id": "96965ea4-1bb4-450e-8410-2b74b9bbef70",
        "columns_name": "drink"
      },
      {
        "id": 6994,
        "sample_name": "L %",
        "analysis_result_id": "96965ea4-1bb4-450e-8410-2b74b9bbef70",
        "columns_name": "L %"
      },
      {
        "id": 6994,
        "sample_name": "PHQ-total",
        "analysis_result_id": "96965ea4-1bb4-450e-8410-2b74b9bbef70",
        "columns_name": "PHQ-total"
      },
      {
        "id": 6994,
        "sample_name": "sleep latency",
        "analysis_result_id": "96965ea4-1bb4-450e-8410-2b74b9bbef70",
        "columns_name": "sleep latency"
      }
    ],
    "outcome": {
      "id": 6994,
      "sample_name": "suicide",
      "analysis_result_id": "96965ea4-1bb4-450e-8410-2b74b9bbef70",
      "columns_name": "suicide"
    }
  },
  "risk_factor_value": "1",
  "family": "binomial",
  "numeric_missing": "median",
  "colors": {
    "logistic": "-"
  },
  "groups_name": {
    "logistic": "-"
  },
  "re_groups_name": {
    "logistic": "-"
  },
  "groups": [
    "logistic"
  ],
  "pipeline_dir": "/opt/brave_prod/workspace/pipeline"
}
```

## react组件的代码如下所示：
```js
import { Button, Col, ColorPicker, ColorPickerProps, Divider, Flex, Form, Input, InputNumber, Row, Select, Switch, Tooltip } from "antd";
import TextArea from "antd/es/input/TextArea";
import axios from "axios";
import { A } from "ollama/dist/shared/ollama.d792a03f.mjs";
import { Component, FC, useEffect, useState, memo } from "react";
import { useSelector } from "react-redux";
import { data, useOutletContext } from "react-router";
import { QuestionCircleOutlined } from "@ant-design/icons";


const ComponentsRender = ({ type, dataMap, constDataMap, componentMap, inputAnalysisMethod, dataKey: dataKey_, data: data_, name, component_id, inputKey, ...rest }: any) => {
    if (!dataMap) return <div></div> //(() => )
    dataMap = { ...dataMap, ...constDataMap }
    const componentObj = componentMap[type] //|| (() => <div>未知类型</div>);
    // if (first_data_key in dataMap_)
    // if(!dataMap_ && !dataMap_.first_data_key){
    //     dataMap_.first_data_key  =""
    // }
    // const { first_data_key, ...dataMap } = dataMap_
    if (!componentObj) {
        return <div>未知类型 {type}</div>
    }
    // debugger
    const { Component, dataKey, ...crest } = componentObj
    // debugger
    console.log(crest)

    let data: any = []
    if (data_) {
        data = data_

        //下游分析从数据库加载其它数据
    } else if (inputAnalysisMethod) {


        data = dataMap[inputAnalysisMethod]

    } else {
        // debugger
        if (dataKey_) {
            if (dataKey_ in dataMap) {
                data = dataMap[dataKey_]
            }
        } else {
            if (dataKey) {
                if (dataKey in dataMap) {
                    data = dataMap[dataKey]
                }

            }
            else if (component_id in dataMap) {
                data = dataMap[component_id]
            } else if ("first_data_key" in dataMap) {
                data = dataMap[dataMap['first_data_key']]
            } else {
                // 上游分析的key
                if (component_id in dataMap) {
                    data = dataMap[component_id]
                }
            }
            // if (inputKey) {


            // } else {


            // }

        }
    }

    // else{
    //     console.log(dataKey)
    //     const values = dataKey['sample_group_list'].map(((it:any)=>it.content.reference))
    //     console.log(values)
    // }
    // console.log(data) 
    // console.log(dataMap[dataKey])
    return <Component {...crest}  {...rest} data={data} name={name} />;
};

const FormJsonComp: FC<any> = memo(({ formJson, dataMap, analysisResultId }) => {
    if (!formJson) return null
    // const { projectObj } = useOutletContext<any>()
    const { projectObj } = useSelector((state: any) => state.user);
    const [parameter, setParameter] = useState<any>()
    useEffect(() => {
        if (projectObj?.parameter) {
            try {
                const param = JSON.parse(projectObj?.parameter)
                setParameter(param)
            } catch (e) {
                setParameter({})
            }
        }
    }, [projectObj?.parameter])

    const rank = [
        {
            label: "SGB",
            value: "SGB"
        }, {
            label: "SPECIES",
            value: "SPECIES"
        }, {
            label: "GENUS",
            value: "GENUS"
        }, {
            label: "FAMILY",
            value: "FAMILY"
        }, {
            label: "ORDER",
            value: "ORDER"
        }, {
            label: "CLASS",
            value: "CLASS"
        }, {
            label: "PHYLUM",
            value: "PHYLUM"
        },
    ]
    const getGroupField = () => {
        if (!projectObj?.metadata_form) return []
        return projectObj?.metadata_form.map((item: any) => ({
            label: item.label,
            value: item.name
        }))
    }
    const constDataMap = {
        "rank": rank,
        group_field: getGroupField()
    }
    // console.log("dataMap",dataMap)
    const componentMap: any = {
        GroupSelect: {
            Component: BaseSelect,
            dataKey: "sample_group_list"
        },
        Input: {
            Component: BaseTextArea,
        },
        BaseTextAreaNum: {
            Component: BaseTextAreaNum,
        },
        GroupCompareSelect: {
            Component: GroupCompareSelect,
        },
        BaseSelect: {
            Component: BaseSelect,
        },
        BaseInputNumber: {
            Component: BaseInputNumber,
        },
        BaseInput: {
            Component: BaseInput,
        }, BaseSwitch: {
            Component: BaseSwitch,
        },
        RankSelect: {
            Component: BaseSelect,
            dataKey: "rank",
            mode: undefined,
            initialValue: "SPECIES"
        }, GroupFieldSelect: {
            Component: BaseSelect,
            dataKey: "group_field",
            mode: undefined,
            initialValue: "group"
        }, FilterFieldSelect: {
            Component: FilterSelect,
            dataKey: "sample_group_list",
            mode: undefined,
            // initialValue:"sample_group"
        },
        GroupSelectSampleButton: {
            Component: GroupSelectSampleButton,
            // dataKey: "sample_group_list"
        }, SimplpeGroupSelect: {
            Component: SimplpeGroupSelect,
        }, CollectedSimplpeGroupSelect: {
            Component: CollectedSimplpeGroupSelect,
        }, CollectedGroupSelectSampleButton: {
            Component: CollectedGroupSelectSampleButton,
            // dataKey: "sample_group_list"
        }, CollectedGroupSelectSampleButton2: {
            Component: CollectedGroupSelectSampleButton2,
        }, CollectedSampleSelect: {
            Component: CollectedSampleSelect,
        }, MetaphlanCladeSelect: {
            Component: MetaphlanCladeSelect,
        }, SelectAll: {
            Component: SelectAll,
            dataKey: "sample_group_list"
        }, Divider: {
            Component: DividerComp,
        }, BaseColorPicker: {
            Component: BaseColorPicker,
        }, ThreeColorPicker: {
            Component: ThreeColorPicker,
        }, DifferenceAnalysisConditions: {
            Component: DifferenceAnalysisConditions,
        }, HeatmapParams: {
            Component: HeatmapParams,
        }
    };
    const getWatchData = (values: any) => {
        const watchData: Record<string, any> = {}

        formJson
            .filter((item: any) => item.depends)
            .forEach((it: any) => {
                it.depends.forEach((dep: any) => {
                    watchData[dep.name] = values?.[dep.name]
                })
            })

        return watchData
    }

    const form = Form.useFormInstance();
    const dependsValue: any = Form.useWatch((values: any) => (getWatchData(values)), form);
    const isDisplay = (depends: any) => {

        const display = depends.map((item: any) => {
            if (dependsValue) {
                return dependsValue[item.name] == item.value
            }
        }).every(Boolean)
        // console.log("dependsValue:", display)
        return display
    }

    //  "depends": [
    //     {
    //       "name": "method",
    //       "value": "elasticnet"
    //     }
    //   ],
    // console.log("dependsValue:", dependsValue)
    // if (depends) {
    //     // const dependsName = depends.map((item:any)=>item.name)

    // const display = depends.map((item: any) => dependsValue[item.name] == item.value).every(Boolean)
    // console.log("dependsValue:", display)

    // }
    return <>
        {/* {JSON.stringify(dependsValue)} */}
        <Row gutter={[8, 0]}>
            {/* {JSON.stringify(projectObj?.parameter)} */}
            {formJson.map((it: any, index: any) => {
                if (!it?.depends) {
                    return <Col span={it?.col ? it?.col : 24} key={index} >
                        {/* {it?.depends && isDisplay(it?.depends) && <>aaaaaaaaaa</>} */}
                        {/* {JSON.stringify(it)} */}

                        <ComponentsRender projParameter={parameter} analysisResultId={analysisResultId} key={index} {...it} dataMap={dataMap} componentMap={componentMap} constDataMap={constDataMap}></ComponentsRender>
                    </Col>
                } else {
                    if (isDisplay(it?.depends)) {
                        return <Col span={it?.col ? it?.col : 24} key={index} >
                            {/* {it?.depends && isDisplay(it?.depends) && <>aaaaaaaaaa</>} */}
                            {/* {JSON.stringify(it)} */}

                            <ComponentsRender projParameter={parameter} analysisResultId={analysisResultId} key={index} {...it} dataMap={dataMap} componentMap={componentMap} constDataMap={constDataMap}></ComponentsRender>
                        </Col>
                    }
                }

            })}
        </Row>


    </>
},
    (prevProps, nextProps) => {
        // console.log(prevProps)
        // console.log(nextProps)
        // // console.log("1111111111111111111"+(prevProps === nextProps))
        // // return prevProps === nextProps; // 自定义比较逻辑
        // console.log(prevProps)
        // console.log(nextProps)
        // return false
        console.log("prevProps==nextProps: ", JSON.stringify(prevProps) === JSON.stringify(nextProps)) //  === JSON.stringify(nextProps)
        // console.log("nextProps",JSON.stringify(nextProps)) 
        return JSON.stringify(prevProps) === JSON.stringify(nextProps) // JSON.stringify(prevProps.formJson) === JSON.stringify(nextProps.formJson) 
    }
)

export default FormJsonComp

const DividerComp: FC<any> = ({ text }) => {
    return <Divider orientation="left" >{text}</Divider>
}
const FilterSelect: FC<any> = ({ label, name, data, rules, field, ...rest }) => {
    const [options, setOptions] = useState<any>()
    const form = Form.useFormInstance();

    useEffect(() => {
        // console.log(data)
        if (data && field) {
            const filterField: any = [...new Set(data.map(field))]
            const options = filterField.map((it: any) => {
                return {
                    label: it,
                    value: it
                }
            })
            setOptions(options)
            // setInitialValue(filterField[0])
            form.setFieldValue(name, filterField[0])
            // console.log(options)
        }
    }, [data])
    return <>
        <Form.Item label={label} name={name} rules={rules}>
            <BasicSelect {...rest} options={options}></BasicSelect>
        </Form.Item>
    </>
}
const BasicSelect: FC<any> = ({ options, clear, value, onChange, projParameter, analysisResultId, ...rest }) => {
    const form = Form.useFormInstance();

    const selectChange = (value: any) => {
        onChange(value)
        if (clear) {
            form.resetFields(clear)
        }
    }
    return <Select showSearch filterOption={(input: any, option: any) =>
        (option?.label ?? '').toLowerCase().includes(input.toLowerCase())} {...rest} options={options} value={value} onChange={selectChange}></Select>
}


export const MetaphlanCladeSelect: FC<any> = ({ label, name, data, initialValue, rules, ...rest }) => {
    const [options, setOptions] = useState<any>()
    const loadData = async () => {
        const resp: any = await axios.get(`/fast-api/get_metaphlan_clade`)
        let data: any = resp.data.map((it: any) => {
            return {
                label: `${it.taxon.replaceAll(" ", "_")}_${it.clade}`,
                value: it.clade
            }
        })
        // data =  [
        //     {
        //         label: "0.2",
        //         value: 0.2
        //     }, {
        //         label: "0",
        //         value: 0
        //     }
        // ]
        setOptions(data)
        // console.log(data)
    }
    useEffect(() => {
        loadData()
    }, [])
    return <>
        <Form.Item initialValue={initialValue} label={label} name={name} rules={rules}>
            <Select   {...rest} options={options} showSearch filterOption={(input: any, option: any) =>
                (option?.label ?? '').toLowerCase().includes(input.toLowerCase())}></Select>
        </Form.Item>
        {/* <Form.Item name={`${name}_`} label={`${label}_`}>
            <Input value={111}></Input>
        </Form.Item> */}
    </>
}
export const GroupCompareSelect: FC<any> = ({ label, name, data, initialValue, rules, ...rest }) => {
    const form = Form.useFormInstance();
    const treatment_group = Form.useWatch(["sites1", "group"], form);
    const control_group = Form.useWatch(["sites2", "group"], form);
    const [query, setQuery] = useState<any>()
    useEffect(() => {
        if (treatment_group && control_group) {
            setQuery([
                {
                    label: `Prevalent in both sites`,
                    value: `Prevalent in both sites`
                }, {
                    label: `Prevalent in ${control_group.join("-")}`,
                    value: `Prevalent in ${control_group.join("-")}`
                }, {
                    label: `Prevalent in ${treatment_group.join("-")}`,
                    value: `Prevalent in ${treatment_group.join("-")}`
                }, {
                    label: `Not prevalent in either sites`,
                    value: `Not prevalent in either sites`
                }
            ])
        }
    }, [treatment_group, control_group])
    return <>
        <Form.Item initialValue={initialValue} label={label} name={name} rules={rules}>
            <Select showSearch filterOption={(input: any, option: any) =>
                (option?.label ?? '').toLowerCase().includes(input.toLowerCase())} {...rest} options={query}></Select>
        </Form.Item>
    </>
}
const BaseTextAreaNum: FC<any> = ({ label, name, tooltip, data, initialValue, rules, ...rest }) => {
    const form = Form.useFormInstance();

    const content = Form.useWatch(name, form);

    return <>

        <Form.Item tooltip={tooltip} extra={`A total of ${content ? content.split(",").length : 0} features are entered`} initialValue={initialValue} label={label} name={name} rules={rules}>
            <TextArea allowClear {...rest} rows={3} />

        </Form.Item>

    </>
}
const BaseTextArea: FC<any> = ({ label, name, data, initialValue, rules, ...rest }) => {
    return <>
        <Form.Item initialValue={initialValue} label={label} name={name} rules={rules}>
            <TextArea {...rest} />
        </Form.Item>
    </>
}
const BaseInput: FC<any> = ({ label, name, form, data, initialValue, rules, ...rest }) => {

    // if (depends) {
    //     // const dependsName = depends.map((item:any)=>item.name)
    //     const dependsValue = Form.useWatch((values: any) => values, form);
    //     const display = depends.map((item: any) => dependsValue[item.name] == item.value).every(Boolean)
    //     console.log("dependsValue:", display)

    // }

    return <>
        {/* {JSON.stringify(depends)} */}
        <Form.Item initialValue={initialValue} label={label} name={name} rules={rules}>
            <Input {...rest} />
        </Form.Item>
    </>
}
const BaseSwitch: FC<any> = ({ label, name, data, initialValue, rules, ...rest }) => {
    return <>
        <Form.Item initialValue={initialValue} label={label} name={name} rules={rules}>
            <Switch {...rest} />
        </Form.Item>
    </>
}

const BaseInputNumber: FC<any> = ({ label, name, data, initialValue, rules, ...rest }) => {
    return <>
        <Form.Item initialValue={initialValue} label={label} name={name} rules={rules}>
            <InputNumber {...rest} />
        </Form.Item>
    </>
}
export const BaseSelect: FC<any> = ({ extra, tooltip, label, name, data, initialValue, rules, ...rest }) => {
    return <>
        {/* {name} */}
        <Form.Item extra={extra} name={name} tooltip={tooltip}
            initialValue={initialValue ? initialValue : null} label={label} rules={rules}>
            <BasicSelect {...rest} options={data}></BasicSelect>
            {/* <Select showSearch filterOption={(input: any, option: any) =>
                (option?.label ?? '').toLowerCase().includes(input.toLowerCase())} {...rest} options={data}></Select> */}
        </Form.Item>
    </>
}


export const CollectedGroupSelectSampleButton2: FC<any> = ({ label, name, rules, data, filter, group, groupField: groupField_, analysisResultId }) => {
    const [sampleGrouped, setSampleGrouped] = useState<any>()
    const [options, setOptions] = useState<any>([])
    // const [collectFiles, setCollectFiles] = useState<any>([])
    const [analysisResult, setAnalysisResult] = useState<any>([])

    // const {  project } = useOutletContext<any>()

    // const [sampleGroupedOptions, setSampleGroupedOptions] = useState<any>([])
    const form = Form.useFormInstance();
    let filterName: any = []
    if (filter) {
        filterName = filter.map((it: any) => it.name)
    }
    const customFilterValue = Form.useWatch((values) => {
        const data = Object.entries(values).filter(([key]) => filterName.includes(key))
        return Object.fromEntries(data)
    }, form);

    const groupField = Form.useWatch(group, form);
    // const selectCollectFile = Form.useWatch([name, "file"], form);


    const calculateGroup = (sampleGroup: any, groupField: any) => {

        const grouped = sampleGroup.reduce((acc: any, item: any) => {
            const key = item[groupField];
            if (!acc[key]) {
                acc[key] = [];
            }
            acc[key].push(item.value);
            return acc;
        }, {});
        setSampleGrouped(grouped)


    }
    useEffect(() => {
        if (data && Array.isArray(data)) {
            const analysisResult = data.find((it: any) => it.analysis_result_id == analysisResultId)
            setAnalysisResult(analysisResult)
        }

    }, [data, analysisResultId])

    useEffect(() => {
        if (analysisResult) {
            // console.log("analysisResult: ", analysisResult)
            // let columnsData = data.find((it: any) => it.id == selectCollectFile)
            // debugger
            if (Array.isArray(analysisResult.columns)) {
                let columnsData = analysisResult.columns.map((it: any) => ({
                    label: it.columns_name,
                    value: it.columns_name,
                    ...it
                }))
                // console.log("columnsData: ", columnsData)
                if (filter && customFilterValue) {
                    columnsData = filter.reduce((result: any, filterHandle: any) => {
                        return result.filter((item: any) => {
                            return filterHandle.method(item) === customFilterValue[filterHandle.name];
                        });
                    }, columnsData);

                    columnsData = columnsData.map((it: any) => {
                        const { label, id, value, ...rest } = it
                        return {
                            label: `${it.label}(${filter[0].method(it)})`,
                            value: it.value,
                            ...rest

                        }
                    })

                }
                // console.log(data)
                // console.log(data)
                if (columnsData && groupField_) {
                    // console.log(data)
                    calculateGroup(columnsData, groupField_)
                } else {
                    if (columnsData && groupField) {
                        calculateGroup(columnsData, groupField)
                    }
                }
                setOptions(columnsData)


            }



        }

    }, [data, analysisResult, groupField, customFilterValue])

    // useEffect(() => {
    //     if (selectCollectFile) {
    //         // form.setFieldsValue(requestParam)
    //         setTimeout(() => {
    //             form.setFieldsValue(requestParam)

    //         }, 50);
    //     }
    // }, [selectCollectFile])
    return <>
        {/* <pre>
            {JSON.stringify(data, null, 2)}
        </pre> */}
        {/* {JSON.stringify(sampleGrouped)} */}
        {/* {JSON.stringify(requestParam)} */}
        {/* {JSON.stringify(analysisResult)} */}
        {/* <Form.Item label={`${label} File`} name={[name, "file"]} rules={rules}>
            <Select options={collectFiles} ></Select>
        </Form.Item> */}

        <Form.Item label={`${label} Columns`} name={[name, "columns"]} rules={rules}>
            <GroupSelectSample sampleGrouped={sampleGrouped} sampleGroup={options} watch={[name, "group"]}></GroupSelectSample>
        </Form.Item>
        <Flex gap="small">
            <Form.Item label={label} name={[name, "group"]} noStyle >
                <GroupSelectButton sampleGrouped={sampleGrouped}></GroupSelectButton>
            </Form.Item>
            <Form.Item name={[name, "group_name"]} >
                <Input size="small" placeholder="Optional group name"></Input>
            </Form.Item>
            <Form.Item name={[name, "color"]} >
                <ColorPickerComp />
            </Form.Item>
        </Flex>

    </>
}
export const CollectedSampleSelect: FC<any> = ({ label, modes = [], columns, name, columns_rules=[], rules, data, filter, group, groupField: groupField_, analysisResultId }) => {
    const [sampleGrouped, setSampleGrouped] = useState<any>()
    const [options, setOptions] = useState<any>([])
    const [collectFiles, setCollectFiles] = useState<any>([])
    // const {  project } = useOutletContext<any>()

    // const [sampleGroupedOptions, setSampleGroupedOptions] = useState<any>([])
    const form = Form.useFormInstance();
    let filterName: any = []
    if (filter) {
        filterName = filter.map((it: any) => it.name)
    }
    const customFilterValue = Form.useWatch((values) => {
        const data = Object.entries(values).filter(([key]) => filterName.includes(key))
        return Object.fromEntries(data)
    }, form);

    const groupField = Form.useWatch(group, form);
    const selectCollectFile = Form.useWatch([name, "file"], form);


    // const calculateGroup = (sampleGroup: any, groupField: any) => {

    //     const grouped = sampleGroup.reduce((acc: any, item: any) => {
    //         const key = item[groupField];
    //         if (!acc[key]) {
    //             acc[key] = [];
    //         }
    //         acc[key].push(item.value);
    //         return acc;
    //     }, {});
    //     setSampleGrouped(sampleGroup)


    // }
    useEffect(() => {
        if (data && Array.isArray(data)) {
            const collectedFiles = data.map((it: any) => ({
                label: `${it.file_name}`,
                value: it.id,
            }))
            setCollectFiles(collectedFiles)
        }

    }, [data])
    useEffect(() => {
        if (selectCollectFile) {
            let columnsData = data.find((it: any) => it.id == selectCollectFile)
            // debugger
            if (columnsData) {
                columnsData = columnsData.columns.map((it: any) => ({
                    label: it.columns_name,
                    value: it.columns_name,
                    ...it
                }))
                if (filter && customFilterValue) {
                    columnsData = filter.reduce((result: any, filterHandle: any) => {
                        return result.filter((item: any) => {
                            return filterHandle.method(item) === customFilterValue[filterHandle.name];
                        });
                    }, columnsData);

                    columnsData = columnsData.map((it: any) => {
                        const { label, id, value, ...rest } = it
                        return {
                            label: `${it.label}(${filter[0].method(it)})`,
                            value: it.value,
                            ...rest

                        }
                    })

                }
                // console.log(data)
                // console.log(data)
                // if (columnsData && groupField_) {
                //     // console.log(data)
                //     calculateGroup(columnsData, groupField_)
                // } else {
                //     if (columnsData && groupField) {
                //         calculateGroup(columnsData, groupField)
                //     }
                // }

                setOptions(columnsData)
            }

        }

    }, [data, selectCollectFile, groupField, customFilterValue])

    // useEffect(() => {
    //     if (selectCollectFile) {
    //         // form.setFieldsValue(requestParam)
    //         setTimeout(() => {
    //             form.setFieldsValue(requestParam)

    //         }, 50);
    //     }
    // }, [selectCollectFile])
    return <>

        <Form.Item label={`${label} File`} name={[name, "file"]} rules={rules}>
            <Select options={collectFiles} ></Select>
        </Form.Item>


        {(columns && Array.isArray(columns)) && columns.map((item: any, index: any) => (
            <div key={index}>
                {/* {JSON.stringify()} */}
                <Form.Item label={`${item} Columns`} name={[name, item]} rules={[{
                    "required": columns_rules[index] ? true : false,
                    "message": "This field cannot be empty!"
                }]}>
                    <Select showSearch
                        allowClear
                        mode={modes[index] ? "multiple" : undefined}
                        filterOption={(input: any, option: any) =>
                            (option?.label ?? '').toLowerCase().includes(input.toLowerCase())}
                        options={options}></Select>
                </Form.Item>

                {/* <Flex gap="small">
                    {item}
                    <Form.Item label={item} name={[name, "group", `${item}`]} noStyle >
                        <GroupSelectButton sampleGrouped={sampleGrouped} field={[name, item]}></GroupSelectButton>
                    </Form.Item>
                    <Form.Item name={[name, "group_name", `${item}`]} >
                        <Input size="small" placeholder="Optional group name"></Input>
                    </Form.Item>
                    <Form.Item name={[name, "color", `${item}`]} >
                        <ColorPickerComp projParameter={projParameter} />
                    </Form.Item>
                </Flex> */}
            </div>
        ))}



    </>
}

export const CollectedGroupSelectSampleButton: FC<any> = ({ label, modes = [], projParameter, columns, name, rules, data, filter, group, groupField: groupField_, analysisResultId }) => {
    const [sampleGrouped, setSampleGrouped] = useState<any>()
    const [options, setOptions] = useState<any>([])
    const [collectFiles, setCollectFiles] = useState<any>([])
    // const {  project } = useOutletContext<any>()

    // const [sampleGroupedOptions, setSampleGroupedOptions] = useState<any>([])
    const form = Form.useFormInstance();
    let filterName: any = []
    if (filter) {
        filterName = filter.map((it: any) => it.name)
    }
    const customFilterValue = Form.useWatch((values) => {
        const data = Object.entries(values).filter(([key]) => filterName.includes(key))
        return Object.fromEntries(data)
    }, form);

    const groupField = Form.useWatch(group, form);
    const selectCollectFile = Form.useWatch([name, "file"], form);


    const calculateGroup = (sampleGroup: any, groupField: any) => {

        const grouped = sampleGroup.reduce((acc: any, item: any) => {
            const key = item[groupField];
            if (!acc[key]) {
                acc[key] = [];
            }
            acc[key].push(item.value);
            return acc;
        }, {});
        setSampleGrouped(grouped)


    }
    useEffect(() => {
        if (data && Array.isArray(data)) {
            const collectedFiles = data.map((it: any) => ({
                label: `${it.file_name}`,
                value: it.id,
            }))
            setCollectFiles(collectedFiles)
        }

    }, [data])
    useEffect(() => {
        if (selectCollectFile) {
            let columnsData = data.find((it: any) => it.id == selectCollectFile)
            // debugger
            if (columnsData) {
                columnsData = columnsData.columns.map((it: any) => ({
                    label: it.columns_name,
                    value: it.columns_name,
                    ...it
                }))
                if (filter && customFilterValue) {
                    columnsData = filter.reduce((result: any, filterHandle: any) => {
                        return result.filter((item: any) => {
                            return filterHandle.method(item) === customFilterValue[filterHandle.name];
                        });
                    }, columnsData);

                    columnsData = columnsData.map((it: any) => {
                        const { label, id, value, ...rest } = it
                        return {
                            label: `${it.label}(${filter[0].method(it)})`,
                            value: it.value,
                            ...rest

                        }
                    })

                }
                // console.log(data)
                // console.log(data)
                if (columnsData && groupField_) {
                    // console.log(data)
                    calculateGroup(columnsData, groupField_)
                } else {
                    if (columnsData && groupField) {
                        calculateGroup(columnsData, groupField)
                    }
                }

                setOptions(columnsData)
            }

        }

    }, [data, selectCollectFile, groupField, customFilterValue])

    // useEffect(() => {
    //     if (selectCollectFile) {
    //         // form.setFieldsValue(requestParam)
    //         setTimeout(() => {
    //             form.setFieldsValue(requestParam)

    //         }, 50);
    //     }
    // }, [selectCollectFile])
    return <>

        <Form.Item label={`${label} File`} name={[name, "file"]} rules={rules}>
            <Select options={collectFiles} ></Select>
        </Form.Item>


        {(columns && Array.isArray(columns)) && columns.map((item: any, index: any) => (
            <div key={index}>
                <Form.Item label={`${item} Columns`} name={[name, item]} rules={rules}>
                    <GroupSelectSample mode={modes[index] ? "multiple" : undefined} sampleGrouped={sampleGrouped} sampleGroup={options} ></GroupSelectSample>
                </Form.Item>
                {modes[index] != 0 && <Flex gap="small">
                    {item}
                    <Form.Item label={item} name={[name, "group", `${item}`]} noStyle >
                        <GroupSelectButton sampleGrouped={sampleGrouped} field={[name, item]}></GroupSelectButton>
                    </Form.Item>
                    <Form.Item name={[name, "group_name", `${item}`]} >
                        <Input size="small" placeholder="Optional group name"></Input>
                    </Form.Item>
                    <Form.Item name={[name, "color", `${item}`]} >
                        <ColorPickerComp projParameter={projParameter} />
                    </Form.Item>
                </Flex>}

            </div>
        ))}



    </>
}


export const CollectedSimplpeGroupSelect: FC<any> = ({ label, projParameter, name, columns, rules, data, filter, group, groupField: groupField_, analysisResultId }) => {

    return <Row>

        {(columns && Array.isArray(columns)) && columns.map((item: any, index: any) => (
            <Col span={12} key={index}>
                <div>
                    {label} {item}:
                </div>
                <Flex gap="small">
                    <Form.Item name={[name, "group_name", `${item}`]} >
                        <Input size="small" placeholder="Optional group name"></Input>
                    </Form.Item>
                    <Form.Item name={[name, "color", `${item}`]} >
                        <ColorPickerComp projParameter={projParameter} />
                    </Form.Item>
                </Flex>
            </Col>

        ))}
    </Row>
}


export const SimplpeGroupSelect: FC<any> = ({ label, projParameter, name, rules, data, filter, group, groupField: groupField_, analysisResultId }) => {

    return <>
        <div>
            {label}:
        </div>
        <Flex gap="small">
            <Form.Item name={[name, "group_name"]} noStyle >
                <Input size="small" placeholder="Optional group name"></Input>
            </Form.Item>
            <Form.Item name={[name, "color"]} noStyle>
                <ColorPickerComp projParameter={projParameter} />
            </Form.Item>

        </Flex>
    </>
}

export const GroupSelectSampleButton: FC<any> = ({ label, projParameter, name, rules, data, filter, group, groupField: groupField_ }) => {
    const [sampleGrouped, setSampleGrouped] = useState<any>()
    const [options, setOptions] = useState<any>([])
    // const {  project } = useOutletContext<any>()

    // const [sampleGroupedOptions, setSampleGroupedOptions] = useState<any>([])
    const form = Form.useFormInstance();
    let filterName: any = []
    if (filter) {
        filterName = filter.map((it: any) => it.name)
    }
    const customFilterValue = Form.useWatch((values) => {
        const data = Object.entries(values).filter(([key]) => filterName.includes(key))
        return Object.fromEntries(data)
    }, form);

    const groupField = Form.useWatch(group, form);

    const calculateGroup = (sampleGroup: any, groupField: any) => {

        const grouped = sampleGroup.reduce((acc: any, item: any) => {
            const key = item[groupField];
            if (!acc[key]) {
                acc[key] = [];
            }
            acc[key].push(item.value);
            return acc;
        }, {});
        setSampleGrouped(grouped)


    }
    useEffect(() => {
        if (filter && customFilterValue) {
            data = filter.reduce((result: any, filterHandle: any) => {
                return result.filter((item: any) => {
                    return filterHandle.method(item) === customFilterValue[filterHandle.name];
                });
            }, data);

            data = data.map((it: any) => {
                const { label, id, value, ...rest } = it
                return {
                    label: `${it.label}(${filter[0].method(it)})`,
                    value: it.value,
                    ...rest

                }
            })
            //    data = 
            // for (let i = 0; i < filter.length; i++) {
            //     const filterHandle: any = filter[i]
            //     const filterData = data.filter((it: any) => {
            //         return filterHandle.method(it) == customFilterValue[filterHandle.name]
            //     })
            //     console.log(filterData)
            // }
        }
        // console.log(data)
        // console.log(data)
        if (data && groupField_) {
            // console.log(data)
            calculateGroup(data, groupField_)
        } else {
            if (data && groupField) {
                calculateGroup(data, groupField)
            }
        }

        setOptions(data)
        // form.resetFields()
        // if (groupField && sampleGroup && sampleGroup.length > 0) {
        //     // console.log("2222222")
        //     const sampleGroupedOptions = calculateGroup(sampleGroup, groupField)
        //     setSampleGroupedOptions(sampleGroupedOptions)

        // }
    }, [data, groupField, customFilterValue])
    return <>
        {/* {JSON.stringify(data)} */}
        <Form.Item label={label} name={[name, "sample"]} rules={rules}>
            <GroupSelectSample sampleGrouped={sampleGrouped} sampleGroup={options} watch={[name, "group"]}></GroupSelectSample>
        </Form.Item>
        <Flex gap="small">
            <Form.Item label={label} name={[name, "group"]} noStyle >
                <GroupSelectButton sampleGrouped={sampleGrouped} field={[name, "sample"]}></GroupSelectButton>
            </Form.Item>
            <Form.Item name={[name, "group_name"]} >
                <Input size="small" placeholder="Optional group name"></Input>
            </Form.Item>
            <Form.Item name={[name, "color"]} >
                {/* <Input size="small" placeholder="Optional group color" ></Input> */}
                <ColorPickerComp projParameter={projParameter} />
            </Form.Item>
        </Flex>

    </>
}
const ThreeColorPicker: FC<any> = ({ label, name, data, initialValue: initialValue_, rules, ...rest }) => {
    const [initialValue, setInitialValue] = useState<any>([null, null, null])
    useEffect(() => {
        if (initialValue_ && Array.isArray(initialValue_) && initialValue_.length == 3) {
            setInitialValue(initialValue_)
        }
    }, [initialValue_])

    return <>
        <Flex gap={"small"}>
            <div>{label}</div>
            <Tooltip title={`The first color represents a low value, and the third color represents a high value.`}>
                <QuestionCircleOutlined style={{ color: "rgba(0,0,0,0.45)" }} />
            </Tooltip>
        </Flex>

        <Row gutter={[8, 0]}>

            <Col span={8}>
                <Form.Item noStyle initialValue={initialValue[0]} name={[name, "color1"]} rules={rules}>
                    <ColorPickerComp {...rest} />
                </Form.Item>
            </Col>
            <Col span={8}>
                <Form.Item initialValue={initialValue[1]} noStyle name={[name, "color2"]} rules={rules}>
                    <ColorPickerComp {...rest} />
                </Form.Item>
            </Col>
            <Col span={8}>
                <Form.Item initialValue={initialValue[2]} noStyle name={[name, "color3"]} rules={rules}>
                    <ColorPickerComp {...rest} />
                </Form.Item>
            </Col>
        </Row>

    </>
}
const BaseColorPicker: FC<any> = ({ label, name, data, initialValue, rules, ...rest }) => {
    return <>
        <Form.Item initialValue={initialValue} label={label} name={name} rules={rules}>
            <ColorPicker {...rest} />
        </Form.Item>
    </>
}

const ColorPickerComp: FC<any> = ({ projParameter, value, onChange, ...rest }) => {
    const customPanelRender: ColorPickerProps['panelRender'] = (
        _,
        { components: { Picker, Presets } },
    ) => (
        <Row justify="space-between" wrap={false}>
            <Col span={12}>
                <Presets />
            </Col>
            <Divider type="vertical" style={{ height: 'auto' }} />
            <Col flex="auto">
                <Picker />
            </Col>
        </Row>
    );
    return <>


        {/* <ColorPicker
            styles={{ popupOverlayInner: { width: 480 } }}
            presets={presets}
           
        /> */}
        {/* {JSON.stringify(value)} */}
        <ColorPicker styles={{ popupOverlayInner: { width: 480 } }}
            panelRender={customPanelRender} presets={(projParameter?.colors) ? projParameter?.colors : []} allowClear value={value} onChange={(color) => {
                const hexColor = color.toHexString();
                onChange(hexColor)
            }}  ></ColorPicker>
    </>
}
export const SelectAll: FC<any> = ({ label, name, data, initialValue, rules, ...rest }) => {
    return <>
        <Form.Item initialValue={initialValue} label={label} name={name} rules={rules}>
            <SelectAllComp data={data} {...rest}></SelectAllComp>
        </Form.Item>
    </>
}
const SelectAllComp: FC<any> = ({ data, value, onChange, mode, ...rest }) => {
    const [selectedItems, setSelectedItems] = useState<any>(value);
    // const [options, setOptions] = useState<any>([]);
    const onChangeSelct = (value: any) => {
        console.log(value)
        onChange(value)
        setSelectedItems(value)
    }

    // useEffect(() => {
    //     setOptions(options)
    // }, [resultTableList])
    return <>

        <Select {...rest} mode={mode} value={selectedItems} onChange={onChangeSelct} allowClear options={data}></Select>
        {mode == "multiple" && <Button onClick={() => {
            const values = data.map((it: any) => it.value)
            // console.log(values)
            setSelectedItems(values)
            onChange(values)
        }}>Select All{selectedItems && <>({selectedItems.length})</>}</Button>}
    </>
}



const GroupSelectSample: FC<any> = ({ value, onChange, mode, sampleGroup, watch, sampleGrouped }) => {
    // const [options, setOptions] = useState<any>([])
    // const [groupedLabel,setGroupedLabel] = useState<any>([])
    // const [grouped, setGrouped] = useState<any>({})
    // const [groupedKey, setGroupedKey] = useState<any>({})
    // const [group, setGroup] = useState<any>()

    // const form = Form.useFormInstance();
    // const group_ = Form.useWatch(watch, form);
    // useEffect(() => {
    //     // setGroup(group_)
    //     if (group_ && (Array.isArray(group_) && group_.length > 0)) {
    //         // console.log("group_", group_)

    //         // console.log(group)
    //         onSelectGroup(group_)
    //     }
    // }, [group_, sampleGrouped])
    // useEffect(() => {

    //     // onSelectGroup(group)
    // }, [group])

    // const onSelectGroup = (keys: any) => {
    //     const merged = Object.entries(sampleGrouped ? sampleGrouped : {})
    //         .filter(([key]) => keys.includes(key)) // 只保留特定 key
    //         .flatMap(([, value]) => value);
    //     // console.log(merged)
    //     // const value = grouped[key]
    //     // console.log(value)
    //     onChange(merged)
    //     // setGroupedKey(key)
    // }


    return <>
        {/* {watch}{group} */}
        {/* {JSON.stringify(value)} */}
        {/* {mode} */}
        <Select showSearch allowClear
            filterOption={(input: any, option: any) =>
                (option?.label ?? '').toLowerCase().includes(input.toLowerCase())}
            mode={mode}
            value={value} onChange={onChange} options={sampleGroup}></Select>
        {(mode == 'multiple' && value) &&
            <>A total of {value.length} samples were selected</>
        }

    </>
}
const GroupSelectButton: FC<any> = ({ value, onChange, field, sampleGrouped }) => {
    // const [options, setOptions] = useState<any>([])
    // const [groupedLabel,setGroupedLabel] = useState<any>([])
    // const [grouped, setGrouped] = useState<any>({})
    // const [groupedKey, setGroupedKey] = useState<any>(value ? value : [])

    //
    //  const [optionsValues,setOptionsValues] = useState<any>(value)
    // useEffect(() => {
    //     const grouped = sampleGroup.reduce((acc: any, item: any) => {
    //         const key = item[group_field];
    //         if (!acc[key]) {
    //             acc[key] = [];
    //         }
    //         acc[key].push(item.value);
    //         return acc;
    //     }, {});
    //     setGrouped(grouped)

    // }, [sampleGroup, group_field])
    const form = Form.useFormInstance();
    const onSelectSample = (keys: any) => {
        const merged = Object.entries(sampleGrouped ? sampleGrouped : {})
            .filter(([key]) => keys.includes(key)) // 只保留特定 key
            .flatMap(([, value]) => value);
        console.log(merged)
        form.setFieldValue(field, merged)
        // const value = grouped[key]
        // console.log(value)
        // onChange(merged)
        // setGroupedKey(key)
    }
    const onSelectGroup = (key: any) => {
        let currentKey: any = []
        if ((value ? value : []).includes(key)) {
            currentKey = (value ? value : []).filter((it: any) => !it.includes(key))
            // setGroupedKey(currentKey)
            onChange(currentKey)
        } else {
            currentKey = [...(value ? value : []), key]
            // setGroupedKey(currentKey)
            onChange(currentKey)
        }
        // console.log(currentKey)
        onSelectSample(currentKey)
        // setGroupedKey([key])

    }
    return <>
        {/* <Select mode={"multiple"} value={value} onChange={onSelectChange} options={options}></Select> */}
        <Flex gap="small" >
            {Object.entries(sampleGrouped ? sampleGrouped : {}).map(([key, value2]: any) => (<span key={key}>
                <Button size="small" type={(value ? value : []).includes(key) ? "primary" : "dashed"} onClick={() => onSelectGroup(key)}>{key}({value2.length})</Button>
            </span>))}
        </Flex>
        {/* {groupedKey} */}

    </>
}

const DifferenceAnalysisConditions: FC<any> = ({ label, sig_type, name, rules }) => {

    return <>
        <DividerComp text={label}></DividerComp>
        <Row gutter={[8, 8]}>
            <Col span={12}>
                <Form.Item
                    initialValue={sig_type && sig_type[0]?.value}
                    label="Significant level type"
                    name={`__${name}_sig_type`}
                    tooltip="Choose the type of significance value: p-value or q-value (adjusted p-value)."
                    rules={rules}
                >
                    <Select
                        options={sig_type}
                    />
                </Form.Item>
            </Col>

            <Col span={12}>
                <Form.Item
                    initialValue={0.05}
                    label="Significant level threshold"
                    name={`__${name}_sig_threshold`}
                    tooltip="Threshold below which a result is considered statistically significant, e.g., 0.05."
                    rules={rules}
                >
                    <InputNumber style={{ width: "100%" }} min={0} max={1} step={0.01} />
                </Form.Item>
            </Col>

            <Col span={12}>
                <Form.Item
                    initialValue={0}
                    label="Effect threshold"
                    name={`__${name}_effect_threshold`}
                    tooltip="Minimum effect size (e.g., log2 fold change) required to consider a result biologically meaningful."
                    rules={rules}
                >
                    <InputNumber style={{ width: "100%" }} min={0} />
                </Form.Item>
            </Col>
        </Row>

    </>
}


const HeatmapParams: FC<any> = ({ label, name, rules }) => {

    return <>
        <DividerComp text={label} />


        <Row gutter={[8, 8]}>
            <Col span={24}>
                <Form.Item
                    initialValue={""}
                    label="title"
                    name={`__${name}_title`}
                    tooltip="heatmap title."
                >
                    <Input />
                </Form.Item>
            </Col>
            {/* Width */}
            <Col span={12}>
                <Form.Item
                    initialValue={8}
                    label="Width (inches)"
                    name={`__${name}_width`}
                    tooltip="Width of the output PDF in inches."
                    rules={rules}
                >
                    <InputNumber style={{ width: "100%" }} min={1} step={0.5} />
                </Form.Item>
            </Col>


            {/* Height */}
            <Col span={12}>
                <Form.Item
                    initialValue={8}
                    label="Height (inches)"
                    name={`__${name}_height`}
                    tooltip="Height of the output PDF in inches."
                    rules={rules}
                >
                    <InputNumber style={{ width: "100%" }} min={1} step={0.5} />
                </Form.Item>
            </Col>
            {/* cluster_rows */}
            <Col span={12}>
                <Form.Item
                    initialValue={true}
                    label="Cluster rows"
                    name={`__${name}_cluster_rows`}
                    tooltip="Whether to perform hierarchical clustering on rows."
                    valuePropName="checked"
                >
                    <Switch />
                </Form.Item>
            </Col>


            {/* cluster_cols */}
            <Col span={12}>
                <Form.Item
                    initialValue={true}
                    label="Cluster columns"
                    name={`__${name}_cluster_cols`}
                    tooltip="Whether to perform hierarchical clustering on columns."
                    valuePropName="checked"
                >
                    <Switch />
                </Form.Item>
            </Col>
            {/* show_rownames */}
            <Col span={12}>
                <Form.Item
                    initialValue={true}
                    label="Show row names"
                    name={`__${name}_show_rownames`}
                    tooltip="Display row labels in the heatmap."
                    valuePropName="checked"
                >
                    <Switch />
                </Form.Item>
            </Col>
            {/* show_colnames */}
            <Col span={12}>
                <Form.Item
                    initialValue={true}
                    label="Show column names"
                    name={`__${name}_show_colnames`}
                    tooltip="Display column labels in the heatmap."
                    valuePropName="checked"
                >
                    <Switch />
                </Form.Item>
            </Col>

        </Row>





    </>
}
```