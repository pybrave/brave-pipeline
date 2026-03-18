## from.json 添加以下内容
```json
    {
      "name": "x_sample_replace_mode",
      "label": "x sample replace mode",
      "type": "BaseSelect",
      "initialValue": "none",
      "data": [
        { "label": "None", "value": "none" },
        { "label": "Regex", "value": "regex" },
        { "label": "K:V TextArea", "value": "kv" }
      ]
    },
    {
      "name": "x_sample_replace_from",
      "label": "x sample replace from (regex, ; separated)",
      "type": "BaseInput",
      "depends": [
        {
          "name": "x_sample_replace_mode",
          "value": "regex"
        }
      ],
      "initialValue": ""
    },
    {
      "name": "x_sample_replace_to",
      "label": "x sample replace to (; separated)",
      "type": "BaseInput",
      "depends": [
        {
          "name": "x_sample_replace_mode",
          "value": "regex"
        }
      ],
      "initialValue": ""
    },
    {
      "name": "x_sample_replace_kv",
      "label": "x sample replace K:V lines",
      "type": "BaseTextArea",
      "depends": [
        {
          "name": "x_sample_replace_mode",
          "value": "kv"
        }
      ],
      "initialValue": ""
    },
    {
      "name": "y_sample_replace_mode",
      "label": "y sample replace mode",
      "type": "BaseSelect",
      "initialValue": "none",
      "data": [
        { "label": "None", "value": "none" },
        { "label": "Regex", "value": "regex" },
        { "label": "K:V TextArea", "value": "kv" }
      ]
    },
    {
      "name": "y_sample_replace_from",
      "label": "y sample replace from (regex, ; separated)",
      "type": "BaseInput",
      "depends": [
        {
          "name": "y_sample_replace_mode",
          "value": "regex"
        }
      ],
      "initialValue": ""
    },
    {
      "name": "y_sample_replace_to",
      "label": "y sample replace to (; separated)",
      "type": "BaseInput",
      "depends": [
        {
          "name": "y_sample_replace_mode",
          "value": "regex"
        }
      ],
      "initialValue": ""
    },
    {
      "name": "y_sample_replace_kv",
      "label": "y sample replace K:V lines",
      "type": "BaseTextArea",
      "depends": [
        {
          "name": "y_sample_replace_mode",
          "value": "kv"
        }
      ],
      "initialValue": ""
    }
```

### main.R 添加以下内容
```r

```