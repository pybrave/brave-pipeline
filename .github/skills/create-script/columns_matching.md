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

split_rule_tokens <- function(x) {
  if (is.null(x) || length(x) == 0) return(character())
  v <- as.character(x[[1]])
  if (is.na(v) || trimws(v) == "") return(character())
  tokens <- strsplit(v, ";", fixed = TRUE)[[1]]
  tokens <- trimws(tokens)
  tokens[tokens != ""]
}

normalize_replace_mode <- function(mode, regex_from = NULL, kv_text = NULL) {
  v <- tolower(trimws(as.character(mode %||% "")[[1]]))
  if (v %in% c("none", "regex", "kv")) return(v)

  if (length(split_rule_tokens(regex_from)) > 0) return("regex")
  if (length(parse_kv_pairs(kv_text)$keys) > 0) return("kv")
  "none"
}

parse_kv_pairs <- function(kv_text) {
  if (is.null(kv_text) || length(kv_text) == 0) {
    return(list(keys = character(), values = character()))
  }

  raw <- as.character(kv_text[[1]])
  if (is.na(raw) || trimws(raw) == "") {
    return(list(keys = character(), values = character()))
  }

  lines <- unlist(strsplit(raw, "\\r?\\n", perl = TRUE), use.names = FALSE)
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  keys <- character()
  values <- character()
  for (line in lines) {
    sep_pos <- regexpr(":", line, fixed = TRUE)
    if (sep_pos[[1]] <= 0) {
      stop(sprintf("K:V 替换规则格式错误（缺少冒号）: %s", line))
    }

    key <- trimws(substr(line, 1, sep_pos[[1]] - 1))
    value <- trimws(substr(line, sep_pos[[1]] + 1, nchar(line)))
    if (key == "") {
      stop(sprintf("K:V 替换规则格式错误（空 key）: %s", line))
    }

    keys <- c(keys, key)
    values <- c(values, value)
  }

  list(keys = keys, values = values)
}

apply_feature_replace_rules <- function(feature_names, from_rules, to_rules) {
  from_vec <- split_rule_tokens(from_rules)
  to_vec <- split_rule_tokens(to_rules)

  if (length(from_vec) == 0) {
    return(list(
      values = feature_names,
      rule_count = 0,
      from = character(),
      to = character(),
      changed_count = 0
    ))
  }

  if (length(to_vec) < length(from_vec)) {
    to_vec <- c(to_vec, rep("", length(from_vec) - length(to_vec)))
  }
  if (length(to_vec) > length(from_vec)) {
    to_vec <- to_vec[seq_along(from_vec)]
  }

  old_values <- feature_names
  new_values <- feature_names
  for (i in seq_along(from_vec)) {
    new_values <- gsub(from_vec[[i]], to_vec[[i]], new_values, perl = TRUE)
  }

  list(
    values = new_values,
    rule_count = length(from_vec),
    from = from_vec,
    to = to_vec,
    changed_count = sum(old_values != new_values)
  )
}

pick_param <- function(primary, fallback = NULL) {
  if (!is.null(primary) && length(primary) > 0) return(primary)
  fallback
}

apply_sample_replace <- function(sample_names, mode, regex_from = NULL, regex_to = NULL, kv_text = NULL) {
  mode_value <- normalize_replace_mode(mode, regex_from = regex_from, kv_text = kv_text)

  if (mode_value == "none") {
    return(list(
      values = sample_names,
      mode = mode_value,
      rule_count = 0,
      changed_count = 0,
      regex_from = character(),
      regex_to = character(),
      kv_keys = character(),
      kv_values = character()
    ))
  }

  if (mode_value == "regex") {
    res <- apply_feature_replace_rules(sample_names, regex_from, regex_to)
    return(list(
      values = res$values,
      mode = mode_value,
      rule_count = res$rule_count,
      changed_count = res$changed_count,
      regex_from = res$from,
      regex_to = res$to,
      kv_keys = character(),
      kv_values = character()
    ))
  }

  kv <- parse_kv_pairs(kv_text)
  new_values <- sample_names
  if (length(kv$keys) > 0) {
    for (i in seq_along(kv$keys)) {
      new_values[new_values == kv$keys[[i]]] <- kv$values[[i]]
    }
  }

  list(
    values = new_values,
    mode = mode_value,
    rule_count = length(kv$keys),
    changed_count = sum(sample_names != new_values),
    regex_from = character(),
    regex_to = character(),
    kv_keys = kv$keys,
    kv_values = kv$values
  )
}

# 读取参数（兼容旧参数名）
x_sample_replace_from <- pick_param(data$x_sample_replace_from, data$x_feature_replace_from)
x_sample_replace_to <- pick_param(data$x_sample_replace_to, data$x_feature_replace_to)
y_sample_replace_from <- pick_param(data$y_sample_replace_from, data$y_feature_replace_from)
y_sample_replace_to <- pick_param(data$y_sample_replace_to, data$y_feature_replace_to)
x_sample_replace_mode <- pick_param(data$x_sample_replace_mode, "none")
y_sample_replace_mode <- pick_param(data$y_sample_replace_mode, "none")
x_sample_replace_kv <- pick_param(data$x_sample_replace_kv, NULL)
y_sample_replace_kv <- pick_param(data$y_sample_replace_kv, NULL)

# 应用样本名替换
x_replace_res <- apply_sample_replace(
  colnames(x_mat),
  mode = x_sample_replace_mode,
  regex_from = x_sample_replace_from,
  regex_to = x_sample_replace_to,
  kv_text = x_sample_replace_kv
)
y_replace_res <- apply_sample_replace(
  colnames(y_mat),
  mode = y_sample_replace_mode,
  regex_from = y_sample_replace_from,
  regex_to = y_sample_replace_to,
  kv_text = y_sample_replace_kv
)

colnames(x_mat) <- make.unique(x_replace_res$values)
colnames(y_mat) <- make.unique(y_replace_res$values)

# 用替换后的列名做匹配
common_samples <- intersect(colnames(x_mat), colnames(y_mat))
if (length(common_samples) < 3) {
  stop("x_input 与 y_input 的共同样本列少于 3 个，无法进行相关性与显著性检验")
}

x_mat <- x_mat[, common_samples, drop = FALSE]
y_mat <- y_mat[, common_samples, drop = FALSE]

```