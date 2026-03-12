# LLM Skill: R Pipeline Script Coding Standard (Generic, Cross-Visualization)

## Scope
本规范用于指导 LLM 编写本仓库中的 R 分析脚本，适用于任意数据处理/统计/可视化任务，不局限于箱线图或热图。

目标：
- 代码可维护：参数解析、默认值、异常处理一致。
- 输出可审计：统一生成结构化 `output/output.md`。
- 运行可复现：输入、参数、统计结果、输出路径全部记录。

## Required Dependencies Pattern
按需引入包，优先显式命名空间调用（例如 `readr::read_tsv`）。

建议头部结构：
```r
library(tidyverse)
library(jsonlite)
```

仅在实际使用时再加载其他包（如 `pheatmap`、`ggdist`、`gghalves` 等）。

## Mandatory Utility Functions
每个脚本建议具备以下基础工具函数（可裁剪，但语义保持一致）：
- `%||%`：空值回退。
- `extract_column_names` / `extract_single_column`：兼容多种参数节点结构。
- `format_vector_for_info`：将向量转为 `output.md` 可读字符串（空则 `none`）。
- `to_bool`、`to_number`、`normalize_*`：参数规范化与边界收敛。

示例：
```r
`%||%` <- function(x, y) {
	if (is.null(x) || length(x) == 0) y else x
}
```

## Input and Parameter Rules
1. 固定读取 `params.json`（除非流程平台明确要求命令行参数）。
2. 对关键输入必须做存在性校验，失败时 `stop(sprintf(...))`。
3. 对数值参数做 `as.numeric` + `is.na` + 上下界保护。
4. 对枚举参数做白名单校验，非法值回退默认值。
5. 对列名选择做完整校验：
	 - 未选择列时报错；
	 - 选择列不存在时报错；
	 - 冲突列（如 feature/sample 重复）时报错。

## Error/Warning/Message Style
- 错误：统一 `stop(sprintf("...: %s", value))`
- 警告：统一 `warning(sprintf("...: %s", value))`
- 运行完成提示：统一 `message(sprintf("...: %s", output_path))`

禁止：
- `stop("固定文本")` 丢失上下文。
- 混用多套风格导致日志不可检索。

## Output Artifact Rules
脚本应至少输出：
- 主结果文件（如 PDF/TSV/CSV 等）。
- `output/output.md`（必须）。

如有矩阵/统计中间结果，建议额外输出：
- `corr_matrix.tsv` / `p_matrix.tsv` / `q_matrix.tsv` 等。

## output.md Strong Convention (MUST)
必须使用以下模式构建 `output.md`：

1. 先构建字符向量：`info_lines <- c(...)`
2. 每一条键值行必须使用 `sprintf`，禁止手工字符串拼接。
3. 最后统一写出：`readr::write_lines(info_lines, file.path(output_dir, "output.md"))`

推荐模板：
```r
info_lines <- c(
	"# Analysis Output",
	"",
	"## Run Info",
	sprintf("- run_time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
	sprintf("- params_path: %s", "params.json"),
	sprintf("- output_path: %s", output_dir),
	"",
	"## Params",
	sprintf("- method: %s", method),
	sprintf("- threshold: %s", threshold),
	"",
	"## Stats",
	sprintf("- input_row_count: %d", nrow(df)),
	sprintf("- valid_count: %d", valid_count),
	sprintf("- output_file: %s", output_file)
)

readr::write_lines(info_lines, file.path(output_dir, "output.md"))
```

格式要求：
- 标题层级固定为 `#` + `##`。
- 参数、统计信息采用 `- key: value`。
- 数值类型与占位符严格匹配：
	- 整数优先 `%d`
	- 一般文本 `%s`
	- 科学计数/显著性可用 `%.3g` 或业务指定格式

## Data Processing and Statistics Rules
1. 明确记录样本匹配过程：总样本、交集、各自独有样本。
2. 含缺失值时，记录清洗前后数量与被移除条目。
3. 统计检验需记录：
	 - 方法（如 `spearman` / `t-test` / `wilcox`）
	 - 多重校正方法（如 `BH`/`none`）
	 - 有效统计量数量（非 NA 计数）
4. 显著性阈值应参数化，且写入 `output.md`。

## Plotting Rules (When Applicable)
1. 图尺寸、字体、角度、标题位置等可配置。
2. 颜色参数必须做合法性校验（非法值回退默认并 warning）。
3. 多面板输出支持：
	 - 单图分面；或
	 - 按面板拆分多个输出文件。
4. 所有输出图路径应可追踪并在日志/message 中出现。

## File and Directory Rules
1. 统一输出目录：`output`。
2. 开始阶段执行：
```r
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
```
3. 所有输出文件使用 `file.path(output_dir, ...)` 生成路径。

## Compatibility and Robustness Rules
1. 允许参数向后兼容（旧字段可作为新字段 fallback）。
2. 关键步骤使用防御式编程：
	 - 长度检查
	 - 空值检查
	 - 类型转换容错
3. 对外部输入（文件、列名、规则文本）先校验再执行。

## LLM Execution Checklist
LLM 产出脚本前必须逐项自检：
- 是否实现 `%||%` 与核心 normalize/helper。
- 是否对输入文件和关键列做了完整校验。
- 是否所有错误/警告/完成提示都使用 `sprintf`。
- 是否生成 `output/output.md`。
- `output.md` 的键值行是否全部来自 `sprintf`。
- 是否记录运行信息、参数信息、统计信息、输出文件信息。
- 是否使用 `readr::write_lines` 一次性写出报告。

## Anti-Patterns (Do Not)
- 不要仅输出图而不输出 `output.md`。
- 不要在 `output.md` 中混用未格式化拼接字符串。
- 不要跳过样本匹配/缺失值处理统计记录。
- 不要把业务参数硬编码在函数内部且不回写到报告。

## Minimal Skeleton
```r
library(tidyverse)
library(jsonlite)

`%||%` <- function(x, y) {
	if (is.null(x) || length(x) == 0) y else x
}

format_vector_for_info <- function(x) {
	x <- as.character(x)
	x <- x[!is.na(x) & x != ""]
	if (length(x) == 0) return("none")
	paste(x, collapse = ", ")
}

params <- jsonlite::fromJSON("params.json", simplifyVector = FALSE)
output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ... load data / validate / process / plot or compute ...

info_lines <- c(
	"# Analysis Output",
	"",
	"## Run Info",
	sprintf("- run_time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
	sprintf("- params_path: %s", "params.json"),
	"",
	"## Stats",
	sprintf("- output_file: %s", output_file)
)

readr::write_lines(info_lines, file.path(output_dir, "output.md"))
message(sprintf("Output saved to: %s", output_file))
```

---
如果新任务是任意新图型或非图统计任务，优先复用本规范，不另起风格。
