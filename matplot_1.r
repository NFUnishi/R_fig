# 必要なパッケージを読み込み
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# Excelファイルのパスを指定
# 実際のファイル名に置き換えてください。
file_path <- "data.xlsx"
output_dir <- "plots"

dir.create(output_dir, showWarnings = FALSE)

# シート名を取得して、各シートごとにプロットを生成して保存
sheet_names <- excel_sheets(file_path)

for (sheet_name in sheet_names) {
  raw_data <- read_excel(file_path, sheet = sheet_name)

  plot_data <- raw_data %>%
    rename(Group = 1) %>%
    pivot_longer(-Group, names_to = "Variable", values_to = "Value") %>%
    mutate(
      Variable = factor(Variable, levels = unique(Variable)),
      Type = if_else(str_detect(Group, "Sim"), "Sim", "Exp"),
      Distance = case_when(
        str_detect(Group, "0\\.08m") ~ "0.08m",
        str_detect(Group, "0\\.38m") ~ "0.38m",
        str_detect(Group, "0\\.68m") ~ "0.68m",
        TRUE ~ "Other"
      )
    )

  p <- ggplot(plot_data, aes(x = Variable, y = Value, group = Group)) +
    geom_line(aes(color = Distance, linetype = Type), size = 1) +
    geom_point(aes(color = Distance, shape = Type), size = 3) +
    scale_color_manual(values = c("0.08m" = "red", "0.38m" = "blue", "0.68m" = "orange", "Other" = "black")) +
    scale_linetype_manual(values = c("Sim" = "solid", "Exp" = "dashed")) +
    scale_shape_manual(values = c("Sim" = 16, "Exp" = 17)) +
    labs(
      title = paste0(sheet_name, " の折れ線グラフ"),
      x = "変数",
      y = "値",
      color = "距離",
      linetype = "種類",
      shape = "種類"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      text = element_text(size = 12),
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 10)
    )

  output_file <- file.path(output_dir, paste0(str_replace_all(sheet_name, "[\\/:*?<>|\\\"]", "_"), ".png"))
  ggsave(output_file, plot = p, width = 10, height = 6, dpi = 300)
}
