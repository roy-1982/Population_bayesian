
get_estat_data <- function(appId) {
  
  # 出生率データ
  data <- estat_getStatsData(appId = appId, statsDataId = "0003411599") %>% 
    select(項目 = `母の年齢(5歳階級)`, 年 = `時間軸(年次)`, 値 = value) %>%
    filter(str_detect(項目, "出生率")) %>% 
    mutate(年 = str_remove(年, "年")) %>%
    pivot_wider(names_from = 項目, values_from = 値)
  
  # 南伊豆補正用データ
  data1 <- estat_getStatsData(appId = appId, statsDataId = "0000020201", cdArea = "22304",
                              cdCat01 = c("A4101", "A4200", "A5101", "A5102")) %>% 
    select(項目 = `Ａ　人口・世帯`, 年 = `調査年`, 値 = value) %>%
    mutate(年 = str_extract(年, "\\d{4}") %>% as.numeric(),
           項目 = str_remove(項目, "^[A-Z0-9]+_") %>% str_sub(1, 4)) %>%
    filter(年 >= 1996) %>% arrange(desc(年)) %>% 
    pivot_wider(names_from = 項目, values_from = 値)
  
  # 2020年基準データ
  data2 <- estat_getStatsData(appId = appId, statsDataId = "0003445139", cdArea = "22304")
  colnames(data2) <- str_trim(colnames(data2))
  base_women_2020 <- data2 %>%
    select(`国籍総数か日本人`, 性別 = `男女`, 年齢 = `年齢`, 値 = value) %>%
    filter(str_detect(`国籍総数か日本人`, "うち日本人"), 性別 == "女", str_detect(年齢, "^[0-9]+歳$")) %>%
    mutate(年齢_num = as.numeric(gsub("歳", "", 年齢))) %>%
    filter(年齢_num >= 20 & 年齢_num <= 39) %>%
    summarise(total = sum(値, na.rm = TRUE)) %>%
    pull(total)
  
  # 教育データ
  data3 <- estat_getStatsData(appId = appId, statsDataId = "0000020205", cdArea = "22304",
                              cdCat01 = c("E9101","E9102","E9103","E9105","E9106"))
  colnames(data3) <- str_trim(colnames(data3))
  edu_trend <- data3 %>%
    select(年 = `調査年`, 学歴 = `Ｅ　教育`, 値 = value) %>%
    mutate(学歴 = str_extract(学歴, "(?<=[（\\()]).+?(?=[）\\)])"), 年 = as.numeric(str_remove(年, "年度"))) %>%
    filter(学歴 %in% c("卒業者総数", "大学・大学院")) %>%
    pivot_wider(names_from = 学歴, values_from = 値) %>%
    mutate(大学進学率 = `大学・大学院` / `卒業者総数`) %>%
    arrange(年)
  edu_growth_base <- tail(edu_trend$大学進学率, 1) - first(edu_trend$大学進学率)
  
  # 社会移動データ
  data_move_raw <- estat_getStatsData(appId = appId, statsDataId = "0003419945", cdArea = "22304")
  colnames(data_move_raw) <- str_trim(colnames(data_move_raw))
  move_pattern <- data_move_raw %>%
    select(年 = `時間軸（年次）`, 国籍, 年齢, 性別, 値 = value, 項目 = `表章項目`) %>% 
    filter(str_detect(国籍, "日本人移動者"), !年齢 %in% c("総数", "再掲", "0～14歳", "15～64歳", "65歳以上"), 性別 != "総数") %>%
    pivot_wider(names_from = 項目, values_from = 値) %>%
    mutate(net_move = as.numeric(`他市町村からの転入者数`) - as.numeric(`他市町村への転出者数`)) %>%
    mutate(下限 = as.numeric(str_extract(年齢, "\\d+")), 上限 = as.numeric(str_extract(年齢, "(?<=～)\\d+")),
           上限 = ifelse(年齢 == "90歳以上", 100, 上限), 上限 = ifelse(is.na(上限), 下限, 上限), n = 上限 - 下限 + 1) %>%
    group_by(性別, 年齢, 下限, 上限, n) %>% 
    summarise(avg_net_move = mean(net_move, na.rm = TRUE), .groups = "drop") %>%
    uncount(n, .remove = FALSE) %>%
    group_by(性別, 年齢) %>%
    mutate(年齢_num = 下限 + row_number() - 1, 性別_code = ifelse(性別 == "男", "M", "F"), move_per_age = as.numeric(avg_net_move) / as.numeric(n)) %>%
    ungroup() %>%
    select(年齢 = 年齢_num, 性別 = 性別_code, move_per_age)
  
  return(list(
    data = data,
    data1 = data1,
    base_women_2020 = base_women_2020,
    edu_growth_base = edu_growth_base,
    move_pattern = move_pattern
  ))
}

# 補正係数(alpha)を計算する関数
calculate_alpha_params <- function(data1, pop_master, fertility_table) {
  calc_alpha <- pop_master %>%
    filter(性別 == "F", 年齢 >= 15 & 年齢 <= 49) %>%
    left_join(fertility_table, by = "年齢") %>%
    summarise(期待出生数 = sum(人口 * 全国出生率, na.rm = TRUE))
  
  target_births <- data1 %>%
    filter(年 >= 2013 & 年 <= 2022) %>%
    summarise(avg_births = mean(出生数, na.rm = TRUE)) %>%
    pull(avg_births)
  
  alpha <- target_births / calc_alpha$期待出生数
  
  alpha_sd_val <- data1 %>%
    filter(年 >= 2013 & 年 <= 2022) %>%
    mutate(yearly_alpha = 出生数 / calc_alpha$期待出生数) %>%
    summarise(sd_val = sd(yearly_alpha, na.rm = TRUE)) %>%
    pull(sd_val)
  
  return(list(alpha = alpha, alpha_sd_val = alpha_sd_val, target_births = target_births))
}

# CSV読み込みとデータ成形
process_local_data <- function(data) {
  # 1. 人口データの読み込みと成形
  df1 <- read_csv("data/population.csv") 
  df2 <- read_csv("data/life_table.csv")
  
  df1_long <- df1 %>%
    select(年齢, 男性, 女性) %>%
    pivot_longer(cols = c("男性", "女性"), names_to = "性別", values_to = "人口") %>%
    mutate(性別 = ifelse(性別 == "男性", "M", "F"))
  
  df2_cleaned <- df2 %>%
    mutate(定常人口 = as.numeric(str_remove_all(定常人口, "[[:space:]]")),
           生存数   = as.numeric(str_remove_all(生存数, "[[:space:]]"))) %>%
    group_by(性別) %>% 
    mutate(生残率 = lead(定常人口) / 定常人口) %>%
    ungroup()
  
  pop_master <- df1_long %>%
    left_join(df2_cleaned, by = c("年齢", "性別"))
  
  # 2. fertility_table (出生率テーブル) の成形
  fertility_table <- data %>%
    filter(年 == "2023") %>%
    pivot_longer(cols = starts_with("出生率"), names_to = "項目", values_to = "全国出生率_千対") %>%
    mutate(
      下限 = as.numeric(str_extract(項目, "\\d{2}")),
      上限 = ifelse(str_detect(項目, "45歳以上"), 49, as.numeric(str_extract(項目, "(?<=～)\\d{2}"))),
      n = 上限 - 下限 + 1
    ) %>%
    uncount(n) %>%
    group_by(項目) %>%
    mutate(年齢 = 下限 + row_number() - 1) %>%
    ungroup() %>%
    transmute(年齢, 全国出生率 = 全国出生率_千対 / 1000)
  
  return(list(
    pop_master = pop_master,
    fertility_table = fertility_table,
    df2_cleaned = df2_cleaned
  ))
}


