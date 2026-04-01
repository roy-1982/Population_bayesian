
# 予測関数
predict_pop_stochastic <- function(base_pop, avg_alpha, sd_alpha, move_pattern, f_table, survival_table, periods = 24, move_adj = 0, edu_impact = 0, stochastic = TRUE, edu_growth_base) {
  
  # --- 101要素（0-100歳）のインデックスを作成 ---
  target_ages <- 0:100
  
  # 基本人口を101要素に強制整形（足りない年齢は0、超える分はカット）
  m_pop_raw <- base_pop %>% filter(性別 == "M")
  f_pop_raw <- base_pop %>% filter(性別 == "F")
  
  m_pop <- numeric(101)
  f_pop <- numeric(101)
  # matchを使って、データの年齢がtarget_agesのどこに該当するか紐付け
  m_pop[m_pop_raw$年齢 + 1] <- m_pop_raw$人口
  f_pop[f_pop_raw$年齢 + 1] <- f_pop_raw$人口
  
  # 生存率も同様に101要素に（念のため）
  m_surv <- numeric(101); f_surv <- numeric(101)
  m_surv_raw <- survival_table %>% filter(性別 == "M")
  f_surv_raw <- survival_table %>% filter(性別 == "F")
  m_surv[m_surv_raw$年齢 + 1] <- m_surv_raw$生残率
  f_surv[f_surv_raw$年齢 + 1] <- f_surv_raw$生残率
  # 100歳以上の生存率がNAなら0にする
  m_surv[is.na(m_surv)] <- 0; f_surv[is.na(f_surv)] <- 0
  
  # 出生率ベクトル（101要素）
  fert_vec <- numeric(101)
  fert_vec[f_table$年齢 + 1] <- f_table$全国出生率
  
  # 社会移動ベクトル（101要素）
  m_move <- numeric(101); f_move <- numeric(101)
  m_move_raw <- move_pattern %>% filter(性別 == "M")
  f_move_raw <- move_pattern %>% filter(性別 == "F")
  m_move[pmin(m_move_raw$年齢 + 1, 101)] <- m_move_raw$move_per_age
  f_move[pmin(f_move_raw$年齢 + 1, 101)] <- f_move_raw$move_per_age
  
  # --- 教育トレードオフの適用 ---
  # 1. Brain Drain: 進学率UPによる18歳の流出加速。スライダーが1のとき、過去20年の進学率上昇分と同じ強さの流出圧力を加える
  # (南伊豆の規模に合わせて係数250で調整)
  drain_val <- edu_growth_base * edu_impact * 250
  m_move[19] <- m_move[19] - drain_val
  f_move[19] <- f_move[19] - drain_val
  
  # 進学による流出の約40%が、別の「教育重視ファミリー」として転入してくる仮定
  attract_val <- (drain_val * 0.4)
  
  # --- 正規分布による重み付け関数の定義 ---
  # mean_age: ピークとなる年齢, sd: 広がり具合
  get_norm_weight <- function(ages, mean_age, sd) {
    w <- dnorm(ages, mean = mean_age, sd = sd)
    return(w / sum(w)) # 合計を1に正規化
  }
  
  # 親世代(30-45歳)の流入：35歳をピークに設定
  parent_ages <- 30:45
  parent_weights <- get_norm_weight(parent_ages, mean_age = 35, sd = 4)
  
  m_move[parent_ages + 1] <- m_move[parent_ages + 1] + (attract_val * parent_weights)
  f_move[parent_ages + 1] <- f_move[parent_ages + 1] + (attract_val * parent_weights)
  
  # 子世代(0-12歳)の流入：7歳（小学校低学年）をピークに設定
  child_ages <- 0:12
  child_weights <- get_norm_weight(child_ages, mean_age = 7, sd = 3)
  
  m_move[child_ages + 1] <- m_move[child_ages + 1] + (attract_val * child_weights)
  f_move[child_ages + 1] <- f_move[child_ages + 1] + (attract_val * child_weights)
  
  
  adj_vec <- numeric(101)
  adj_vec[21:40] <- move_adj # 20-39歳に調整適用
  
  res_list <- vector("list", periods)
  fac_list <- vector("list", periods)
  
  base_year <- 2026
  
  for (year in 1:periods) {
    curr_total <- sum(m_pop) + sum(f_pop)
    
    if (stochastic) {
      target_alpha <- rnorm(1, mean = avg_alpha, sd = sd_alpha)
      noise_m <- rnorm(101, mean = 0, sd = 0.5 + abs(m_move + adj_vec) * 0.2)
      noise_f <- rnorm(101, mean = 0, sd = 0.5 + abs(f_move + adj_vec) * 0.2)
    } else {
      target_alpha <- avg_alpha
      noise_m <- 0
      noise_f <- 0
    }
    
    total_births <- sum(f_pop * fert_vec * target_alpha, na.rm = TRUE)
    
    # 加齢・生残
    next_m_pop_pre <- c(total_births * (105/205), m_pop[1:100] * m_surv[1:100])
    next_f_pop_pre <- c(total_births * (100/205), f_pop[1:100] * f_surv[1:100])
    
    # 自然増減（次世代プレ - 現世代合計）
    natural_change <- (sum(next_m_pop_pre) + sum(next_f_pop_pre)) - curr_total
    
    # 社会移動適用
    m_pop <- pmax(next_m_pop_pre + m_move + adj_vec + noise_m, 0)
    f_pop <- pmax(next_f_pop_pre + f_move + adj_vec + noise_f, 0)
    
    social_change <- (sum(m_pop) + sum(f_pop)) - (sum(next_m_pop_pre) + sum(next_f_pop_pre))
    
    # 探索時以外は詳細データを保存
    if (stochastic) {
      curr_year_val <- base_year + year
      res_list[[year]] <- data.frame(
        年 = as.character(curr_year_val),
        年齢 = 0:100, M = m_pop, F = f_pop
      )
      fac_list[[year]] <- data.frame(
        年 = curr_year_val, 自然増減 = natural_change, 社会増減 = social_change, 出生数 = total_births
      )
    }
  }
  
  # 戻り値の処理
  if (stochastic) {
    res_df <- do.call(rbind, res_list) %>%
      pivot_longer(cols = c(M, F), names_to = "性別", values_to = "人口")
    return(list(pop = res_df, fac = do.call(rbind, fac_list)))
  } else {
    # 探索時は最後の年のデータだけを最小構成で返す
    final_df <- data.frame(年 = "2050", 年齢 = 0:100, M = m_pop, F = f_pop) %>%
      pivot_longer(cols = c(M, F), names_to = "性別", values_to = "人口")
    return(list(pop = final_df, fac = NULL))
  }
}
