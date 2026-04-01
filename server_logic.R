
observe({
  showModal(modalDialog(
    title = span(style = "color: #00d4ff; font-weight: bold;", 
                 bsicons::bs_icon("rocket-takeoff-fill"), "シミュレーターへようこそ"),
    div(style = "line-height: 1.6;",
        "南伊豆町の未来を構想するための", tags$b("シミュレーター"), "です。", tags$br(), tags$br(),
        "本バージョンでは、以下の分析機能がご利用いただけます：", 
        tags$ul(
          tags$li(tags$b("詳細分析タブ：", style = "color: #ff9f43;"), "人口ピラミッドの変化や、消滅可能性のリスクを詳細に可視化します。"),
          tags$li(tags$b("教育環境パラメータ：", style = "color: #ff9f43;"), "進学志向や定着支援が人口移動に与える影響をシミュレートできます。"),
          tags$li(tags$b("AI最適化：", style = "color: #ff9f43;"), "「若年女性の維持」と「現役世代の負担軽減」を両立する政策バランスをAIが探索します。")
        ),
        "左パネルの各数値を自由に調整し、", tags$b("「シミュレーション実行」"), "または", tags$b("「最適政策バランスを探索」"), "ボタンを押して、町が目指すべき未来を探ってみてください。", tags$br(), tags$br(),
        # --- お問い合わせへの誘導 ---
        div(style = "border-top: 1px solid #444; padding-top: 10px; margin-top: 10px; font-size: 0.85rem;",
            bsicons::bs_icon("info-circle", style = "color: #00d4ff;"),
            " 本ツールに関するご質問やご相談は、画面右上の", 
            tags$b("「お問い合わせ」", style = "border-bottom: 1px solid #ffffff;"),
            "ボタンよりお気軽にご連絡ください。"
        )
    ),
    footer = modalButton("シミュレーションを開始する"),
    size = "m",
    easyClose = TRUE
  ))
})

  


evaluate_policy_internal <- function(x) {
    res <- predict_pop_stochastic(
      base_pop = pop_master, 
      avg_alpha = x[1], 
      sd_alpha = alpha_sd_val, 
      move_pattern = move_pattern, 
      f_table = fertility_table, 
      survival_table = df2_cleaned, 
      periods = 24, 
      move_adj = x[2],
      edu_impact = x[3],
      stochastic = FALSE,
      edu_growth_base = edu_growth_base
    )
    final_pop <- res$pop %>% filter(年 == "2050")
    
    women <- sum(final_pop$人口[final_pop$性別 == "F" & final_pop$年齢 >= 20 & final_pop$年齢 <= 39])
    working <- sum(final_pop$人口[final_pop$年齢 >= 15 & final_pop$年齢 <= 64])
    elderly <- sum(final_pop$人口[final_pop$年齢 >= 65])
    ratio <- ifelse(working > 0, elderly / working, 999)
    penalty_weight <- 50  # ペナルティの強さを決める変数
    cost_move <- exp(x[2] * 4) * 15      
    cost_alpha <- (x[1] - alpha)^2 * 1000 
    cost_edu <- (x[3])^2 * 500
    return(c(-women + cost_move + cost_alpha + cost_edu, ratio))
  } 
  
  # 「最適政策バランスを探索」ボタンの動作
  observeEvent(input$find_optimal, {
    # --- ボタン連打防止 ---
    shinyjs::disable("find_optimal")
    shinyjs::disable("do_sim") 
    on.exit({
      shinyjs::enable("find_optimal")
      shinyjs::enable("do_sim")
    })
    
    withProgress(message = 'AIが最適な政策バランスを計算中...', value = 0, {
      steps <- c(10, 30, 50)
      opt_res <- NULL
      
      for (g in steps) {
        opt_res <- mco::nsga2(
          fn = evaluate_policy_internal, 
          idim = 3, odim = 2,
          lower.bounds = c(1.0, -0.2, -2.0),
          upper.bounds = c(2.0,  1.5,  2.0),
          popsize = 40,      
          generations = g     
        )
        # 進捗率を更新
        incProgress(1/length(steps))
      }
      
      scores <- scale(opt_res$value[,1]) + scale(opt_res$value[,2])
      best_idx <- which.min(scores)
      best_params <- opt_res$par[best_idx, ]
      
      if(!any(is.na(best_params))) {
        updateSliderInput(session, "alpha_val", value = round(best_params[1], 2))
        updateSliderInput(session, "move_plus", value = round(best_params[2], 1))
        updateSliderInput(session, "edu_focus", value = round(best_params[3], 2))
      }
      
      shinyWidgets::sendSweetAlert(
        session, 
        title = "探索完了", 
        text = tags$div(
          style = "text-align: left; line-height: 1.6;",
          HTML(paste0(
            "南伊豆町の将来推計に基づき、<b>「若年女性人口の維持」</b>と<b>「現役世代の負担軽減」</b>を両立する現実的なバランスを特定しました。<br><br>",
            "<b>【AIが推奨する政策パッケージ】</b><br>",
            "1. <b>子育て支援</b> (出生率): 係数 <b>", round(best_params[1], 2), "</b><br>",
            "2. <b>移住・定住施策</b>: <b>", round(best_params[2], 1), "人</b> / 各年齢（20-39歳）<br>",
            "3. <b>教育環境への投資</b>: スライダー <b>", round(best_params[3], 1), "</b> レベルを維持<br><br>",
            "<div style='background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 4px solid #00d4ff;'>",
            "<span style='font-size: 0.85em; color: #444;'>",
            "<b>AIの分析結果:</b><br>",
            "教育投資によるファミリー層の流入（35歳前後）と、進学に伴う若者の流出（18歳）のトレードオフを考慮した結果、上記の数値が最も持続可能性が高いと判断されました。",
            "</span></div><br>",
            "<span style='font-size: 0.9em; color: #ff9f43; font-weight: bold;'>※スライダーを自動更新しました。「予測実行」ボタンを押して詳細を確認してください。</span>"
          ))
        ),
        html = TRUE, 
        type = "success"
      )
    })
  }
  )
  
  # 各種入力の監視
  observeEvent(input$n_sim, {
    if (input$n_sim < 10) updateSliderInput(session, "n_sim", value = 10)
  })
  # ----------------------------------------
  
  # 目標出生数からスライダーを自動更新するロジック
  observeEvent(input$target_birth_n, {
    req(input$target_birth_n) 
    
    if (input$target_birth_n > 0) {
      current_expected <- pop_master %>%
        filter(性別 == "F", 年齢 >= 15 & 年齢 <= 49) %>%
        left_join(fertility_table, by = "年齢") %>%
        summarise(val = sum(人口 * 全国出生率, na.rm = TRUE)) %>%
        pull(val)
      
      if (!is.na(current_expected) && current_expected > 0) {
        required_alpha <- input$target_birth_n / current_expected
        
        # スライダーの値を更新
        updateSliderInput(
          session, 
          "alpha_val", 
          value = round(required_alpha, 2)
        )
      }
    }
  })
  
  raw_sim_results <- eventReactive(input$do_sim, {
    req(input$n_sim > 0)
    shinyjs::disable("do_sim")    
    shinyjs::disable("find_optimal")
    on.exit({
      shinyjs::enable("do_sim")
      shinyjs::enable("find_optimal")
    }) 
    
    msge_list <- c(
      "結果を計算中...",
      "移住シナリオを検証中...",
      "出生率のゆらぎを適用中...",
      paste0(input$n_sim, "通りの可能性を探索中..."),
      "統計的な不確実性を解析中...",
      "判定フラグを生成中...",
      "まもなく結果が確定します..."
    )
    
    withProgress(message = '予測エンジン起動...', value = 0, {
      all_pop <- list()
      all_fac <- list()
      
      for (i in 1:input$n_sim) {
        # 進捗に合わせてメッセージを切り替え
        idx <- ceiling((i / input$n_sim) * length(msge_list))
        current_msg <- msge_list[idx]
        
        incProgress(1/input$n_sim, 
                    message = current_msg, 
                    detail = paste0(i, " / ", input$n_sim, " 回目の試行"))
        
        res <- predict_pop_stochastic(
          base_pop = pop_master, 
          avg_alpha = input$alpha_val,
          sd_alpha = alpha_sd_val, 
          move_pattern = move_pattern, 
          f_table = fertility_table, 
          survival_table = df2_cleaned, 
          move_adj = input$move_plus,
          edu_impact = input$edu_focus,
          edu_growth_base = edu_growth_base
        )
        
        all_pop[[i]] <- res$pop %>% mutate(sim_id = i)
        all_fac[[i]] <- res$fac %>% mutate(sim_id = i)
      }
      # 全ての計算（ループ）が終わった瞬間に音を鳴らす命令をブラウザに送る
      session$sendCustomMessage("play_sound", list())
      list(pop = bind_rows(all_pop), fac = bind_rows(all_fac))
    })
  }, ignoreNULL = FALSE)
  
  # 統計集計（グラフ描画用）
  summary_df <- reactive({
    req(raw_sim_results())
    dat <- raw_sim_results()$pop 
    
    dat %>%
      mutate(年 = as.numeric(年)) %>%
      group_by(年, sim_id) %>%
      summarise(
        総人口 = sum(人口, na.rm = TRUE),
        現役世代 = sum(人口[年齢 >= 15 & 年齢 <= 64], na.rm = TRUE),
        高齢者 = sum(人口[年齢 >= 65], na.rm = TRUE),
        背負う人数 = 高齢者 / 現役世代,
        若年女性 = sum(人口[性別 == "F" & 年齢 >= 20 & 年齢 <= 39]),
        .groups = "drop"
      ) %>%
      group_by(年) %>%
      summarise(
        中央値 = median(総人口),
        下限95 = quantile(総人口, 0.025),
        上限95 = quantile(総人口, 0.975),
        下限50 = quantile(総人口, 0.25),   
        上限50 = quantile(総人口, 0.75), 
        扶養中央 = median(背負う人数),
        若年女性中央 = median(若年女性),
        若年女性下限 = quantile(若年女性, 0.025),
        若年女性上限 = quantile(若年女性, 0.975),
        .groups = "drop"
      )
  })
  
  # 将来推計グラフ
  output$distPlot <- renderPlot({
    s_dat <- summary_df()
    req(s_dat)
    y_limit <- max(s_dat$上限95, na.rm = TRUE) * 1.2
    
    ggplot(s_dat, aes(x = 年)) +
      geom_ribbon(aes(ymin = 下限95, ymax = 上限95), fill = "white", alpha = 0.15) +
      geom_ribbon(aes(ymin = 下限50, ymax = 上限50), fill = "white", alpha = 0.25) +
      geom_line(aes(y = 中央値), color = "#00d4ff", linewidth = 0.8) +
      geom_point(aes(y = 中央値), color = "#00d4ff", size = 3) +
      geom_text(aes(y = 中央値, label = scales::comma(round(中央値, 0))), 
                vjust = -1.7, color = "#00d4ff", size = 3, family = "mplus", fontface = "bold") +
      expand_limits(y = y_limit) +
      labs(title = "将来人口予測（コーホート要因法 × ベイズ統計）", x = "", y = "推定総人口") +
      theme_minimal(base_family = "mplus") +
      theme(
        plot.background = element_rect(fill = "#1a1a1a", color = NA),
        panel.background = element_rect(fill = "#1a1a1a", color = NA),
        plot.title = element_text(family = "dela", size = 17),
        # plot.title = element_text(size = 21, hjust = 0.5, family = "dela", color = "white"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        panel.grid.major = element_line(color = "gray30", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 0, 20) 
      )
  }, res = 96)
  
  # ピラミッドグラフ
  output$pyramidPlot <- renderPlot({
    req(raw_sim_results())
    sim_dat <- raw_sim_results()$pop
    
    df_2026 <- pop_master %>%
      select(年齢, 性別, 人口) %>%
      mutate(時期 = "2026年 (現在)")
    df_2050 <- sim_dat %>%
      filter(年 == "2050") %>%
      group_by(年齢, 性別) %>%
      summarise(人口 = median(人口, na.rm = TRUE), .groups = "drop") %>%
      mutate(時期 = "2050年 (予測)")
    
    plot_df <- bind_rows(df_2026, df_2050) %>%
      mutate(人口_plot = ifelse(性別 == "M", -人口, 人口))
    
    ggplot(plot_df, aes(x = 年齢, y = 人口_plot, fill = 性別)) +
      geom_bar(stat = "identity", width = 0.8, alpha = 0.8) +
      coord_flip() + 
      facet_wrap(~時期) + 
      scale_y_continuous(labels = function(x) abs(x)) + 
      scale_fill_manual(
        values = c("M" = "#00d4ff", "F" = "#ff4d88"),
        breaks = c("M", "F"),               
        labels = c("M" = "男性", "F" = "女性")
      ) +
      labs(x = "年齢", y = "人口", fill = NULL) + 
      theme_minimal(base_family = "mplus") +
      theme(
        plot.background = element_rect(fill = "#1a1a1a", color = NA),
        panel.background = element_rect(fill = "#1a1a1a", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        strip.text = element_text(color = "white", size = 16, family = "dela"),
        panel.grid.major = element_line(color = "gray30", linewidth = 0.1),
        panel.grid = element_line(color = "gray20", linewidth = 0.1),
        legend.position = "bottom"
      )
  }, res = 96)
  
  # 消滅可能性分析
  output$youngWomenPlot <- renderPlot({
    req(raw_sim_results())
    
    y_dat <- raw_sim_results()$pop %>%
      mutate(年 = as.numeric(年)) %>%
      filter(性別 == "F", 年齢 >= 20, 年齢 <= 39) %>%
      group_by(年, sim_id) %>%
      summarise(count = sum(人口), .groups = "drop") %>%
      group_by(年) %>%
      summarise(
        中央値 = median(count), 
        下限 = quantile(count, 0.025), 
        上限 = quantile(count, 0.975),
        .groups = "drop"
      )
    
    ggplot(y_dat, aes(x = 年, y = 中央値)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = base_women_2020 * 0.5, 
               fill = "#ff4d88", alpha = 0.03) + # 危険ゾーン
      geom_ribbon(aes(ymin = 下限, ymax = 上限), fill = "#ff4d88", alpha = 0.1) +
      geom_line(color = "#ff4d88", linewidth = 1.2) +
      geom_point(aes(y = 中央値), color = "#ff4d88", size = 2.5) +
      annotate("text", x = 2026, y = y_dat$中央値[y_dat$年==2026], 
               label = "現在地", color = "white", vjust = -1.5, family = "mplus", fontface="bold") +
      # --- 判定ラインを2020年比の50%に ---
      geom_hline(yintercept = base_women_2020 * 0.5, linetype = "dashed", color = "#ffffcc", linewidth = 0.5, alpha = 0.6) +
      annotate("text", x = 2049, y = (base_women_2020 * 0.5) * 1.15, 
               label = paste0("消滅可能性ライン\n(2020年比-50%: ", round(base_women_2020 * 0.5), "人)"), 
               color = "#ffffcc", family = "mplus", hjust = 1, alpha = 0.7) +
      labs(title = "若年女性人口(20-39歳)の推移", 
           subtitle = paste0("※ 基準(2020年): ", base_women_2020, "人 / 現在予測(2026年時点): ", round(y_dat$中央値[y_dat$年 == 2026]), "人"),
           x = "", y = "人口（人）",
           caption = "※本グラフは消滅可能性自治体の定義（2020年比）に基づいた減少幅を可視化しています。\n（定義：2020年から2050年にかけて20〜39歳の女性人口が50%以上減少する自治体）") +
      theme_minimal(base_family = "mplus") +
      theme(
        plot.background = element_rect(fill = "#1a1a1a", color = NA), 
        text = element_text(color = "white"),
        plot.title = element_text(family = "dela", size = 17),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(color = "white"), 
        panel.grid.major = element_line(color = "gray30", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 0, 20),
        plot.caption = element_text(
          color = "gray70",      
          size = 10,              
          hjust = 0,
          margin = margin(t = 15))
      ) 
  }, res = 96)
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    req(hover)
    
    y_dat <- summary_df()
    req(y_dat)
    
    point <- nearPoints(y_dat, 
                        hover, 
                        xvar = "年", 
                        yvar = "若年女性中央", 
                        threshold = 8, 
                        maxpoints = 1)
    
    if (nrow(point) == 0) return(NULL)
    
    px_x <- hover$coords_img$x
    px_y <- hover$coords_img$y 
    
    is_right_side <- px_x > (hover$range$right / 2)
    x_transform <- if(is_right_side) "translateX(calc(-100% - 15px))" else "translateX(15px)"
    
    style <- paste0(
      "position: absolute; z-index: 1000; pointer-events: none; ",
      "background-color: rgba(26, 26, 26, 0.95); color: white; padding: 10px; ",
      "border: 1px solid #ff4d88; border-radius: 5px; box-shadow: 0 4px 12px rgba(0,0,0,0.5); ",
      "left: ", px_x, "px; top: ", px_y, "px; ",
      "transform: ", x_transform, " translateY(-50%); white-space: nowrap;"
    )
    
    div(style = style,
        div(style = "font-weight: bold; border-bottom: 1px solid #ff4d88; margin-bottom: 5px; padding-bottom: 4px;", 
            paste0(point$年, "年")),
        div(paste0("中央値: ", round(point$若年女性中央, 1), "人")),
        div(style = "font-size: 0.85em; color: #aaa;",
            paste0("予測範囲: ", round(point$若年女性下限, 1), " 〜 ", round(point$若年女性上限, 1), "人"))
    )
  })
  
  # 扶養指数グラフ
  output$supportPlot <- renderPlot({
    s_dat <- summary_df()
    req(s_dat)
    y_max <- max(s_dat$扶養中央, na.rm = TRUE) * 1.2
    
    ggplot(s_dat, aes(x = 年, y = 扶養中央)) +
      geom_line(color = "#00d4ff", linewidth = 0.8) +
      geom_point(color = "#00d4ff", size = 3) +
      geom_text(aes(label = paste0(round(扶養中央, 2), "人")), 
                vjust = -1.7, color = "#00d4ff", size = 2.5, family = "mplus", fontface = "bold") +
      expand_limits(y = y_max) +
      labs(title = "現役1人が背負う高齢者数（肩車指数）", subtitle = "※ 現役世代1人(15-64歳)が何人の高齢者の生活を支えるか", 
           x = "", y = "高齢者の人数（人）") +
      theme_minimal(base_family = "mplus") +
      theme(
        plot.background = element_rect(fill = "#1a1a1a", color = NA),
        panel.background = element_rect(fill = "#1a1a1a", color = NA),
        text = element_text(color = "white"),
        plot.title = element_text(family = "dela", size = 17),
        plot.subtitle = element_text(size = 10),
        axis.text = element_text(color = "white"),
        panel.grid.major = element_line(color = "gray30", linewidth = 0.1),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 0, 20) 
      )
  }, res = 96)
  
  # 数値カード出力
  output$pop_median <- renderText({
    req(summary_df())
    res <- summary_df() %>% filter(年 == 2050)
    paste0(scales::comma(round(res$中央値)), " 人")
  })
  
  output$pop_range <- renderText({
    req(summary_df())
    res <- summary_df() %>% filter(年 == 2050)
    paste0(scales::comma(round(res$下限95)), " 〜 ", scales::comma(round(res$上限95)), " 人")
  })
  
  output$status_box_ui <- renderUI({
    req(raw_sim_results())
    
    # 2040年の若年女性人口（20-39歳）を全試行で集計
    risk_stats <- raw_sim_results()$pop %>%
      filter(年 == "2050", 性別 == "F", 年齢 >= 20, 年齢 <= 39) %>%
      group_by(sim_id) %>%
      summarise(total = sum(人口)) %>%
      mutate(is_safe = total > (base_women_2020 * 0.5))
    
    success_rate <- mean(risk_stats$is_safe)
    
    status_config <- case_when(
      success_rate >= 0.7 ~ list(label = "回避見込み", theme = "success", icon = "check-circle-fill", text = "現在の設定なら、高い確率で消滅可能性を脱却できます。"),
      success_rate >= 0.2 ~ list(label = "要警戒", theme = "warning", icon = "exclamation-triangle-fill", text = "回避できる可能性がありますが、不確実性が残ります。"),
      TRUE ~ list(label = "消滅可能性大", theme = "danger", icon = "x-octagon-fill", text = "2050年に基準を下回るリスクが非常に高い状態です。")
    )
    
    value_box(
      title = "2050年 消滅可能性判定",
      value = status_config$label,
      showcase = bsicons::bs_icon(status_config$icon),
      theme = status_config$theme,
      p(status_config$text, style = "font-size: 0.8rem;"),
      p(paste0("回避確率: ", round(success_rate * 100, 1), "%"), style = "font-weight: bold;")
    )
  })
