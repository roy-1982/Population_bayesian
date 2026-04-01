pacman::p_load(estatapi,shiny,shinyjs,shinyWidgets,bslib,tidyverse,showtext,bsicons,ggiraph,mco)

font_add_google("Dela Gothic One", "dela")
font_add_google("M PLUS 1p", "mplus")
showtext_auto() 

#------------------------------------------

source("data_prep_functions.R")
source("simulation_engine.R")

appId <- Sys.getenv("ESTAT_APPID")

estat_results <- get_estat_data(appId)

local_results <- process_local_data(estat_results$data)

pop_master      <- local_results$pop_master
fertility_table <- local_results$fertility_table
df2_cleaned     <- local_results$df2_cleaned

alpha_results <- calculate_alpha_params(estat_results$data1, pop_master, fertility_table)

alpha         <- alpha_results$alpha
alpha_sd_val  <- alpha_results$alpha_sd_val
target_births <- alpha_results$target_births

base_women_2020 <- estat_results$base_women_2020
edu_growth_base <- estat_results$edu_growth_base
move_pattern    <- estat_results$move_pattern

#----------------------------------------------------------------

# --- UI部 ---
ui <- page_sidebar(
  useShinyjs(),
  theme = bs_theme(
    bg = "#1a1a1a", fg = "#ffffff", primary = "#00d4ff",
    base_family = "mplus"
  ),
  tags$head(
    tags$style(HTML("
      .shiny-notification-close {
        display: none !important;
      }
      .shiny-progress .progress-close {
        display: none !important;
      }
      .shiny-notification {
        pointer-events: none !important;
      }
       .shiny-notification-close {
        display: none !important;
      }
      .popover {
        border: 1px solid rgba(255, 255, 255, 0.3) !important; 
        background-color: rgba(26, 26, 26, 0.9) !important;
      }
      .popover-body {
        color: #ffffff !important;
      }
      .blink-icon svg, .blink-icon i {
        animation: blink-animation 1.5s infinite ease-in-out;
      }
      @keyframes blink-animation {
       0% { 
        opacity: 1.0; 
        filter: drop-shadow(0 0 2px #ffcc80);
        transform: scale(1);
      }
      50% { 
        opacity: 0.4; 
        filter: drop-shadow(0 0 10px #ffcc80);
        transform: scale(1.1); 
      }
      100% { 
        opacity: 1.0; 
        filter: drop-shadow(0 0 2px #ffcc80);
        transform: scale(1);
      }
      }
      /*-------------------------------------------------------------/*
      /* バリデーションCSS */
      /* p2, p3, p4 のタブリンクを強制的に無効化・グレーアウト */
      # .nav-link[data-value='tab_2'], 
      # .nav-link[data-value='tab_3'], 
      # .nav-link[data-value='tab_4'],
      # #find_optimal,
      # #edu_focus_container {
      #   pointer-events: none !important;
      #   opacity: 0.3 !important;
      #   filter: grayscale(1);
      #   cursor: not-allowed !important;
      #  }
      /*-------------------------------------------------------------/*
    ")),
    
    tags$audio(id = "success-sound", src = "success.mp3", type = "audio/mpeg"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('play_sound', function(message) {
        var audio = document.getElementById('success-sound');
        if(audio) {
          audio.currentTime = 0; // 再生位置を先頭に戻す
          audio.volume = 0.3;
          audio.play().catch(function(error) {
            console.log('再生に失敗しました。ユーザーの操作が必要です:', error);
          });
        }
      });
    "))
  ),
  
  title = span(
    style = "display: flex; align-items: center; width: 100%; gap: 15px;",
    
    span("南伊豆町人口予測シミュレーター", 
         style = "font-size: 1.3rem; padding: 10px 0; color: #ffffff; font-family: 'Osaka',sans-serif;"),
    popover(
      span(
        class = "blink-icon",
        bsicons::bs_icon("book", size = "1.2em"), 
        style = "color: #ffcc40; cursor: help; filter: drop-shadow(0 0 2px #ffcc40);"
      ),
      title = "本ツールの解析手法について",
      div(id = "methodology-pop",
          style = "text-align: left; line-height: 1.6; padding: 8px;",
          HTML("町の『今』を精密に投影するため、以下の手法を採用しています：<br><br>",
               "<b>1. 1歳刻みのコーホート要因法</b><br>",
               "社人研（国立社研）と同じ正攻法を、より精密な『1歳単位』で実行。18歳の進学流出などの急激な変動をピンポイントで捉えます。<br><br>",
               "<b>2. ベイズ統計による確率推計</b><br>",
               "小規模自治体特有の『偶然のバラツキ』を考慮し、不確実性を95%信頼区間で可視化します。<br><br>",
               "<b>3. 南伊豆独自係数（α・移動モデル）</b><br>",
               "町の過去20年の実績から算出した独自パラメータを使用。地域の個性を反映しています。")
      ),
      placement = "bottom",
      options = list(trigger = "hover focus") 
      ),
    # 右側：お問い合わせボタン
    tags$a(
      href = "mailto:dataanalytics332@gmail.com?subject=人口推計シミュレーターについてのお問い合わせ",
      class = "btn btn-outline-light",
      style = "margin-left: auto; font-size: 0.8rem; padding: 5px 12px; border-color: #ffffff; color: #ffffff; font-family: 'mplus';",
      icon("envelope"),
      " お問い合わせ"
    )
  ),
  
  sidebar = sidebar(
    title = div(
      span("パラメータ設定", 
           style = "font-family: 'dela', sans-serif; 
                font-weight: bold; 
                font-size: 1.4rem;
                display: inline-block;
                /* 文字自体のグラデーション（左から右に色が変化） */
                background: linear-gradient(90deg, #ffffff 0%, #00d4ff 100%);
                -webkit-background-clip: text;
                -webkit-text-fill-color: transparent;
                
                /* 右下に向かって伸びる光の影（ロンググロー） */
                filter: drop-shadow(3px 3px 0.5px rgba(0, 212, 255, 0.2)) 
                        drop-shadow(6px 5px 1px rgba(0, 212, 255, 0.1)) 
                        drop-shadow(10px 8px 2px rgba(0, 212, 255, 0.05));"
      )
    ),
    width = 350,
    sliderInput(
      inputId = "n_sim",
      label = "試行回数 (初期値:10回)",
      min = 0,
      max = 500,
      value = 10,
      step = 10,
      ticks = TRUE 
    ),
    helpText(style = "font-size: 0.8rem;",
             "100回以上を推奨します。回数が多いほど、偶然の偏りが排除された確実性の高い予測になりますが、計算に時間がかかります。"),
    hr(),
    numericInput("target_birth_n", "目標出生数 (年あたり)", value = 0, min = 0, max = 100),
    helpText(style = "font-size: 0.8rem;",
             "「毎年〇人産まれる」というシナリオを試すためのalpha値を自動計算し、下のスライダーに自動セットします。"),
    hr(),
    sliderInput("alpha_val", "出生率補正係数 (alpha)", 
                min = 0, max = 5.0, value = round(alpha, 2), step = 0.05),
    helpText(
      tags$span(
        "※目標出生数を入力すると自動調整されますが、手動で微調整することも可能です。",
        style = "font-size: 10px; color: #ffcc99;" 
      )
    ),
    tooltip(
      span(bsicons::bs_icon("question-circle"), " この数値の意味は？"),
      div(style = "text-align: left; line-height: 1.6; padding: 8px;",
          HTML(paste0("全国平均の出生率を1.0とした時の南伊豆町の比率です。初期値は現在の約", round(alpha, 2), "で計算されています。<br><br>",
                      "<span style='font-size: 0.85em; color: #555;'>※ 政府統計e-Statの出生数が2022年までしか更新されていないので、それまでのデータで算出しています。</span>"))),
    ),
    hr(),
    sliderInput("move_plus", "若年層(20-39歳)の社会移動調整 (歳あたり)", 
                min = -6, max = 6, value = 0, step = 0.1),
    tooltip(
      span(bsicons::bs_icon("question-circle"), " この数値の意味は？"),
      div(style = "text-align: left; line-height: 1.6; padding: 8px;",
          HTML("20歳から39歳の各年齢において、毎年何人増減するかを設定します。<br>",
               "例えば「+1.0」にすると、この世代全体で毎年20人の転入超過が発生する計算になります。"))
    ), 
    hr(),
    #-------------------------------------------------------------
    # sliderInput("edu_focus", "教育環境への投資・進学志向レベル", 
    #             min = -2.0, max = 2.0, value = 0, step = 0.1),
    #-------------------------------------------------------------
    tags$div(
      id = "edu_focus_container", 
      sliderInput("edu_focus", "教育環境への投資・進学志向レベル", 
                  min = -2.0, max = 2.0, value = 0, step = 0.1)
    ),
    tooltip(
      span(bsicons::bs_icon("question-circle"), " この数値の意味は？"),
      div(style = "text-align: left; line-height: 1.6; padding: 8px;",
          HTML("南伊豆町の過去20年のデータ（進学率が約5%上昇）をベースにした指標です。<br><br>",
               "<b>+1.0（進学・ブランド重視）:</b><br>",
               "18歳の流出圧力が過去20年と同ペースで強まります(+2は2倍のペース)。<br>",
               "代償として、教育環境に惹かれた子育て世帯の流入がセットで発生します。<br><br>",
               "<b>-1.0（地元定着・雇用重視）:</b><br>",
               "高卒後の地元就職が促進され、18歳の流出を過去20年前の水準まで抑制するシナリオです。")
      )
    ),
    tags$div(style = "height: 2px; background: linear-gradient(90deg, transparent, #00d4ff, transparent); margin: 25px 0; opacity: 0.4;"),
    actionButton("do_sim", "シミュレーション実行", class = "btn-primary w-100", style = "font-weight: bold; height: 50px;"),
    tags$div(style = "height: 2px; background: linear-gradient(90deg, transparent, #00d4ff, transparent); margin: 25px 0; opacity: 0.4;"),
    
    actionButton("find_optimal", "最適政策バランスを探索",
                 class = "btn-outline-primary w-100 ",
                 icon = icon("magic")),
    helpText(style = "font-size: 0.8rem; margin-top: 10px;",
             "AIが「将来の若年女性人口の最大化」と「現役世代の負担軽減」のバランスが最も取れる数値を自動計算します。")
  ),
  
  #-------------------------------------------------------------
  # メインコンテンツエリア
  navset_card_underline(
    id = "main_nav",
    title = "分析結果",
    full_screen = TRUE,
    nav_panel("人口推移", value = "tab_1", plotOutput("distPlot", height = "500px")),
    nav_panel("人口ピラミッド", value = "tab_2", plotOutput("pyramidPlot", height = "500px")),
    nav_panel("消滅可能性分析", value = "tab_3",
              div(style = "position: relative;",
                  plotOutput("youngWomenPlot", height = "500px", hover = hoverOpts(id = "plot_hover", delay = 30, delayType = "debounce", clip = T)),
                  uiOutput("hover_info")
              )
    ),
    nav_panel("現役世代の負担感", value = "tab_4", plotOutput("supportPlot", height = "500px"))
  #-------------------------------------------------------------
  ),
  
  layout_column_wrap(
    width = 1/3,
    value_box(
      title = "2050年 推定人口 (中央値)",
      value = textOutput("pop_median"),
      showcase = bsicons::bs_icon("people-fill"),
      theme = "secondary"
    ),
    value_box(
      title = "2050年 予測幅 (95%区間)",
      value = textOutput("pop_range"),
      showcase = bsicons::bs_icon("graph-up-arrow"),
      theme = "secondary"
    ),
    uiOutput("status_box_ui"),
  )
)

server <- function(input, output, session) {
  source("server_logic.R", local = TRUE)
}

shinyApp(ui = ui, server = server)


