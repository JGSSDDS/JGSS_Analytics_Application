
#### global定義

## ライブラリー設定
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(foreign)
library(DT)
library(caret)
library(randomForest)
library(nnet)
library(MASS)
library(kernlab)
library(car)
library(psych)
library(haven)
data(spam)

## データの取扱い最大個数の変更
options(shiny.maxRequestSize = 1 * 1024 ^ 4)

## 数値データの最大表示桁数の変更
options(digits=10)


#### ui定義

## 「データセットの読込み」タブ

filereadTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel("データセットの読込み", 
           fluidRow(
             column(
               3,
               fileInput(ns("file_SPSS"), "SPSSファイルを読込む",
                         accept = "application/x-spss-sav")
             ), 
             column(
               9,
               # 読み込みファイル名の表示
               textOutput(ns("spssFileName")),
               # 読み込みデータのサマリー可視化
               h4(textOutput(ns(
                 "spssSummaryTitle"
               )),
               htmlOutput(ns(
                 "spssSummaryLink"
               ), align="right")
               ),
               withSpinner(tableOutput(ns("spssSummary"
               ))
               )
             )
           ))
}


## 「変数の分布と代表値」タブ

analyticsTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel("変数の分布と代表値",
           fluidRow(
             column(
               3,
               tags$style(type="text/css", ".column-background {background-color: #f5f5f5;}"), # 新しい背景色
               div(class = "column-background",
                   br(),
                   h2("変数選択", align="center"),
                   p(hr(),"読込んだデータセット：",textOutput(ns("spssName")),hr()),
                   selectInput(ns("selected_variable"),
                               "分析対象の変数を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable")),hr()),
                   p(id = "CheckBox_variable",
                     uiOutput(ns("checkGroupUi"))),
                   p(id = "slider_filter_ui_wrapper",
                     uiOutput(ns("slider_filter_ui")))
               )
             ),
             column(
               3,
               tags$style(type="text/css", "#analytics_column {background-color: #f5f5f5;}"),
               div(id = "analytics_column",
                   br(),
                   h2("分析実行", align="center"),
                   hr(),
                   uiOutput(ns("analytics_button")),br(),
               )
             ),

             column(6,
                    br(),
                    h2("結果", align="center"),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    h4(withSpinner(uiOutput(ns(
                      "stats"
                    ))), align = "center"), )
           ))
}


## 「二変量分析」タブ

bivariateTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel("二変量分析・二重クロス表",
           fluidRow(
             column(
               3,
               tags$style(type="text/css", ".column-background {background-color: #f5f5f5;}"), 
               div(class = "column-background",
                   br(),
                   h2("変数選択と分析実行", align="center"),
                   p(hr(),"読込んだデータセット：",textOutput(ns("spssName")),hr()),
                   selectInput(ns("selected_variable"),
                               "分析対象の変数（x）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable")),hr()),
                   br(),
                   br(),
                   selectInput(ns("selected_variable2"),
                               "分析対象の変数（y）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable2")),hr()),
                   br(),
                   br(),
                   p(strong("以上の変数（x, y）にて分析を実行する")),
                   uiOutput(ns("bivariate_button")),
                   br()
               )
             ),
             column(9,
                    br(),
                    h2("結果", align="center"),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    div(h4(withSpinner(uiOutput(ns("stats")))), align = "center")
             )))
}


## 「三重クロス表」タブ

triple_crossTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel("三重クロス表",
           fluidRow(
             column(
               3,
               tags$style(type="text/css", ".column-background {background-color: #f5f5f5;}"), 
               div(class = "column-background",
                   br(),
                   h2("変数選択と分析実行", align="center"),
                   p(hr(),"読込んだデータセット：",textOutput(ns("spssName")),hr()),
                   selectInput(ns("selected_variable"),
                               "分析対象の変数（x）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable")),hr()),
                   br(),
                   br(),
                   selectInput(ns("selected_variable2"),
                               "分析対象の変数（y）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable2")),hr()),
                   br(),
                   br(),
                   selectInput(ns("selected_variable3"),
                               "分析対象の変数（z）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable3")),hr()),
                   br(),
                   br(),
                   p(strong("以上の変数（x, y, z）にて分析を実行する")),
                   uiOutput(ns("triple_cross_button")),
                   br()
               )
             ),
             column(9,
                    br(),
                    h2("結果", align="center"),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    div(h4(withSpinner(uiOutput(ns("stats")))), align = "center")
             )))
}


## 「多変量分析」タブ

multivariateTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "多変量分析",
    fluidRow(
      column(
        3,
        tags$style(type="text/css", ".column-background {background-color: #f5f5f5;}"), 
        div(class = "column-background",
            br(),
            h2("変数選択と分析実行", align = "center"),
            p(hr(), "読込んだデータセット：", textOutput(ns("spssName")), hr()),
            
            selectInput(
              ns("data_for_regressionX"),
              label = "データセットを選択してください。",
              choices = c(
                "アヤメのデータ" = "iris",
                "不妊症の比較データ" = "infert",
                "ボストン近郊の不動産価格データ" = "Boston",
                "スパムと正常メールのデータ" = "spam",
                "ニューヨークの大気状態データ" = "airquality",
                "タイタニックの乗客データ" = "titanic",
                "取込みデータ.sav" = "spss"
              ),
              selected = "spss"
            ),
            selectInput(
              ns("data_for_regressionY"),
              label = "目的変数を選択",
              choices = NULL
            ),
            h4("選択された説明変数はこちら"),
            verbatimTextOutput(ns("rows_selected")),
            selectInput(
              ns("regression_type"),
              label = "分析の手法を選択",
              choices = c(
                "重回帰分析(一般線形モデル)" = "lm",
                "ランダムフォレスト" = "rf",
                "3層ニューラルネット" = "nnet",
                "ロジスティック回帰分析" = "logistic",
                "二元配置分散分析" = "manova",
                "主成分分析" = "pca",
                "因子分析" = "fa",
                "偏相関係数（説明変数3以上）" = "partial_corr"
              )
            ),
            actionButton(ns("regression_button"), "実行")
        )
      ),
      column(
        9,
        br(),
        h2("結果", align = "center"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table",
            h4("説明変数を選択してください。"),
            DT::dataTableOutput(ns("data_table_for_regression"))
          ),
          tabPanel("分析結果", verbatimTextOutput(ns("summary_regression"))),
          tabPanel(
            "プロット",
            plotOutput(ns("plot_regression"))
          )
        )
      )
    )
  )
}


## ui全体の統合
ui <- shinyUI(fluidPage(
  tags$head(tags$style(
    HTML(
      "#shiny-notification-panel {
                position:fixed;
                top: 0;
              }
              .shiny-notification {
                opacity: 1;
              }
              input#analytics-selected_variable-selectized {
                 width: 10em !important;
              }
              .selectize-dropdown [data-selectable] .highlight {
                 background: rgba(255, 237, 40, 0);
              }
      .shiny-table.spacing-s>thead>tr>th {
        text-align: center !important;
      }
      .shiny-table.spacing-s>tbody>tr>td:nth-of-type(1) {
        text-align: left !important;
      }
              "
    )
  )),
  
  navbarPage(
    strong(em("JGSS オンライン分析アプリケーション")),br(),
    id = "navbar",

    # データセットの読込み
    filereadTabPanel(id = "fileread"),
    
    # 変数の分布と代表値
    analyticsTabPanel(id = "analytics"),
    
    # 二変量分析
    bivariateTabPanel(id = "bivariate"),

    # 三重クロス表
    triple_crossTabPanel(id = "triple_cross"),
        
    # 多変量分析
    multivariateTabPanel(id = "multivariate"),

    # FooterupdateSelectizeInput
    footer= h5(hr(),"Copyright(C) 1999-2023, Japanese General Social Surveys. All Rights Reserved.", align = "center")
    
  )
))

### serverを定義

## 「データの読込み」タブのサーバーモジュール

filereader <- function(id) {
  " File reader returning dataframe list of spss and numeric version of spss."
  moduleServer(id,
               function(input, output, session) {
                 # SPSSファイルの読込み
                 dfSpss <- eventReactive(input$file_SPSS, {
                   req(endsWith(input$file_SPSS$datapath, '.sav'))
                   
                   df <- read.spss(
                     input$file_SPSS$datapath,
                     reencode = "UTF-8",
                     to.data.frame = T,
                     use.value.labels = T,
                     add.undeclared.levels = "no"
                   )
                   colNames <- names(df)
                   variableLabels <-
                     as.character(attr(df, "variable.labels"))
                   for (i in 1:length(variableLabels)) {
                     if (variableLabels[i] == " ") {
                       variableLabels[i] <- paste(colNames[i], "()")
                     } else {
                       variableLabels[i] <- paste(variableLabels[i], "(", colNames[i], ")")
                     }
                   }
                   names(df) <- variableLabels
                   df
                 })
                 
                 dfSpssValue <- eventReactive(input$file_SPSS, {
                   req(endsWith(input$file_SPSS$datapath, '.sav'))
                   
                   df <- read.spss(
                     input$file_SPSS$datapath,
                     reencode = "UTF-8",
                     to.data.frame = T,
                     use.value.labels = F
                   )
                   colNames <- names(df)
                   variableLabels <-
                     as.character(attr(df, "variable.labels"))
                   for (i in 1:length(variableLabels)) {
                     if (variableLabels[i] == " ") {
                       variableLabels[i] <- paste(colNames[i], "()")
                     } else {
                       variableLabels[i] <- paste(variableLabels[i], "(", colNames[i], ")")
                     }
                   }
                   names(df) <- variableLabels
                   df
                 })
                 
                 output$spssSummaryTitle <- renderText({
                   req(dfSpss())
                   paste("読込んだ変数一覧（合計", format(length(names(dfSpss())), big.mark=",", scientific=F), "種類）")
                 })
                 
                 output$spssSummaryLink <- renderUI({
                   req(dfSpss())
                   actionLink("link_to_analytics", "▶ このデータセットを分析する")
                 })
                 
                 output$spssSummary <- renderTable({
                   data.frame(
                     "変数" = names(dfSpss()),
                     "回答数" = format(sapply(dfSpss(), function(x)
                       sum(!is.na(x))), big.mark=",", scientific=F),
                     "欠損数" = format(sapply(dfSpss(), function(x)
                       sum(is.na(x))), big.mark=",", scientific=F)
                   )
                 }, rownames = FALSE, colnames = TRUE)
                 
                 # 不正な拡張子の通知
                 observeEvent(input$file_SPSS, {
                   req(!endsWith(input$file_SPSS$datapath, '.sav'))
                   
                   showNotification(".sav以外のファイル拡張子です。", type = "error")
                 })
                 
                 # ファイル名の取得
                 spssName <- eventReactive(input$file_SPSS, {
                   req(endsWith(input$file_SPSS$name, '.sav'))
                   input$file_SPSS$name
                 })
                 
                 return(list(
                   spss = dfSpss,
                   spssValue = dfSpssValue,
                   spssName = spssName
                 ))
                 
               })
}

## 「変数の分布と代表値」タブのサーバーモジュール

fileNameGenerator <- function(id, spssName) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   output$spssName <- renderText(spssName())
                 })
               })
}

selectorGenerator <- function(id, dfSpss) {
  moduleServer(id,
               function(input, output, session) {
                 # 変数を選択するためのプルダウン項目制御
                 observe({
                   updateSelectizeInput(session,
                                     "selected_variable",
                                     options=list(maxOptions = 10000),
                                     choices = names(dfSpss()))
                 })
               })
}

typeOfVariableGenerator <- function(id, df) {
  " Generator of type of variable text based on selector input."
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   # データ列名を取得
                   column <- input$selected_variable
                   
                   # データフレームに変数が含まれるか確認
                   req(column %in% names(df()))
                   
                   # 変数の型の種類のテキストを生成
                   output$typeOfVariable <- renderText({
                     x <- df()[, column]
                     if (is.factor(x)) {
                       "データ型：カテゴリー値"
                     } else{
                       choiceList = list()
                       "データ型：数値"
                     }
                   })
                 })
               })
}

checkboxGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # チェックボックス生成
                 observe({
                   output$checkGroupUi <- renderUI({
                     # 名前空間を定義
                     ns = NS(id)
                     
                     # データ列名を取得
                     column <- input$selected_variable
                     
                     # データフレームに変数が含まれるか確認
                     req (column %in% names(df()))
                     
                     # データの型に応じてチェックボックスを作成
                     x <- df()[, column]
                     xLabels <-
                       attr(dfValue()[, column], "value.labels")
                     
                     req(is.factor(x))
                     tagList(
                       p(style = "display:inline-block", actionButton(ns(
                         "check_all"
                       ), "全選択")),
                       p(style = "display:inline-block", actionButton(ns(
                         "uncheck_all"
                       ), "全解除")),
                       p(id = "CheckBox_variable",
                         checkboxGroupInput(
                           ns("checkGroup"),
                           "Checkbox group"
                         ))
                     )
                   })
                 })
                 
                 # チェックボックス更新
                 observe({
                   # データ列名を取得
                   column <- input$selected_variable
                   
                   # データフレームに変数が含まれるか確認
                   req (column %in% names(df()))
                   
                   # データの型に応じてチェックボックスを作成
                   x <- df()[, column]
                   xLabels <-
                     attr(dfValue()[, column], "value.labels")
                   
                   if (is.factor(x)) {
                     choiceList = levels(x)
                     values <-
                       as.integer(lapply(choiceList, function(c) {
                         if (is.na(xLabels[c])) {
                           c
                         } else{
                           xLabels[c]
                         }
                       }))
                     choiceList <- choiceList[order(values)]
                     choiceNameList <-
                       paste(values[order(values)], "：", choiceList)
                     updateCheckboxGroupInput(
                       session,
                       "checkGroup",
                       label = "",
                       choiceNames = choiceNameList,
                       choiceValues = choiceList,
                       selected = choiceList,
                       
                     )
                   } else{
                     choiceList = list()
                     toggle("CheckBox_variable", condition = FALSE)
                     updateCheckboxGroupInput(
                       session,
                       "checkGroup",
                       label = NA,
                       choices = choiceList,
                       selected = choiceList
                     )
                   }
                 })
                 
                 # チェックボックスの全選択
                 observe({
                   req(input$check_all)
                   
                   # データ列名を取得
                   column <- input$selected_variable
                   
                   # データフレームに変数が含まれるか確認
                   req (column %in% names(df()))
                   
                   # データの型に応じてチェックボックスを作成
                   x <- df()[, column]
                   xLabels <-
                     attr(dfValue()[, column], "value.labels")
                   
                   if (is.factor(x)) {
                     choiceList = levels(x)
                     values <-
                       as.integer(lapply(choiceList, function(c) {
                         if (is.na(xLabels[c])) {
                           c
                         } else{
                           xLabels[c]
                         }
                       }))
                     choiceList <- choiceList[order(values)]
                     choiceNameList <-
                       paste(values[order(values)], "：", choiceList)
                     updateCheckboxGroupInput(
                       session,
                       "checkGroup",
                       label = "",
                       choiceNames = choiceNameList,
                       choiceValues = choiceList,
                       selected = choiceList,
                       
                     )
                   }
                 })
                 
                 # チェックボックスの全解除
                 observe({
                   req(input$uncheck_all)
                   
                   # データ列名を取得
                   column <- input$selected_variable
                   
                   # データフレームに変数が含まれるか確認
                   req (column %in% names(df()))
                   
                   # データの型に応じてチェックボックスを作成
                   x <- df()[, column]
                   xLabels <-
                     attr(dfValue()[, column], "value.labels")
                   
                   if (is.factor(x)) {
                     choiceList = levels(x)
                     values <-
                       as.integer(lapply(choiceList, function(c) {
                         if (is.na(xLabels[c])) {
                           c
                         } else{
                           xLabels[c]
                         }
                       }))
                     choiceList <- choiceList[order(values)]
                     choiceNameList <-
                       paste(values[order(values)], "：", choiceList)
                     updateCheckboxGroupInput(
                       session,
                       "checkGroup",
                       label = "",
                       choiceNames = choiceNameList,
                       choiceValues = choiceList,
                     )
                   }
                 })
               })
}

sliderGenerator <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 # スライダ生成
                 output$slider_filter_ui <- renderUI({
                   # 名前空間を定義
                   ns = NS(id)
                   
                   # 計算に必要なデータを取得
                   column <- input$selected_variable
                   x <- df()[, column]
                   
                   req(!is.factor(x))
                   p(sliderInput(
                     ns("slider_filter"),
                     h5("数値範囲の絞込み"),
                     min = min(x),
                     max = max(x),
                     value = c(min(x), max(x))
                   ))
                 })
               })
}

analyticsButtonGenerator <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 # selectorの変更に応じた記述統計ボタンの変更
                 observe({
                   req(nrow(df()) > 0)
                   
                   output$analytics_button <- renderUI({
                     # 名前空間を定義
                     ns = NS(id)
                     
                     # 計算に必要なデータを取得
                     column <- input$selected_variable
                     x <- df()[, column]
                     
                     # データの型に応じて計算
                     if (!is.factor(x)) {
                       # 非カテゴリー型
                       tagList(
                         p(strong("■ 変数の分布")),
                         p("ヒストグラムの階級数（bin数）を指定"),
                         p(
                           sliderInput(
                             ns("slider_input_data"),
                             "",
                             min = 1,
                             max = 20,
                             value = 10
                           )
                         ),
                         p(actionButton(
                           ns("trigger_histogram"), "ヒストグラムを出力", width = "180px"), align = "center"),
                         hr(),
                         p(actionButton(
                           ns("trigger_summary"), "度数分布表を出力", width = "180px"), align = "center"),
                         hr(),
                         p(strong("■ 変数の代表値")),
                         p(actionButton(ns(
                           "trigger_average"
                         ), "平均値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_median"
                         ), "中央値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_max"
                         ), "最大値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_min"
                         ), "最小値", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_unbiased_var"), "不偏分散", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_sample_var"), "標本分散", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_unbiased_std"), "不偏標準偏差", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_sample_std"), "標本標準偏差", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_ste"
                         ), "標準誤差", width = "180px"), align = "center"),
                       )
                     } else {
                       # カテゴリー型
                       tagList(
                         p(strong("■ 変数の概観")),
                         p(actionButton(
                           ns("trigger_summary"), "度数分布表を出力", width = "180px"), align = "center"),
                         hr(),
                         p(strong("■ 変数の代表値")),
                         p(actionButton(ns(
                           "trigger_average"
                         ), "平均値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_median"
                         ), "中央値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_max"
                         ), "最大値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_min"
                         ), "最小値", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_unbiased_var"), "不偏分散", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_sample_var"), "標本分散", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_unbiased_std"), "不偏標準偏差", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_sample_std"), "標本標準偏差", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_ste"
                         ), "標準誤差", width = "180px"), align = "center"),
                       )
                     }
                   })
                 })
               })
}


summaryGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた概観テーブル生成
                 observe({
                   req(input$trigger_summary)
                   req(nrow(df()) > 0)
                   
                   output$summary <- renderTable({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     xLabels <-
                       isolate(attr(dfValue()[, column], "value.labels"))
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # チェックボックス値を取得
                       checked <- input$checkGroup
                       # チェックなしならば処理をしない 2022/09/10
                       if (length(checked) == 0) {
                         return()
                       }
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       # 前のfactor情報が残るので再factorする 2022/09/10
                       x <- factor(x)
                       
                       # 集計テーブル作成
                       Categories <- levels(x)
                       Values <-
                         as.integer(lapply(Categories, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         }))
                       Frequency <-
                         as.numeric(table(x, useNA = "no"))
                       Proportioin <- round(as.numeric(prop.table(Frequency)), 3) * 100
                       data <-
                         data.frame(Values, Categories, Frequency, Proportioin)
                       
                       # 表示行を絞り込み 2022/09/10 前で絞り込んでいるのでここでこの処理は不要
                       #data <-
                       #  data[which(Categories %in% checked),]
                       
                       # ソート　 累積度数計算 2022/08/21 tosa cumsumを使った累積計算
                       data[order(data$Values),]
                       data$Accumulation <- 0
                       data$AccumulationP <- 0
                       
                       cumfreq <- cumsum(Frequency)
                       cumpercent <- cumsum(Frequency)/length(x)*100.0
                       
                       data$Frequency <- paste(format(round(data$Frequency), big.mark=",", scientific=F))
                       data$Accumulation<- paste(format(round(cumfreq), big.mark=",", scientific=F))
                       data$Proportioin <- paste(round(data$Proportioin,1),"%")
                       data$AccumulationP <- paste(round(cumpercent,1),"%")
                       #
                       data <- data[, colnames(data) != "Values"]
                       print(data)
                       # 合計行を足す 2022/08/21
                       totalAcc <- data$Accumulation[length(data$Accumulation)]
                       totalAccP <- data$AccumulationP[length(data$Accumulation)]
                       tempDf <- data.frame(Categories="合計",Frequency=totalAcc,Proportioin=totalAccP,Accumulation="",AccumulationP="")
                       data <- rbind(data,tempDf)
                       colnames(data) <- c("カテゴリー","度数","パーセント","累積度数","累積パーセント")
                       data
                     } else {
                       "連続値データです。"
                       # 集計テーブル作成
                       # スライダー値を取得 2023/03/21
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       x <- factor(x)
                       Categories <- levels(x)
                       Values <- Categories
                       Frequency <-
                         as.numeric(table(x, useNA = "no"))
                       Proportioin <- round(as.numeric(prop.table(Frequency)), 3) * 100
                       data <-
                         data.frame(Values, Categories, Frequency, Proportioin)
                       
                       # ソート　 累積度数計算 2022/08/21 tosa cumsumを使った累積計算
                       data[order(data$Values),]
                       data$Accumulation <- 0
                       data$AccumulationP <- 0
                       
                       cumfreq <- cumsum(Frequency)
                       cumpercent <- cumsum(Frequency)/length(x)*100.0
                       
                       data$Frequency <- paste(format(round(data$Frequency), big.mark=",", scientific=F))
                       data$Accumulation<- paste(format(round(cumfreq), big.mark=",", scientific=F))
                       data$Proportioin <- paste(round(data$Proportioin,1),"%")
                       data$AccumulationP <- paste(round(cumpercent,1),"%")
                       #
                       data <- data[, colnames(data) != "Values"]
                       # 合計行を足す 2022/08/21
                       totalAcc <- data$Accumulation[length(data$Accumulation)]
                       totalAccP <- data$AccumulationP[length(data$Accumulation)]
                       tempDf <- data.frame(Categories="合計",Frequency=totalAcc,Proportioin=totalAccP,Accumulation="",AccumulationP="")
                       data <- rbind(data,tempDf)
                       colnames(data) <- c("カテゴリー","度数","パーセント","累積度数","累積パーセント")
                       data
                     }
                   },align = "r")
                 })
               })
}


averageGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた平均値生成
                 observe({
                   req(input$trigger_average)
                   req(nrow(df()) > 0)
                   
                   output$average <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の平均を四捨五入し、一番近いラベル番号の要素を出力
                       if (length(x) > 0) {
                         # 都道府県、就労などの集計方法は仕様確認後に修正する予定
                         m <- mean(as.integer(x))
                         # stat <- my_round(m)
                         stat <- round(m)
                         stat <-
                            which(abs(as.numeric(xLabels) - stat) == min(abs(as.numeric(
                             xLabels
                           ) -
                             stat)))
                         paste(xLabels[stat], "：", names(xLabels)[stat])
                         # paste(format(m, digit = 3), "(", stat, "：", names(xLabels)[which(xLabels == stat)], ")")
                         paste(format(m, digit = 3))
                       } else{
                         paste("NA")
                       }
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       round(mean(x), 3)
                     }
                   })
                 })
               })
}

medianGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた中央値生成
                 observe({
                   req(input$trigger_median)
                   req(nrow(df()) > 0)
                   
                   output$median <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の中央値の要素を出力
                       if (length(x) > 0) {
                         stat <- median(as.integer(x))
                         # 中央値の表示方法を修正(8/13)
                         paste(stat, "(", stat, "：", names(xLabels)[which(xLabels == stat)], ")")
                       } else{
                         paste("NA")
                       }
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       median(x)
                     }
                   })
                 })
               })
}

maxGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた最大値生成
                 observe({
                   req(input$trigger_max)
                   req(nrow(df()) > 0)
                   
                   output$max <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       if (length(x) > 0) {
                         # 順序の最大値の要素を出力
                         stat <- max(as.integer(x))
                         paste(stat, "：", names(xLabels)[which(xLabels == stat)])
                       } else{
                         paste("NA")
                       }
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       max(x)
                     }
                   })
                 })
               })
}

minGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた最小値生成
                 observe({
                   req(input$trigger_min)
                   req(nrow(df()) > 0)
                   
                   output$min <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の最小値の要素を出力
                       if (length(x)) {
                         stat <- min(as.integer(x))
                         paste(stat, "：", names(xLabels)[which(xLabels == stat)])
                       } else{
                         paste("NA")
                       }
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       min(x)
                     }
                   })
                 })
               })
}

unbiasedVarGenerator <- function(id, df, dfValue) {
   moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた分散生成
                 observe({
                   req(input$trigger_unbiased_var)
                   req(nrow(df()) > 0)
                   
                   output$unbiased_var <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の不偏分散を出力
                       round(var(as.integer(x)), 3)
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       round(var(x), 3)
                     }
                   })
                 })
               })
}

sampleVarGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた分散生成
                 observe({
                   req(input$trigger_sample_var)
                   req(nrow(df()) > 0)
                   
                   output$sample_var <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の標本分散を出力
                       round(var(as.integer(x)) * (length(x) - 1) / length(x), 3)
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       round(var(x) * (length(x) - 1) / length(x), 3)
                     }
                   })
                 })
               })
}

unbiasedStdGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた標準偏差生成
                 observe({
                   req(input$trigger_unbiased_std)
                   req(nrow(df()) > 0)
                   
                   output$unbiased_std <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の不偏標準偏差を出力
                       round(sd(as.integer(x)), 3)
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       round(sd(x), 3)
                     }
                   })
                 })
               })
}

sampleStdGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた標準偏差生成
                 observe({
                   req(input$trigger_sample_std)
                   req(nrow(df()) > 0)
                   
                   output$sample_std <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の標本標準偏差を出力
                       variance <-
                         var(as.integer(x)) * (length(x) - 1) / length(x)
                       round(sqrt(variance), 3)
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       variance <-
                         var(x) * (length(x) - 1) / length(x)
                       round(sqrt(variance), 3)
                     }
                   })
                 })
               })
}

steGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた標準誤差生成
                 observe({
                   req(input$trigger_ste)
                   req(nrow(df()) > 0)
                   
                   output$ste <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の標準誤差を出力
                       round(sd(as.integer(x)) / sqrt(length(x)), 3)
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       round(sd(x) / sqrt(length(x)), 3)
                     }
                   })
                 })
               })
}

histgramGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じたヒストグラム生成
                 observe({
                   req(input$trigger_histogram)
                   req(nrow(df()) > 0)
                   
                   output$histgram <- renderPlot({
                     # プロットに必要なデータを取得
                     column <- isolate(input$selected_variable)
                     bin_number <- isolate(input$slider_input_data)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じてプロット
                     par(family = "HiraMaruProN-W4")
                     if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       par(mar = c(10, 4, 3, 3))
                       x <-
                         sapply(x, function(c)
                           gsub("(.{10})", "\\1\n", c)) # 10文字毎に折り返し
                       barplot(
                         table(x),
                         col = 'darkgray',
                         border = 'white',
                         main = column,
                         xlab = NA,
                         ylab = "Frequency",
                         las = 2
                       )
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       if (is.na(var(x))) {
                         bins <- c(min(x))
                       } else{
                         bins <- seq(min(x), max(x), length.out = bin_number + 1)
                       }
                       h <- hist(
                         x,
                         breaks = bins,
                         col = 'darkgray',
                         border = 'white',
                         main = column,
                         xlab = NA,
                         ylab = "Frequency"
                       )
                       # 2022/8/21 tosa
                       keta <- as.integer(log10(max(h$counts)))
                       memori <- 10^keta
                       y = round(max(h$counts)) + memori
                       hist(
                         x,
                         breaks = bins,
                         col = 'darkgray',
                         border = 'white',
                         main = column,
                         xlab = NA,
                         ylab = "Frequency",
                         label = T,
                         ylim = c(0,y)
                       )
                     }
                   }, height = 600, width = 400)
                 })
               })
}

selectorGenerator2 <- function(id, dfSpss) {
  moduleServer(id,
               function(input, output, session) {
                 # 変数を選択するためのプルダウン項目制御
                 # ここらへんで項目数をとる
                 observe({
                   updateSelectizeInput(session,
                                        "selected_variable2",
                                        options=list(maxOptions = 10000),
                                        choices = names(dfSpss()))
                 })
               })
}

typeOfVariableGenerator2 <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   # データ列名を取得
                   column <- input$selected_variable2
                   
                   # データフレームに変数が含まれるか確認
                   req(column %in% names(df()))
                   
                   # 変数の型の種類のテキストを生成
                   output$typeOfVariable2 <- renderText({
                     x <- df()[, column]
                     if (is.factor(x)) {
                       "データ型：カテゴリー値"
                     } else{
                       choiceList = list()
                       "データ型：数値"
                     }
                   })
                 })
               })
}

bivariateButtonGenerator <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 # selectorの変更に応じた記述統計ボタンの変更
                 observe({
                   req(nrow(df()) > 0)
                   
                   output$bivariate_button <- renderUI({
                     # 名前空間を定義
                     ns = NS(id)
                     
                     # 計算に必要なデータを取得
                     column <- input$selected_variable2
                     y <- df()[, column]
                     
                     # データの型に応じて計算
                     if (!is.factor(y)) {
                       # 非ラベル型
                       tagList(
                         p(actionButton(ns(
                           "trigger_scatter"
                         ), "散布図 / 相関係数", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_t_test_result"
                         ), "t検定", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_anova_result"
                         ), "F検定", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_simple_regression_result"
                         ), "単回帰分析", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_double_cross_table_result"
                         ), "二重クロス表", width = "180px"), align = "center"),
                       )
                     } else {
                       # ラベル型
                       tagList(
                         p(actionButton(ns(
                           "trigger_scatter"
                         ), "散布図 / 相関係数", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_t_test_result"
                         ), "t検定", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_anova_result"
                         ), "F検定", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_simple_regression_result"
                         ), "単回帰分析", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_double_cross_table_result"
                         ), "二重クロス表", width = "180px"), align = "center"),
                       )
                     }
                   })
                 })
               })
}


scatterGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた散布図生成
                 observe({
                   req(input$trigger_scatter)
                   req(nrow(df()) > 0)
                   
                   data <- reactive({
                     # 計算に必要なデータを取得
                     x_column <- isolate(input$selected_variable)
                     # 対象がfactorの場合xの値を数値にする
                     if (is.factor(df()[, x_column])) {
                       x <- as.numeric(df()[, x_column])
                     }else{
                       x <- df()[, x_column]
                     }
                     y_column <- isolate(input$selected_variable2)
                     # 対象がfactorの場合xの値を数値にする
                     if (is.factor(df()[, y_column])) {
                       y <- as.numeric(df()[, y_column])
                     }else{
                       y <- df()[, y_column]
                     }
                     
                     # データを結合して返す
                     data.frame(x, y)
                   })
                   
                   output$scatter <- renderPlot({
                     # isolate()を使用して不必要な再計算を防止
                     plot(x = data()[, 1], y = data()[, 2], 
                          xlab = input$selected_variable, ylab = input$selected_variable2,
                          main = paste("Scatterplot of", input$selected_variable, "vs.", input$selected_variable2))
                     # 相関係数を計算
                     correlation <- cor(data()[, 1], data()[, 2], use="complete.obs", method="pearson")
                     # 相関係数をプロットに追加
                     legend("bottomright", legend = paste("相関係数:", round(correlation, 2)), bty = "n", cex = 3)
                   })
                   
                   res <- nearPoints(dfValue(), input$plot_click, xvar = input$selected_variable,
                                     yvar = input$selected_variable2,
                                     threshold = 5, maxpoints = 10)
                   if (nrow(res) == 0) {
                     return()
                   }
                   res
                 })
               })
}

t_testGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   req(input$trigger_t_test_result)
                   req(nrow(df()) > 0)
                   
                   data <- reactive({
                     # 計算に必要なデータを取得
                     x_column <- isolate(input$selected_variable)
                     if (is.factor(df()[, x_column])) {
                       x <- as.numeric(df()[, x_column])
                     } else {
                       x <- df()[, x_column]
                     }
                     y_column <- isolate(input$selected_variable2)
                     if (is.factor(df()[, y_column])) {
                       y <- as.numeric(df()[, y_column])
                     } else {
                       y <- df()[, y_column]
                     }
                     
                     # データを結合して返す
                     data.frame(x, y)
                   })
                   
                   # t検定を実行し、結果を表示
                   output$t_test_result <- renderPrint({
                     t_test <- t.test(data()[, 1], data()[, 2])
                     t_test
                   })
                 })
               })
}

f_testGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   req(input$trigger_anova_result)
                   req(nrow(df()) > 0)
                   
                   data <- reactive({
                     # 計算に必要なデータを取得
                     x_column <- isolate(input$selected_variable)
                     x <- as.factor(df()[, x_column])
                     
                     y_column <- isolate(input$selected_variable2)
                     if (is.factor(df()[, y_column])) {
                       y <- as.numeric(df()[, y_column])
                     } else {
                       y <- df()[, y_column]
                     }
                     
                     # データを結合して返す
                     data.frame(x, y)
                   })
                   
                   # ANOVAを実行し、結果を表示
                   output$anova_result <- renderPrint({
                     anova_test <- aov(data()[, 2] ~ data()[, 1])
                     summary(anova_test)
                   })
                 })
               })
}

simple_regressionGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   req(input$trigger_simple_regression_result)
                   req(nrow(df()) > 0)
                   
                   data <- reactive({
                     # 計算に必要なデータを取得
                     x_column <- isolate(input$selected_variable)
                     if (is.factor(df()[, x_column])) {
                       x <- as.numeric(df()[, x_column])
                     } else {
                       x <- df()[, x_column]
                     }
                     y_column <- isolate(input$selected_variable2)
                     if (is.factor(df()[, y_column])) {
                       y <- as.numeric(df()[, y_column])
                     } else {
                       y <- df()[, y_column]
                     }
                     
                     # データを結合して返す
                     data.frame(x, y)
                   })
                   
                   # 単回帰分析を実行し、結果を表示
                   output$simple_regression_result <- renderPrint({
                     simple_regression_model <- lm(data()[, 2] ~ data()[, 1])
                     summary(simple_regression_model)
                   })
                 })
               })
}


double_cross_tableGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   req(input$trigger_double_cross_table_result)
                   req(nrow(df()) > 0)
                   
                   data <- reactive({
                     # 計算に必要なデータを取得
                     x_column <- isolate(input$selected_variable)
                     if (is.factor(df()[, x_column])) {
                       x <- as.numeric(df()[, x_column])
                     } else {
                       x <- df()[, x_column]
                     }
                     y_column <- isolate(input$selected_variable2)
                     if (is.factor(df()[, y_column])) {
                       y <- as.numeric(df()[, y_column])
                     } else {
                       y <- df()[, y_column]
                     }
                     
                     # データを結合して返す
                     data.frame(x, y)
                   })
                   
                   # 二重クロス表を生成し、結果を表示
                   output$double_cross_table_result <- renderTable({
                     double_cross_table <- table(data()[, 1], data()[, 2])
                     double_cross_table
                   })
                 })
               })
}

selectorGenerator3 <- function(id, dfSpss) {
  moduleServer(id,
               function(input, output, session) {
                 # 変数を選択するためのプルダウン項目制御
                 # ここらへんで項目数をとる
                 observe({
                   updateSelectizeInput(session,
                                        "selected_variable3",
                                        options=list(maxOptions = 10000),
                                        choices = names(dfSpss()))
                 })
               })
}

typeOfVariableGenerator3 <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   # データ列名を取得
                   column <- input$selected_variable3
                   
                   # データフレームに変数が含まれるか確認
                   req(column %in% names(df()))
                   
                   # 変数の型の種類のテキストを生成
                   output$typeOfVariable3 <- renderText({
                     x <- df()[, column]
                     if (is.factor(x)) {
                       "データ型：カテゴリー値"
                     } else{
                       choiceList = list()
                       "データ型：数値"
                     }
                   })
                 })
               })
}

triple_crossButtonGenerator <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 # selectorの変更に応じた記述統計ボタンの変更
                 observe({
                   req(nrow(df()) > 0)
                   
                   output$triple_cross_button <- renderUI({
                     # 名前空間を定義
                     ns = NS(id)
                     
                     # 計算に必要なデータを取得
                     column <- input$selected_variable3
                     z <- df()[, column]
                     
                     # データの型に応じて計算
                     if (!is.factor(z)) {
                       # 非ラベル型
                       tagList(
                         p(actionButton(ns(
                           "trigger_triple_cross_table_result"
                         ), "三重クロス表", width = "180px"), align = "center"))
                     } else {
                       # ラベル型
                       tagList(
                         p(actionButton(ns(
                           "trigger_triple_cross_table_result"
                         ), "三重クロス表", width = "180px"), align = "center"))
                     }
                   })
                 })
               })
}

triple_cross_tableGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   req(input$trigger_triple_cross_table_result) # ここを修正
                   req(nrow(df()) > 0)
                   
                   data <- reactive({
                     # 計算に必要なデータを取得
                     x_column <- isolate(input$selected_variable)
                     if (is.factor(df()[, x_column])) {
                       x <- as.numeric(df()[, x_column])
                     } else {
                       x <- df()[, x_column]
                     }
                     y_column <- isolate(input$selected_variable2)
                     if (is.factor(df()[, y_column])) {
                       y <- as.numeric(df()[, y_column])
                     } else {
                       y <- df()[, y_column]
                     }
                     z_column <- isolate(input$selected_variable3)
                     if (is.factor(df()[, z_column])) {
                       z <- as.numeric(df()[, z_column])
                     } else {
                       z <- df()[, z_column]
                     }
                     
                     # データを結合して返す
                     data.frame(x, y, z)
                   })
                   
                   # 三重クロス表を生成し、結果を表示
                   output$triple_cross_table_result <- renderTable({ # ここを修正
                     triple_cross_table <- table(data()[, 1], data()[, 2], data()[, 3])
                     triple_cross_table
                   })
                 })
               })
}


multivariateGenerator <- function(id, dfSpss, dfSpssName) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    data_for_regression <- reactive({
      data <- switch(input$data_for_regressionX,
                     "iris" = iris,
                     "infert" = infert,
                     "Boston" = Boston,
                     "spam" = spam,
                     "airquality" = airquality,
                     "titanic" = data.frame(lapply(data.frame(Titanic), 
                                                   function(i){rep(i, data.frame(Titanic)[, 5])})),
                     "spss" = dfSpss()
      )
      updateSelectInput(session, "data_for_regressionY", choices = colnames(data), 
                        selected = colnames(data)[1])
      
      return(data)
    })
    
    output$debug_data_for_regression <- renderPrint({
      cat("data_for_regression():\n")
      print(data_for_regression())
    })
    
    output$data_table_for_regression <- DT::renderDataTable(
      t(data_for_regression()[1:10, ]), selection = list(target = 'row')
    )
    
    output$rows_selected <- renderPrint(
      input$data_table_for_regression_rows_selected
    )
    
    data_train_and_test <- reactiveValues()
    
    regression_summary <- reactive({
      input$regression_button
      
      y <- data_for_regression()[, isolate(input$data_for_regressionY)]
      x <- data_for_regression()[, isolate(input$data_table_for_regression_rows_selected)]
      
      tmp_data <- data.frame(na.omit(x), na.omit(y))
      colnames(tmp_data) <- c(colnames(x), "dependent_variable")
      
      # Remove rows with NA values
      tmp_data <- na.omit(tmp_data)
      
      if (length(colnames(x)) < 2 && input$regression_type != "pca" && input$regression_type != "fa" && input$regression_type != "partial_corr") {
        return("この分析では2つ以上の説明変数を選択する必要があります。")
      }
      
      if (input$regression_type == "logistic") {
        formula <- as.formula(paste(isolate(input$data_for_regressionY), "~ ."))
        train_index <- createDataPartition(tmp_data$dependent_variable, p = .7,
                                           list = FALSE,
                                           times = 1)
        data_train_and_test$train <- tmp_data[train_index, ]
        data_train_and_test$test <- tmp_data[-train_index, ]
        glm_result <- glm(formula, data = data_train_and_test$train, family = binomial)
        return(glm_result)
      } else if (input$regression_type == "manova") {
        response_vars <- tmp_data[, -ncol(tmp_data)]
        response_matrix <- as.matrix(response_vars)
        manova_result <- manova(response_matrix ~ 1, data = tmp_data)
        return(manova_result)
      } else if (input$regression_type == "pca") {
        pca_result <- prcomp(tmp_data[, -ncol(tmp_data)], center = TRUE, scale. = TRUE)
        return(pca_result)
      } else if (input$regression_type == "fa") {
        library(psych)
        fa_result <- fa(tmp_data[, -ncol(tmp_data)], nfactors = 3, rotate = "varimax")
        return(fa_result)
      } else if (input$regression_type == "partial_corr") {
        if (length(colnames(x)) < 3) {
          return("この分析では3つ以上の説明変数を選択する必要があります。")
        }
        library(ppcor)
        tmp_data <- cbind(x, y)
        partial_corr_result <- pcor(tmp_data)$estimate
        return(partial_corr_result)
      } else {
        formula <- as.formula(paste("dependent_variable ~", paste(colnames(x), collapse = " + ")))
        
        train_index <- createDataPartition(tmp_data$dependent_variable, p = .7,
                                           list = FALSE,
                                           times = 1)
        data_train_and_test$train <- tmp_data[train_index, ]
        data_train_and_test$test <- tmp_data[-train_index, ]
        
        train_result <- train(formula,
                              data = data_train_and_test$train,
                              method = isolate(input$regression_type),
                              tuneLength = 4,
                              preProcess = c('center', 'scale'),
                              trControl = trainControl(method = "cv"),
                              linout = TRUE)
        return(train_result)
      }
    })
    
    output$summary_regression <- renderPrint({
      summary_text <- regression_summary()
      if (is.character(summary_text)) {
        cat(summary_text)
      } else if (input$regression_type == "logistic") {
        cat("Coefficients:\n")
        print(summary_text$coefficients)
        cat("\n\nConfusion matrix:\n")
        print(table(predict(regression_summary(), data_train_and_test$test), 
                    data_train_and_test$test$dependent_variable))
      } else if (input$regression_type == "manova") {
        cat("MANOVA summary:\n")
        print(summary(summary_text))
      } else if (input$regression_type == "pca") {
        cat("PCA summary:\n")
        print(summary(summary_text))
      } else if (input$regression_type == "fa") {
        cat("Factor analysis summary:\n")
        print(summary_text)
      } else if (input$regression_type == "partial_corr") {
        cat("Partial correlation coefficients:\n")
        print(summary_text)
      } else {
        predict_result_residual <- predict(regression_summary(), data_train_and_test$test) - 
          data_train_and_test$test$"dependent_variable"
        cat("MSE（平均二乗誤差）")
        print(sqrt(sum(predict_result_residual ^ 2) / nrow(data_train_and_test$test)))
        summary(regression_summary())
      }
    })
    
    output$debug_regression_summary <- renderPrint({
      cat("regression_summary():\n")
      print(regression_summary())
    })
    
    output$plot_regression <- renderPlot({
      plot_text <- regression_summary()
      if (is.character(plot_text)) {
        cat(plot_text)
        plot.new()
        title(main = plot_text, line = 2)
        return()
      } else if (input$regression_type == "pca") {
        screeplot(plot_text, type = "lines")
      } else if (input$regression_type == "fa") {
        library(GPArotation)
        fa.diagram(plot_text)
      } else {
        predicted <- predict(regression_summary(), data_train_and_test$test)
        observed <- data_train_and_test$test$"dependent_variable"
        
        # Remove infinite or NaN values
        valid_indices <- which(is.finite(predicted) & is.finite(observed))
        predicted <- predicted[valid_indices]
        observed <- observed[valid_indices]
        
        if (length(predicted) == 0 || length(observed) == 0) {
          plot.new()
          title(main = "Error: Predicted or observed values are empty", line = 2)
          return()
        }
        
        if (length(predicted) != length(observed)) {
          plot.new()
          title(main = "Error: Lengths of predicted and observed values do not match", line = 2)
          return()
        }
        plot(predicted, observed, xlab="prediction", ylab="real")
        abline(a=0, b=1, col="red", lwd=2)
      }
    })
  })
}
        
        

statsSelector <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 satatsType <- reactiveVal("histgram")
                 
                 # selectorを変更
                 observe({
                   req(input$selected_variable)
                   satatsType("blank")
                 })
                 
                 # 概観ボタンを押下
                 observe({
                   req(input$trigger_summary)
                   satatsType("summary")
                 })
                 
                 # 平均値ボタンを押下
                 observe({
                   req(input$trigger_average)
                   satatsType("average")
                 })
                 
                 # 中央値ボタンを押下
                 observe({
                   req(input$trigger_median)
                   satatsType("median")
                 })
                 
                 # 最大値ボタンを押下
                 observe({
                   req(input$trigger_max)
                   satatsType("max")
                 })
                 
                 # 最小値ボタンを押下
                 observe({
                   req(input$trigger_min)
                   satatsType("min")
                 })
                 
                 # 不偏分散ボタンを押下
                 observe({
                   req(input$trigger_unbiased_var)
                   satatsType("unbiased_var")
                 })
                 
                 # 標本分散ボタンを押下
                 observe({
                   req(input$trigger_sample_var)
                   satatsType("sample_var")
                 })
                 
                 # 不偏標準偏差ボタンを押下
                 observe({
                   req(input$trigger_unbiased_std)
                   satatsType("unbiased_std")
                 })
                 
                 # 標本標準偏差ボタンを押下
                 observe({
                   req(input$trigger_sample_std)
                   satatsType("sample_std")
                 })
                 
                 # 標準誤差ボタンを押下
                 observe({
                   req(input$trigger_ste)
                   satatsType("ste")
                 })
                 
                 # ヒストグラムボタンを押下
                 observe({
                   req(input$trigger_histogram)
                   satatsType("histgram")
                 })
                 
                 # 散布図ボタンを押下
                 observe({
                   req(input$trigger_scatter)
                   satatsType("scatter")
                 })
                 
                 # t検定ボタンを押下
                 observe({
                   req(input$trigger_t_test_result)
                   satatsType("t_test_result")
                 })

                 # F検定ボタンを押下
                 observe({
                   req(input$trigger_anova_result)
                   satatsType("anova_result")
                 })
                 
                 # 単回帰分析ボタンを押下
                 observe({
                   req(input$trigger_simple_regression_result)
                   satatsType("simple_regression_result")
                 })
                 
                 # 二重クロス表ボタンを押下
                 observe({
                   req(input$trigger_double_cross_table_result)
                   satatsType("double_cross_table_result")
                 })
                 
                 # 三重クロス表ボタンを押下
                 observe({
                   req(input$trigger_triple_cross_table_result)
                   satatsType("triple_cross_table_result")
                 })
                 
                 # 押下した記述統計を出力
                 output$stats <- renderUI({
                   switch(
                     satatsType(),
                     'blank' = textOutput(ns('blank')),
                     'summary' = tableOutput(ns('summary')),
                     'average' = textOutput(ns('average')),
                     'median' = textOutput(ns('median')),
                     'max' = textOutput(ns('max')),
                     'min' = textOutput(ns('min')),
                     'unbiased_var' = textOutput(ns('unbiased_var')),
                     'sample_var' = textOutput(ns('sample_var')),
                     'unbiased_std' = textOutput(ns('unbiased_std')),
                     'sample_std' = textOutput(ns('sample_std')),
                     'ste' = textOutput(ns('ste')),
                     'histgram' = plotOutput(ns('histgram')),
                     'scatter' = plotOutput(ns('scatter')),
                     't_test_result' = verbatimTextOutput(ns('t_test_result')),
                     'anova_result' = verbatimTextOutput(ns('anova_result')),
                     'simple_regression_result' = verbatimTextOutput(ns('simple_regression_result')),
                     'double_cross_table_result' = tableOutput(ns('double_cross_table_result')),
                     'triple_cross_table_result' = tableOutput(ns('triple_cross_table_result')),
                   )
                 })
               })
}

## server全体の統合
server <- shinyServer(function(input, output, session) {
  ## navbar全体の制御
  
  # タブ移動
  observeEvent(input$link_to_analytics, {
    showTab("navbar", target = "変数の分布と代表値", select = TRUE)
  })
  
  
  ## 「データの読込み」navbar
  
  # savの読み込みオブザーバー
  dfList <- filereader(id = "fileread")
  
  
  ## 「変数の分布と代表値」navbar
  
  # ファイル名生成
  fileNameGenerator(id = "analytics",
                    spssName = dfList$spssName)
  
  # プルダウン生成
  selectorGenerator(id = "analytics",
                    dfSpss = dfList$spss)
  
  # 変数の型のテキストを生成
  typeOfVariableGenerator(id = "analytics", df = dfList$spss)
  
  # チェックボックス生成
  checkboxGenerator(id = "analytics",
                    df = dfList$spss,
                    dfValue = dfList$spssValue)
  
  # スライダー生成
  sliderGenerator(id = "analytics", df = dfList$spss)
  
  # アクションボタンを生成
  analyticsButtonGenerator(id = "analytics", df = dfList$spss)
  
  # 概観を生成
  summaryGenerator(id = "analytics",
                   df = dfList$spss,
                   dfValue = dfList$spssValue)
  
  # 平均値生成
  averageGenerator(id = "analytics",
                   df = dfList$spss,
                   dfValue = dfList$spssValue)
  
  # 中央値生成
  medianGenerator(id = "analytics",
                  df = dfList$spss,
                  dfValue = dfList$spssValue)
  
  # 最大値生成
  maxGenerator(id = "analytics",
               df = dfList$spss,
               dfValue = dfList$spssValue)
  
  # 最小値生成
  minGenerator(id = "analytics",
               df = dfList$spss,
               dfValue = dfList$spssValue)
  
  # 不偏分散生成
  unbiasedVarGenerator(id = "analytics",
                       df = dfList$spss,
                       dfValue = dfList$spssValue)
  
  # 標本分散生成
  sampleVarGenerator(id = "analytics",
                     df = dfList$spss,
                     dfValue = dfList$spssValue)
  
  # 不偏標準偏差生成
  unbiasedStdGenerator(id = "analytics",
                       df = dfList$spss,
                       dfValue = dfList$spssValue)
  
  # 標本標準偏差生成
  sampleStdGenerator(id = "analytics",
                     df = dfList$spss,
                     dfValue = dfList$spssValue)
  
  # 標準誤差生成
  steGenerator(id = "analytics",
               df = dfList$spss,
               dfValue = dfList$spssValue)
  
  # ヒストグラム生成
  histgramGenerator(id = "analytics",
                    df = dfList$spss,
                    dfValue = dfList$spssValue)
  
  # 表示する記述統計を選択
  statsSelector(id = "analytics")
  
  
  ## 「二変量分析」navbar
  
  # ファイル名生成
  fileNameGenerator(id = "bivariate",
                    spssName = dfList$spssName)
  
  # プルダウン生成
  selectorGenerator(id = "bivariate",
                    dfSpss = dfList$spss)

  # プルダウン2生成
  selectorGenerator2(id = "bivariate",
                    dfSpss = dfList$spss)
  
  # 変数の型のテキストを生成
  typeOfVariableGenerator(id = "bivariate", df = dfList$spss)
  
  # 変数の型2のテキストを生成
  typeOfVariableGenerator2(id = "bivariate", df = dfList$spss)
  
  # アクションボタンを生成
  bivariateButtonGenerator(id = "bivariate", df = dfList$spss)

  
  # 散布図と相関係数生成
  scatterGenerator(id = "bivariate",
                    df = dfList$spss,
                    dfValue = dfList$spssValue)

  # t検定生成
  t_testGenerator(id = "bivariate",
                  df = dfList$spss,
                  dfValue = dfList$spssValue)
  
  # F検定生成
  f_testGenerator(id = "bivariate",
                  df = dfList$spss,
                  dfValue = dfList$spssValue)
  
  # 単回帰分析生成
  simple_regressionGenerator(id = "bivariate",
                             df = dfList$spss,
                             dfValue = dfList$spssValue)

  # 二重クロス表生成
  double_cross_tableGenerator(id = "bivariate",
                             df = dfList$spss,
                             dfValue = dfList$spssValue)
  
  # 表示する記述統計を選択
  statsSelector(id = "bivariate")

  
  ## 「三重クロス表分析」navbar  
  
  # ファイル名生成
  fileNameGenerator(id = "triple_cross",
                    spssName = dfList$spssName)
  
  # プルダウン生成
  selectorGenerator(id = "triple_cross",
                    dfSpss = dfList$spss)
  
  # プルダウン2生成
  selectorGenerator2(id = "triple_cross",
                     dfSpss = dfList$spss)
  
  # プルダウン3生成
  selectorGenerator3(id = "triple_cross",
                     dfSpss = dfList$spss)
  
  # 変数の型のテキストを生成
  typeOfVariableGenerator(id = "triple_cross", df = dfList$spss)
  
  # 変数の型2のテキストを生成
  typeOfVariableGenerator2(id = "triple_cross", df = dfList$spss)
  
  # 変数の型3のテキストを生成
  typeOfVariableGenerator3(id = "triple_cross", df = dfList$spss)
  
  # アクションボタンを生成
  triple_crossButtonGenerator(id = "triple_cross", df = dfList$spss)
  
  # 三重クロス表生成
  triple_cross_tableGenerator(id = "triple_cross",
                              df = dfList$spss,
                              dfValue = dfList$spssValue)

  # 表示する記述統計を選択
  statsSelector(id = "triple_cross")
    
  ## 「多変量分析」navbar  
  
  # ファイル名生成
  fileNameGenerator(id = "multivariate",
                    spssName = dfList$spssName)
  
  # 回帰分析生成
  multivariateGenerator(id = "multivariate", 
                        dfSpss = dfList$spss, 
                        dfSpssName = dfList$spssName)   
  
})
      
  


### アプリを実行

shinyApp(ui = ui, server = server)
