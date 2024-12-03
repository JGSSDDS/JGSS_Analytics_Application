#### global定義

## ライブラリー設定
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(haven)
library(DT)
library(caret)
library(randomForest)
library(nnet)
library(MASS) 
library(kernlab)
library(car)
library(psych)
library(ggplot2)
library(dplyr)
library(foreign)
library(GPArotation)
library(reshape2)

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


## 「二変量解析」タブ

bivariateTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel("二変量解析・二重クロス表",
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
                   selectInput(ns("selected_variable2"),
                               "分析対象の変数（y）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable2")),hr()),
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

## 「三変量解析」タブ
## 「二元配置分散分析」タブ
two_way_anovaTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel("三変量解析（二元配置分散分析）",
           fluidRow(
             column(
               3,
               tags$style(type = "text/css", ".column-background {background-color: #f5f5f5;}"), 
               div(class = "column-background",
                   br(),
                   h2("変数選択と分析実行", align = "center"),
                   p(hr(), "読込んだデータセット：", textOutput(ns("spssName")), hr()),
                   selectInput(ns("selected_variable"),
                               "カテゴリー型説明変数（x）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable_factor")), hr()),
                   br(),
                   selectInput(ns("selected_variable2"),
                               "カテゴリー型説明変数（y）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable2_factor")), hr()),
                   br(),
                   selectInput(ns("selected_variable3_2"),  # inputIdを一致させる
                               "数値型目的変数（z）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable3_2_number_factor")), hr()),
                   br(),
                   p(strong("以上の変数（x, y, z）にて分析を実行する")),
                   uiOutput(ns("two_way_anova_button")),
                   br()
               )
             ),
             column(
               9,
               br(),
               h2("結果", align = "center"),
               br(),
               br(),
               br(),
               br(),
               br(),
               div(h4(withSpinner(verbatimTextOutput(ns("two_way_anova_result")))), align = "center")
             )
           ))
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
                               "分析対象のカテゴリー型変数（x）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable_factor")),hr()),
                   br(),
                   selectInput(ns("selected_variable2"),
                               "分析対象のカテゴリー型変数（y）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable2_factor")),hr()),
                   br(),
                   selectInput(ns("selected_variable3"),
                               "分析対象のカテゴリー型変数（z）を選択",
                               choices = NULL),
                   p(textOutput(ns("typeOfVariable3_factor")),hr()),
                   br(),
                   uiOutput(ns("select_z_variable")), # 3つ目の変数の値を選択するためのUIを追加
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
                    div(h4(withSpinner(uiOutput(ns("triple_cross_table_result")))), align = "center") # 結果表示部分のIDを更新
             )))
}





## 「多変量解析（回帰分析）」タブ

multivariateTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "重回帰分析・偏相関分析・ロジスティック回帰分析",
    fluidRow(
      column(
        3,
        tags$style(type = "text/css", ".column-background {background-color: #f5f5f5;}"), 
        div(class = "column-background",
            br(),
            h2("変数選択と分析実行", align = "center"),
            p(hr(), "読込んだデータセット：", textOutput(ns("spssName")), hr()),
            
            # selectInputを非表示にするためにdivタグで囲み、styleを適用する
            tags$div(style = "display: none;", 
                     selectInput(
                       ns("data_for_regressionX"),
                       label = "データセットを選択してください。",
                       choices = c(
                         "取込みデータ.sav" = "spss"
                       ),
                       selected = "spss"
                     )
            ),
            selectInput(
              ns("data_for_regressionY"),
              label = tags$div(
                "回帰分析：目的変数を選択",
                tags$br(),
                "偏相関分析：制御変数を選択"
              ),
              choices = NULL
            ),
            br(),
            p(strong(
              "回帰分析：説明変数",
              tags$br(),
              "偏相関分析：対象の変数",
              tags$br(),
              "（Tableより選択）"
            )),
            verbatimTextOutput(ns("rows_selected")),
            br(),
            br(),  # 「分析の手法を選択」の前に1行空ける
            selectInput(
              ns("regression_type"),
              label = "分析の手法を選択",
              choices = c(
                "重回帰分析(一般線形モデル)" = "lm",
                #                "ランダムフォレスト" = "rf",
                #                "3層ニューラルネット" = "nnet",
                "偏相関分析" = "partial_corr",
                "ロジスティック回帰分析" = "logistic"
                # "二元配置分散分析" = "two_way_anova",
                #                "主成分分析" = "pca",
                #                "因子分析" = "fa",
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
          id = ns("multivariateTabset"),
          type = "tabs",
          tabPanel(
            "Table",
            h4("変数を選択してください。"),
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




## 「多変量解析（因子分析・主成分分析）」タブ
multivariateTabPanel2 <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    "因子分析・主成分分析",
    fluidRow(
      column(
        3,
        tags$style(type="text/css", ".column-background {background-color: #f5f5f5;}"), 
        div(class = "column-background",
            br(),
            h2("変数選択と分析実行", align = "center"),
            p(hr(), "読込んだデータセット：", textOutput(ns("spssName")), hr()),
            
            tags$h4(style = "font-weight: bold; font-size: medium;", "変数（Tableより選択）"),
            verbatimTextOutput(ns("rows_selected2")),
            br(),
            # 動的に更新される因子数/主成分数の選択メニューを追加
            uiOutput(ns("num_factors_ui")),
            br(),
            selectInput(
              ns("analysis_type"),
              label = "分析の手法を選択",
              choices = c(
                "因子分析" = "fa",
                "主成分分析" = "pca"
              )
            ),
            actionButton(ns("run_analysis2"), "実行")
        )
      ),
      column(
        9,
        br(),
        h2("結果", align = "center"),
        tabsetPanel(
          id = ns("multivariateTabset2"),
          type = "tabs",
          tabPanel(
            "Table",
            h4("変数を選択してください。"),
            DT::dataTableOutput(ns("data_table_for_analysis2"))
          ),
          tabPanel("分析結果", verbatimTextOutput(ns("analysis_results2"))),
          tabPanel(
            "プロット",
            # プロットを統一して表示
            plotOutput(ns("plot_analysis2"))
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
    
    # 二変量解析・二重クロス表
    bivariateTabPanel(id = "bivariate"),
    
    # 三変量解析
    two_way_anovaTabPanel(id = "two_way_anova"),
    
    # 三重クロス表
    triple_crossTabPanel(id = "triple_cross"),
    
    # 多変量解析（回帰分析）
    multivariateTabPanel(id = "multivariate"),
    
    # 多変量解析（因子分析・主成分分析）
    multivariateTabPanel2(id = "multivariate2"),  # 新しいタブを追加
    
    # FooterupdateSelectizeInput
    footer= h5(hr(),"Copyright(C) 1999-2024, Japanese General Social Surveys. All Rights Reserved.", align = "center")
    
  )
))

### serverを定義

## 「データの読込み」タブのサーバーモジュール

filereader <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 # SPSSファイルの読込み（元の形式）
                 dfSpss <- eventReactive(input$file_SPSS, {
                   req(endsWith(input$file_SPSS$datapath, '.sav'))
                   
                   df <- read.spss(
                     input$file_SPSS$datapath,
                     reencode = "UTF-8",
                     to.data.frame = TRUE,
                     use.value.labels = TRUE,
                     add.undeclared.levels = "no",
                     na.strings = "NA"
                   )
                   processVariables(df)
                 })
                 
                 # 数値化されたデータの読込み（読込時から数値化）
                 dfSpssValue <- eventReactive(input$file_SPSS, {
                   req(endsWith(input$file_SPSS$datapath, '.sav'))
                   
                   df <- read.spss(
                     input$file_SPSS$datapath,
                     reencode = "UTF-8",
                     to.data.frame = TRUE,
                     use.value.labels = FALSE
                   )
                   processVariables(df)
                 })
                 
                 # データフレーム内の変数の処理（共通関数）
                 processVariables <- function(df) {
                   colNames <- names(df)
                   variableLabels <- as.character(attr(df, "variable.labels"))
                   for (i in 1:length(variableLabels)) {
                     label <- if(variableLabels[i] == "") {
                       paste(colNames[i], "()")
                     } else {
                       paste(variableLabels[i], "(", colNames[i], ")")
                     }
                     names(df)[i] <- label
                   }
                   return(df)
                 }
                 
                 # 全て数値化されたデータ（後処理で数値化）
                 dfNumeric <- eventReactive(input$file_SPSS, {
                   df1 <- dfSpss() # 元のデータフレーム
                   df2 <- dfSpssValue() # 数値化されたデータフレーム
                   
                   for(colName in names(df1)) {
                     if(is.factor(df1[[colName]]) && all(df2[[colName]] %in% c(0, 1, NA))) {
                       df1[[colName]] <- as.numeric(as.character(df1[[colName]])) # Factorを数値に変換
                     }
                   }
                   df1
                 })
                 
                 # 最終データフレームの生成
                 finalDataFrame <- reactive({
                   df1 <- dfSpss() # 元のデータ
                   df2 <- dfSpssValue() # 数値化されたデータ
                   df3 <- dfNumeric() # 後処理で数値化されたデータ
                   
                   for(colName in names(df1)) {
                     if(all(df2[[colName]] %in% c(0, 1, NA)) && !all(df1[[colName]] %in% c(0, 1, NA))) {
                       df1[[colName]] <- df2[[colName]] # 条件に応じてdf2のデータを使用
                     } else {
                       df1[[colName]] <- df3[[colName]] # それ以外はdf3を使用
                     }
                   }
                   df1
                 })
                 
                 # 以下の出力処理はdfNumericに基づいて調整されます...
                 output$spssSummaryTitle <- renderText({
                   req(dfNumeric())
                   paste("読込んだ変数一覧（合計", format(length(names(dfNumeric())), big.mark=",", scientific=F), "種類）")
                 })
                 
                 output$spssSummaryLink <- renderUI({
                   req(dfNumeric())
                   actionLink("link_to_analytics", "▶ このデータセットを分析する")
                 })
                 
                 output$spssSummary <- renderTable({
                   data.frame(
                     "変数" = names(dfNumeric()),
                     "回答数" = format(sapply(dfNumeric(), function(x)
                       sum(!is.na(x))), big.mark=",", scientific=F),
                     "欠損数" = format(sapply(dfNumeric(), function(x)
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
                   numeric = dfNumeric,
                   final = finalDataFrame,
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

selectorGenerator_factor <- function(id, dfSpss) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   # 変数名とその型を取得
                   varNames <- names(dfSpss())
                   varTypes <- sapply(dfSpss(), class)
                   
                   # factor型の変数のみを選択
                   factorVars <- varNames[varTypes == "factor"]
                   
                   updateSelectizeInput(session,
                                        "selected_variable",
                                        options=list(maxOptions = 10000),
                                        choices = factorVars)
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

typeOfVariableGenerator_factor <- function(id, df) {
  " Generator of type of variable text based on selector input."
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   # データ列名を取得
                   column <- input$selected_variable
                   
                   # データフレームに変数が含まれるか確認
                   req(column %in% names(df()))
                   
                   # 変数の型の種類のテキストを生成
                   output$typeOfVariable_factor <- renderText({
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
                   
                   # 数値データでない場合やデータが存在しない場合はスライダを表示しない
                   req(!is.factor(x) && length(x[!is.na(x)]) > 0)
                   
                   # NA値を除外してminとmaxを計算
                   min_val <- min(x, na.rm = TRUE)
                   max_val <- max(x, na.rm = TRUE)
                   
                   # スライダ入力を生成
                   p(sliderInput(
                     ns("slider_filter"),
                     h5("数値範囲の絞込み"),
                     min = min(x, na.rm = TRUE),
                     max = max(x, na.rm = TRUE),
                     value = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
                     
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

selectorGenerator2_factor  <- function(id, dfSpss) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   # 変数名とその型を取得
                   varNames <- names(dfSpss())
                   varTypes <- sapply(dfSpss(), class)
                   
                   # factor型の変数のみを選択
                   factorVars <- varNames[varTypes == "factor"]
                   
                   updateSelectizeInput(session,
                                        "selected_variable2",
                                        options=list(maxOptions = 10000),
                                        choices = factorVars)
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

typeOfVariableGenerator2_factor <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   # データ列名を取得
                   column <- input$selected_variable2
                   
                   # データフレームに変数が含まれるか確認
                   req(column %in% names(df()))
                   
                   # 変数の型の種類のテキストを生成
                   output$typeOfVariable2_factor <- renderText({
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
                           "trigger_var_test_result"
                         ), "等分散性検定", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_t_test_result"
                         ), "t検定", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_anova_result"
                         ), "一元配置分散分析", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_simple_regression_result"
                         ), "単回帰分析", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_double_cross_table_result"
                         ), "二重クロス表", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_chi_square_test_result"
                         ), "カイ二乗検定", width = "180px"), align = "center"),
                       )
                     } else {
                       # ラベル型
                       tagList(
                         p(actionButton(ns(
                           "trigger_scatter"
                         ), "散布図 / 相関係数", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_var_test_result"
                         ), "等分散性検定", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_t_test_result"
                         ), "t検定", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_anova_result"
                         ), "一元配置分散分析", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_simple_regression_result"
                         ), "単回帰分析", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_double_cross_table_result"
                         ), "二重クロス表", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_chi_square_test_result"
                         ), "カイ二乗検定", width = "180px"), align = "center"),
                       )
                     }
                   })
                 })
               })
}


scatterGenerator <- function(id, df, dfValue) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   req(input$trigger_scatter)
                   req(nrow(df()) > 0)
                   
                   # データの準備
                   data <- reactive({
                     x_column <- isolate(input$selected_variable)
                     y_column <- isolate(input$selected_variable2)
                     
                     if (is.factor(df()[[x_column]])) {
                       x <- as.numeric(df()[[x_column]])
                     } else {
                       x <- df()[[x_column]]
                     }
                     if (is.factor(df()[[y_column]])) {
                       y <- as.numeric(df()[[y_column]])
                     } else {
                       y <- df()[[y_column]]
                     }
                     
                     # 度数を計算するための一時的なデータフレーム
                     temp_df <- data.frame(x = x, y = y)
                     temp_df <- na.omit(temp_df)
                     
                     # 各点の度数（出現回数）を計算
                     temp_df <- transform(temp_df, freq = ave(x, list(x, y), FUN = length))
                     
                     temp_df
                   })
                   
                   output$scatter <- renderPlot({
                     # 相関係数とP値を計算
                     test_result <- cor.test(data()$x, data()$y, method = "pearson")
                     # 相関係数のテキストを生成（小数点以下4桁まで）
                     correlation_text <- paste("相関係数:", round(test_result$estimate, 4))
                     p_value_text <- paste("P値:", round(test_result$p.value, 4))
                     # 総データポイント数のテキストを生成
                     total_points_text <- paste("度数:", nrow(data()))
                     # プロットのメインタイトルに追加
                     main_title <- paste(isolate(input$selected_variable), "vs.", isolate(input$selected_variable2),
                                         "\n", "\n", "［", correlation_text, "　", p_value_text, "　", total_points_text, "］")
                     
                     # プロットを生成し、度数に基づいて点の大きさを調整
                     with(data(), plot(x, y, 
                                       xlab = isolate(input$selected_variable), ylab = isolate(input$selected_variable2),
                                       main = main_title,
                                       cex = sqrt(freq)  # 度数の平方根を点の大きさとして使用
                     ))
                   })
                   
                   # クリックされた点の情報を取得（必要に応じて）
                   observeEvent(input$plot_click, {
                     res <- nearPoints(dfValue(), input$plot_click, xvar = input$selected_variable,
                                       yvar = input$selected_variable2,
                                       threshold = 5, maxpoints = 10)
                     if (nrow(res) == 0) {
                       return()
                     }
                     # resを利用して必要な処理を行う
                   })
                 })
               })
}

t_testGenerator <- function(id, df, dfValue) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$trigger_t_test_result)
      req(nrow(df()) > 0)
      
      data <- reactive({
        x_column <- isolate(input$selected_variable)
        y_column <- isolate(input$selected_variable2)
        
        # xを因子型に変換し、ラベルを保持
        x_factor <- as.factor(df()[, x_column])
        x_levels <- levels(x_factor)  # ラベルを取得
        x_numeric <- as.numeric(x_factor)  # 数値型に変換
        x <- x_numeric  # 計算用に数値型を使用
        
        # yを数値型に変換
        y <- if (is.factor(df()[, y_column])) {
          as.numeric(df()[, y_column])
        } else {
          df()[, y_column]
        }
        
        # データを結合し、欠損値を削除
        combined_data <- data.frame(x = x, y = y)
        colnames(combined_data) <- c(x_column, y_column)
        initial_data <- combined_data
        combined_data <- na.omit(combined_data)
        
        # 各変数のNを計算
        N_x <- sum(!is.na(initial_data[[x_column]]))
        N_y <- sum(!is.na(initial_data[[y_column]]))
        valid_records <- nrow(combined_data)
        
        # 各変数のDeletedを計算
        Deleted_x <- N_x - valid_records
        Deleted_y <- N_y - valid_records
        
        list(
          data = combined_data,
          initial_data = initial_data,
          N_x = N_x,
          N_y = N_y,
          Deleted_x = Deleted_x,
          Deleted_y = Deleted_y,
          x_column = x_column,
          y_column = y_column,
          x_levels = x_levels  # ラベルをリストに追加
        )
      })
      
      output$t_test_result <- renderPrint({
        data_list <- data()
        combined_data <- data_list$data
        initial_data <- data_list$initial_data
        N_x <- data_list$N_x
        N_y <- data_list$N_y
        Deleted_x <- data_list$Deleted_x
        Deleted_y <- data_list$Deleted_y
        valid_records <- nrow(combined_data)
        x_column <- data_list$x_column
        y_column <- data_list$y_column
        x_levels <- data_list$x_levels  # ラベルを取得
        
        # 数値を四捨五入して小数点以下4桁目まで表示
        round_format <- function(x) sprintf("%.4f", round(as.numeric(x), 4))
        
        # データセットの度数の表示
        cat("Frequency in the dataset:", valid_records, "\n\n")
        
        # 記述統計情報の表示
        cat("Descriptive Statistics:\n")
        descriptive_stats <- data.frame(
          Variable = c(x_column, y_column),
          N = c(N_x, N_y),
          Deleted = c(Deleted_x, Deleted_y),
          Mean = c(
            round_format(mean(initial_data[[x_column]], na.rm = TRUE)), 
            round_format(mean(initial_data[[y_column]], na.rm = TRUE))
          ),
          SD = c(
            round_format(sd(initial_data[[x_column]], na.rm = TRUE)), 
            round_format(sd(initial_data[[y_column]], na.rm = TRUE))
          ),
          SE = c(
            round_format(sd(initial_data[[x_column]], na.rm = TRUE) / sqrt(N_x)), 
            round_format(sd(initial_data[[y_column]], na.rm = TRUE) / sqrt(N_y))
          ),
          Min = c(
            round_format(min(initial_data[[x_column]], na.rm = TRUE)), 
            round_format(min(initial_data[[y_column]], na.rm = TRUE))
          ),
          Q1 = c(
            round_format(quantile(initial_data[[x_column]], 0.25, na.rm = TRUE)), 
            round_format(quantile(initial_data[[y_column]], 0.25, na.rm = TRUE))
          ),
          Median = c(
            round_format(median(initial_data[[x_column]], na.rm = TRUE)), 
            round_format(median(initial_data[[y_column]], na.rm = TRUE))
          ),
          Q3 = c(
            round_format(quantile(initial_data[[x_column]], 0.75, na.rm = TRUE)), 
            round_format(quantile(initial_data[[y_column]], 0.75, na.rm = TRUE))
          ),
          Max = c(
            round_format(max(initial_data[[x_column]], na.rm = TRUE)), 
            round_format(max(initial_data[[y_column]], na.rm = TRUE))
          )
        )
        rownames(descriptive_stats) <- NULL  # ナンバリングを除去
        print(descriptive_stats, row.names = FALSE)
        cat("\n")
        
        # t検定の実行
        t_test_equal_var <- t.test(
          combined_data[[y_column]] ~ combined_data[[x_column]], 
          var.equal = TRUE
        )
        t_test_welch <- t.test(
          combined_data[[y_column]] ~ combined_data[[x_column]], 
          var.equal = FALSE
        )
        
        # 結果のフォーマット関数
        formatResult <- function(test_result) {
          p_value_str <- format(round(test_result$p.value, 4), nsmall = 4)
          
          conf_int_str <- sapply(test_result$conf.int, function(x) {
            format(round(x, 4), nsmall = 4)
          })
          
          estimate_str <- sapply(test_result$estimate, function(x) {
            format(round(x, 4), nsmall = 4)
          })
          
          cat("p-value:", p_value_str, "\n")
          cat("Confidence interval:", paste(conf_int_str, collapse = " to "), "\n")
          cat("Estimate:", paste(estimate_str, collapse = ", "), "\n")
        }
        
        cat("\nT_Test_Equal_Var\n")
        formatResult(t_test_equal_var)
        
        cat("\nT_Test_Welch\n")
        formatResult(t_test_welch)
        
        # グループ別の統計情報を計算して表示
        group_means <- tapply(
          combined_data[[y_column]], 
          combined_data[[x_column]], 
          mean
        )
        group_sd <- tapply(
          combined_data[[y_column]], 
          combined_data[[x_column]], 
          sd
        )
        group_n <- table(combined_data[[x_column]])
        group_se <- group_sd / sqrt(group_n)
        
        group_means <- round(group_means, 4)
        group_sd <- round(group_sd, 4)
        group_se <- round(group_se, 4)
        
        # グループラベルを再適用
        group_levels <- names(group_means)
        names(group_means) <- x_levels[as.numeric(group_levels)]
        names(group_sd) <- x_levels[as.numeric(group_levels)]
        names(group_se) <- x_levels[as.numeric(group_levels)]
        names(group_n) <- x_levels[as.numeric(names(group_n))]
        
        cat("\nGroup Means:\n")  
        print(group_means)
        cat("\nGroup SD:\n")
        print(group_sd)
        cat("\nGroup SE:\n")
        print(group_se)
        cat("\nGroup N:\n")
        print(group_n)
      })
    })
  })
}



var_testGenerator <- function(id, df, dfValue) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$trigger_var_test_result)
      req(nrow(df()) > 0)
      
      data <- reactive({
        x_column <- isolate(input$selected_variable)  # xの変数名を取得
        y_column <- isolate(input$selected_variable2)  # yの変数名を取得
        
        # xを因子型に変換し、数値型に変換
        x <- if (is.factor(df()[, x_column])) {
          as.numeric(df()[, x_column])
        } else {
          df()[, x_column]
        }
        
        # yを因子型に変換し、数値型に変換
        y <- if (is.factor(df()[, y_column])) {
          as.numeric(df()[, y_column])
        } else {
          df()[, y_column]
        }
        
        # データを結合し、欠損値を削除
        combined_data <- data.frame(x = x, y = y)
        colnames(combined_data) <- c(x_column, y_column)  # 変数名を使用してカラム名を設定
        initial_data <- combined_data
        combined_data <- na.omit(combined_data)
        
        # 各変数のNを計算
        N_x <- sum(!is.na(initial_data[[x_column]]))
        N_y <- sum(!is.na(initial_data[[y_column]]))
        valid_records <- nrow(combined_data)
        
        # 各変数のDeletedを計算
        Deleted_x <- N_x - valid_records
        Deleted_y <- N_y - valid_records
        
        list(
          data = combined_data,
          initial_data = initial_data,
          N_x = N_x,
          N_y = N_y,
          Deleted_x = Deleted_x,
          Deleted_y = Deleted_y,
          x_column = x_column,  # 変数名をリストに追加
          y_column = y_column   # 変数名をリストに追加
        )
      })
      
      output$var_test_result <- renderPrint({
        data_list <- data()
        combined_data <- data_list$data
        initial_data <- data_list$initial_data
        N_x <- data_list$N_x
        N_y <- data_list$N_y
        Deleted_x <- data_list$Deleted_x
        Deleted_y <- data_list$Deleted_y
        valid_records <- nrow(combined_data)
        x_column <- data_list$x_column  # 変数名を取得
        y_column <- data_list$y_column  # 変数名を取得
        
        # 数値を四捨五入して小数点以下4桁目まで表示
        round_format <- function(x) sprintf("%.4f", round(x, 4))
        
        # データセットの度数の表示
        cat("Frequency in the dataset:", valid_records, "\n\n")
        
        # 記述統計情報の表示
        cat("Descriptive Statistics:\n")
        descriptive_stats <- data.frame(
          Variable = c(x_column, y_column),  # ここで変数名を表示
          N = c(N_x, N_y),
          Deleted = c(Deleted_x, Deleted_y),
          Mean = c(round_format(mean(initial_data[[x_column]], na.rm = TRUE)), round_format(mean(initial_data[[y_column]], na.rm = TRUE))),
          SD = c(round_format(sd(initial_data[[x_column]], na.rm = TRUE)), round_format(sd(initial_data[[y_column]], na.rm = TRUE))),
          SE = c(round_format(sd(initial_data[[x_column]], na.rm = TRUE) / sqrt(N_x)),
                 round_format(sd(initial_data[[y_column]], na.rm = TRUE) / sqrt(N_y))),
          Min = c(round_format(min(initial_data[[x_column]], na.rm = TRUE)), round_format(min(initial_data[[y_column]], na.rm = TRUE))),
          Q1 = c(round_format(quantile(initial_data[[x_column]], 0.25, na.rm = TRUE)), round_format(quantile(initial_data[[y_column]], 0.25, na.rm = TRUE))),
          Median = c(round_format(median(initial_data[[x_column]], na.rm = TRUE)), round_format(median(initial_data[[y_column]], na.rm = TRUE))),
          Q3 = c(round_format(quantile(initial_data[[x_column]], 0.75, na.rm = TRUE)), round_format(quantile(initial_data[[y_column]], 0.75, na.rm = TRUE))),
          Max = c(round_format(max(initial_data[[x_column]], na.rm = TRUE)), round_format(max(initial_data[[y_column]], na.rm = TRUE)))
        )
        rownames(descriptive_stats) <- NULL  # ナンバリングを除去
        print(descriptive_stats, row.names = FALSE)
        cat("\n")
        
        # xに2つのグループがない場合のエラーメッセージ
        if (length(unique(combined_data[[x_column]])) != 2) {
          return("等分散性の検定には2つのグループが必要です。")
        }
        
        require(car)
        levene_test_result <- car::leveneTest(combined_data[[y_column]] ~ as.factor(combined_data[[x_column]]), data = combined_data)
        
        # 結果のテキストを加工して表示
        levene_output <- capture.output(print(levene_test_result))
        formatted_output <- sapply(levene_output, function(line) {
          line <- gsub(pattern = "(\\d\\.\\d{4})\\d+", replacement = "\\1", x = line)
          gsub(pattern = "\\d\\.\\d+e[-+]?\\d+", replacement = "0.0000", x = line)
        })
        cat(formatted_output, sep = "\n")
      })
    })
  })
}

# ANOVAサマリーのフォーマットを修正する関数
formatSummary <- function(summary_result) {
  formatted_result <- summary_result
  for (i in 1:length(formatted_result)) {
    formatted_result[[i]][, -1] <- lapply(formatted_result[[i]][, -1], function(x) {
      x <- as.numeric(format(x, scientific = FALSE))
      round(x, 4)
    })
  }
  return(formatted_result)
}

# イータ二乗の計算関数
calculateEtaSquared <- function(anova_summary) {
  # 効果による平方和（モデルの平方和）
  effect_ss <- anova_summary[[1]]$`Sum Sq`[1]  # モデルの平方和を取得
  # 残差による平方和（残差の平方和）
  residual_ss <- anova_summary[[1]]$`Sum Sq`[2]  # 残差の平方和を取得
  # 総平方和（効果の平方和 + 残差の平方和）
  total_ss <- effect_ss + residual_ss
  # イータ二乗の計算
  eta_squared <- effect_ss / total_ss
  return(eta_squared)
}


# f_testGenerator関数の定義（イータ二乗の表示部分の修正を含む）
f_testGenerator <- function(id, df, dfValue) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$trigger_anova_result)
      req(nrow(df()) > 0)
      
      data <- reactive({
        # 説明変数と目的変数の指定
        x_column <- isolate(input$selected_variable)
        y_column <- isolate(input$selected_variable2)
        
        # 説明変数xは因子型、目的変数yは数値型に変換
        x <- df()[[x_column]]
        y <- df()[[y_column]]
        
        # xが因子型、yが数値型であることを確認
        if (!is.factor(x) || !is.numeric(y)) {
          stop("エラー: xはカテゴリー型であり、yは数値型である必要があります。")
        }
        
        # データを結合し、欠損値を削除
        combined_data <- data.frame(x = x, y = y)
        colnames(combined_data) <- c(x_column, y_column)  # カラム名を変数名に設定
        initial_data <- combined_data
        combined_data <- na.omit(combined_data)
        
        # 各変数のNとDeletedを計算
        N_x <- sum(!is.na(initial_data[[x_column]]))
        N_y <- sum(!is.na(initial_data[[y_column]]))
        valid_records <- nrow(combined_data)
        Deleted_x <- N_x - valid_records
        Deleted_y <- N_y - valid_records
        
        list(data = combined_data, initial_data = initial_data, N_x = N_x, N_y = N_y, Deleted_x = Deleted_x, Deleted_y = Deleted_y, x_column = x_column, y_column = y_column)
      })
      
      output$anova_result <- renderPrint({
        data_result <- tryCatch({
          data()
        }, error = function(e) {
          cat(e$message, "\n")
          return(NULL)
        })
        
        if (is.null(data_result)) {
          return()
        }
        
        combined_data <- data_result$data
        initial_data <- data_result$initial_data
        N_x <- data_result$N_x
        N_y <- data_result$N_y
        Deleted_x <- data_result$Deleted_x
        Deleted_y <- data_result$Deleted_y
        valid_records <- nrow(combined_data)
        x_column <- data_result$x_column  # xの変数名を取得
        y_column <- data_result$y_column  # yの変数名を取得
        
        # 数値を四捨五入して小数点以下4桁目まで表示
        round_format <- function(x) sprintf("%.4f", round(x, 4))
        
        # データセットの度数の表示
        cat("Frequency in the dataset:", valid_records, "\n\n")
        
        # 記述統計情報の表示
        cat("Descriptive Statistics:\n")
        descriptive_stats <- data.frame(
          Variable = c(x_column, y_column),  # 変数名を使って表示
          N = c(N_x, N_y),
          Deleted = c(Deleted_x, Deleted_y),
          Mean = c(
            round_format(mean(as.numeric(factor(combined_data[[x_column]])), na.rm = TRUE)), 
            round_format(mean(initial_data[[y_column]], na.rm = TRUE))
          ),
          SD = c(
            round_format(sd(as.numeric(factor(combined_data[[x_column]])), na.rm = TRUE)), 
            round_format(sd(initial_data[[y_column]], na.rm = TRUE))
          ),
          SE = c(
            round_format(sd(as.numeric(factor(combined_data[[x_column]])), na.rm = TRUE) / sqrt(N_x)),
            round_format(sd(initial_data[[y_column]], na.rm = TRUE) / sqrt(N_y))
          ),
          Min = c(
            round_format(min(as.numeric(factor(combined_data[[x_column]])), na.rm = TRUE)), 
            round_format(min(initial_data[[y_column]], na.rm = TRUE))
          ),
          Q1 = c(
            round_format(quantile(as.numeric(factor(combined_data[[x_column]])), 0.25, na.rm = TRUE)), 
            round_format(quantile(initial_data[[y_column]], 0.25, na.rm = TRUE))
          ),
          Median = c(
            round_format(median(as.numeric(factor(combined_data[[x_column]])), na.rm = TRUE)), 
            round_format(median(initial_data[[y_column]], na.rm = TRUE))
          ),
          Q3 = c(
            round_format(quantile(as.numeric(factor(combined_data[[x_column]])), 0.75, na.rm = TRUE)), 
            round_format(quantile(initial_data[[y_column]], 0.75, na.rm = TRUE))
          ),
          Max = c(
            round_format(max(as.numeric(factor(combined_data[[x_column]])), na.rm = TRUE)), 
            round_format(max(initial_data[[y_column]], na.rm = TRUE))
          )
        )
        rownames(descriptive_stats) <- NULL  # ナンバリングを除去
        print(descriptive_stats, row.names = FALSE)
        cat("\n")
        
        # 一元配置分散分析の実行（as.formulaを使って動的に式を生成）
        # バッククォートで変数名を囲む
        anova_test <- aov(as.formula(paste("`", y_column, "`", "~", "`", x_column, "`", sep = "")), data = combined_data)
        anova_summary <- summary(anova_test)
        
        cat("ANOVA Summary for", x_column, "and", y_column, ":\n")
        formatted_anova_summary <- formatSummary(anova_summary)
        print(formatted_anova_summary)
        
        eta_squared <- calculateEtaSquared(summary(anova_test))
        eta_squared_formatted <- format(round(eta_squared, 4), nsmall = 4, scientific = FALSE)
        cat("\nEta squared: ", eta_squared_formatted, "\n")
        
        cat("\nBonferroni Post-Hoc Test:\n")
        bonferroni_result <- pairwise.t.test(combined_data[[y_column]], combined_data[[x_column]], p.adjust.method = "bonferroni")
        printResult(bonferroni_result)
        
        # グループ別の統計情報を計算して表示
        group_means <- tapply(combined_data[[y_column]], combined_data[[x_column]], mean)
        group_sd <- tapply(combined_data[[y_column]], combined_data[[x_column]], sd)
        group_n <- table(combined_data[[x_column]])
        group_se <- group_sd / sqrt(group_n)
        
        # 結果を四捨五入
        group_means <- round(group_means, 4)
        group_sd <- round(group_sd, 4)
        group_se <- round(group_se, 4)
        
        cat("\nGroup Means:\n")
        print(group_means)
        cat("\nGroup SD:\n")
        print(group_sd)
        cat("\nGroup SE:\n")
        print(group_se)
        cat("\nGroup N:\n")
        print(group_n)
      })
    })
  })
}




# p値を四捨五入して表示する関数（pairwise.htestのみ対応）
printResult <- function(test_result) {
  if (inherits(test_result, "pairwise.htest")) {
    cat("data: ", test_result$data.name, "\n")
    
    # p値を四捨五入
    p_values_rounded <- round(test_result$p.value, 4)
    
    comparisons <- c()
    max_length <- 0
    has_significant_result <- FALSE # 有意な結果があるかどうかを追跡するフラグ
    
    # 行と列の名前から比較ペアを生成し、最長の比較文字列を見つける
    for (row in rownames(test_result$p.value)) {
      for (col in colnames(test_result$p.value)) {
        if (!is.na(test_result$p.value[row, col])) {
          comparison_str <- paste(row, "-", col)
          comparisons <- c(comparisons, comparison_str)
          max_length <- max(max_length, nchar(comparison_str))
          has_significant_result <- TRUE # 少なくとも1つの比較が表示される
        }
      }
    }
    
    if (has_significant_result) {
      cat("Pairwise comparisons:\n")
      for (comparison_str in comparisons) {
        # 比較文字列から行名と列名を取り出す
        parts <- strsplit(comparison_str, " - ")[[1]]
        row <- parts[1]
        col <- parts[2]
        p_value <- test_result$p.value[which(rownames(test_result$p.value) == row), which(colnames(test_result$p.value) == col)]
        # 比較文字列とp値をフォーマットして出力
        cat(sprintf("%-*s %s\n", max_length, comparison_str, format(p_values_rounded[which(rownames(test_result$p.value) == row), which(colnames(test_result$p.value) == col)], nsmall = 4, scientific = FALSE)))
      }
    } else {
      cat("このデータの組合せでは有意な結果が得られませんでした。\n")
    }
  } else {
    # 他のテスト結果タイプの処理が必要な場合はここに追加
    print(test_result)
  }
}


simple_regressionGenerator <- function(id, df, dfValue) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$trigger_simple_regression_result)
      req(nrow(df()) > 0)
      
      data <- reactive({
        # 計算に必要なデータを取得
        x_column <- isolate(input$selected_variable)  # x の変数名を取得
        y_column <- isolate(input$selected_variable2)  # y の変数名を取得
        
        if (is.factor(df()[, x_column])) {
          x <- as.numeric(df()[, x_column])
        } else {
          x <- df()[, x_column]
        }
        
        if (is.factor(df()[, y_column])) {
          y <- as.numeric(df()[, y_column])
        } else {
          y <- df()[, y_column]
        }
        
        # データを結合し、欠損値を削除
        combined_data <- data.frame(x, y)
        colnames(combined_data) <- c(x_column, y_column)  # カラム名を設定
        initial_data <- combined_data
        combined_data <- na.omit(combined_data)
        
        # 正規化/標準化を行う
        normalize <- function(x) (x - mean(x)) / sd(x)
        combined_data <- as.data.frame(lapply(combined_data, normalize))
        colnames(combined_data) <- c(x_column, y_column)  # カラム名を再設定
        
        list(
          data = combined_data,
          initial_data = initial_data,
          total_records = nrow(initial_data),
          valid_records = nrow(combined_data),
          x_column = x_column,
          y_column = y_column
        )
      })
      
      # 単回帰分析を実行し、結果を表示
      output$simple_regression_result <- renderPrint({
        combined_data <- data()$data
        initial_data <- data()$initial_data
        total_records <- data()$total_records
        valid_records <- data()$valid_records
        x_column <- data()$x_column  # x の変数名を取得
        y_column <- data()$y_column  # y の変数名を取得
        
        # 単回帰モデルの作成（変数名を使用）
        simple_regression_model <- lm(
          as.formula(paste("`", y_column, "` ~ `", x_column, "`", sep = "")),
          data = combined_data
        )
        regression_summary <- summary(simple_regression_model)
        
        # 数値を四捨五入して小数点以下4桁まで表示
        round_format <- function(x) sprintf("%.4f", x)
        
        # データセットの度数の表示
        cat("Frequency in the dataset:", valid_records, "\n\n")
        
        # 記述統計情報の表示
        cat("Descriptive Statistics:\n")
        descriptive_stats <- data.frame(
          Variable = colnames(initial_data),
          N = sapply(initial_data, function(x) sum(!is.na(x))),
          Deleted = sapply(initial_data, function(x) sum(!is.na(x)) - valid_records),
          Mean = sapply(initial_data, function(x) round_format(mean(x, na.rm = TRUE))),
          SD = sapply(initial_data, function(x) round_format(sd(x, na.rm = TRUE))),
          SE = sapply(initial_data, function(x) round_format(sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))),
          Min = sapply(initial_data, function(x) round_format(min(x, na.rm = TRUE))),
          Q1 = sapply(initial_data, function(x) round_format(quantile(x, 0.25, na.rm = TRUE))),
          Median = sapply(initial_data, function(x) round_format(median(x, na.rm = TRUE))),
          Q3 = sapply(initial_data, function(x) round_format(quantile(x, 0.75, na.rm = TRUE))),
          Max = sapply(initial_data, function(x) round_format(max(x, na.rm = TRUE)))
        )
        rownames(descriptive_stats) <- NULL
        print(descriptive_stats)
        cat("\n")
        
        coefficients <- regression_summary$coefficients
        formatted_coefficients <- as.data.frame(apply(coefficients, 2, round_format))
        rownames(formatted_coefficients) <- rownames(coefficients)
        colnames(formatted_coefficients) <- colnames(coefficients)
        
        cat("Coefficients:\n")
        print(formatted_coefficients)
        
        # 残差標準誤差なども四捨五入して表示
        cat("\nResidual standard error:", round_format(regression_summary$sigma), "\n")
        cat("Multiple R-squared:", round_format(regression_summary$r.squared), "\n")
        cat("Adjusted R-squared:", round_format(regression_summary$adj.r.squared), "\n")
        
        # F-statistic と p-value を表示
        fstat <- regression_summary$fstatistic
        f_pvalue <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
        cat(
          "F-statistic:", round_format(fstat[1]), "on", fstat[2], "and",
          fstat[3], "DF,  p-value:", round_format(f_pvalue), "\n"
        )
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
                   
                   x_column <- input$selected_variable
                   y_column <- input$selected_variable2
                   x <- df()[, x_column]
                   y <- df()[, y_column]
                   
                   # クロス表を生成
                   cross_table <- table(x, y)
                   
                   # 各行の合計を計算してクロス表に追加
                   row_totals <- rowSums(cross_table)
                   cross_table_with_totals <- cbind(cross_table, 合計 = row_totals)
                   
                   # 各列の合計を計算（合計行用）
                   col_totals <- colSums(cross_table)
                   total_of_totals <- sum(row_totals)
                   col_totals_with_total <- c(col_totals, 合計=total_of_totals)
                   
                   # 合計行を追加
                   cross_table_with_totals_and_grand_total <- rbind(cross_table_with_totals, 合計=col_totals_with_total)
                   
                   # 各セルの値を行和の比率（パーセンテージ）に変換
                   percentage_table <- sweep(cross_table_with_totals, 1, row_totals, FUN = "/") * 100
                   
                   # 3番目の表用のパーセンテージ計算（列の合計を使って各セルをパーセンテージに変換）
                   col_percentage_table <- sweep(cross_table, 2, col_totals, FUN = "/") * 100
                   col_percentage_with_total <- rbind(col_percentage_table, 合計=rep(100, ncol(cross_table)))
                   
                   # HTMLテーブルの生成
                   output$double_cross_table_result <- renderUI({
                     div(
                       tags$style(type = 'text/css', "
                         .custom-table, .custom-table-small {
                           width: 100%;
                           table-layout: auto;
                         }
                         .custom-table th, .custom-table td,
                         .custom-table-small th, .custom-table-small td {
                           min-width: 150px;
                         }
                         .custom-table th:first-child, .custom-table td:first-child,
                         .custom-table th:last-child, .custom-table td:last-child {
                           width: 15%;
                         }
                         .custom-table-small {
                           width: 85%;
                           margin-left: 0;
                           margin-right: auto;
                           table-layout: auto;
                         }
                         .custom-table-small th:first-child, .custom-table-small td:first-child {
                           width: 17.65%;
                         }
                         .text-right { text-align: right; }
                         .text-left { text-align: left; }
                         .text-center { text-align: center; }
                         /* 太字を解除する新しいスタイル */
                         .custom-table th, .custom-table td,
                         .custom-table-small th, .custom-table-small td {
                           font-weight: normal;
                         }
                       "),
                       tags$table(class = 'table table-bordered custom-table', 
                                  tags$thead(
                                    tags$tr(
                                      tags$th(paste("　"), class = "text-right"),  
                                      lapply(colnames(cross_table_with_totals_and_grand_total), tags$th)
                                    )
                                  ), 
                                  tags$tbody(
                                    lapply(1:nrow(cross_table_with_totals_and_grand_total), function(i) {
                                      tags$tr(
                                        tags$th(rownames(cross_table_with_totals_and_grand_total)[i]), 
                                        lapply(cross_table_with_totals_and_grand_total[i, ], function(cell) {
                                          cellValue <- ifelse(is.na(cell) | is.nan(cell), "―", cell)
                                          cellStyle <- if(grepl("^[0-9.]+%?$", cellValue) || cellValue == "―") "text-right" else "text-left"
                                          tags$td(cellValue, class = cellStyle)
                                        })
                                      )
                                    })
                                  )
                       ),
                       tags$table(class = 'table table-bordered custom-table', 
                                  tags$thead(
                                    tags$tr(
                                      tags$th(paste("　"), class = "text-right"), 
                                      lapply(colnames(percentage_table), tags$th)
                                    )
                                  ), 
                                  tags$tbody(
                                    lapply(1:nrow(percentage_table), function(i) {
                                      tags$tr(
                                        tags$th(rownames(percentage_table)[i]), 
                                        lapply(percentage_table[i, ], function(cell) {
                                          cellValue <- ifelse(is.na(cell) | is.nan(cell), "―", sprintf("%.2f%%", cell))
                                          cellStyle <- if(grepl("^[0-9.]+%?$", cellValue) || cellValue == "―") "text-right" else "text-left"
                                          tags$td(cellValue, class = cellStyle)
                                        })
                                      )
                                    })
                                  )
                       ),
                       tags$table(class = 'table table-bordered custom-table-small', 
                                  tags$thead(
                                    tags$tr(
                                      tags$th(paste("　"), class = "text-right"),
                                      lapply(colnames(col_percentage_with_total), tags$th)
                                    )
                                  ), 
                                  tags$tbody(
                                    lapply(1:nrow(col_percentage_with_total), function(i) {
                                      tags$tr(
                                        tags$th(rownames(col_percentage_with_total)[i]), 
                                        lapply(col_percentage_with_total[i, ], function(cell) {
                                          cellValue <- ifelse(is.na(cell) | is.nan(cell), "―", sprintf("%.2f%%", cell))
                                          cellStyle <- if(grepl("^[0-9.]+%?$", cellValue) || cellValue == "―") "text-right" else "text-left"
                                          tags$td(cellValue, class = cellStyle)
                                        })
                                      )
                                    })
                                  )
                       )
                     )
                   })
                 })
               }
  )
}






chiSquareTestGenerator <- function(id, df, dfValue) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$trigger_chi_square_test_result)
      req(nrow(df()) > 0)
      
      data <- reactive({
        # 説明変数と目的変数の指定
        x_column <- isolate(input$selected_variable)  # x の変数名を取得
        y_column <- isolate(input$selected_variable2)  # y の変数名を取得
        
        # データの準備（因子型に変換）
        x <- as.factor(df()[, x_column])
        y <- as.factor(df()[, y_column])
        
        # x, yを数値型ラベルに変換
        x_numeric <- as.numeric(x)
        y_numeric <- as.numeric(y)
        
        # データを結合し、欠損値を削除
        combined_data <- data.frame(x = x, y = y, x_numeric = x_numeric, y_numeric = y_numeric)
        colnames(combined_data) <- c(x_column, y_column, "x_numeric", "y_numeric")  # カラム名を設定
        initial_data <- combined_data
        combined_data <- na.omit(combined_data)
        
        # 各変数のNとDeletedを計算
        N_x <- sum(!is.na(initial_data$x))
        N_y <- sum(!is.na(initial_data$y))
        valid_records <- nrow(combined_data)
        Deleted_x <- N_x - valid_records
        Deleted_y <- N_y - valid_records
        
        list(
          data = combined_data,
          initial_data = initial_data,
          N_x = N_x,
          N_y = N_y,
          Deleted_x = Deleted_x,
          Deleted_y = Deleted_y,
          x_column = x_column,  # 変数名をリストに追加
          y_column = y_column   # 変数名をリストに追加
        )
      })
      
      output$chi_square_test_result <- renderPrint({
        data_result <- data()
        
        combined_data <- data_result$data
        initial_data <- data_result$initial_data
        N_x <- data_result$N_x
        N_y <- data_result$N_y
        Deleted_x <- data_result$Deleted_x
        Deleted_y <- data_result$Deleted_y
        valid_records <- nrow(combined_data)
        x_column <- data_result$x_column  # x の変数名を取得
        y_column <- data_result$y_column  # y の変数名を取得
        
        # 小数点以下4桁まで表示するフォーマット関数
        round_format <- function(x) sprintf("%.4f", round(x, 4))
        
        # データセットの度数の表示
        cat("Frequency in the dataset:", valid_records, "\n\n")
        
        # 記述統計情報の表示
        cat("Descriptive Statistics:\n")
        descriptive_stats <- data.frame(
          Variable = c(x_column, y_column),  # 変数名を使用
          N = c(N_x, N_y),
          Deleted = c(Deleted_x, Deleted_y),
          Mean = c(round_format(mean(combined_data$x_numeric, na.rm = TRUE)), 
                   round_format(mean(combined_data$y_numeric, na.rm = TRUE))),
          SD = c(round_format(sd(combined_data$x_numeric, na.rm = TRUE)), 
                 round_format(sd(combined_data$y_numeric, na.rm = TRUE))),
          SE = c(round_format(sd(combined_data$x_numeric, na.rm = TRUE) / sqrt(N_x)),
                 round_format(sd(combined_data$y_numeric, na.rm = TRUE) / sqrt(N_y))),
          Min = c(round_format(min(combined_data$x_numeric, na.rm = TRUE)), 
                  round_format(min(combined_data$y_numeric, na.rm = TRUE))),
          Q1 = c(round_format(quantile(combined_data$x_numeric, 0.25, na.rm = TRUE)), 
                 round_format(quantile(combined_data$y_numeric, 0.25, na.rm = TRUE))),
          Median = c(round_format(median(combined_data$x_numeric, na.rm = TRUE)), 
                     round_format(median(combined_data$y_numeric, na.rm = TRUE))),
          Q3 = c(round_format(quantile(combined_data$x_numeric, 0.75, na.rm = TRUE)), 
                 round_format(quantile(combined_data$y_numeric, 0.75, na.rm = TRUE))),
          Max = c(round_format(max(combined_data$x_numeric, na.rm = TRUE)), 
                  round_format(max(combined_data$y_numeric, na.rm = TRUE)))
        )
        rownames(descriptive_stats) <- NULL  # ナンバリングを除去
        print(descriptive_stats, row.names = FALSE)
        cat("\n")
        
        # カイ二乗検定の実行
        test_result <- chisq.test(table(combined_data[[x_column]], combined_data[[y_column]]))
        
        # 結果の表示形式を変更
        formatted_result <- sprintf("X-squared = %.4f, df = %d, p-value = %.4f", 
                                    test_result$statistic, test_result$parameter, test_result$p.value)
        
        # 結果を表示
        cat(formatted_result)
      })
    })
  })
}



# selectorGenerator3 <- function(id, dfSpss) {
#  moduleServer(id,
#               function(input, output, session) {
#                 # 変数を選択するためのプルダウン項目制御
#                 # ここらへんで項目数をとる
#                 observe({
#                   updateSelectizeInput(session,
#                                        "selected_variable3",
#                                        options=list(maxOptions = 10000),
#                                        choices = names(dfSpss()))
#                 })
#               })
#}

selectorGenerator3_factor <- function(id, dfSpss) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   # 変数名とその型を取得
                   varNames <- names(dfSpss())
                   varTypes <- sapply(dfSpss(), class)
                   
                   # factor型の変数のみを選択
                   factorVars <- varNames[varTypes == "factor"]
                   
                   updateSelectizeInput(session,
                                        "selected_variable3",
                                        options=list(maxOptions = 10000),
                                        choices = factorVars)
                 })
               })
}

#typeOfVariableGenerator3 <- function(id, df) {
#  moduleServer(id,
#               function(input, output, session) {
#                 observe({
#                   # データ列名を取得
#                   column <- input$selected_variable3
#                   
#                   # データフレームに変数が含まれるか確認
#                   req(column %in% names(df()))
#                   
#                   # 変数の型の種類のテキストを生成
#                   output$typeOfVariable3 <- renderText({
#                     x <- df()[, column]
#                     if (is.factor(x)) {
#                       "データ型：カテゴリー値"
#                     } else{
#                       choiceList = list()
#                       "データ型：数値"
#                     }
#                   })
#                 })
#               })
#}

typeOfVariableGenerator3_factor <- function(id, df) {
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   # データ列名を取得
                   column <- input$selected_variable3
                   
                   # データフレームに変数が含まれるか確認
                   req(column %in% names(df()))
                   
                   # 変数の型の種類のテキストを生成
                   output$typeOfVariable3_factor <- renderText({
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
                 # 3つ目の変数の選択肢を動的に更新
                 observeEvent(input$selected_variable3, {
                   choices <- levels(factor(df()[[input$selected_variable3]]))
                   updateSelectInput(session, "selected_z_value", "Select Z Value:", choices = choices)
                 }, ignoreInit = TRUE)
                 
                 observe({
                   req(input$trigger_triple_cross_table_result)
                   req(nrow(df()) > 0)
                   req(input$selected_z_value)
                   
                   x_column <- input$selected_variable
                   y_column <- input$selected_variable2
                   selected_z_value <- input$selected_z_value
                   
                   df_filtered <- df()[df()[, input$selected_variable3] == selected_z_value, ]
                   x <- df_filtered[, x_column]
                   y <- df_filtered[, y_column]
                   
                   cross_table <- table(x, y)
                   row_totals <- rowSums(cross_table)
                   cross_table_with_totals <- cbind(cross_table, 合計 = row_totals)
                   
                   col_totals <- colSums(cross_table)
                   total_of_totals <- sum(row_totals)
                   col_totals_with_total <- c(col_totals, 合計=total_of_totals)
                   
                   cross_table_with_totals_and_grand_total <- rbind(cross_table_with_totals, 合計=col_totals_with_total)
                   
                   percentage_table <- sweep(cross_table_with_totals, 1, row_totals, FUN = "/") * 100
                   col_percentage_table <- sweep(cross_table, 2, col_totals, FUN = "/") * 100
                   col_percentage_with_total <- rbind(col_percentage_table, 合計=rep(100, ncol(cross_table)))
                   
                   output$triple_cross_table_result <- renderUI({
                     div(
                       tags$style(type = 'text/css', "
                         .custom-table, .custom-table-small {
                           width: 100%;
                           table-layout: fixed;
                         }
                         .custom-table th:first-child, .custom-table td:first-child,
                         .custom-table th:last-child, .custom-table td:last-child {
                           width: 15%;
                         }
                         .custom-table th:not(:first-child):not(:last-child), .custom-table td:not(:first-child):not(:last-child) {
                           width: auto;
                         }
                         /* 太字を解除するスタイルの更新 */
                         .custom-table th, .custom-table td,
                         .custom-table-small th, .custom-table-small td {
                           font-weight: normal; /* 太字解除 */
                         }
                         .custom-table-small {
                           width: 85%;
                           margin-left: 0;
                           margin-right: auto;
                         }
                         .custom-table-small th:first-child, .custom-table-small td:first-child {
                           width: 17.65%;
                         }
                         .text-right { text-align: right; }
                         .text-left { text-align: left; }
                       "),
                       tags$table(class = 'table table-bordered custom-table', 
                                  tags$thead(
                                    tags$tr(
                                      tags$th(paste("　"), class = "text-right"), 
                                      lapply(colnames(cross_table_with_totals_and_grand_total), function(col) tags$th(col, style = "font-weight: normal;"))
                                    )
                                  ), 
                                  tags$tbody(
                                    lapply(1:nrow(cross_table_with_totals_and_grand_total), function(i) {
                                      tags$tr(
                                        tags$th(rownames(cross_table_with_totals_and_grand_total)[i], style = "font-weight: normal;"), 
                                        lapply(cross_table_with_totals_and_grand_total[i, ], function(cell) {
                                          cellValue <- ifelse(is.na(cell) | is.nan(cell), "―", cell)
                                          cellStyle <- if(grepl("^[0-9.]+%?$", cellValue) || cellValue == "―") "text-right" else "text-left"
                                          tags$td(cellValue, class = cellStyle, style = "font-weight: normal;")
                                        })
                                      )
                                    })
                                  )
                       ),
                       tags$table(class = 'table table-bordered custom-table', 
                                  tags$thead(
                                    tags$tr(
                                      tags$th(paste("　"), class = "text-right"), 
                                      lapply(colnames(percentage_table), function(col) tags$th(col, style = "font-weight: normal;"))
                                    )
                                  ), 
                                  tags$tbody(
                                    lapply(1:nrow(percentage_table), function(i) {
                                      tags$tr(
                                        tags$th(rownames(percentage_table)[i], style = "font-weight: normal;"), 
                                        lapply(percentage_table[i, ], function(cell) {
                                          cellFormatted <- ifelse(is.nan(cell), "―", sprintf("%.2f%%", cell))
                                          tags$td(cellFormatted, class = "text-right", style = "font-weight: normal;")
                                        })
                                      )
                                    })
                                  )
                       ),
                       tags$table(class = 'table table-bordered custom-table-small', 
                                  tags$thead(
                                    tags$tr(
                                      tags$th(paste("　"), class = "text-right"), 
                                      lapply(colnames(col_percentage_with_total), function(col) tags$th(col, style = "font-weight: normal;"))
                                    )
                                  ), 
                                  tags$tbody(
                                    lapply(1:nrow(col_percentage_with_total), function(i) {
                                      tags$tr(
                                        tags$th(rownames(col_percentage_with_total)[i], style = "font-weight: normal;"), 
                                        lapply(col_percentage_with_total[i, ], function(cell) {
                                          cellFormatted <- ifelse(is.nan(cell), "―", sprintf("%.2f%%", cell))
                                          tags$td(cellFormatted, class = "text-right", style = "font-weight: normal;")
                                        })
                                      )
                                    })
                                  )
                       )
                     )
                   })
                 })
                 
                 output$select_z_variable <- renderUI({
                   selectInput(session$ns("selected_z_value"), "変数（z）詳細", choices = character(0))
                 })
               }
  )
}

selectorGenerator3_2_number_factor <- function(id, dfSpss) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(dfSpss())  # データフレームの存在を確認
      
      # データフレームの変数名とその型を取得
      varNames <- names(dfSpss())
      varTypes <- sapply(dfSpss(), class)
      
      # 数値型の変数のみを選択
      selectedVars <- varNames[varTypes %in% c("numeric", "integer")]
      
      # UIを更新して数値型の変数を表示
      updateSelectizeInput(
        session = session,
        inputId = "selected_variable3_2",  # inputIdがUIと一致しているか確認
        options = list(maxOptions = 10000),
        choices = selectedVars
      )
    })
  })
}

typeOfVariableGenerator3_2_number_factor <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(df())  # データフレームの存在を確認
      
      column <- input$selected_variable3_2  # 正しいinputIdを使用
      
      req(column %in% names(df()))  # データフレームに変数が含まれるか確認
      
      output$typeOfVariable3_2_number_factor <- renderText({
        x <- df()[[column]]
        if (is.numeric(x)) {
          "データ型：数値"
        } else {
          "データ型：カテゴリー値"
        }
      })
    })
  })
}





# 二元配置分散分析を実行するためのボタンを生成するモジュール
two_way_anovaButtonGenerator <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(nrow(df()) > 0)
      
      output$two_way_anova_button <- renderUI({
        ns = NS(id)
        tagList(
          p(actionButton(ns("trigger_two_way_anova"), "二元配置分散分析", width = "180px"), align = "center")
        )
      })
    })
  })
}


# 二元配置分散分析を実行するモジュール
two_way_anovaGenerator <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(input$trigger_two_way_anova)  # ボタンが押されたことを確認
      req(nrow(df()) > 0)  # データフレームが空でないことを確認
      
      data_for_analysis <- reactive({
        # 入力から変数名を取得
        x_column <- isolate(input$selected_variable)
        y_column <- isolate(input$selected_variable2)
        z_column <- isolate(input$selected_variable3_2)
        
        # データの取得
        x <- df()[[x_column]]
        y <- df()[[y_column]]
        z <- df()[[z_column]]
        
        # 変数の型変換（数値として必要な変数のみ変換）
        z <- as.numeric(as.character(z))
        
        # xとyをファクターに変換
        x <- as.factor(x)
        y <- as.factor(y)
        
        # カテゴリー変数xとyの数値型ラベルへの変換
        x_numeric <- as.numeric(factor(x, levels = unique(x)))
        y_numeric <- as.numeric(factor(y, levels = unique(y)))
        
        # 初期データの作成
        initial_data <- data.frame(
          z = z,
          x_numeric = x_numeric,
          y_numeric = y_numeric
        )
        initial_data[[x_column]] <- x
        initial_data[[y_column]] <- y
        
        # 欠損値を除去したデータフレームを作成
        clean_data <- na.omit(initial_data)
        
        # データが存在するかチェック
        if (nrow(clean_data) == 0) {
          cat("エラー: データが空です。変数選択に問題があります。\n")
          return(NULL)
        }
        
        # 各変数のNと削除数を計算
        N_x <- sum(!is.na(initial_data[[x_column]]))
        N_y <- sum(!is.na(initial_data[[y_column]]))
        N_z <- sum(!is.na(initial_data$z))
        Deleted_x <- N_x - nrow(clean_data)
        Deleted_y <- N_y - nrow(clean_data)
        Deleted_z <- N_z - nrow(clean_data)
        
        list(
          data = clean_data,
          initial_data = initial_data,
          x_column = x_column,
          y_column = y_column,
          z_column = z_column,
          N_x = N_x,
          N_y = N_y,
          N_z = N_z,
          Deleted_x = Deleted_x,
          Deleted_y = Deleted_y,
          Deleted_z = Deleted_z
        )
      })
      
      output$two_way_anova_result <- renderPrint({
        data <- data_for_analysis()
        
        if (is.null(data)) {
          cat("データがありません。変数選択を確認してください。\n")
          return()
        }
        
        clean_data <- data$data
        initial_data <- data$initial_data
        x_column <- data$x_column
        y_column <- data$y_column
        z_column <- data$z_column
        N_x <- data$N_x
        N_y <- data$N_y
        N_z <- data$N_z
        Deleted_x <- data$Deleted_x
        Deleted_y <- data$Deleted_y
        Deleted_z <- data$Deleted_z
        valid_records <- nrow(clean_data)
        
        # 小数点以下4桁まで表示するフォーマット関数
        round_format <- function(x) {
          x_rounded <- round(x, 4)
          # 科学的記法を避けて小数表示し、nsmallで小数点以下桁数を固定
          x_formatted <- format(x_rounded, nsmall = 4, scientific = FALSE)
          return(x_formatted)
        }
        
        # p値を特別にフォーマットする関数
        p_value_format <- function(p) {
          if (is.na(p)) return(NA)
          if (p < 0.0001) {
            "< 0.0001"
          } else {
            format(round(p, 4), nsmall = 4, scientific = FALSE)
          }
        }
        
        # データセットの度数の表示
        cat("Frequency in the dataset:", valid_records, "\n\n")
        
        # 記述統計情報の表示
        cat("Descriptive Statistics:\n")
        descriptive_stats <- data.frame(
          Variable = c(x_column, y_column, z_column),
          N = c(N_x, N_y, N_z),
          Deleted = c(Deleted_x, Deleted_y, Deleted_z),
          Mean = c(
            round_format(mean(clean_data$x_numeric, na.rm = TRUE)), 
            round_format(mean(clean_data$y_numeric, na.rm = TRUE)), 
            round_format(mean(clean_data$z, na.rm = TRUE))
          ),
          SD = c(
            round_format(sd(clean_data$x_numeric, na.rm = TRUE)), 
            round_format(sd(clean_data$y_numeric, na.rm = TRUE)), 
            round_format(sd(clean_data$z, na.rm = TRUE))
          ),
          SE = c(
            round_format(sd(clean_data$x_numeric, na.rm = TRUE) / sqrt(N_x)),
            round_format(sd(clean_data$y_numeric, na.rm = TRUE) / sqrt(N_y)),
            round_format(sd(clean_data$z, na.rm = TRUE) / sqrt(N_z))
          ),
          Min = c(
            round_format(min(clean_data$x_numeric, na.rm = TRUE)), 
            round_format(min(clean_data$y_numeric, na.rm = TRUE)), 
            round_format(min(clean_data$z, na.rm = TRUE))
          ),
          Q1 = c(
            round_format(quantile(clean_data$x_numeric, 0.25, na.rm = TRUE)), 
            round_format(quantile(clean_data$y_numeric, 0.25, na.rm = TRUE)), 
            round_format(quantile(clean_data$z, 0.25, na.rm = TRUE))
          ),
          Median = c(
            round_format(median(clean_data$x_numeric, na.rm = TRUE)), 
            round_format(median(clean_data$y_numeric, na.rm = TRUE)), 
            round_format(median(clean_data$z, na.rm = TRUE))
          ),
          Q3 = c(
            round_format(quantile(clean_data$x_numeric, 0.75, na.rm = TRUE)), 
            round_format(quantile(clean_data$y_numeric, 0.75, na.rm = TRUE)), 
            round_format(quantile(clean_data$z, 0.75, na.rm = TRUE))
          ),
          Max = c(
            round_format(max(clean_data$x_numeric, na.rm = TRUE)), 
            round_format(max(clean_data$y_numeric, na.rm = TRUE)), 
            round_format(max(clean_data$z, na.rm = TRUE))
          )
        )
        rownames(descriptive_stats) <- NULL
        print(descriptive_stats, row.names = FALSE)
        cat("\n")
        
        # xとyのコントラストを設定
        contrasts(clean_data[[x_column]]) <- contr.sum(length(levels(clean_data[[x_column]])))
        contrasts(clean_data[[y_column]]) <- contr.sum(length(levels(clean_data[[y_column]])))
        
        # 二元配置分散分析の実行 (Type III)
        formula <- as.formula(paste("z ~ `", x_column, "` * `", y_column, "`", sep=""))
        lm_model <- lm(formula, data = clean_data)
        anova_summary <- Anova(lm_model, type = "III")
        
        cat("ANOVA Summary (Type III):\n")
        
        # anova_summaryをデータフレームに変換
        anova_df <- as.data.frame(anova_summary)
        # 列名を取得
        col_names <- colnames(anova_df)
        
        # 各列に対してフォーマット関数を適用
        for (col in c("Sum Sq", "Mean Sq", "F value")) {
          if (col %in% col_names) {
            anova_df[[col]] <- sapply(anova_df[[col]], function(x) round_format(as.numeric(x)))
          }
        }
        # Pr(>F) 列のフォーマット
        if ("Pr(>F)" %in% col_names) {
          anova_df[["Pr(>F)"]] <- sapply(anova_df[["Pr(>F)"]], p_value_format)
        }
        
        # 行名を列に追加
        anova_df <- cbind(Effect = rownames(anova_df), anova_df)
        rownames(anova_df) <- NULL
        
        # anova_dfを表示
        print(anova_df, row.names = FALSE)
        
        # イータ二乗の計算（ANOVAの結果から適切に取得）
        ss_total <- sum(as.numeric(anova_summary[, "Sum Sq"]))  # 総平方和
        ss_effects <- as.numeric(anova_summary[1:(nrow(anova_summary) - 1), "Sum Sq"])  # 各効果の平方和
        eta_squared <- ss_effects / ss_total
        eta_squared_formatted <- sapply(eta_squared, round_format)
        cat("\nEta squared: ", paste(eta_squared_formatted, collapse = ", "), "\n")
        
        # Bonferroni Test用のデータをチェック
        bonferroni_data <- interaction(clean_data[[x_column]], clean_data[[y_column]], drop=TRUE)
        if (is.null(bonferroni_data)) {
          cat("エラー: Bonferroni用のデータがNULLです。\n")
          return()
        }
        
        # xのカテゴリ間の比較
        cat("\nBonferroni Post-Hoc Test for", x_column, ":\n")
        bonferroni_result_x <- tryCatch({
          pairwise.t.test(clean_data$z, clean_data[[x_column]], p.adjust.method = "bonferroni")
        }, error = function(e) {
          cat("エラー:", x_column, "のBonferroni事後検定中にエラーが発生しました。", e$message, "\n")
          return(NULL)
        })
        if (!is.null(bonferroni_result_x)) {
          # p値をフォーマット
          p_values_formatted <- apply(bonferroni_result_x$p.value, c(1, 2), p_value_format)
          p_values_df <- as.data.frame(p_values_formatted)
          # NAを "-" に置換
          p_values_df[is.na(p_values_df)] <- "-"
          # 結果を表示
          cat("\tPairwise comparisons using t tests with pooled SD \n\n")
          cat("data: ", bonferroni_result_x$data.name, "\n\n")
          print(p_values_df)
          cat("\nP value adjustment method:", bonferroni_result_x$p.adjust.method, "\n")
        }
        
        # yのカテゴリ間の比較
        cat("\nBonferroni Post-Hoc Test for", y_column, ":\n")
        bonferroni_result_y <- tryCatch({
          pairwise.t.test(clean_data$z, clean_data[[y_column]], p.adjust.method = "bonferroni")
        }, error = function(e) {
          cat("エラー:", y_column, "のBonferroni事後検定中にエラーが発生しました。", e$message, "\n")
          return(NULL)
        })
        if (!is.null(bonferroni_result_y)) {
          # p値をフォーマット
          p_values_formatted <- apply(bonferroni_result_y$p.value, c(1, 2), p_value_format)
          p_values_df <- as.data.frame(p_values_formatted)
          # NAを "-" に置換
          p_values_df[is.na(p_values_df)] <- "-"
          # 結果を表示
          cat("\tPairwise comparisons using t tests with pooled SD \n\n")
          cat("data: ", bonferroni_result_y$data.name, "\n\n")
          print(p_values_df)
          cat("\nP value adjustment method:", bonferroni_result_y$p.adjust.method, "\n")
        }
        
        # x:yの組み合わせでの比較結果
        cat("\nBonferroni Post-Hoc Test for", x_column, ":", y_column, "interaction:\n")
        bonferroni_result <- tryCatch({
          pairwise.t.test(clean_data$z, bonferroni_data, p.adjust.method = "bonferroni")
        }, error = function(e) {
          cat("エラー:", x_column, "と", y_column, "のBonferroni事後検定中にエラーが発生しました。", e$message, "\n")
          return(NULL)
        })
        if (!is.null(bonferroni_result)) {
          # p値をフォーマット
          p_values_formatted <- apply(bonferroni_result$p.value, c(1, 2), p_value_format)
          p_values_df <- as.data.frame(p_values_formatted)
          # NAを "-" に置換
          p_values_df[is.na(p_values_df)] <- "-"
          # 結果を表示
          cat("\tPairwise comparisons using t tests with pooled SD \n\n")
          cat("data: ", bonferroni_result$data.name, "\n\n")
          print(p_values_df)
          cat("\nP value adjustment method:", bonferroni_result$p.adjust.method, "\n")
        }
        
        # グループ別の統計情報を計算して表示
        group_means <- tapply(clean_data$z, list(clean_data[[x_column]], clean_data[[y_column]]), mean)
        group_sd <- tapply(clean_data$z, list(clean_data[[x_column]], clean_data[[y_column]]), sd)
        group_n <- table(clean_data[[x_column]], clean_data[[y_column]])
        group_se <- group_sd / sqrt(group_n)
        
        # グループ別統計情報の出力
        cat("\nGroup Means:\n")
        print(round(group_means, 4))
        cat("\nGroup SD:\n")
        print(round(group_sd, 4))
        cat("\nGroup SE:\n")
        print(round(group_se, 4))
        cat("\nGroup N:\n")
        print(group_n)
      })
    })
  })
}







multivariateGenerator <- function(id, finalDataFrame, dfSpss, dfSpssName) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$regression_button, {
      updateTabsetPanel(session, "multivariateTabset", selected = "分析結果")
    })
    
    data_for_regression <- reactive({
      data <- switch(input$data_for_regressionX, "spss" = data.frame(finalDataFrame()))
      data[] <- lapply(data, function(column) {
        if (is.factor(column)) {
          unique_values <- unique(as.character(column))
          if (!all(unique_values %in% c("0", "1", NA))) {
            return(as.integer(column))
          }
        }
        return(column)
      })
      updateSelectInput(session, "data_for_regressionY", choices = colnames(data), selected = colnames(data)[1])
      return(data)
    })
    
    output$data_table_for_regression <- DT::renderDataTable({
      filtered_data <- data_for_regression()[1:10, ]
      if (!is.null(input$search_column) && input$search_column != "") {
        filtered_data <- filtered_data[grep(input$search_column, filtered_data[[1]], ignore.case = TRUE), ]
      }
      filtered_data_transposed <- t(filtered_data)
      DT::datatable(filtered_data_transposed, selection = list(target = 'row'), options = list(autoWidth = TRUE, searching = TRUE, paging = FALSE))
    }, server = FALSE)
    
    output$rows_selected <- renderPrint({
      input$data_table_for_regression_rows_selected
    })
    
    regression_summary <- reactive({
      input$regression_button
      
      selected_variables <- isolate(input$data_table_for_regression_rows_selected)
      data <- data_for_regression()
      
      # 選択された変数と目的変数を含むデータセットを作成
      x <- data[, selected_variables, drop = FALSE]
      y <- data[, isolate(input$data_for_regressionY), drop = FALSE]
      
      # xとyを結合し、欠損値を含む行を削除
      initial_data <- cbind(y, x)
      tmp_data <- na.omit(initial_data)
      colnames(tmp_data)[1] <- isolate(input$data_for_regressionY)
      total_records <- nrow(tmp_data) # 正規化されたデータセットのレコード数
      
      if (length(colnames(x)) < 2 && input$regression_type != "pca" && input$regression_type != "fa" && input$regression_type != "partial_corr") {
        return("この分析では2つ以上の説明変数を選択する必要があります。")
      }
      
      if (input$regression_type == "logistic") {
        formula <- as.formula(paste(isolate(input$data_for_regressionY), "~ ."))
        glm_result <- glm(formula, data = tmp_data, family = binomial())
        
        # オッズ比の計算
        odds_ratios <- exp(coef(glm_result))
        
        # Pseudo R-squared and Log-likelihood
        pR2 <- 1 - glm_result$deviance / glm_result$null.deviance
        logLik_value <- logLik(glm_result)
        
        # VIFの計算
        library(car)
        vif_values <- vif(glm_result)
        vif_df <- data.frame(Variable = names(vif_values), VIF = round(vif_values, 4), check.names = FALSE)
        rownames(vif_df) <- NULL  # ナンバリングを除去
        
        return(list(result = glm_result, odds_ratios = odds_ratios, pR2 = pR2, logLik = logLik_value, vif_df = vif_df, data = tmp_data, initial_data = initial_data, total_records = total_records))
      } else if (input$regression_type == "two_way_anova") {
        y <- tmp_data[, 1]
        x1 <- tmp_data[, 2]
        x2 <- tmp_data[, 3]
        x1 <- as.factor(x1)
        x2 <- as.factor(x2)
        two_way_anova_result <- aov(y ~ x1 * x2, data = tmp_data)
        return(list(result = two_way_anova_result, data = tmp_data, initial_data = initial_data, total_records = total_records))
      } else if (input$regression_type == "pca") {
        pca_result <- prcomp(tmp_data[, -1], center = TRUE, scale. = TRUE)
        return(list(result = pca_result, data = tmp_data, initial_data = initial_data, total_records = total_records))
      } else if (input$regression_type == "fa") {
        library(psych)
        fa_result <- fa(tmp_data[, -1], nfactors = 3, rotate = "varimax")
        return(list(result = fa_result, data = tmp_data, initial_data = initial_data, total_records = total_records))
      } else if (input$regression_type == "partial_corr") {
        library(ppcor)
        x_data <- tmp_data[, -1, drop = FALSE]  # Yを除くX変数
        y_data <- tmp_data[, 1, drop = FALSE]   # Y変数
        var_names <- colnames(x_data)
        n_vars <- length(var_names)
        partial_corr_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
        p_value_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars)
        colnames(partial_corr_matrix) <- var_names
        rownames(partial_corr_matrix) <- var_names
        colnames(p_value_matrix) <- var_names
        rownames(p_value_matrix) <- var_names
        
        for (i in 1:n_vars) {
          for (j in i:n_vars) {
            xi <- x_data[, i]
            xj <- x_data[, j]
            # エラーハンドリングを追加
            res <- tryCatch({
              pcor.test(xi, xj, y_data)
            }, error = function(e) {
              list(estimate = NA, p.value = NA)
            })
            partial_corr_matrix[i, j] <- res$estimate
            partial_corr_matrix[j, i] <- res$estimate  # 対称性
            p_value_matrix[i, j] <- res$p.value
            p_value_matrix[j, i] <- res$p.value
          }
        }
        
        return(list(partial_corr_matrix = partial_corr_matrix, p_value_matrix = p_value_matrix, data = tmp_data, initial_data = initial_data, total_records = total_records))
      } else {
        formula <- as.formula(paste(isolate(input$data_for_regressionY), "~", paste(colnames(x), collapse = " + ")))
        model <- lm(formula, data = tmp_data)
        
        # VIFの計算
        library(car)
        vif_values <- vif(model)
        
        # VIFの結果をデータフレームに変換し、列名を設定
        vif_df <- data.frame(Variable = names(vif_values), VIF = round(vif_values, 4), check.names = FALSE)
        rownames(vif_df) <- NULL  # ナンバリングを除去
        
        # 標準化偏回帰係数の計算
        sdy <- sd(tmp_data[[isolate(input$data_for_regressionY)]])
        sdx <- apply(tmp_data[, -1], 2, sd)
        beta_coefficients <- coef(model)[-1] * (sdx / sdy)
        beta_coefficients <- setNames(round(beta_coefficients, 4), colnames(tmp_data)[-1])
        beta_coefficients_df <- data.frame(Variable = names(beta_coefficients), `Standardized Coefficients (β)` = beta_coefficients, check.names = FALSE)
        rownames(beta_coefficients_df) <- NULL  # ナンバリングを除去
        
        return(list(model = model, beta_coefficients = beta_coefficients_df, vif_df = vif_df, data = tmp_data, initial_data = initial_data, total_records = total_records))
      }
    })
    
    output$summary_regression <- renderPrint({
      summary_text <- regression_summary()
      round_format <- function(x) sprintf("%.4f", x)
      
      # データセットの度数の表示
      cat("Frequency in the dataset:", summary_text$total_records, "\n\n")
      
      # 記述統計情報の表示
      cat("Descriptive Statistics:\n")
      initial_data <- summary_text$initial_data
      tmp_data <- summary_text$data
      total_records <- summary_text$total_records
      
      descriptive_stats <- data.frame(
        Variable = colnames(initial_data),
        N = sapply(initial_data, function(x) sum(!is.na(x))),  # initial_dataを使用してNを計算
        Deleted = sapply(initial_data, function(x) abs(total_records - sum(!is.na(x)))),  # initial_dataを使用してDeletedを計算
        Mean = sapply(tmp_data, function(x) round_format(mean(x, na.rm = TRUE))),  # tmp_dataを使用してMeanを計算
        SD = sapply(tmp_data, function(x) round_format(sd(x, na.rm = TRUE))),  # tmp_dataを使用してSDを計算
        SE = sapply(tmp_data, function(x) round_format(sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))),  # tmp_dataを使用してSEを計算
        Min = sapply(tmp_data, function(x) round_format(min(x, na.rm = TRUE))),  # tmp_dataを使用してMinを計算
        Q1 = sapply(tmp_data, function(x) round_format(quantile(x, 0.25, na.rm = TRUE))),  # tmp_dataを使用してQ1を計算
        Median = sapply(tmp_data, function(x) round_format(median(x, na.rm = TRUE))),  # tmp_dataを使用してMedianを計算
        Q3 = sapply(tmp_data, function(x) round_format(quantile(x, 0.75, na.rm = TRUE))),  # tmp_dataを使用してQ3を計算
        Max = sapply(tmp_data, function(x) round_format(max(x, na.rm = TRUE))),  # tmp_dataを使用してMaxを計算
        check.names = FALSE
      )
      rownames(descriptive_stats) <- NULL  # ナンバリングを除去
      print(descriptive_stats, row.names = FALSE)  # 行名を表示しないように指定
      cat("\n")
      
      if (is.character(summary_text)) {
        cat(summary_text)
      } else if (input$regression_type == "logistic") {
        cat("Coefficients:\n")
        coefficients <- summary(summary_text$result)$coefficients
        formatted_coefficients <- as.data.frame(apply(coefficients, 2, round_format))
        rownames(formatted_coefficients) <- rownames(coefficients)
        colnames(formatted_coefficients) <- colnames(coefficients)
        print(formatted_coefficients, row.names = FALSE)  # 行名を表示しないように指定
        
        # オッズ比の表示
        cat("\nOdds Ratios:\n")
        odds_ratios_rounded <- sapply(summary_text$odds_ratios, round_format)  # オッズ比を小数4桁に丸める
        print(odds_ratios_rounded, row.names = FALSE)  # 行名を表示しないように指定
        
        # R2乗決定係数の表示
        cat("\nPseudo R-squared:", round_format(summary_text$pR2), "\n")
        
        # 対数尤度の表示
        cat("Log-likelihood:", round_format(summary_text$logLik), "\n")
        
        # VIFの表示
        cat("\nVIF values:\n")
        vif_df <- summary_text$vif_df
        print(vif_df, row.names = FALSE)  # 行名を表示しないように指定
        
        # 混同行列の表示
        cat("\n\nConfusion Matrix:\n")
        # 予測値を閾値0.5で生成
        predicted <- ifelse(predict(summary_text$result, summary_text$data, type = "response") > 0.5, 1, 0)
        actual <- as.numeric(as.character(summary_text$data[[isolate(input$data_for_regressionY)]]))
        
        # 混同行列の作成と表示
        confusion_matrix <- table(Predicted = predicted, Actual = actual)
        print(confusion_matrix, row.names = FALSE)  # 行名を表示しないように指定
      } else if (input$regression_type == "two_way_anova") {
        cat("Two-way ANOVA summary:\n")
        anova_summary <- summary(summary_text$result)
        formatted_summary <- capture.output(anova_summary)
        formatted_summary <- gsub("([0-9]+\\.[0-9]{4})([0-9]+)", "\\1", formatted_summary)
        cat(paste(formatted_summary, collapse = "\n"))
      } else if (input$regression_type == "pca") {
        cat("PCA summary:\n")
        pca_summary <- summary(summary_text$result)
        formatted_summary <- capture.output(pca_summary)
        formatted_summary <- gsub("([0-9]+\\.[0-9]{4})([0-9]+)", "\\1", formatted_summary)
        cat(paste(formatted_summary, collapse = "\n"))
      } else if (input$regression_type == "fa") {
        cat("Factor analysis summary:\n")
        fa_summary <- summary_text$result
        print(fa_summary, digits = 4)
      } else if (input$regression_type == "partial_corr") {
        cat("Partial correlation coefficients (controlling for", isolate(input$data_for_regressionY), "):\n\n")
        partial_corr_matrix <- summary_text$partial_corr_matrix
        print(round(partial_corr_matrix, 4))
        cat("\nP-values:\n\n")
        p_value_matrix <- summary_text$p_value_matrix
        print(round(p_value_matrix, 4))
      } else if (input$regression_type == "lm") {
        model <- summary_text$model
        cat("Coefficients:\n")
        coefficients <- summary(model)$coefficients
        formatted_coefficients <- as.data.frame(apply(coefficients, 2, round_format))
        rownames(formatted_coefficients) <- rownames(coefficients)
        colnames(formatted_coefficients) <- colnames(coefficients)
        print(formatted_coefficients, row.names = FALSE)  # 行名を表示しないように指定
        
        cat("\nStandardized Coefficients (β):\n")
        beta_coefficients_df <- summary_text$beta_coefficients
        print(beta_coefficients_df, row.names = FALSE)  # 行名を表示しないように指定
        
        cat("\nVIF values:\n")
        vif_df <- summary_text$vif_df
        print(vif_df, row.names = FALSE)  # 行名を表示しないように指定
        
        cat("\nResidual standard error:", round_format(summary(model)$sigma), "\n")
        cat("Multiple R-squared:", round_format(summary(model)$r.squared), "\n")
        cat("Adjusted R-squared:", round_format(summary(model)$adj.r.squared), "\n")
        
        fstat <- summary(model)$fstatistic
        f_pvalue <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
        cat("F-statistic:", round_format(fstat[1]), "on", fstat[2], "and", fstat[3], "DF,  p-value:", round_format(f_pvalue), "\n")
      } else {
        model_summary <- summary(summary_text$result)
        cat("Model summary:\n")
        print(model_summary)
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
        screeplot(plot_text$result, type = "lines")
      } else if (input$regression_type == "fa") {
        library(GPArotation)
        fa.diagram(plot_text$result)
      } else if (input$regression_type == "partial_corr") {
        # 偏相関行列のヒートマップを描画
        partial_corr_matrix <- plot_text$partial_corr_matrix
        heatmap(partial_corr_matrix, symm = TRUE, main = "Partial Correlation Matrix")
      } else {
        if (input$regression_type == "lm") {
          predicted <- predict(plot_text$model, plot_text$data)
          observed <- plot_text$data[[isolate(input$data_for_regressionY)]]
        } else {
          predicted <- predict(plot_text$result, plot_text$data)
          observed <- plot_text$data[[isolate(input$data_for_regressionY)]]
        }
        
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


multivariateGenerator2 <- function(id, finalDataFrame) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 分析ボタンが押されたときにタブを切り替える
    observeEvent(input$run_analysis2, {
      updateTabsetPanel(session, "multivariateTabset2", selected = "分析結果")
    })
    
    # データテーブルのレンダリング
    data_for_regression <- reactive({
      # データフレームを生成し、型変換
      data <- data.frame(finalDataFrame())
      data[] <- lapply(data, function(column) {
        if (is.factor(column)) {
          unique_values <- unique(as.character(column))
          if (!all(unique_values %in% c("0", "1", NA))) {
            return(as.integer(column))
          }
        }
        return(as.numeric(as.character(column)))  # すべてのデータを数値に変換
      })
      updateSelectInput(session, "data_for_regressionY", choices = colnames(data), selected = colnames(data)[1])
      return(data)
    })
    
    output$data_table_for_analysis2 <- DT::renderDataTable({
      # データの最初の10行を転置して表示
      transposed_data <- t(data_for_regression()[1:10, ])
      DT::datatable(transposed_data, selection = 'multiple', options = list(autoWidth = TRUE, searching = TRUE, paging = FALSE))
    }, server = FALSE)
    
    # 選択された行の表示
    output$rows_selected2 <- renderPrint({
      input$data_table_for_analysis2_rows_selected
    })
    
    # 動的に因子数/主成分数の選択メニューを生成
    output$num_factors_ui <- renderUI({
      req(input$data_table_for_analysis2_rows_selected)
      num_vars <- length(input$data_table_for_analysis2_rows_selected)
      
      # 最大値を変数の数に設定
      max_factors <- num_vars
      
      # デフォルト値を設定（因子分析の場合は変数の数 - 1、主成分分析の場合は変数の数）
      default_value <- if (input$analysis_type == "fa") num_vars - 1 else num_vars
      
      selectInput(
        ns("num_factors"),
        label = "抽出する因子/主成分の数を選択",
        choices = 1:max_factors,
        selected = default_value
      )
    })
    
    # 分析結果を表示するための反応オブジェクト
    analysis_result <- reactive({
      req(input$data_table_for_analysis2_rows_selected)
      req(input$num_factors)
      
      # データ準備
      selected_vars <- input$data_table_for_analysis2_rows_selected
      data <- data_for_regression()
      initial_data <- data[, selected_vars, drop = FALSE]  # 初期データ
      
      # 欠損値の除去と型変換
      initial_data[] <- lapply(initial_data, function(column) {
        if (is.factor(column)) {
          unique_values <- unique(as.character(column))
          if (!all(unique_values %in% c("0", "1", NA))) {
            return(as.integer(column))
          }
        }
        return(as.numeric(as.character(column)))  # すべてのデータを数値に変換
      })
      
      # 欠損値の再除去
      tmp_data <- na.omit(initial_data)  # すべての変換後に欠損値を除去
      total_records <- nrow(tmp_data)  # 欠損値を除去したデータセットのレコード数
      
      # 主成分分析または因子分析を実行
      result <- tryCatch({
        switch(input$analysis_type,
               "pca" = prcomp(tmp_data, center = TRUE, scale. = TRUE),
               "fa" = {
                 # 相関行列の確認
                 correlation_matrix <- cor(tmp_data, use = "pairwise.complete.obs")
                 if (any(is.na(correlation_matrix))) {
                   stop("Error: missing values (NAs) in the correlation matrix do not allow me to continue.")
                 }
                 rotations <- c("none", "varimax", "promax", "oblimin")
                 fa_results <- list()
                 for (rot in rotations) {
                   # 最尤法（ml）を指定
                   fa_result <- fa(tmp_data, nfactors = as.numeric(input$num_factors), rotate = rot, fm = 'ml')
                   fa_results[[rot]] <- fa_result
                 }
                 fa_results
               })
      }, error = function(e) {
        # エラーメッセージをキャプチャ
        return(list(error = e$message))
      })
      
      return(list(result = result, initial_data = initial_data, tmp_data = tmp_data, total_records = total_records))
    })
    
    # 結果の出力
    output$analysis_results2 <- renderPrint({
      result_list <- analysis_result()
      result <- result_list$result
      initial_data <- result_list$initial_data
      tmp_data <- result_list$tmp_data
      total_records <- result_list$total_records
      
      # Frequency in the datasetとDescriptive Statisticsの表示
      cat("Frequency in the dataset:", total_records, "\n\n")
      cat("Descriptive Statistics:\n")
      round_format <- function(x) sprintf("%.4f", x)  # 小数点以下4桁まで表示
      descriptive_stats <- data.frame(
        Variable = colnames(initial_data),
        N = sapply(initial_data, function(x) sum(!is.na(x))),  # 各変数の欠損値がないデータの数を計算
        Deleted = sapply(initial_data, function(x) sum(!is.na(x)) - total_records),  # NからFrequency in the datasetを引いた値
        Mean = sapply(tmp_data, function(x) round_format(mean(x, na.rm = TRUE))),  # tmp_dataを使用してMeanを計算
        SD = sapply(tmp_data, function(x) round_format(sd(x, na.rm = TRUE))),  # tmp_dataを使用してSDを計算
        SE = sapply(tmp_data, function(x) round_format(sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))),  # tmp_dataを使用してSEを計算
        Min = sapply(tmp_data, function(x) round_format(min(x, na.rm = TRUE))),  # tmp_dataを使用してMinを計算
        Q1 = sapply(tmp_data, function(x) round_format(quantile(x, 0.25, na.rm = TRUE))),  # tmp_dataを使用してQ1を計算
        Median = sapply(tmp_data, function(x) round_format(median(x, na.rm = TRUE))),  # tmp_dataを使用してMedianを計算
        Q3 = sapply(tmp_data, function(x) round_format(quantile(x, 0.75, na.rm = TRUE))),  # tmp_dataを使用してQ3を計算
        Max = sapply(tmp_data, function(x) round_format(max(x, na.rm = TRUE))),  # tmp_dataを使用してMaxを計算
        check.names = FALSE
      )
      rownames(descriptive_stats) <- NULL  # ナンバリングを除去
      print(descriptive_stats, row.names = FALSE)  # 行名を表示しないように指定
      cat("\n")
      
      # エラーが発生した場合の処理
      if (is.list(result) && !is.null(result$error)) {
        cat("エラーが発生しました:\n")
        cat(result$error, "\n")
        cat("抽出する因子の数が多すぎる可能性があります。変数の数よりも少ない因子の数を選択してください。\n")
        return()
      }
      
      # 主成分分析または因子分析の結果を表示
      if (input$analysis_type == "pca") {
        cat("Principal Component Analysis Results:\n\n")
        
        # 固有値と分散説明率の表示（指定された数の主成分のみ）
        cat("Eigenvalues and Variance Explained:\n")
        eigenvalues <- result$sdev^2
        variance_explained <- eigenvalues / sum(eigenvalues)
        cumulative_variance <- cumsum(variance_explained)
        pca_table <- data.frame(
          Component = paste0("PC", 1:length(eigenvalues)),
          Eigenvalue = round(eigenvalues, 4),
          Variance_Explained = round(variance_explained * 100, 2),
          Cumulative_Variance = round(cumulative_variance * 100, 2)
        )
        print(pca_table[1:as.numeric(input$num_factors), ])
        
        # 主成分負荷量の表示（指定された数の主成分のみ）
        cat("Component Loadings:\n")
        loadings <- result$rotation[, 1:as.numeric(input$num_factors), drop = FALSE] %*% diag(result$sdev[1:as.numeric(input$num_factors)])
        colnames(loadings) <- paste0("PC", 1:ncol(loadings))
        print(round(loadings, 4))
        
      } else if (input$analysis_type == "fa") {
        # 固有値と分散説明率の表示（因子分析の場合）
        cat("Eigenvalues and Variance Explained:\n")
        eigenvalues <- eigen(cor(tmp_data))$values
        total_variance <- sum(eigenvalues)
        variance_explained <- eigenvalues / total_variance
        cumulative_variance <- cumsum(variance_explained)
        fa_table <- data.frame(
          Component = paste0("Factor", 1:length(eigenvalues)),
          Eigenvalue = round(eigenvalues, 4),
          Variance_Explained = round(variance_explained * 100, 2),
          Cumulative_Variance = round(cumulative_variance * 100, 2)
        )
        print(fa_table[1:as.numeric(input$num_factors), ])
        
        cat("\nFactor Analysis Results:\n")
        for (rot in names(result)) {
          cat("\nRotation method:", rot, "\n")
          fa_res <- result[[rot]]
          # 因子負荷量を表示
          if (!is.null(fa_res$loadings)) {
            colnames(fa_res$loadings) <- paste0("FA", 1:ncol(fa_res$loadings))
            # 共通性を取得
            communalities <- fa_res$communality
            # 因子負荷量と共通性を結合
            loadings_with_comm <- cbind(fa_res$loadings[, ], Communality = communalities)
            print(round(loadings_with_comm, 4))
          } else {
            cat("No factor loadings available for rotation method:", rot, "\n")
          }
          # 因子間相関行列の表示
          if (!is.null(fa_res$Phi)) {
            cat("\nFactor Intercorrelation Matrix:", rot, "\n")
            rownames(fa_res$Phi) <- paste0("FA", 1:nrow(fa_res$Phi))
            colnames(fa_res$Phi) <- paste0("FA", 1:ncol(fa_res$Phi))
            print(round(fa_res$Phi, 4))
          } else {
            cat("\nNo Factor Intercorrelation Matrix for rotation method:", rot, "\n")
          }
          # 分散説明量の表示
          if (!is.null(fa_res$Vaccounted)) {
            cat("\nVariance Accounted For:\n")
            colnames(fa_res$Vaccounted) <- paste0("FA", 1:ncol(fa_res$Vaccounted))
            rownames(fa_res$Vaccounted) <- c("SS loadings", "Proportion Var", "Cumulative Var", "Proportion Explained", "Cumulative Proportion")
            print(round(fa_res$Vaccounted, 4))
          }
        }
      }
    })
    
    # プロットの出力
    output$plot_analysis2 <- renderPlot({
      result_list <- analysis_result()
      result <- result_list$result
      num_components <- as.numeric(input$num_factors)
      
      # エラーが発生した場合はプロットを表示しない
      if (is.list(result) && !is.null(result$error)) {
        return()
      }
      
      if (input$analysis_type == "pca") {
        # 主成分分析の場合の固有値を取得
        eigenvalues <- result$sdev^2
      } else if (input$analysis_type == "fa") {
        # 因子分析の場合の固有値を取得
        tmp_data <- result_list$tmp_data
        eigenvalues <- eigen(cor(tmp_data))$values
      }
      
      # Scree Plot のデータフレームを作成
      scree_data <- data.frame(
        Component = 1:length(eigenvalues),
        Eigenvalue = eigenvalues
      )
      # 指定された数のコンポーネントまでを表示
      scree_data <- scree_data[1:num_components, ]
      
      # ggplot2 でプロットを作成
      ggplot(scree_data, aes(x = Component, y = Eigenvalue)) +
        geom_line() +
        geom_point() +
        geom_text(aes(label = round(Eigenvalue, 2)), vjust = -0.5, size = 3) +
        labs(
          title = "Scree Plot",
          x = ifelse(input$analysis_type == "pca", "Principal Component", "Factor"),
          y = "Eigenvalue"
        ) +
        theme_minimal()
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
                 
                 # 等分散性検定ボタンを押下
                 observe({
                   req(input$trigger_var_test_result)
                   satatsType("var_test_result")
                 })
                 
                 # t検定ボタンを押下
                 observe({
                   req(input$trigger_t_test_result)
                   satatsType("t_test_result")
                 })
                 
                 # 一元配置分散分析ボタンを押下
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
                 
                 # カイ二乗検定ボタンを押下
                 observe({
                   req(input$trigger_chi_square_test_result)
                   satatsType("chi_square_test_result")
                 })
                 
                 # 三変量解析のボタンを押下
                 observe({
                   req(input$trigger_two_way_anova_result)
                   satatsType("two_way_anova_result")
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
                     'var_test_result' = verbatimTextOutput(ns('var_test_result')),
                     't_test_result' = verbatimTextOutput(ns('t_test_result')),
                     'anova_result' = verbatimTextOutput(ns('anova_result')),
                     'simple_regression_result' = verbatimTextOutput(ns('simple_regression_result')),
                     'double_cross_table_result' = uiOutput(ns('double_cross_table_result')),
                     'chi_square_test_result' = verbatimTextOutput(ns('chi_square_test_result')),
                     'triple_cross_table_result' = tableOutput(ns('triple_cross_table_result')),
                     'two_way_anova_result' = verbatimTextOutput(ns('two_way_anova_result'))
                   )
                 })
               })
}

## server全体の統合
server <- shinyServer(function(input, output, session) {
  ## navbar全体の制御
  
  # タブ移動
  observeEvent(input$link_to_analytics, {
    updateTabsetPanel(session, "navbar", selected = "変数の分布と代表値")
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
  
  ## 「二変量解析」navbar
  
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
  
  # 等分散性の検定生成
  var_testGenerator(id = "bivariate",
                    df = dfList$spss,
                    dfValue = dfList$spssValue)
  
  # t検定生成
  t_testGenerator(id = "bivariate",
                  df = dfList$spss,
                  dfValue = dfList$spssValue)
  
  # 一元配置分散分析生成
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
  
  # カイ2乗検定生成
  chiSquareTestGenerator(id = "bivariate",
                         df = dfList$spss,
                         dfValue = dfList$spssValue)
  
  # 表示する記述統計を選択
  statsSelector(id = "bivariate")
  
  
  ## 「二元配置分散分析」navbar用SERVER設定
  
  # ファイル名生成
  fileNameGenerator(id = "two_way_anova", 
                    spssName = dfList$spssName)
  
  # プルダウン生成（カテゴリー値）
  selectorGenerator_factor(id = "two_way_anova", 
                           dfSpss = dfList$spss)
  
  # プルダウン2生成（カテゴリー値）
  selectorGenerator2_factor(id = "two_way_anova", 
                            dfSpss = dfList$spss)
  
  # プルダウン3生成（数値とカテゴリー値）
  selectorGenerator3_2_number_factor(id = "two_way_anova", 
                            dfSpss = dfList$spss)
  
  # 変数の型のテキストを生成（カテゴリー値）
  typeOfVariableGenerator_factor(id = "two_way_anova", 
                                 df = dfList$spss)
  
  # 変数の型2のテキストを生成（カテゴリー値）
  typeOfVariableGenerator2_factor(id = "two_way_anova", 
                                  df = dfList$spss)
  
  # 変数の型3のテキストを生成（カテゴリー値）
  typeOfVariableGenerator3_2_number_factor(id = "two_way_anova", 
                                  df = dfList$spss)
  
  # アクションボタンを生成
  two_way_anovaButtonGenerator(id = "two_way_anova", 
                               df = dfList$spss)
  
  # 二元配置分散分析結果の生成
  two_way_anovaGenerator(id = "two_way_anova", 
                               df = dfList$spss)
  
  # 表示する記述統計を選択（必要に応じて保持）
  statsSelector(id = "two_way_anova")
  
  
  ## 「三重クロス表分析」navbar  
  
  # ファイル名生成
  fileNameGenerator(id = "triple_cross",
                    spssName = dfList$spssName)
  
  # プルダウン生成（カテゴリー値）
  selectorGenerator_factor(id = "triple_cross",
                           dfSpss = dfList$spss)
  
  # プルダウン2生成（カテゴリー値）
  selectorGenerator2_factor(id = "triple_cross",
                            dfSpss = dfList$spss)
  
  # プルダウン3生成（カテゴリー値）
  selectorGenerator3_factor(id = "triple_cross",
                            dfSpss = dfList$spss)
  
  # 変数の型のテキストを生成（カテゴリー値）
  typeOfVariableGenerator_factor(id = "triple_cross", df = dfList$spss)
  
  # 変数の型2のテキストを生成（カテゴリー値）
  typeOfVariableGenerator2_factor(id = "triple_cross", df = dfList$spss)
  
  # 変数の型3のテキストを生成（カテゴリー値）
  typeOfVariableGenerator3_factor(id = "triple_cross", df = dfList$spss)
  
  # アクションボタンを生成
  triple_crossButtonGenerator(id = "triple_cross", df = dfList$spss)
  
  # 三重クロス表生成
  triple_cross_tableGenerator(id = "triple_cross",
                              df = dfList$spss,
                              dfValue = dfList$spssValue)
  
  # 表示する記述統計を選択
  statsSelector(id = "triple_cross")
  
  
  ## 「多変量解析（回帰分析）」navbar  
  
  # ファイル名生成
  fileNameGenerator(id = "multivariate",
                    spssName = dfList$spssName)
  
  # 回帰分析生成
  multivariateGenerator(id = "multivariate", 
                        finalDataFrame = dfList$final)
  
  ## 「多変量解析（因子分析・主成分分析）」navbar  
  
  # ファイル名生成
  fileNameGenerator(id = "multivariate2",
                    spssName = dfList$spssName)
  
  # 多変量解析（因子分析・主成分分析）
  multivariateGenerator2(id = "multivariate2", 
                         finalDataFrame = dfList$final)   
})


### アプリを実行

shinyApp(ui = ui, server = server)