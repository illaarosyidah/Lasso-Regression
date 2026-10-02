# ============================================================
# Dashboard Evaluasi Model Energy Efficiency (Lasso vs Ridge vs Linear)
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)
library(glmnet)

# ------------------------------------------------------------
# 1. DATA HASIL ANALISIS
# ------------------------------------------------------------
# PASTIKAN FILE data_full.rds SUDAH ADA DI FOLDER YANG SAMA
df_full              <- readRDS("data_full.rds")
df_metrics_all       <- readRDS("metrics_all.rds")
df_coef_HL           <- readRDS("coeficient_Y1.rds")
pred_HL              <- readRDS("pred_Y1.rds")
stability_results_HL <- readRDS("stabilitasi_Y1.rds")
df_coef_CL           <- readRDS("coeficient_Y2.rds")
pred_CL              <- readRDS("pred_Y2.rds")
stability_results_CL <- readRDS("stabilitas_Y2.rds")

# ------------------------------------------------------------
# 2. HELPER & KONFIGURASI
# ------------------------------------------------------------
chr_cols <- function(df) {
  names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
}

ensure_feature <- function(df) {
  df <- as.data.frame(df)
  if (length(chr_cols(df)) == 0) df <- tibble::rownames_to_column(df, "Feature")
  df
}

first_chr <- function(df) chr_cols(df)[1]

COL_MODEL  <- if ("Model"  %in% names(df_metrics_all)) "Model"  else first_chr(df_metrics_all)
COL_TARGET <- if ("Target" %in% names(df_metrics_all)) "Target" else NA_character_

metric_choices <- intersect(c("RMSE", "MAE", "R_Squared"), names(df_metrics_all))

# Palet hijau
GREEN <- list(dark = "#1B4332", main = "#2D6A4F", mid = "#40916C",
              light = "#74C69D", pale = "#B7E4C7", bg = "#F1F8F4")

model_color_map <- function(x) {
  x <- unique(as.character(x))
  cols <- ifelse(grepl("Lasso", x, ignore.case = TRUE), GREEN$dark,
                 ifelse(grepl("Ridge", x, ignore.case = TRUE), GREEN$light, "#A7B5A0"))
  stats::setNames(cols, x)
}

# Label lengkap tiap variabel (dipakai di subtitle kartu & sumbu grafik)
feature_labels <- c(X1 = "Relative Compactness", X2 = "Surface Area", X3 = "Wall Area",
                    X4 = "Roof Area", X5 = "Overall Height", X6 = "Orientation",
                    X7 = "Glazing Area", X8 = "Glazing Area Distribution",
                    Y1 = "Heating Load", Y2 = "Cooling Load")
feat_label <- function(v) {
  ifelse(v %in% names(feature_labels), paste0(v, " - ", feature_labels[v]), v)
}

# ------------------------------------------------------------
# 2b. DATA SIMULATOR
# ------------------------------------------------------------
sim_ok  <- file.exists("train_Y1.rds") && file.exists("train_Y2.rds")
sim_fit <- NULL
train_list <- NULL

# Model untuk simulator prediksi (kartu "Hitung Prediksi"): hanya lambda optimal + OLS
pred_models <- c("Lasso"                   = "lasso_min",
                 "Ridge"                   = "ridge",
                 "Linear Regression (OLS)" = "ols")

no_data_box <- box(width = 12, status = "warning", solidHeader = TRUE,
                   title = "Data latih belum tersedia",
                   p("Simulator butuh data latih. Tambahkan dua baris ini di akhir skrip analisis lalu jalankan ulang:"),
                   tags$pre('saveRDS(train_HL, "train_Y1.rds")\nsaveRDS(train_CL, "train_Y2.rds")'),
                   p("Letakkan kedua file tersebut satu folder dengan app.R."))

if (sim_ok) {
  train_list <- list(Y1 = readRDS("train_Y1.rds"), Y2 = readRDS("train_Y2.rds"))
  
  x_cols <- setdiff(names(train_list$Y1)[vapply(train_list$Y1, is.numeric, logical(1))], c("Y1", "Y2"))
  x_mat_list <- lapply(train_list, function(tr) as.matrix(tr[, x_cols]))
  
  # --- TOTAL DATA UNTUK INPUT SIMULATOR ---
  if (file.exists("data_lengkap.rds")) {
    df_lengkap <- readRDS("data_lengkap.rds")
    x_all <- unique(as.matrix(df_lengkap[, x_cols]))
  } else {
    x_all <- unique(rbind(x_mat_list$Y1, x_mat_list$Y2))
  }
  # -----------------------------------------------
  
  ols_fit <- function(x, y) lm.fit(cbind(1, x), y)$coefficients
  
  ols_cv_rmse <- function(x, y, fold_id) {
    mse <- vapply(1:5, function(k) {
      tr <- fold_id != k
      b  <- ols_fit(x[tr, , drop = FALSE], y[tr])
      b[is.na(b)] <- 0
      mean((y[!tr] - cbind(1, x[!tr, , drop = FALSE]) %*% b)^2)
    }, numeric(1))
    sqrt(mean(mse))
  }
  
  fit_sim <- function(target) {
    x <- x_mat_list[[target]]
    y <- train_list[[target]][[target]]
    set.seed(123)
    fold_id <- sample(rep(1:5, length.out = nrow(x)))
    ob <- ols_fit(x, y)
    names(ob) <- c("(Intercept)", x_cols)
    list(lasso    = cv.glmnet(x, y, alpha = 1, foldid = fold_id),
         ridge    = cv.glmnet(x, y, alpha = 0, foldid = fold_id),
         ols_coef = ob,
         ols_cv   = ols_cv_rmse(x, y, fold_id),
         x_sd     = apply(x, 2, sd))
  }
  sim_fit <- list(Y1 = fit_sim("Y1"), Y2 = fit_sim("Y2"))
  
  cv_rmse_at <- function(cvo, sel) sqrt(cvo$cvm[match(cvo[[sel]], cvo$lambda)])
  
  rmse_one <- function(fits, model) {
    switch(model,
           lasso_min = cv_rmse_at(fits$lasso, "lambda.min"),
           ridge     = cv_rmse_at(fits$ridge, "lambda.min"),
           ols       = fits$ols_cv)
  }
  
  predict_one <- function(fits, model, x) {
    nx <- matrix(x, nrow = 1, dimnames = list(NULL, x_cols))
    switch(model,
           lasso_min = as.numeric(predict(fits$lasso, newx = nx, s = "lambda.min")),
           ridge     = as.numeric(predict(fits$ridge, newx = nx, s = "lambda.min")),
           ols       = { b <- fits$ols_coef; b[is.na(b)] <- 0; sum(b * c(1, x)) })
  }
  
  pred_inputs <- lapply(seq_along(x_cols), function(i) {
    vals <- sort(unique(x_all[, i]))
    med  <- median(x_all[, i])
    ctl <- if (length(vals) <= 15) {
      selectInput(paste0("pin_", i), feat_label(x_cols[i]),
                  choices  = as.character(vals),
                  selected = as.character(vals[which.min(abs(vals - med))]))
    } else {
      numericInput(paste0("pin_", i), feat_label(x_cols[i]), value = round(med, 3))
    }
    column(6, ctl)
  })
}

# ------------------------------------------------------------
# 2c. KOMPONEN TAMPILAN & PENGATURAN DIAGNOSTIK
# ------------------------------------------------------------
# Kartu statistik: angka selalu putih agar terbaca di semua warna kartu
# (penyebab kartu "Jumlah Total Data" tampak kosong di versi lama: angka berwarna
# hijau tua di atas kartu hijau tua yang sama).
stat_card <- function(value, label, icon_name, kind = "green") {
  div(class = paste("stat-card", paste0("stat-", kind)),
      div(class = "stat-value", as.character(value)),
      div(class = "stat-label", label),
      div(class = "stat-icon", icon(icon_name)))
}

# Judul kotak menu di bagian atas (ikon + judul + keterangan singkat)
tab_title <- function(icon_name, title, desc) {
  tagList(span(class = "tab-title", icon(icon_name), " ", title),
          span(class = "tab-desc", desc))
}

# Ambang "error besar" pada grafik diagnostik: |residual| > K_SD x standar deviasi residual
K_SD <- 2

# CSS aplikasi (token {dark}, {main}, dst. diganti dengan warna palet hijau)
app_css <- "
  /* ===== Header & latar ===== */
  .skin-green .main-header .navbar { background-color: {dark}; }
  .skin-green .main-header .logo { background-color: #081C15; color: #ffffff; font-family: Arial, sans-serif; }
  .skin-green .main-header .logo:hover { background-color: #081C15; }
  .content-wrapper, .right-side { background-color: {bg}; }
  .content-wrapper h3 { color: {dark}; }
  .section-title { font-weight: 700; margin: 4px 0 12px 0; }

  /* ===== Kotak (box): satu gaya seragam ===== */
  .box { border-radius: 8px; box-shadow: 0 1px 3px rgba(27, 67, 50, 0.12); }
  .box.box-primary, .box.box-success, .box.box-info, .box.box-warning, .box.box-danger { border-top-color: {main}; }
  .box.box-solid.box-primary, .box.box-solid.box-success, .box.box-solid.box-info,
  .box.box-solid.box-warning { border: 1px solid {main}; }
  .box.box-solid.box-primary > .box-header, .box.box-solid.box-success > .box-header,
  .box.box-solid.box-info > .box-header, .box.box-solid.box-warning > .box-header {
    background: {main}; background-color: {main}; color: #ffffff;
  }
  .box.box-solid.box-danger { border: 1px solid {dark}; }
  .box.box-solid.box-danger > .box-header { background: {dark}; background-color: {dark}; color: #ffffff; }
  .box.box-solid > .box-header .box-title { color: #ffffff !important; font-weight: 600; font-size: 16px; }
  table.dataTable thead th { color: {dark}; }

  /* ===== Kartu statistik ===== */
  .stat-grid-top  { display: grid; grid-template-columns: repeat(2, 1fr); gap: 12px; margin-bottom: 12px; }
  .stat-grid-x    { display: grid; grid-template-columns: repeat(4, 1fr); gap: 12px; margin-bottom: 20px; }
  .stat-grid-sim  { display: grid; grid-template-columns: repeat(4, 1fr); gap: 12px; margin-bottom: 20px; }
  .stat-card { position: relative; overflow: hidden; border-radius: 8px; padding: 14px 16px;
               min-height: 96px; color: #ffffff; box-shadow: 0 1px 3px rgba(27, 67, 50, 0.18); }
  .stat-card .stat-value { font-size: 30px; font-weight: 700; line-height: 1.1; color: #ffffff; }
  .stat-card .stat-label { margin-top: 6px; font-size: 13px; color: rgba(255, 255, 255, 0.92);
                           position: relative; z-index: 1; padding-right: 36px; }
  .stat-card .stat-icon  { position: absolute; right: 12px; bottom: 6px; font-size: 42px;
                           color: rgba(255, 255, 255, 0.18); }
  .stat-navy  { background-color: {dark}; }
  .stat-red   { background-color: #C1121F; }
  .stat-green { background-color: {main}; }
  .stat-olive { background-color: {mid}; }
  .stat-teal  { background-color: #52796F; }
  .small-box h3 { color: #ffffff !important; }
  @media (max-width: 992px) {
    .stat-grid-x, .stat-grid-sim { grid-template-columns: repeat(2, 1fr); }
  }
  @media (max-width: 576px) {
    .stat-grid-top { grid-template-columns: 1fr; }
  }

  /* ===== Menu berupa kotak di bagian atas ===== */
  .tab-boxes .nav-pills { display: flex; gap: 12px; margin-bottom: 18px; }
  .tab-boxes .nav-pills:before, .tab-boxes .nav-pills:after { display: none; }
  .tab-boxes .nav-pills > li { float: none; flex: 1 1 0; margin: 0 !important; }
  .tab-boxes .nav-pills > li > a {
    height: 100%; background: #ffffff; color: {dark}; text-align: center; padding: 12px 14px;
    border: 1px solid {pale}; border-top: 4px solid {light}; border-radius: 8px;
    box-shadow: 0 1px 3px rgba(27, 67, 50, 0.12);
  }
  .tab-boxes .nav-pills > li > a:hover { background: {pale}; color: {dark}; }
  .tab-boxes .nav-pills > li.active > a,
  .tab-boxes .nav-pills > li.active > a:hover,
  .tab-boxes .nav-pills > li.active > a:focus {
    background: {main}; color: #ffffff; border-color: {main}; border-top-color: {dark};
  }
  .tab-boxes .tab-title { display: block; font-weight: 700; font-size: 16px; }
  .tab-boxes .tab-desc  { display: block; font-size: 12px; opacity: 0.85; margin-top: 2px; }
  @media (max-width: 768px) { .tab-boxes .nav-pills { flex-direction: column; } }

  /* ===== Header terpusat ===== */
  .app-header-banner {
    background: linear-gradient(135deg, {dark} 0%, {main} 100%);
    color: #ffffff; text-align: center; border-radius: 8px;
    padding: 22px 20px 18px 20px; margin-bottom: 18px;
  }
  .app-header-banner h1 { margin: 0; font-weight: 700; letter-spacing: 1px; font-size: 30px; }
  .app-header-banner h4 { margin: 4px 0 14px 0; font-weight: 400; color: {pale}; }
  .app-header-banner .form-group, .app-header-banner label { color: #ffffff !important; }
  .app-header-banner .selectize-input { color: #222222; }

  /* ===== Tombol, catatan, penjelasan ===== */
  .btn-hitung { background-color: {mid}; color: white; font-weight: bold; font-size: 16px; border: none; padding: 10px; margin-top: 15px; }
  .btn-hitung:hover { background-color: {main}; color: white; }
  .btn-lam { background-color: {mid}; color: #ffffff; border: none; margin-right: 8px; }
  .btn-lam:hover, .btn-lam:focus { background-color: {main}; color: #ffffff; }
  .lam-readout { font-weight: 700; color: {dark}; margin: 2px 0 12px 0; }
  .note-text { color: #52796F; font-size: 12.5px; margin-top: 8px; }
  .explain-list { padding-left: 18px; margin-bottom: 12px; }
  .explain-list li { margin-bottom: 6px; }
"
for (nm in names(GREEN)) {
  app_css <- gsub(paste0("{", nm, "}"), GREEN[[nm]], app_css, fixed = TRUE)
}

# ------------------------------------------------------------
# 3. UI
# ------------------------------------------------------------
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Evaluasi Model Energy", titleWidth = 300),
  
  # Sidebar dimatikan: menu pindah ke kotak-kotak di bagian atas halaman
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$head(tags$style(HTML(app_css))),
    
    # ---- Header terpusat: judul besar + subjudul per tab + filter target ----
    div(class = "app-header-banner",
        h1("ENERGY EFFICIENCY"),
        h4(textOutput("subtitle_main", inline = TRUE)),
        fluidRow(
          column(4, offset = 4,
                 selectInput("target_var", NULL,
                             choices = c("Heating Load (Y1)" = "Y1",
                                         "Cooling Load (Y2)" = "Y2"),
                             width = "100%"))
        )
    ),
    
    # ---- Menu berupa kotak di atas; urutan pertama: Diagnostik dan Deskripsi Data ----
    div(class = "tab-boxes",
        tabsetPanel(
          id = "tabs", type = "pills",
          
          # ---------------- TAB 1: DIAGNOSTIK DAN DESKRIPSI DATA ----------------
          tabPanel(
            title = tab_title("chart-line", "Diagnostik dan Deskripsi Data", "Ringkasan data & kualitas error model"),
            value = "diagnostics",
            
            h3("Statistik Deskriptif Data Keseluruhan", class = "section-title"),
            uiOutput("stat_cards"),
            fluidRow(
              box(width = 12, title = "Distribusi Variabel (Boxplot)", status = "primary", solidHeader = TRUE,
                  plotlyOutput("plot_desc_box"))
            ),
            
            h3("Evaluasi dan Diagnostik Model", class = "section-title"),
            fluidRow(
              box(width = 6, title = "Actual vs Predicted", status = "primary", solidHeader = TRUE,
                  plotlyOutput("plot_actual")),
              box(width = 6, title = "Residual vs Predicted", status = "success", solidHeader = TRUE,
                  plotlyOutput("plot_residual"))
            ),
            fluidRow(
              box(width = 6, title = "Distribusi Residual", status = "primary", solidHeader = TRUE,
                  plotlyOutput("plot_hist")),
              box(width = 6, title = "QQ-Plot Residual", status = "success", solidHeader = TRUE,
                  plotOutput("plot_qq"))
            )
          ),
          
          # ---------------- TAB 2: BENCHMARKING MODEL ----------------
          tabPanel(
            title = tab_title("chart-bar", "Benchmarking Model", "Lasso vs Ridge vs Linear"),
            value = "metrics",
            
            fluidRow(
              box(width = 12, title = "Perbandingan Model", status = "primary", solidHeader = TRUE,
                  radioButtons("metric_pick", "Pilih metrik:", choices = metric_choices, inline = TRUE),
                  plotlyOutput("plot_metrics", height = "360px"))
            ),
            fluidRow(
              box(width = 12, title = "Tabel Metrik Kinerja", status = "primary", solidHeader = TRUE,
                  DTOutput("table_metrics"))
            ),
            fluidRow(
              box(width = 7, title = "Tabel Koefisien Regresi", status = "success", solidHeader = TRUE,
                  DTOutput("table_coef")),
              box(width = 5, title = "Stabilitas Seleksi Fitur Lasso (%)", status = "info", solidHeader = TRUE,
                  DTOutput("table_stability"))
            )
          ),
          
          # ---------------- TAB 3: SIMULATOR ----------------
          tabPanel(
            title = tab_title("sliders-h", "Simulator", "Prediksi beban & uji penalti λ"),
            value = "simulator",
            
            if (!sim_ok) {
              no_data_box
            } else {
              tagList(
                h3("Simulator Prediksi Beban Energi", class = "section-title"),
                fluidRow(
                  box(width = 5, title = "Desain Bangunan", status = "info", solidHeader = TRUE,
                      do.call(fluidRow, pred_inputs),
                      actionButton("btn_predict", "Hitung Prediksi", class = "btn-hitung", width = "100%", icon = icon("calculator"))
                  ),
                  column(width = 7,
                         fluidRow(valueBoxOutput("vb_pred", width = 12)),
                         box(width = 12, title = "Prediksi dan Rentang dari Semua Model", status = "primary", solidHeader = TRUE,
                             plotlyOutput("plot_pred_range", height = "320px"),
                             br(),
                             DTOutput("table_pred"),
                             p(style = "margin-top:8px;",
                               "Rentang = prediksi ± 1 × RMSE cross-validation pada data latih. Ini gambaran kasar besar error, ",
                               "bukan interval kepercayaan statistik.")
                         )
                  )
                ),
                fluidRow(
                  box(width = 12, status = "danger", solidHeader = TRUE, title = "Yang Tidak Boleh Disimpulkan",
                      tags$ul(
                        tags$li("Prediksi hanya layak dipercaya untuk konfigurasi bangunan yang mirip dengan data latih."),
                        tags$li("Data berasal dari simulasi, bukan pengukuran bangunan nyata, sehingga hasilnya tidak otomatis berlaku untuk bangunan sesungguhnya."),
                        tags$li("Koefisien menunjukkan asosiasi dalam data, bukan hubungan sebab-akibat. Fitur yang dinolkan Lasso belum tentu tidak berpengaruh; bisa jadi ia hanya redundan dengan fitur lain yang berkorelasi tinggi."),
                        tags$li("Rentang ± RMSE adalah gambaran rata-rata. Pada kasus tertentu error bisa jauh lebih besar (lihat pola residual di tab Diagnostik).")
                      ))
                ),
                
                hr(),
                
                # ===== Simulator uji penalti lambda: Lasso vs Ridge (di bawah simulator prediksi) =====
                h3("Simulator Uji Penalti (λ) - Pengaruhnya ke Lasso dan Ridge", class = "section-title"),
                fluidRow(
                  box(width = 12, status = "info", solidHeader = TRUE, title = "Atur Kekuatan Penalti",
                      p("Geser nilai penalti (λ, di sebagian referensi disebut alpha). ",
                        "Lasso dan Ridge langsung dihitung ulang dan dibandingkan di bawah ini."),
                      uiOutput("ui_lambda"),
                      div(class = "lam-readout", textOutput("txt_lambda")),
                      actionButton("btn_lasso_opt", "Pakai λ optimal Lasso", icon = icon("bullseye"), class = "btn-lam"),
                      actionButton("btn_ridge_opt", "Pakai λ optimal Ridge", icon = icon("bullseye"), class = "btn-lam"))
                ),
                
                uiOutput("sim_cards"),
                
                fluidRow(
                  box(width = 7, title = "Perbandingan Lasso vs Ridge pada λ Terpilih", status = "primary", solidHeader = TRUE,
                      uiOutput("txt_l1l2"),
                      DTOutput("table_cmp"),
                      div(class = "note-text", textOutput("txt_ols_note"))),
                  box(width = 5, title = "Error Model (RMSE CV) vs λ", status = "primary", solidHeader = TRUE,
                      plotlyOutput("plot_cv", height = "340px"),
                      div(class = "note-text",
                          "Garis merah = λ terpilih. Garis putus-putus oranye = RMSE OLS (tanpa penalti)."))
                ),
                
                fluidRow(
                  box(width = 12, status = "info", solidHeader = TRUE, title = "Cara Membaca",
                      tags$ul(class = "explain-list",
                              tags$li(tags$b("Geser λ ke kanan: "), "penalti makin kuat, koefisien menyusut. Geser ke kiri: mendekati OLS (tanpa penalti)."),
                              tags$li(tags$b("Lasso (L1) "), "bisa menolkan koefisien sehingga menyeleksi fitur (sel merah muda = fitur dibuang). ",
                                      tags$b("Ridge (L2) "), "hanya mengecilkan koefisien, tidak pernah tepat 0."),
                              tags$li(tags$b("Grafik error: "), "error terendah ada di λ sedang. λ terlalu besar membuat error naik (underfitting). ",
                                      "Tombol \"λ optimal\" memindahkan slider ke titik error CV terendah tiap model."),
                              tags$li("Koefisien dibuat terstandarisasi (dikali SD fitur) agar pengaruh antarfitur bisa dibandingkan langsung.")
                      ))
                )
              )
            }
          )
        )
    )
  )
)

# ------------------------------------------------------------
# 4. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  target_label <- reactive(ifelse(input$target_var == "Y1", "Heating Load", "Cooling Load"))
  target_full  <- reactive(ifelse(input$target_var == "Y1", "Heating Load (Y1)", "Cooling Load (Y2)"))
  
  # Subjudul di header terpusat, mengikuti tab yang sedang aktif
  output$subtitle_main <- renderText({
    switch(input$tabs,
           metrics     = "Benchmarking Lasso, Ridge & Linear Regression",
           diagnostics = "Deskripsi Data & Diagnostik Error Model",
           simulator   = "Simulator Prediksi Beban & Uji Penalti Lambda",
           "")
  })
  
  metrics_sel <- reactive({
    d <- df_metrics_all
    if (!is.na(COL_TARGET)) {
      keep <- grepl(input$target_var, d[[COL_TARGET]], fixed = TRUE)
      if (any(keep)) d <- d[keep, , drop = FALSE]
    }
    d
  })
  coef_sel <- reactive(ensure_feature(if (input$target_var == "Y1") df_coef_HL else df_coef_CL))
  stab_sel <- reactive(ensure_feature(if (input$target_var == "Y1") stability_results_HL else stability_results_CL))
  
  # ===================== TAB 1: BENCHMARKING =====================
  output$plot_metrics <- renderPlotly({
    req(input$metric_pick)
    d <- metrics_sel()
    p <- ggplot(d, aes(x = .data[[COL_MODEL]], y = .data[[input$metric_pick]], fill = .data[[COL_MODEL]])) +
      geom_col(width = 0.55) +
      scale_fill_manual(values = model_color_map(d[[COL_MODEL]])) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = input$metric_pick) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$table_metrics <- renderDT({
    d <- metrics_sel()
    if (!is.na(COL_TARGET)) d <- d[, setdiff(names(d), COL_TARGET), drop = FALSE]
    datatable(d, selection = "none", rownames = FALSE,
              options = list(dom = "t", pageLength = 6, scrollX = TRUE)) %>%
      formatRound(columns = intersect(c("RMSE", "MAE", "R_Squared"), names(d)), digits = 3)
  })
  
  output$table_coef <- renderDT({
    datatable(coef_sel(), class = "row-border hover", rownames = FALSE, filter = "none",
              options = list(pageLength = 15, dom = "t")) %>%
      formatRound(columns = c("Lasso_Coef", "Ridge_Coef", "Linear_Coef"), digits = 4)
  })
  
  output$table_stability <- renderDT({
    datatable(stab_sel(), rownames = FALSE, filter = "none",
              options = list(pageLength = 15, dom = "t")) %>%
      formatRound(columns = "stability_percent", digits = 1)
  })
  
  # ===================== TAB 1: DIAGNOSTIK DAN DESKRIPSI DATA =====================
  
  # --- STATISTIK DESKRIPTIF DATA KESELURUHAN ---
  data_desc_full <- reactive({
    req(exists("df_full"))
    if (input$target_var == "Y1") {
      df_full %>% select(X1, X2, X3, X4, X5, X6, X7, X8, Y1)
    } else {
      df_full %>% select(X1, X2, X3, X4, X5, X6, X7, X8, Y2)
    }
  })
  
  # Kartu: total data & rata-rata target (baris atas), lalu rata-rata X1-X8 (grid 4 x 2)
  output$stat_cards <- renderUI({
    d    <- data_desc_full()
    ycol <- input$target_var
    xs   <- d %>% select(-all_of(ycol))
    
    tagList(
      div(class = "stat-grid-top",
          stat_card(format(nrow(d), big.mark = "."), "Jumlah Total Data (Baris)", "database", "navy"),
          stat_card(formatC(mean(d[[ycol]], na.rm = TRUE), format = "f", digits = 2),
                    paste0("Rata-rata ", feat_label(ycol)),
                    if (ycol == "Y1") "fire" else "snowflake",
                    if (ycol == "Y1") "red" else "teal")),
      div(class = "stat-grid-x",
          lapply(names(xs), function(col_name) {
            stat_card(formatC(mean(xs[[col_name]], na.rm = TRUE), format = "f", digits = 2),
                      paste("Rata-rata", feat_label(col_name)),
                      "chart-pie", "green")
          }))
    )
  })
  
  output$plot_desc_box <- renderPlotly({
    d <- data_desc_full()
    d_melt <- pivot_longer(d, cols = everything(), names_to = "Variabel", values_to = "Nilai")
    d_melt$Variabel <- feat_label(d_melt$Variabel)
    
    p <- ggplot(d_melt, aes(x = Variabel, y = Nilai)) +
      geom_boxplot(fill = GREEN$light, color = GREEN$dark, alpha = 0.8) +
      labs(x = NULL, y = "Nilai") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% layout(margin = list(b = 110))
  })
  
  # --- DIAGNOSTIK ERROR ---
  pred_flag <- reactive({
    d <- if (input$target_var == "Y1") mutate(pred_HL, actual = Y1) else mutate(pred_CL, actual = Y2)
    cutoff <- K_SD * sd(d$residual, na.rm = TRUE)
    d %>% mutate(status = ifelse(abs(residual) > cutoff, "Error besar", "Normal"))
  })
  
  status_cols <- c("Normal" = GREEN$mid, "Error besar" = "#D62828")
  
  output$plot_actual <- renderPlotly({
    d <- pred_flag()
    p <- ggplot(d, aes(x = actual, y = .pred, color = status)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
      scale_color_manual(values = status_cols) +
      labs(x = paste0("Actual (", input$target_var, ")"), y = "Predicted", color = NULL) +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_residual <- renderPlotly({
    d <- pred_flag()
    p <- ggplot(d, aes(x = .pred, y = residual, color = status)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      scale_color_manual(values = status_cols) +
      labs(x = "Predicted", y = "Residual (Error)", color = NULL) +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_hist <- renderPlotly({
    d <- pred_flag()
    p <- ggplot(d, aes(x = residual)) +
      geom_histogram(bins = 30, fill = GREEN$mid, color = "white") +
      geom_vline(xintercept = 0, linetype = "dashed") +
      labs(x = "Residual", y = "Frekuensi") +
      theme_minimal(base_size = 14)
    ggplotly(p)
  })
  
  output$plot_qq <- renderPlot({
    d <- pred_flag()
    ggplot(d, aes(sample = residual)) +
      stat_qq(color = GREEN$mid, alpha = 0.7, size = 2) +
      stat_qq_line(color = "#D62828", linewidth = 1) +
      labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +
      theme_minimal(base_size = 14)
  })
  
  # ===================== TAB 3A: SIMULATOR PREDIKSI BEBAN =====================
  pred_x <- eventReactive(input$btn_predict, {
    req(sim_ok)
    raw <- lapply(seq_along(x_cols), function(i) input[[paste0("pin_", i)]])
    req(!any(vapply(raw, is.null, logical(1))))
    v <- as.numeric(unlist(raw))
    req(!anyNA(v), length(v) == length(x_cols))
    names(v) <- x_cols
    v
  }, ignoreNULL = FALSE)
  
  pred_all <- reactive({
    v <- pred_x()
    k <- 1
    t <- input$target_var
    fits <- sim_fit[[t]]
    tl <- if (t == "Y1") "Heating Load (Y1)" else "Cooling Load (Y2)"
    d <- do.call(rbind, lapply(seq_along(pred_models), function(j) {
      m <- unname(pred_models[j])
      p <- predict_one(fits, m, v)
      r <- rmse_one(fits, m)
      data.frame(Target = tl, Model = names(pred_models)[j], Prediksi = p, RMSE_CV = r,
                 Bawah = p - k * r, Atas = p + k * r, stringsAsFactors = FALSE)
    }))
    d$Model <- factor(d$Model, levels = names(pred_models))
    d
  })
  
  pred_primary <- reactive({
    m <- names(pred_models)[pred_models == "lasso_min"]
    pred_all() %>% filter(Model == m)
  })
  
  output$vb_pred <- renderValueBox({
    d <- pred_primary()
    valueBox(sprintf("%.1f", d$Prediksi),
             sprintf("%s  |  rentang %.1f - %.1f", target_full(), d$Bawah, d$Atas),
             icon = icon(if (input$target_var == "Y1") "fire" else "snowflake"),
             color = "green")
  })
  
  output$plot_pred_range <- renderPlotly({
    d <- pred_all()
    p <- ggplot(d, aes(x = Model, y = Prediksi, ymin = Bawah, ymax = Atas, color = Model)) +
      geom_pointrange(linewidth = 0.8) +
      scale_color_manual(values = setNames(c(GREEN$dark, GREEN$mid, "#8D99AE"),
                                           names(pred_models))) +
      labs(x = NULL, y = paste("Prediksi", target_full())) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$table_pred <- renderDT({
    d <- pred_all() %>% select(-Target)
    datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 10)) %>%
      formatRound(columns = c("Prediksi", "RMSE_CV", "Bawah", "Atas"), digits = 2)
  })
  
  # ===================== TAB 3B: SIMULATOR UJI PENALTI (LAMBDA) =====================
  # Satu slider λ berlaku untuk Lasso DAN Ridge sekaligus, jadi perbandingan langsung muncul.
  sim <- reactive({
    req(sim_ok)
    sim_fit[[input$target_var]]
  })
  
  # Rentang log10(λ) gabungan Lasso & Ridge
  lam_range <- reactive({
    s <- sim()
    r <- log10(range(c(s$lasso$lambda, s$ridge$lambda)))
    c(floor(r[1] * 100) / 100, ceiling(r[2] * 100) / 100)
  })
  
  output$ui_lambda <- renderUI({
    r <- lam_range()
    sliderInput("log_lambda", "log10(λ)  -  geser ke kanan = penalti makin kuat",
                min = r[1], max = r[2],
                value = round(log10(sim()$lasso$lambda.min), 2),
                step = 0.01, width = "100%")
  })
  
  lam <- reactive({
    req(input$log_lambda)
    10^input$log_lambda
  })
  
  observeEvent(input$btn_lasso_opt, {
    updateSliderInput(session, "log_lambda", value = round(log10(sim()$lasso$lambda.min), 2))
  })
  observeEvent(input$btn_ridge_opt, {
    updateSliderInput(session, "log_lambda", value = round(log10(sim()$ridge$lambda.min), 2))
  })
  
  output$txt_lambda <- renderText({
    sprintf("λ terpilih (%s) = %.5f", target_full(), lam())
  })
  
  # Koefisien terstandarisasi (dikali SD fitur) pada satu atau banyak nilai λ
  coef_std <- function(cvo, lambda_val, sdv) {
    b <- as.matrix(coef(cvo$glmnet.fit, s = lambda_val))[-1, , drop = FALSE]
    b * sdv
  }
  
  # RMSE cross-validation pada λ tertentu (interpolasi di sepanjang kurva CV)
  cv_rmse_interp <- function(cvo, log_lam) {
    sqrt(approx(log(cvo$lambda), cvo$cvm, xout = log_lam, rule = 2)$y)
  }
  
  rmse_now <- reactive({
    s <- sim()
    l <- log(lam())
    list(lasso      = cv_rmse_interp(s$lasso, l),
         ridge      = cv_rmse_interp(s$ridge, l),
         ols        = s$ols_cv,
         lasso_best = sqrt(min(s$lasso$cvm)),
         ridge_best = sqrt(min(s$ridge$cvm)))
  })
  
  # Tabel perbandingan OLS / Ridge / Lasso pada λ terpilih
  cmp_tbl <- reactive({
    s <- sim()
    l <- lam()
    data.frame(Fitur = x_cols,
               OLS   = as.numeric(s$ols_coef[-1] * s$x_sd),
               Ridge = as.numeric(coef_std(s$ridge, l, s$x_sd)),
               Lasso = as.numeric(coef_std(s$lasso, l, s$x_sd)),
               stringsAsFactors = FALSE) %>%
      mutate(Status_Lasso = ifelse(abs(Lasso) > 1e-10, "Aktif", "Dinolkan"))
  })
  
  # Kartu ringkas: fitur aktif & RMSE CV (Lasso, Ridge, OLS)
  output$sim_cards <- renderUI({
    s <- sim()
    r <- rmse_now()
    n_aktif <- sum(cmp_tbl()$Status_Lasso == "Aktif")
    div(class = "stat-grid-sim",
        stat_card(paste0(n_aktif, " / ", length(x_cols)), "Fitur aktif Lasso", "filter", "green"),
        stat_card(sprintf("%.3f", r$lasso), "RMSE CV Lasso", "bullseye", "olive"),
        stat_card(sprintf("%.3f", r$ridge), "RMSE CV Ridge", "bullseye", "teal"),
        stat_card(sprintf("%.3f", r$ols), "RMSE CV OLS (tanpa penalti)", "ruler", "navy"))
  })
  
  # Penjelasan otomatis yang mengikuti λ terpilih
  output$txt_l1l2 <- renderUI({
    d <- cmp_tbl()
    r <- rmse_now()
    dropped <- d$Fitur[d$Status_Lasso == "Dinolkan"]
    
    kalimat_l1l2 <- if (length(dropped) == 0) {
      "Pada λ ini Lasso belum menolkan fitur apa pun, jadi hasilnya masih mirip Ridge. Geser λ ke kanan sampai ada koefisien Lasso yang menjadi 0."
    } else if (length(dropped) == nrow(d)) {
      "Penalti terlalu kuat: Lasso menolkan semua fitur sehingga prediksinya hanya berupa rata-rata data (underfitting)."
    } else {
      paste0("Lasso menolkan ", paste(feat_label(dropped), collapse = ", "),
             ", sedangkan Ridge hanya mengecilkan koefisiennya (tetap tidak nol). ",
             "Inilah beda L1 (seleksi fitur) dan L2 (penyusutan tanpa menolkan).")
    }
    
    status_err <- function(rm, best) {
      if (rm <= best * 1.02) {
        "mendekati error terbaiknya"
      } else {
        sprintf("%.0f%% di atas error terbaiknya, mulai underfitting", (rm / best - 1) * 100)
      }
    }
    
    tags$ul(class = "explain-list",
            tags$li(kalimat_l1l2),
            tags$li(sprintf("Lasso: RMSE CV %.3f (%s).", r$lasso, status_err(r$lasso, r$lasso_best))),
            tags$li(sprintf("Ridge: RMSE CV %.3f (%s).", r$ridge, status_err(r$ridge, r$ridge_best))))
  })
  
  output$table_cmp <- renderDT({
    d <- cmp_tbl() %>% select(-Status_Lasso)
    d$Fitur <- feat_label(d$Fitur)
    datatable(d, rownames = FALSE, selection = "none",
              options = list(dom = "t", pageLength = 10, ordering = FALSE, scrollX = TRUE)) %>%
      formatRound(columns = c("OLS", "Ridge", "Lasso"), digits = 3) %>%
      formatStyle("Lasso",
                  backgroundColor = styleInterval(c(-1e-10, 1e-10),
                                                  c("transparent", "#F8D7DA", "transparent")))
  })
  
  output$txt_ols_note <- renderText({
    o <- sim()$ols_coef[-1]
    dasar <- paste0("Koefisien memakai skala terstandarisasi (dikali SD fitur) agar antarfitur bisa dibandingkan. ",
                    "Sel merah muda = koefisien Lasso tepat 0 (fitur dibuang).")
    if (anyNA(o)) {
      paste0(dasar, " OLS tidak bisa menaksir ", paste(names(o)[is.na(o)], collapse = ", "),
             " karena kolinear sempurna dengan fitur lain (kolom OLS dikosongkan/NA).")
    } else dasar
  })
  
  # Kurva error CV Lasso & Ridge pada satu sumbu λ
  output$plot_cv <- renderPlotly({
    req(input$log_lambda)
    s <- sim()
    r <- lam_range()
    g <- seq(r[1], r[2], length.out = 200)
    kurva <- function(cvo, nama) {
      data.frame(log10_lambda = g,
                 RMSE  = sqrt(approx(log10(cvo$lambda), cvo$cvm, xout = g, rule = 2)$y),
                 Model = nama)
    }
    d <- rbind(kurva(s$lasso, "Lasso"), kurva(s$ridge, "Ridge"))
    p <- ggplot(d, aes(x = log10_lambda, y = RMSE, color = Model)) +
      geom_line(linewidth = 0.9) +
      geom_hline(yintercept = s$ols_cv, linetype = "dashed", color = "#F4A261") +
      geom_vline(xintercept = input$log_lambda, color = "#D62828") +
      scale_color_manual(values = c(Lasso = GREEN$dark, Ridge = GREEN$light)) +
      labs(x = "log10(λ)", y = "RMSE (5-fold CV)", color = NULL) +
      theme_minimal(base_size = 13)
    ggplotly(p)
  })
  
}

shinyApp(ui, server)