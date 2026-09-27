#Memanggil library
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(plotly)

# ===================================
# 1. Masukkan Hasil analisis ke Dashboard
df_metrics_all <- readRDS("metrics_all.rds")
df_coef_HL <-  readRDS("coeficient_Y1.rds")
pred_HL <- readRDS("pred_Y1.rds")
stability_results_HL <- readRDS("stabilitasi_Y1.rds")
df_coef_CL <-  readRDS("coeficient_Y2.rds")
pred_CL <- readRDS("pred_Y2.rds")
stability_results_CL <- readRDS("stabilitas_Y2.rds")

#================================================
#2. Definisikan Ui 
ui <- dashboardPage(
  dashboardHeader(title = "Evaluasi Model Energy Efficiency", titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      # Tepat 3 Tab Utama
      menuItem("Benchmarking Model", tabName = "metrics", icon = icon("chart-bar")),
      menuItem("Seleksi dan Stabilitas Fitur", tabName = "features", icon = icon("filter")),
      menuItem("Diagnostik", tabName = "diagnostics", icon = icon("chart-line")),
      
      hr(), # Garis pemisah visual
      
      # Filter Global Interaktif untuk Tab 2 dan 3
      selectInput("target_var", "PILIH TARGET ANALISIS:",
                  choices = c("Heating Load (Y1)" = "Y1",
                              "Cooling Load (Y2)" = "Y2"))
    )
  ),
  
  dashboardBody(
    # Menyisipkan CSS Kustom
    tags$head(tags$style(HTML('
      /* Mengubah warna pita atas (Header) */
      .skin-blue .main-header .navbar { background-color: #1B263B; }
      .skin-blue .main-header .logo { background-color: #0D1B2A; color: #ffffff; font-family: "Arial", sans-serif; }
      .skin-blue .main-header .logo:hover { background-color: #0D1B2A; }
      
      /* Mengubah warna panel kiri (Sidebar) */
      .skin-blue .main-sidebar { background-color: #415A77; }
      
      /* Mengubah warna sorotan menu saat diklik */
      .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
        border-left-color: #E0E1DD;
        background-color: #1B263B;
      }
      
      /* Mengubah warna latar belakang abu-abu di konten utama */
      .content-wrapper, .right-side { background-color: #F8F9FA; }
    '))),
    
    tabItems(
      
      # TAB 1: METRIK KINERJA (Statis, menampilkan Y1 & Y2)
      tabItem(tabName = "metrics",
              h2("Benchmarking Metrik Kinerja (Lasso vs Ridge vs Linear)"),
              box(width = 12, title = "Tabel Metrik Keseluruhan (Y1 & Y2)", status = "primary",
                  dataTableOutput("table_metrics"))
      ),
      
      # TAB 2: KOEFISIEN & STABILITAS (Dinamis sesuai Dropdown)
      tabItem(tabName = "features",
              h2(textOutput("title_features")),
              fluidRow(
                box(width = 6, title = "Perbandingan Koefisien", status = "warning", 
                    dataTableOutput("table_coef")),
                box(width = 6, title = "Stabilitas Seleksi Fitur (%)", status = "success", 
                    dataTableOutput("table_stability"))
              )
      ),
      
      # TAB 3: DIAGNOSTIK ERROR (Dinamis sesuai Dropdown)
      tabItem(tabName = "diagnostics",
              h2(textOutput("title_diagnostics")),
              fluidRow(
                box(width = 6, title = "Actual vs Predicted", status = "primary", plotlyOutput("plot_actual")),
                box(width = 6, title = "Residual vs Predicted", status = "danger", plotlyOutput("plot_residual"))
              ),
              fluidRow(
                box(width = 12, title = "Top 10 Error Terbesar", status = "danger", 
                    dataTableOutput("table_top_error"))
              )
      )
    )
  )
)

# 3. DEFINISI SERVER (Logika Interaktif)
# ========================================================
server <- function(input, output) {
  
  # --- TAB 1: METRIK ---
  output$table_metrics <- renderDT({ 
    datatable(df_metrics_all, selection = 'none', options = list(pageLength = 6, scrollX = TRUE)) %>%
      formatRound(columns = c("RMSE", "R_Squared"), digits = 3)
  })
  # --- DINAMIKA JUDUL HALAMAN ---
  output$title_features <- renderText({
    paste("Analisis Koefisien & Stabilitas Lasso -", ifelse(input$target_var == "Y1", "Heating Load", "Cooling Load"))
  })
  output$title_diagnostics <- renderText({
    paste("Diagnostik Error -", ifelse(input$target_var == "Y1", "Heating Load", "Cooling Load"))
  })
  
  # --- TAB 2: KOEFISIEN & STABILITAS ---
  output$table_coef <- renderDT({
    data_tampil <- if(input$target_var == "Y1") df_coef_HL else df_coef_CL
    
    datatable(data_tampil, 
              class = 'row-border hover', # Menambah garis sel dan efek hover
              rownames = FALSE, 
              filter = 'none',  
              options = list(pageLength = 15)) %>%
      formatRound(columns = c("Lasso_Coef", "Ridge_Coef", "Linear_Coef"), digits = 4) 
  })
  
  output$table_stability <- renderDT({
    data_tampil <- if(input$target_var == "Y1") stability_results_HL else stability_results_CL
    
    # PERHATIAN: Jika masih error "stability_percent", hapus baris formatRound di bawah ini
    datatable(data_tampil, 
              rownames = FALSE, 
              filter = 'none',  
              options = list(pageLength = 15)) %>%
      formatRound(columns = "stability_percent", digits = 1) 
  })
  
  # --- TAB 3: DIAGNOSTIK ---
  output$plot_actual <- renderPlotly({
    if(input$target_var == "Y1") {
      # Simpan grafik ke dalam variabel 'p'
      p <- ggplot(pred_HL, aes(x = Y1, y = .pred)) +
        geom_point(color = "steelblue", alpha = 0.7, size = 2) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
        labs(x = "Actual (Y1)", y = "Predicted") + theme_minimal(base_size = 14)
      
      # Ubah menjadi interaktif
      ggplotly(p)
      
    } else {
      p <- ggplot(pred_CL, aes(x = Y2, y = .pred)) +
        geom_point(color = "seagreen", alpha = 0.7, size = 2) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
        labs(x = "Actual (Y2)", y = "Predicted") + theme_minimal(base_size = 14)
      
      ggplotly(p)
    }
  })
  
  output$plot_residual <- renderPlotly({
    if(input$target_var == "Y1") {
      p <- ggplot(pred_HL, aes(x = .pred, y = residual)) +
        geom_point(color = "darkorange", alpha = 0.7, size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
        labs(x = "Predicted", y = "Residual (Error)") + theme_minimal(base_size = 14)
      
      ggplotly(p)
      
    } else {
      p <- ggplot(pred_CL, aes(x = .pred, y = residual)) +
        geom_point(color = "purple", alpha = 0.7, size = 2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
        labs(x = "Predicted", y = "Residual (Error)") + theme_minimal(base_size = 14)
      
      ggplotly(p)
    }
  })
  
  output$table_top_error <- renderDT({
    if(input$target_var == "Y1") {
      data_error <- pred_HL %>% arrange(desc(abs_residual)) %>% select(Y1, .pred, residual, abs_residual) %>% head(10)
    } else {
      data_error <- pred_CL %>% arrange(desc(abs_residual)) %>% select(Y2, .pred, residual, abs_residual) %>% head(10)
    }
    
    # Membungkus dengan datatable() agar bisa diformat 3 digit
    datatable(data_error, 
              rownames = FALSE, 
              options = list(dom = 't')) %>%
      formatRound(columns = c(".pred", "residual", "abs_residual"), digits = 3)
  })
}

# Jalankan Aplikasi
shinyApp(ui, server)
