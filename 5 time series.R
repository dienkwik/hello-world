# ==============================================================================
rm(list=ls())
setwd("~/S2-Sains-Komputasi-ITB/CASES/Heavenly Blush/Reportase")
library(dplyr)
library(tidyr)
library(factoextra)
library(parallel)
library(forecast)
library(TTR)
library(tseries)
library(TSstudio)

num_core = 7

# load data dulu
load("~/S2-Sains-Komputasi-ITB/CASES/Heavenly Blush/Data Mentah/ready.rda")

# bikin label dulu
label_bulan_tahun = c(paste(1:12,2019,sep = "-"),
                      paste(1:12,2020,sep = "-"),
                      paste(1:12,2021,sep = "-"),
                      paste(1:12,2022,sep = "-"),
                      paste(1:12,2023,sep = "-"))

# kita ambil semua datanya
path     = "~/S2-Sains-Komputasi-ITB/CASES/Heavenly Blush/Data Mentah/Pembagian Brand untuk Ikang.xlsx"
df_brand = read_excel(path) %>% janitor::clean_names() %>% rename(kategori = level_0)


# kita enrich df_all dengan data label bulan di atas
df_ts = 
  df_all |>
  mutate(timeline = paste(bulan,tahun,sep = "-")) |>
  mutate(timeline = factor(timeline,levels = label_bulan_tahun)) |>
  merge(df_brand,all.x = T) %>% 
  mutate(kategori = ifelse(is.na(kategori),
                           "BREAD",
                           kategori)) %>% 
  group_by(timeline,kategori) %>% 
  summarise(omset = sum(value_nett)) %>% 
  ungroup()

# format miliar
format_miliar = function(x){
  label = x / 1000000000
  label = round(label,2)
  label = paste0(label,"M")
  return(label)
}

# format juta
format_juta = function(x){
  label = x / 1000000
  label = round(label,2)
  label = paste0(label,"jt")
  return(label)
}
# ==============================================================================


# ==============================================================================
# kita buat object time seriesnya terlebih dahulu

df_ts$kategori %>% unique()

# Greek
ts_greek = df_ts %>% filter(kategori == "GREEK") %>% arrange(timeline) %>% .$omset
ts_greek = ts(ts_greek,start = c(2019,1), frequency = 12)

# Yo
ts_yo    = df_ts %>% filter(kategori == "YO") %>% arrange(timeline) %>% .$omset
ts_yo    = ts(ts_yo,start = c(2019,1), frequency = 12)

# Yoguruto
ts_yogur = df_ts %>% filter(kategori == "YOGURUTO") %>% arrange(timeline) %>% .$omset
ts_yogur = ts(ts_yogur,start = c(2019,1), frequency = 12)

# Tummy
ts_tummy = df_ts %>% filter(kategori == "TUMMY") %>% arrange(timeline) %>% .$omset
ts_tummy = ts(ts_tummy,start = c(2019,1), frequency = 12)

# bikin function dekomposisi
dekompos_donk = function(ts){
  decomposition_aso = stl(ts,s.window = 52)
  plt               = plot(decomposition_aso)
  dekomposisi_new   = as.data.frame(decomposition_aso$time.series)
  output            = list(plot   = plt,
                           dframe = dekomposisi_new)
  return(output)
}

# kita hitung dulu
hasil_greek = dekompos_donk(ts_greek)
hasil_yo    = dekompos_donk(ts_yo)
hasil_yogur = dekompos_donk(ts_yogur)
hasil_tummy = dekompos_donk(ts_tummy)

# kita ambil trendline nya dulu
trend = data.frame(greek    = hasil_greek$dframe$trend,
                   yo       = hasil_yo$dframe$trend,
                   yoguruto = hasil_yogur$dframe$trend,
                   tummy    = hasil_tummy$dframe$trend,
                   timeline = df_ts$timeline %>% unique() %>% .[1:53])

plt_dekom = 
  trend %>% 
  reshape2::melt(id.vars = "timeline") %>% 
  mutate(value = as.numeric(value),
         label = format_miliar(value)) %>% 
  ggplot(aes(x = timeline,
             y = value,
             color = variable,
             group = variable)) +
  geom_line(linewidth = 1.25) +
  geom_point(alpha = .5) +
  ggrepel::geom_label_repel(aes(label = label),size = 2.25,alpha = .75)+
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(n.dodge = 4)) +
  labs(title    = "Perbandingan TREN Dekomposisi Antar Kategori",
       subtitle = "Sumber data sales Jabodetabek",
       y        = "Omset",
       x        = "Timeline",
       color    = "Keterangan") +
  theme(axis.text.y   = element_blank(),
        plot.title    = element_text(size = 20),
        plot.subtitle = element_text(size = 17),
        axis.title    = element_text(size = 15),
        axis.text.x   = element_text(size = 12),
        legend.text   = element_text(size = 12),
        legend.title  = element_text(size = 14))
# ==============================================================================

# ==============================================================================
# ini PR terbaru ya
# kita akan buat grafik lengkapnya di sini untuk masing-masing kategori







# ==============================================================================
# kita bikin per area utk yang greek
# kita enrich df_all dengan data label bulan di atas
df_ts_2 = 
  df_all |>
  mutate(timeline = paste(bulan,tahun,sep = "-")) |>
  mutate(timeline = factor(timeline,levels = label_bulan_tahun)) |>
  # =======================
  # ini yang baru
  merge(df_brand,all.x = T) %>% 
  mutate(kategori = ifelse(is.na(kategori),
                           "BREAD",
                           kategori)) %>% 
  filter(kategori == "GREEK") %>% 
  # =======================
  group_by(timeline,cust_area) %>% 
  summarise(omset = sum(value_nett)) %>% 
  ungroup()

# kita buat object time seriesnya terlebih dahulu

df_ts_2$cust_area %>% unique()

# area
ts_area   = df_ts_2 %>% filter(cust_area == "E-COMMERCE") %>% arrange(timeline) %>% .$omset
ts_area_1 = ts(ts_area,start = c(2019,1), frequency = 12)

# area
ts_area   = df_ts_2 %>% filter(cust_area == "IBT") %>% arrange(timeline) %>% .$omset
ts_area_2 = ts(ts_area,start = c(2019,1), frequency = 12)

# area
ts_area   = df_ts_2 %>% filter(cust_area == "JABODETABEK") %>% arrange(timeline) %>% .$omset
ts_area_3 = ts(ts_area,start = c(2019,1), frequency = 12)

# area
ts_area   = df_ts_2 %>% filter(cust_area == "JAWA BARAT") %>% arrange(timeline) %>% .$omset
ts_area_4 = ts(ts_area,start = c(2019,1), frequency = 12)

# area
ts_area   = df_ts_2 %>% filter(cust_area == "JAWA TENGAH") %>% arrange(timeline) %>% .$omset
ts_area_5 = ts(ts_area,start = c(2019,1), frequency = 12)

# area
ts_area   = df_ts_2 %>% filter(cust_area == "JAWA TIMUR") %>% arrange(timeline) %>% .$omset
ts_area_6 = ts(ts_area,start = c(2019,1), frequency = 12)

# area
ts_area   = df_ts_2 %>% filter(cust_area == "KALIMANTAN") %>% arrange(timeline) %>% .$omset
ts_area_7 = ts(ts_area,start = c(2019,1), frequency = 12)

# area
ts_area   = df_ts_2 %>% filter(cust_area == "SULAWESI") %>% arrange(timeline) %>% .$omset
ts_area_8 = ts(ts_area,start = c(2019,1), frequency = 12)

# area
ts_area   = df_ts_2 %>% filter(cust_area == "SUMATERA 1") %>% arrange(timeline) %>% .$omset
ts_area_9 = ts(ts_area,start = c(2019,1), frequency = 12)

# area
ts_area    = df_ts_2 %>% filter(cust_area == "SUMATERA 2") %>% arrange(timeline) %>% .$omset
ts_area_10 = ts(ts_area,start = c(2019,1), frequency = 12)

# area
ts_area    = df_ts_2 %>% filter(cust_area == "SUMATERA 3") %>% arrange(timeline) %>% .$omset
ts_area_11 = ts(ts_area,start = c(2019,1), frequency = 12)



# kita hitung dulu
hasil_1   = dekompos_donk(ts_area_1)
hasil_2   = dekompos_donk(ts_area_2)
hasil_3   = dekompos_donk(ts_area_3)
hasil_4   = dekompos_donk(ts_area_4)
hasil_5   = dekompos_donk(ts_area_5)
hasil_6   = dekompos_donk(ts_area_6)
hasil_7   = dekompos_donk(ts_area_7)
hasil_8   = dekompos_donk(ts_area_8)
hasil_9   = dekompos_donk(ts_area_9)
hasil_10  = dekompos_donk(ts_area_10)
hasil_11  = dekompos_donk(ts_area_11)

df_ts_2$cust_area %>% unique()

# kita ambil trendline nya dulu
trend = data.frame(`E-COMMERCE`     = hasil_1$dframe$trend,
                   `IBT`            = hasil_2$dframe$trend,
                   `JABODETABEK`    = hasil_3$dframe$trend,
                   `JAWA BARAT`     = hasil_4$dframe$trend,
                   `JAWA TENGAH`    = hasil_5$dframe$trend,
                   `JAWA TIMUR`     = hasil_6$dframe$trend,
                   `KALIMANTAN`     = hasil_7$dframe$trend,
                   `SULAWESI`       = hasil_8$dframe$trend,
                   `SUMATERA 1`     = hasil_9$dframe$trend,
                   `SUMATERA 2`     = hasil_10$dframe$trend,
                   `SUMATERA 3`     = hasil_11$dframe$trend,
                   timeline         = df_ts_2$timeline %>% unique() %>% .[1:53])

plt_dekom_2 = 
  trend %>% 
  reshape2::melt(id.vars = "timeline") %>% 
  mutate(value = as.numeric(value),
         label = ifelse(timeline == "5-2023",
                        as.character(variable),
                        NA)) %>% 
  ggplot(aes(x = timeline,
             y = value,
             color = variable,
             group = variable)) +
  geom_line(linewidth = 1.25) +
  geom_point(alpha = .5) +
  ggrepel::geom_label_repel(aes(label = label),size = 2.25,alpha = .75) +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(n.dodge = 4)) +
  labs(title    = "Perbandingan TREN Dekomposisi Antar Area",
       subtitle = "Data sales Greek",
       y        = "Omset",
       x        = "Timeline",
       color    = "Keterangan") +
  theme(axis.text.y     = element_blank(),
        plot.title      = element_text(size = 20),
        plot.subtitle   = element_text(size = 17),
        axis.title      = element_text(size = 15),
        axis.text.x     = element_text(size = 12),
        legend.position = "none")


save(plt_dekom,plt_dekom_2,
     file = "timeseries.rda")