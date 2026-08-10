# ===========================================================
# SAMA Banking Sector — Time Series Analysis (1963-2025)
# تحليل سلاسل زمنية لبيانات القطاع المصرفي — البنك المركزي السعودي
# ===========================================================


library(rvest)   # مكتبة قراءة صفحات HTML
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

page <- read_html("SAMA_StatisticalReport_08082026083539.xls")  
tables <- html_table(page, fill = TRUE, header = TRUE)   # قراءة الجداول


annual_raw <- tables[[1]]    # اختيار الجدول السنوي تحديدا
    
      str(annual_raw)   


annual_clean <- annual_raw %>% 
  rename(item = 'اسم البند') %>% 
  mutate(across(-1, ~ na_if(., "-")))
         
      annual_clean


annual_long <- annual_clean %>% 
  pivot_longer(cols = -item, names_to = "year", values_to = "value")
       
     head(annual_long)


saveRDS(annual_long, "annual_long.rds")


unique(annual_clean$item)  #اختيار العائد على الاصول
item_list <- unique(annual_clean$item)   # تحديدالانواع
          item_list
item_list[34]   #  العائد على الاصول
target_item <- item_list[34]

roa_trend <- annual_long %>%       
  filter(item == target_item) %>%   # فلتره من غير اخطاء لغويه 
  mutate(year = as.numeric(year), value = as.numeric(value))
       head(roa_trend)
        
roa_trend %>% 
  filter(!is.na(value)) %>% 
  summarise(اول_سنة = min(year), اخر_سنة = max(year), عدد_النقاط = n())
        
roa_trend %>% filter(!is.na(value)) %>%
  ggplot(aes(x = year, y = value)) +
geom_line(color = "#1B5E3F", linewidth = 1) +
geom_point(color = "#B8862E", size = 2) +
labs(
  title = "العائد على الأصول للقطاع المصرفي السعودي عبر الزمن",
   x = "السنة", y = "العائد على الأصول (%)" ) +
theme_minimal(base_size = 13)


ggsave("roa_trend.png", plot = last_plot(), width = 9, height = 5, dpi = 300) 
#____________________________________________________

#  NBL   القروض المتعثرة

item_list[[33]]
target_item2 <- item_list[[33]]
nbl_trend <-  annual_long %>% 
  filter(item == target_item2) %>% 
  mutate(year = as.numeric(year), value = as.numeric(value))

head(nbl_trend)

nbl_trend %>% filter(!is.na(value)) %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(color = "#1B5E3F", linewidth = 1)+
  geom_point(color = "#B8862E", size = 2)+
  labs(
    title = "القروض المتعثرة — القطاع المصرفي السعودي",
    x = "السنة", y = "القروض المتعثرة (%)",
    caption = "المصدر: البنك المركزي السعودي"  ) +
  theme_minimal(base_size = 13)


ggsave("nbl_trend.png", plot = last_plot(), width = 9, height = 5, dpi = 300)
        
        
