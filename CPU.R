library(shiny)
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

#git
# Указываем URL страницы
url_p <- "https://www.tomshardware.com/news/lowest-cpu-prices"
url_3d <- "https://benchmarks.ul.com/compare/best-cpus"

# Загружаем HTML содержимое страницы
page <- read_html(url_p)

# Извлекаем все таблицы на странице
tables <- page %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

# Теперь нужно очистить и объединить таблицы

# Применим очистку для всех таблиц (каждая таблица должна содержать 3 столбца)
cleaned_tables <- lapply(tables, function(tbl) {
  tbl %>%
    # Убираем лишние строки и столбцы
    filter(!is.na(`CPU Model`)) %>%
    select(`CPU Model`, `Best US Price`, `Lowest-Ever US Price`) %>%
    # Преобразуем в более удобный формат
    mutate(
      `Best US Price` = gsub("\\$|,", "", `Best US Price`), # Убираем символы доллара и запятые
      `Best US Price` = as.numeric(`Best US Price`), # Преобразуем в числовой формат
      `Lowest-Ever US Price` = gsub("\\$|,", "", `Lowest-Ever US Price`),
      `Lowest-Ever US Price` = as.numeric(`Lowest-Ever US Price`) # Преобразуем в числовой формат
    ) %>%
    rename(`Price` = `Best US Price`)  # Переименовываем столбец
})

# Объединяем все таблицы в одну
price_cpu <- bind_rows(cleaned_tables)

# Сохраняем итоговую таблицу в файл (например, CSV)
write.csv(price_cpu, "cpu_prices.csv", row.names = FALSE)





# Чтение HTML-страницы
page <- read_html(url_3d)

# Извлечение таблицы
table <- page %>%
  html_node("table") %>%
  html_table()

# Очистка и форматирование данных
clean_table_cpu <- table %>% mutate(across(where(is.character), ~na_if(., "n/a"))) # Замена "n/a" на NA


clean_table_cpu <- clean_table_cpu %>% rename(
  `CPU Model` = `Device`,
  `3DMark CPU` = `3DMark CPU Profile Max threads score`,
  `Price` = `MSRP Price`)


clean_table_cpu <- clean_table_cpu %>% mutate(
  `Price` = as.numeric(gsub("\\$|,", "", `Price`)),
  `Value for Money` = as.numeric(`Value for Money`))


clean_table_cpu <- clean_table_cpu %>% mutate(
  `CPU Model` = str_replace_all(`CPU Model`, "\\s+", " "),  # Заменяем все пробельные символы на один пробел
  `CPU Model` = str_replace(`CPU Model`, "DirectX 12.0", ""),  # Удаляем фразу "DirectX 12.0"
  `CPU Model` = str_trim(`CPU Model`))


clean_table_cpu <- clean_table_cpu %>%
  mutate(`CPU Model` = str_squish(str_replace(`CPU Model`, "processor", "")))


clean_table_cpu <- clean_table_cpu %>%
  mutate(`CPU Model` = str_squish(str_replace(`CPU Model`, "Processor", "")))

clean_table_cpu <- clean_table_cpu %>%
  mutate(`CPU Model` = str_replace(`CPU Model`, "(i\\d) (\\d+)", "\\1-\\2"))


clean_table_cpu <- clean_table_cpu %>%
  mutate(`CPU Model` = str_replace_all(`CPU Model`, "\\?", ""))

# Обновление цены
clean_table_cpu <- clean_table_cpu %>%
  mutate(
    `Price` = ifelse(
      !is.na(price_cpu$`Price`[match(`CPU Model`, price_cpu$`CPU Model`)]),  
      price_cpu$`Price`[match(`CPU Model`, price_cpu$`CPU Model`)],  
      `Price`  
    )
  )

clean_table_cpu <- clean_table_cpu %>%
  filter(!str_detect(`CPU Model`, "Threadripper"))


clean_table_cpu <- clean_table_cpu %>%
  select(-c("Rank", "16 threads", "8 threads", "4 threads", "2 threads", "Single thread"))

colnames(clean_table_cpu)


#Фультруем только те где есть цена
clean_table_cpu <- clean_table_cpu %>% filter(!is.na(`Price`)) 


#Убираем все NA
clean_table_cpu <- clean_table_cpu %>%
  mutate_all(~replace(., is.na(.), 0))


# Добавляем новый столбец "CPU Brand"
clean_table_cpu$`CPU Brand` <- ifelse(grepl("AMD", clean_table_cpu$`CPU Model`), "AMD", 
                                      ifelse(grepl("Intel", clean_table_cpu$`CPU Model`), "Intel", NA))


# диаппазоны цен
clean_table_cpu <- clean_table_cpu %>%
  mutate(range = case_when(
    Price  < 200 ~ "100-200",
    Price >= 200 & Price < 300 ~ "200-300",
    Price >= 300 & Price < 400 ~ "300-400",
    Price >= 400 & Price < 500 ~ "400-500",
    Price >= 500 & Price < 600 ~ "500-600",
    Price >= 600 & Price < 700 ~ "600-700",
    Price >= 700 & Price < 800 ~ "700-800",
    Price >= 800 & Price < 900 ~ "800-900",
    Price >= 900 & Price < 2000 ~ "900-1000",
    TRUE ~ NA_character_  # для случаев с NA или отсутствием цены
  ))


all_data <- clean_table_cpu


# UI часть Shiny-приложения
ui <- fluidPage(
  titlePanel("Best CPUs Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceRange", 
                  "Select Price Range (USD):", 
                  min = 0,  # Минимальная цена
                  max = round(max(all_data$Price), -2),  # Максимальная цена из данных
                  value = c(100, 500),  # Диапазон по умолчанию от 100 до 500
                  step = 50,  # Шаг слайдера
                  pre = "$", 
                  sep = ",", 
                  animate = TRUE),
      helpText("Choose a price range to filter the CPUs.")
    ),
    
    mainPanel(
      plotOutput("cpu_plot"),
      tableOutput("highlighted_points")  # Добавляем вывод таблицы
    )
  )
)

# Серверная часть Shiny-приложения
server <- function(input, output) {
  
  # Реактивная функция для фильтрации данных по диапазону цен
  filtered_data <- reactive({
    data <- all_data  # Все данные из датафрейма
    data %>%
      filter(Price >= input$priceRange[1], Price <= input$priceRange[2])  # Фильтруем процессоры по диапазону цен
  })
  
  # Обрабатываем и находим выделенные точки
  highlighted_points <- reactive({
    data <- filtered_data()  # Получаем отфильтрованные данные
    
    # Находим процессоры Intel с максимальным 3DMark
    Intel <- data %>%
      filter(str_detect(`CPU Model`, "Intel")) %>%
      arrange(desc(`3DMark CPU`)) %>%
      slice(1) %>%
      mutate(type = "Intel")
    
    # Находим процессоры AMD с максимальным 3DMark
    AMD <- data %>%
      filter(str_detect(`CPU Model`, "AMD")) %>%
      arrange(desc(`3DMark CPU`)) %>%
      slice(1) %>%
      mutate(type = "AMD")
    
    # Находим процессор с наилучшей ценой за производительность
    Value_for_Money <- data %>%
      arrange(desc(`Value for Money`)) %>%
      slice(1) %>%
      mutate(type = "Value for Money")
    
    # Находим самый популярный процессор
    Popularity <- data %>%
      arrange(desc(`Popularity`)) %>%
      slice(1) %>%
      mutate(type = "Popularity")
    
    # Собираем все точки для отображения на графике
    points <- rbind(Intel, AMD, Value_for_Money, Popularity)
    
    return(points)
  })
  
  # Отображаем график с выделенными точками
  output$cpu_plot <- renderPlot({
    data <- filtered_data()  # Получаем отфильтрованные данные
    points <- highlighted_points()  # Получаем выделенные точки
    
    # Строим график
    ggplot(data, aes(x = `3DMark CPU`, y = `Price`)) +
      geom_point(aes(color = "Other"), size = 1) +  # Все точки (будет отображаться, но не попадет в легенду)
      geom_point(data = points, aes(color = type), size = 5) +  # Выделяем 4 точки, используя переменную type
      scale_color_manual(
        values = c("Intel" = "blue", "AMD" = "red", 
                   "Value for Money" = "green", "Popularity" = "orange", 
                   "Other" = "grey"),
        limits = c("Intel", "AMD", "Value for Money", "Popularity"),  # Ограничиваем легенду только нужными категориями
        labels = c("Intel CPUs", "AMD CPUs", "Best Value for Money", "Most Popular CPU")  # Изменяем подписи для легенды
      ) + 
      geom_text(data = points, aes(label = `CPU Model`), vjust = -1, hjust = 1) +  # Подписи для выделенных точек
      labs(title = "3DMark CPU vs MSRP Price", x = "3DMark CPU", y = "Price", color = "CPU Type") +  # Легенда
      theme_minimal() +
      theme(legend.position = "top")  # Это настройка может помочь в улучшении отображения легенды
  })
  
  # Отображаем таблицу с выделенными точками
  output$highlighted_points <- renderTable({
    highlighted_points() %>%  # Получаем выделенные точки
      select(`CPU Model`, `Price`, `3DMark CPU`, type)  # Выбираем нужные столбцы
  })
}

# Запуск приложения
shinyApp(ui = ui, server = server)

