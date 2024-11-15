library(shiny)
library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(readxl)
library(RCurl)



# Получаем текущую дату
current_date <- Sys.Date()

# Генерируем список дат на последние 10 дней
dates <- seq(from = current_date, by = "-1 day", length.out = 10)

# Генерируем список ссылок на файлы в формате "дд.мм.гг"
urls <- paste0("https://www.regard.ru/api/price/regard_priceList_new.", format(dates, "%d.%m.%y"), ".xlsx")

# Проверяем, существует ли файл по каждой ссылке и выводим результат
for (url in urls) {
  if (url.exists(url)) {
    cat("Файл доступен по URL: ", url, "\n")
    # Вы можете скачать файл здесь, если хотите:
    local_file <- tempfile(fileext = ".xlsx")
    download.file(url, local_file, mode = "wb")
    break  # Как только файл найден, останавливаем проверку
  } else {
    cat("Файл не найден по URL: ", url, "\n")
  }
}


# читаем файл
regard <- read_excel(local_file, skip = 19)


# Удаляем лишние строки, оставляем процессоры
regard <- regard %>%
  select(6, 7) %>%
  filter(str_detect(.[[1]], "Процессор (AMD|Intel)")) %>%
  filter(!str_detect(.[[1]], "\\.{3,}")) %>%
  filter(!str_detect(.[[1]], "Threadripper"))

# Переименование колонок
regard <- regard %>% 
  rename(`CPU Model` = `Наименование`) %>% 
  rename(`Price_rub` = `Цена, руб.`)



# Удаляем указанные слова из столбца CPU Model
regard$`CPU Model` <- regard$`CPU Model` %>%
  str_replace_all("OEM", "") %>%
  str_replace_all("Процессор ", "") %>%
  str_replace_all(" BOX (без кулера)", "") %>%
  str_replace_all(" BOX", "") %>%
  str_replace_all(" (без кулера)", "") %>%
  str_replace_all("(без кулера)", "") %>%
  str_replace_all("\\(\\)", "")  # Убираем пустые скобки

regard$`CPU Model` <- trimws(regard$`CPU Model`)

# Заменяем все " - " на "-" в колонке 'CPU Model'
regard$`CPU Model` <- gsub(" - ", "-", regard$`CPU Model`)


# Убираем дубликаты по столбцу 'CPU Model'
regard <- regard %>% distinct(`CPU Model`, .keep_all = TRUE)




# Указываем URL страницы

url_3d <- "https://benchmarks.ul.com/compare/best-cpus"


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


clean_table_cpu <- clean_table_cpu %>%
  filter(!str_detect(`CPU Model`, "Threadripper"))


clean_table_cpu <- clean_table_cpu %>%
  select(-c("Rank", "16 threads", "8 threads", "4 threads", "2 threads", "Single thread"))


colnames(clean_table_cpu)

# добавлене цены
clean_table_cpu <- left_join(clean_table_cpu, regard[, c("CPU Model", "Price_rub")], by = "CPU Model")



#Фультруем только те где есть цена
clean_table_cpu <- clean_table_cpu %>% filter(!is.na(`Price_rub`)) 


clean_table_cpu$VFM <- round(clean_table_cpu$`3DMark CPU` / clean_table_cpu$Price_rub *100)

clean_table_cpu$Price_rub2 <- clean_table_cpu$Price_rub / 1000



# Добавляем новый столбец "CPU Brand"
clean_table_cpu$`CPU Brand` <- ifelse(grepl("AMD", clean_table_cpu$`CPU Model`), "AMD", 
                                      ifelse(grepl("Intel", clean_table_cpu$`CPU Model`), "Intel", NA))




all_data <- clean_table_cpu


# UI часть Shiny-приложения
ui <- fluidPage(
  titlePanel("Best CPUs Comparison"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceRange", 
                  "Select Price Range (Rub):", 
                  min = 0,  # Минимальная цена
                  max = round(max(all_data$Price_rub2), -1),  # Максимальная цена из данных
                  value = c(10, 30),  
                  step = 10,  # Шаг слайдера
                  post = "k", 
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
      filter(Price_rub2 >= input$priceRange[1], Price_rub2 <= input$priceRange[2])  # Фильтруем процессоры по диапазону цен
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
      arrange(desc(`VFM`)) %>%
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
  
  
  
  output$cpu_plot <- renderPlot({
    data <- filtered_data()  # Получаем отфильтрованные данные
    points <- highlighted_points()  # Получаем выделенные точки
    
    # Находим максимальное и минимальное значение для оси Y
    min_y <- min(data$Price_rub2, na.rm = TRUE)
    max_y <- max(data$Price_rub2, na.rm = TRUE)
    
    # Устанавливаем нижнюю границу на 5 меньше минимального значения
    y_limit_lower <- min_y - 5
    
    # Устанавливаем верхнюю границу на 5 больше максимального значения
    y_limit_upper <- max_y + 5
    
    # Строим график
    ggplot(data, aes(x = `3DMark CPU`, y = `Price_rub2`)) +
      geom_point(aes(color = "Other"), size = 1) +  # Все точки (будет отображаться, но не попадет в легенду)
      geom_point(data = points, aes(color = type), size = 5) +  # Выделяем 4 точки, используя переменную type
      scale_color_manual(
        values = c("Intel" = "blue", "AMD" = "red", 
                   "Value for Money" = "green", "Popularity" = "orange", 
                   "Other" = "grey"),
        limits = c("Intel", "AMD", "Value for Money", "Popularity"),  # Ограничиваем легенду только нужными категориями
        labels = c("Intel CPUs", "AMD CPUs", "Value for Money", "Popular CPU")  # Изменяем подписи для легенды
      ) + 
      # Используем ggrepel для автоматического сдвига подписей
      geom_text_repel(data = points, aes(label = `CPU Model`), size = 4, box.padding = 0.4) +  # Подписи для выделенных точек
      labs(title = "3DMark CPU vs Price", x = "3DMark CPU", y = "Price", color = "") +  # Легенда
      theme_minimal() +
      theme(
        legend.position = "top",  # Позиция легенды
        legend.text = element_text(size = 13),  # Увеличиваем размер текста в легенде
        legend.title = element_text(size = 14)  # Увеличиваем размер заголовка легенды (если нужен)
      ) + 
      scale_y_continuous(
        limits = c(y_limit_lower, y_limit_upper),  # Устанавливаем динамические пределы оси Y
        expand = c(0, 0)  # Убираем отступы с обеих сторон оси Y
      )  # Ограничиваем ось Y и добавляем отступы
  })
  
  
  
  
  
  output$highlighted_points <- renderTable({
    highlighted_points() %>%
      mutate(Price_rub = format(Price_rub, big.mark = " ", scientific = FALSE)) %>%  # Добавляем разделитель тысяч
      select(`CPU Model`, `Price_rub`, `3DMark CPU`, type)  # Выбираем нужные столбцы
  })
  
}

# Запуск приложения
shinyApp(ui = ui, server = server)

