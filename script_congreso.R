library(rvest)
library(stringr)
library(dplyr)

cleanText <- function(str) {
  noBreaks <- gsub("[.\r\n]", "", str)
  return(gsub("\\s+", " ", noBreaks))
}

getProvince <- function(str) {
  return (str_replace(cleanText(str), 'Diputa.*por ', ''))
}

getTwitterUser <- function(str) {
  return (str_replace(str, 'https://twitter.com/', ''))
}

getAltaBaja <- function(str) {
  return(
    str_extract_all(str, "\\d{2}/\\d{2}/\\d{4}", simplify = FALSE) %>%
      toString() %>%
      trimws()
  )
}

datos = data.frame()
time_start <- Sys.time()
print(paste('The script starts at', time_start))

# The scrape will take about 10-15' to download all the data and save it as a csv file
legislatura <- 13
for (dip in seq(1, 360, by = 1)) {
  url = paste0(
    "http://www.congreso.es/portal/page/portal/Congreso/Congreso/Diputados/BusqForm?_piref73_1333155_73_1333154_1333154.next_page=/wc/fichaDiputado?idDiputado=",
    dip,
    "&idLegislatura=",
    legislatura,
    ""
  )
  
  congreso <- read_html(url)
  
  curriculum <- congreso %>%
    html_node("#curriculum")
  
  
  if (is.na(curriculum)) {
    # = 404 skip loop
    next
  }
  
  name <- curriculum %>%
    html_node(".nombre_dip") %>%
    html_text %>%
    unlist()
  
  texto_dip <- curriculum %>%
    html_node('.texto_dip')
  
  provincia <- texto_dip %>%
    html_nodes(".dip_rojo:nth-child(1)") %>%
    html_text %>%
    unlist() %>%
    trimws() %>%
    getProvince()
  
  grupo <- texto_dip %>%
    html_nodes(".dip_rojo:nth-child(2)") %>%
    html_text %>%
    unlist() %>%
    trimws() %>%
    cleanText()
  
  fecha_alta <- ''
  fecha_baja <- ''
  
  has_fecha_alta <- texto_dip %>%
    html_nodes('.dip_rojo') %>%
    html_nodes(xpath = '//text()[contains(., "alta:")]')
  
  has_fecha_baja <- texto_dip %>%
    html_nodes('.dip_rojo') %>%
    html_nodes(xpath = '//text()[contains(., "baja")]')
  
  if (length(has_fecha_alta) == 1) {
    fecha_alta <- texto_dip %>%
      html_nodes('.dip_rojo') %>%
      html_nodes(xpath = '//text()[contains(., "alta:")]') %>%
      html_text %>%
      getAltaBaja()
  }
  if (length(has_fecha_baja) == 1) {
    fecha_baja <- texto_dip %>%
      html_nodes('.dip_rojo') %>%
      html_nodes(xpath = '//text()[contains(., "baja")]') %>%
      html_text %>%
      getAltaBaja()
  }
  
  twitter_url <- ''
  twitter_user <- ''
  
  twitterNode <- curriculum %>%
    html_nodes(xpath = ".//a[contains(@href,'https://twitter.com')]")
  
  if (length(twitterNode) != 0) {
    twitter_url <- twitterNode %>%
      html_attr('href')
    
    twitter_user <- twitter_url %>%
      getTwitterUser()
  }
  
  
  row <- data.frame(name,
                    provincia,
                    grupo,
                    fecha_alta,
                    fecha_baja,
                    twitter_url,
                    twitter_user)
  
  print(paste(dip,name))
  
  datos <- rbind(datos, row)
}

time_end <- Sys.time()
print(paste('The script started at', time_start))
print(paste('The script ended at', time_end))
print(time_end - time_start)

write.table(
  datos,
  file = "congress.csv",
  quote = F,
  row.names = F,
  sep = '\t'
)
