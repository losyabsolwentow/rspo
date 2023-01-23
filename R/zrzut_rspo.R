#' @title Tworzenie zrzutu bazy RSPO
#' @details Funkcja zwraca przy użyciu API bazy RSPO surową ramkę danych dla
#' zadanego numeru strony
#' @param api_adress ciąg znaków będących linkiem do strony z API bazy RSPO
#' @param page_number wartość liczbowa będąca numerem strony API. API bazy RSPO
#' ma ograniczenie do zwracania 100 obserwacji na stronę. Ten argument pozwala
#' wybrać, która strona ma zostać pobrana.
#' @return data.frame
get_df_api = function(api_adress = "https://api-rspo.mein.gov.pl/api/placowki",
                    page_number) {
  
  stopifnot(is.character(api_adress),
            is.numeric(page_number),
            page_number > 0)
  
  link = paste0(api_adress, "/?page=", page_number)
  
  res = GET(link)
  res = fromJSON(rawToChar(res$content))
  res = as.data.frame(res$`hydra:member`, row.names = NULL)
  
  return(res)
}
#' @title Tworzenie zrzutu bazy RSPO
#' @details Funkcja zwraca przy użyciu API bazy RSPO surową ramkę danych dla
#' zadanego zakresu stron (użytkownik podaje tylko maksymalną wartość zakresu
#' stron), czyli domyślnie dla całego zbioru.
#' @param max_page liczba stron, które mają zostać pobrane z api. Domyślnie jest
#' to wartość 999999 (przyjęto, że jest to wystarczająco wysoka wartość, żeby
#' liczba rzeczywistych stron nie przekroczyła tej wartości), bo domyślnie
#' przyjmuje się, że użytkownik chce pobrać całą bazę i raczej nie ma wiedzy o
#' tym, co konkretnie jest w danych 100 obserwacjach, które można jednorazowo
#' pobrać przez API.
#' @return data.frame
get_raw_rspo = function(max_page = 999999) {
  stopifnot(is.numeric(max_page),
            max_page > 1)
  
  rspo = data.frame()
  
  for (i in 1:max_page) {
    page = get_df_api(page_number = i)
    number_of_rows = nrow(page)
    
    if (number_of_rows > 0) {
      rspo = bind_rows(rspo, page)
    } else {
      break
    }
  }
  
  if (max_page %in% 999999) {
    message(paste0("Pobrano obserwacje z ", i - 1, " stron w API bazy RSPO"))
  } else {
    message(paste0("Pobrano obserwacje z ", i, " stron w API bazy RSPO"))
  }
  return(rspo)
}
