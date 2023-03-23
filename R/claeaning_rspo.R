#' @title Czyszczenie zrzutu z bazy RSPO
#' @description Funkcja wykonująca wszystkie operacje czyszczenia zbioru RSPO po
#' kolei, czyli:
#' \itemize{
#'   \item{\code{[df_into_vector()]}}{konwersja ramek danych na wektory}
#'   \item{\code{[remove_columns()]}}{usuwanie zbędnych kolumn}
#'   \item{\code{[convert_date()]}}{konwersja na daty i zmienne liczbowe}
#'   \item{\code{[new_columns()]}}{tworzenie nowych kolumn}
#' }
#' @param x ramka danych będąca bazą RSPO - najlepiej wynik działania funkcji
#' \code{[get_raw_rspo()]}
#' @return data.frame
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% 
#' @export
clean_rspo = function(x) {
  stopifnot(is.data.frame(x) | is_tibble(x),
            length(colnames(x)) > 0)
  
  x = x %>% 
    df_into_vector() %>% 
    remove_columns() %>% 
    convert_date() %>% 
    new_columns()
  
  return(x)
}
#' @title Czyszczenie zrzutu z bazy RSPO
#' @description Funkcja spłaszcza ramkę danych konwertując kolumny zawierające
#' inne ramki danych lub listy do wektorów. Zarówno ramki danych, jak i listy
#' mają podobne struktury, gdzie istotne informacje znajdują się zazwyczaj w
#' tylko jednej kolumnie/elemencie listy, dlatego konwersja często polega na
#' wyciągnięciu tej wartości i do osobnej kolumny zbioru RSPO.
#' Niektóre listy są na tyle rozbudowane, że nie można w prosty sposób ich
#' spłaszczyć, dlatego użytkownik może za pomocą parametru \code{listy}
#' zdecydować czy wykluczyć ze zbioru te listy czy zostawić je w formie 
#' niezmienionej (niespłaszczonej). W przypadku decyzji o niespłąszczaniu list
#' mogą wystąpić problemy z zapisywaniem zbioru z takimi kolumnami do pliku
#' o rozszerzeniu .csv.
#' @param x ramka danych będąca bazą RSPO - najlepiej wynik działania funkcji
#' \code{[get_raw_rspo()]}
#' @param listy czy mają zostać zwrócone zmienne będące listami? Zmienne jako
#' listy mogą się nie zapisywać do pliku .csv
#' @return data.frame
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% mutate .data select
df_into_vector = function(x, listy = FALSE) {
  stopifnot(is.data.frame(x) | is_tibble(x),
            length(colnames(x)) > 0,
            is.logical(listy))
  
  nazwy = colnames(x)
  
  if ("typ" %in% nazwy) {
    x = x %>% 
      mutate(typPodmiotu = .data$typ$nazwa) %>% 
      select(-c("typ"))
  }
  if ("statusPublicznoPrawny" %in% nazwy) {
    x$statusPublicznoPrawny = x$statusPublicznoPrawny[, 4]
  }
  if ("etapyEdukacji" %in% nazwy) {
    x$etapyEdukacji = unname(unlist(x$etapyEdukacji)[2])
  }
  if ("kategoriaUczniow" %in% nazwy) {
    x$kategoriaUczniow = x$kategoriaUczniow[, 4]
  }
  if ("specyfikaSzkoly" %in% nazwy) {
    x$specyfikaSzkoly = x$specyfikaSzkoly[, 4]
  }
  if ("zwiazanieOrganizacyjne" %in% nazwy) {
    x$zwiazanieOrganizacyjne = x$zwiazanieOrganizacyjne[, 4]
  }
  if ("podmiotPrzekazujacyDaneDoRSPO" %in% nazwy) {
    x = x %>% 
      mutate(
        podmiotPrzekazujacyDaneDoRSPO_nazwa = .data$podmiotPrzekazujacyDaneDoRSPO$nazwa,
        podmiotPrzekazujacyDaneDoRSPO_typ = .data$podmiotPrzekazujacyDaneDoRSPO$typ.nazwa,
        podmiotPrzekazujacyDaneDoRSPO_regon = as.numeric(.data$podmiotPrzekazujacyDaneDoRSPO$regon)
      ) %>% 
      select(-c("podmiotPrzekazujacyDaneDoRSPO"))
  }
  if ("podmiotProwadzacy" %in% nazwy) {
    x = x %>% 
      mutate(
        podmiotProwadzacy_nazwa = .data$podmiotProwadzacy$nazwa,
        podmiotProwadzacy_typ = .data$podmiotProwadzacy$typ.nazwa,
        podmiotProwadzacy_regon = as.numeric(.data$podmiotProwadzacy$regon)
      ) %>% 
      select(-c("podmiotProwadzacy"))
  }
  if ("geolokalizacja" %in% nazwy) {
    x = x %>% 
      mutate(
        geolokalizacja_lat = .data$geolokalizacja$latitude,
        geolokalizacja_lon = .data$geolokalizacja$longitude
      ) %>% 
      select(-c("geolokalizacja"))
  }
  
  # zwracanie list jako listy - są to głównie liczebności w zawodach
  if (!listy) {
    ksztalcenie_listy = c("ksztalcenieZawodowe", "ksztalcenieZawodoweProfilowane", "ksztalcenieZawodoweArtystyczne", "ksztalcenieNKJO", "ksztalcenieWKolegiachNauczycielskich", "ksztalcenieWkolegiachPracownikowSluzbSpolecznych", "placowkiPodrzedne")
    if (any(ksztalcenie_listy %in% nazwy)) {
      x[, ksztalcenie_listy] = NULL
    }
  } else {
    x$placowkiPodrzedne = ifelse(is.null(unlist(x$placowkiPodrzedne)),
                                 x$placowkiPodrzedne,
                                 unlist(unname(x$placowkiPodrzedne)) %>% 
                                   {gsub("/api/placowki/", "", .data)} %>%
                                   list())
  }
  
  return(x)
}
#' @title Czyszczenie zrzutu z bazy RSPO
#' @description Funkcja usuwa niepotrzebne kolumny ze zbioru RSPO, jeśli takowe
#' występują. Aby funkcja spełniała swoje założenia najlepiej, aby zbiór
#' przekazywany do argumentu \code{x} był wynikiem działania funkcji
#' \code{[get_raw_rspo()]}.
#' @param x ramka danych będąca bazą RSPO - najlepiej wynik działania funkcji
#' \code{[get_raw_rspo()]}
#' @return data.frame
#' @importFrom tibble is_tibble
#' @importFrom dplyr select .data
remove_columns = function(x) {
  stopifnot(is.data.frame(x) | is_tibble(x),
            length(colnames(x)) > 0)
  
  nazwy = colnames(x)
  
  if ("@id" %in% nazwy) {x = select(x, -.data$`@id`)}
  if ("@type" %in% nazwy) {x = select(x, -.data$`@type`)}
  # ta kolumna zwraca puste listy albo puste wartości logiczne - nie ma tam wartości
  if ("opiekaDydaktycznoNaukowaUczelni" %in% nazwy) {x = select(x, -.data$opiekaDydaktycznoNaukowaUczelni)}
  
  return(x)
}
#' @title Czyszczenie zrzutu z bazy RSPO
#' @description Funkcja konwertuje wartości numeryczne, które są przechowywane
#' jako wartości tekstowe w niektórych kolumnach zbioru, na wartości klasy
#' \code{integer} oraz wartości będące datą, ale przechowywane jako tekst,
#' na wartości klasy \code{date}.
#' @param x ramka danych będąca bazą RSPO - najlepiej wynik działania funkcji
#' \code{[get_raw_rspo()]}
#' @return data.frame
#' @importFrom tibble is_tibble
convert_date = function(x) {
  stopifnot(is.data.frame(x) | is_tibble(x),
            length(colnames(x)) > 0)
  
  nazwy = colnames(x)
  
  ## converting dates
  if ("dataZalozenia" %in% nazwy) {
    x$dataZalozenia = ifelse(substr(x$dataZalozenia, 1, 4) %in% "9999", NA, x$dataZalozenia)
    x$dataZalozenia = as.Date(x$dataZalozenia, format = "%Y-%m-%d")
  }
  if ("dataRozpoczecia" %in% nazwy) {
    x$dataRozpoczecia = ifelse(substr(x$dataRozpoczecia, 1, 4) %in% "9999", NA, x$dataRozpoczecia)
    x$dataRozpoczecia = as.Date(x$dataRozpoczecia, format="%Y-%m-%d")
  }
  if ("dataZakonczenia" %in% nazwy) {
    x$dataZakonczenia = ifelse(substr(x$dataZakonczenia, 1, 4) %in% "9999", NA, x$dataZakonczenia)
    x$dataZakonczenia = as.Date(x$dataZakonczenia, format = "%Y-%m-%d")
  }
  if ("dataWlaczeniaDoZespolu" %in% nazwy) {
    x$dataWlaczeniaDoZespolu = ifelse(substr(x$dataWlaczeniaDoZespolu, 1, 4) %in% "9999", NA, x$dataWlaczeniaDoZespolu)
    x$dataWlaczeniaDoZespolu = as.Date(x$dataWlaczeniaDoZespolu, format="%Y-%m-%d")
  }
  if ("dataWylaczeniaZZespolu" %in% nazwy) {
    x$dataWylaczeniaZZespolu = ifelse(substr(x$dataWylaczeniaZZespolu, 1, 4) %in% "9999", NA, x$dataWylaczeniaZZespolu)
    x$dataWylaczeniaZZespolu = as.Date(x$dataWylaczeniaZZespolu, format="%Y-%m-%d")
  }
  if ("dataLikwidacji" %in% nazwy) {
    x$dataLikwidacji = ifelse(substr(x$dataLikwidacji, 1, 4) %in% "9999", NA, x$dataLikwidacji)
    x$dataLikwidacji = as.Date(x$dataLikwidacji, format="%Y-%m-%d")
  }
  ## converting characters into numerics
  if ("nip" %in% nazwy) {
    x$nip = as.numeric(x$nip)
  }
  if ("regon" %in% nazwy) {
    x$regon = as.numeric(x$regon)
  }
  
  return(x)
}
#' @title Czyszczenie zrzutu z bazy RSPO
#' @description Funkcja tworzy nowe zmienne na podstwie zmiennych z bazy RSPO, o
#' ile zmienne składowe w niej występują.
#' Tworzone są zmienne:
#' \itemize{
#'   \item{\code{adresSzkola}}{pełny adres szkoły}
#'   \item{\code{adresDoKorespondencjiSzkola}}{pełny adres korespondencyjny
#'   szkoły}
#'   \item{\code{dyrektor}}(pełne imię i nazwisko dyrektora szkoły)
#' }
#' @param x ramka danych będąca bazą RSPO - najlepiej wynik działania funkcji
#' \code{[get_raw_rspo()]}
#' @return data.frame
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% mutate .data case_when
new_columns = function(x) {
  stopifnot(is.data.frame(x) | is_tibble(x),
            length(colnames(x)) > 0)
  
  nazwy = colnames(x)
  
  if (all(c("ulica", "numerBudynku", "numerLokalu", "miejscowosc", "kodPocztowy") %in% nazwy)) {
    x = x %>% 
      mutate(
        adresSzkola = case_when(
          .data$ulica %in% "" | is.na(.data$numerBudynku) ~ paste0(trimws(.data$miejscowosc), ", ", trimws(.data$kodPocztowy)),
          !(.data$ulica %in% "") & !(.data$numerBudynku %in% "") & (.data$numerLokalu %in% "") ~ paste0(trimws(.data$ulica), " ", trimws(.data$numerBudynku), ", ", trimws(.data$miejscowosc), ", ", trimws(.data$kodPocztowy)),
          !(.data$ulica %in% "") & !(.data$numerBudynku %in% "") & !(.data$numerLokalu %in% "") ~ paste0(trimws(.data$ulica), " ", trimws(.data$numerBudynku), "/", .data$numerLokalu,", ", trimws(.data$miejscowosc), ", ", trimws(.data$kodPocztowy))
        ))
  }
  if (all(c("adresDoKorespondecjiUlica", "adresDoKorespondecjiNumerBudynku", "adresDoKorespondecjiNumerLokalu", "adresDoKorespondecjiMiejscowosc", "adresDoKorespondecjiKodPocztowy") %in% nazwy)) {
    x = x %>% 
      mutate(
        adresDoKorespondencjiSzkola = case_when(
          .data$adresDoKorespondecjiUlica %in% "" | is.na(.data$adresDoKorespondecjiNumerBudynku) ~ paste0(trimws(.data$adresDoKorespondecjiMiejscowosc), ", ", trimws(.data$adresDoKorespondecjiKodPocztowy)),
          !(.data$adresDoKorespondecjiUlica %in% "") & !(.data$adresDoKorespondecjiNumerBudynku %in% "") & (.data$adresDoKorespondecjiNumerLokalu %in% "") ~ paste0(trimws(.data$adresDoKorespondecjiUlica), " ", trimws(.data$adresDoKorespondecjiNumerBudynku), ", ", trimws(.data$adresDoKorespondecjiMiejscowosc), ", ", trimws(.data$adresDoKorespondecjiKodPocztowy)),
          !(.data$adresDoKorespondecjiUlica %in% "") & !(.data$adresDoKorespondecjiNumerBudynku %in% "") & !(.data$adresDoKorespondecjiNumerLokalu %in% "") ~ paste0(trimws(.data$adresDoKorespondecjiUlica), " ", trimws(.data$adresDoKorespondecjiNumerBudynku), "/", .data$adresDoKorespondecjiNumerLokalu,", ", trimws(.data$adresDoKorespondecjiMiejscowosc), ", ", trimws(.data$adresDoKorespondecjiKodPocztowy))
        ))
  }
  if (all(c("dyrektorImie", "dyrektorNazwisko") %in% nazwy)) {
    x = x %>% 
      mutate(dyrektor = case_when(
        .data$dyrektorImie != "" & .data$dyrektorNazwisko != "" ~ paste(.data$dyrektorImie, .data$dyrektorNazwisko),
        .data$dyrektorImie %in% "" | .data$dyrektorNazwisko %in% "" ~ paste0("")
      ))
  }
  
  return(x)
}
