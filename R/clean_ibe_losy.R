#' @title Czyszczenie zrzutu z bazy RSPO na potrzeby projektu Losy2
#' @details Funkcja oblicza na podstawie zbioru RSPO 3 nowe kolumny na potrzeby
#' prac w projekcie Losy2 w Instytucie Badań Edukacyjnych. Funkcja tworzy
#' następujące zmienne:
#' \itemize{
#'   \item{\code{kl_wlk_miejsc}}{informacja o klasie wielkości miejscowości,
#'   w której znajduje się placówka. Przyjmuje 4 wartości: `mnpp` - miasta z
#'   delegaturami oraz miasta na prawach powiatu; `m` - miasta powiatowe; `w` -
#'   wieś; `mw` - inne miasta}
#'   \item{\code{kl_wlk_miejsc_3}}{informacja o klasie wielkości miejscowości,
#'   ale zawężona do 3 kategorii poprzez połączenie dwóch kategorii zmiennej
#'   \code{kl_wlk_miejsc} - połączono kategorie `w` i `mw` w jedną}
#'   \item{\code{miasto}}{zmienna binarna o wartości `TRUE` jeśli szkoła
#'   znajduje w obszarze miejskim i `FALSE` jeśli nie znajduje się w obszarze
#'   miejskim}
#' }
#' @description Zmienna \code{kl_wlk_miejsc} powstaje na podstawie kodu TERYT na
#' poziomie gminy (kod 6-cio lub 7-mio cyfrowy, zawierający w ostatniej cyfrze
#' kodu informację o rodzaju gminy (w bazie RSPO sama ta informacja
#' przechowywana jest dodatkowo w zmiennej \code{gminaRodzajKod})). Poniżej
#' opisano logikę tworzenia zmiennej na podstawie wspomnianego numeru TERYT:
#' \itemize{
#'   \item{miasta na prawach powiatu}{Jako, że podział jest tworzony na poziomie
#'   całych gmin, to nie są brane pod uwagę obszary miejskie i obszary wiejskie
#'   w ramach gmin miejsko-wiejskich ani dzielnice ani delegatury. Miasta na
#'   prawach powiatu to gminy miejskie albo powiaty, których 2 cyfry TERYTu
#'   (czyli 3. i 4. wartość TERYTu) opisujące powiat są większe od 60.}
#'   \item{miasta, wsie i obszary miejsko-wiejskie}{Pozostałe 3 kategorie
#'   wyodrębniane są na podstawie 7. cyfry w numerze TERYT, która w rejestrze
#'   TERC jest wartością zmiennej RODZ. Kolejne wartości podziału kodowane są
#'   następująco: 1 = miasto, 2 = wieś, 3 = obszar miejsko-wiejski}
#' }
#' Zmienna \code{kl_wlk_miejsc_3} jest ważną zmienną w pracy ze zbiorami z
#' sondaży realizowanych w projekcie Losy2 w IBE, ponieważ to do jej rozkładu w
#' populacji absolwentów ważone są dane sondażowe.
#' @param x ramka danych będąca wyczyszczoną bazą RSPO - najlepiej wynik
#' działania funkcji \code{[clean_rspo()]}
#' @return data.frame
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% mutate .data case_when across
#' @export
ibe_miejsc = function(x) {
  stopifnot(is.data.frame(x) | is_tibble(x),
            length(colnames(x)) > 0)
  
  if ("gminaKodTERYT" %in% colnames(x)) {
    x = x %>% 
      mutate(
        kl_wlk_miejsc = ifelse(
          nchar(as.numeric(.data$gminaKodTERYT)) %in% 6,
          case_when(
            substr(as.numeric(.data$gminaKodTERYT), 2, 3) > 60 ~ "mnpp",
            substr(as.numeric(.data$gminaKodTERYT), 6, 6) %in% 1 ~ "m",
            substr(as.numeric(.data$gminaKodTERYT), 6, 6) %in% 2 ~ "w",
            substr(as.numeric(.data$gminaKodTERYT), 6, 6) %in% 3 ~ "mw",
            substr(as.numeric(.data$gminaKodTERYT), 6, 6) > 3 ~ "mw"),
          case_when(
            substr(as.numeric(.data$gminaKodTERYT), 3, 4) > 60 ~ "mnpp",
            substr(as.numeric(.data$gminaKodTERYT), 7, 7) %in% 1 ~ "m",
            substr(as.numeric(.data$gminaKodTERYT), 7, 7) %in% 2 ~ "w",
            substr(as.numeric(.data$gminaKodTERYT), 7, 7) %in% 3 ~ "mw",
            substr(as.numeric(.data$gminaKodTERYT), 7, 7) > 3 ~ "mw")
        ),
        miasto = ifelse(kl_wlk_miejsc %in% c("m", "mnpp"), TRUE, FALSE)
      ) %>% 
      mutate(across(.data$kl_wlk_miejsc,
                    ~ifelse(. %in% c("w", "mw"), "w_mw", .),
                    .names = "kl_wlk_miejsc_3"))
  }
  
  return(x)
}
