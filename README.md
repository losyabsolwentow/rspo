# Pakiet `rspo`

Pakiet `rspo` powstał w celu zwiększenia poziomu automatyzacji pobierania zrzutu z bazy RSPO (Rejestr Szkół i Placówek Oświatowych) dostępnej pod adresem: https://rspo.gov.pl/. Będzie to przydatne ze względu na potrzebę posiadania lokalnie kilku wersji bazy RSPO z różnych momentów w roku.

## Instalacja

Pakiet można zainstalować z githuba poniższymi komendami:

``` r
# install.packages("devtools")
devtools::install_github("losyabsolwentow/rspo")
```

## Przykład użycia

Na razie pakiet pozwala na pobranie bazy RSPO za pomocą wywołania jednej funkcji. Niestety ze względu na ograniczenia w API (limit pobierania 100 rekordów z bazy na stronę) trwa to długo - w zależności od prędkości łącza i parametrów komputera 1 do 2 godzin. Natomiast, zaletą tego rozwiązania jest to, że można pobieranie mieć gdzieś w tle zamiast robić to ręcznie.

### Pobieranie surowej bazy RSPO

``` r
library(rspo)
rspo = get_raw_rspo()
save(rspo, file = "rspo.RData")
```

### Czyszczenie pobranej bazy

Aby wygodnie używać bazy RSPO należy dokonać czyszczenia i przekształceń zmiennych, które można zrobić we własnym zakresie lub przy użyciu funkcji `clean_rspo()` - jak w przykładzie poniżej:

```r
rspo = clean_rspo(rspo)
```

Funkcja `clean_rspo()` dokonuje następujących przekształceń:

* spłaszczenie ramki danych poprzez konwersję zmiennych zawierających zagnieżdżone ramki lub listy do wektorów;
* usuwanie zbędnych kolumn;
* konwersja wybranych zmiennych do formatu liczbowego lub daty (w surowym zbiorze są one przechowywane jako tekst);
* tworzenie nowych kolumn

Dodatkowe przekształcenie można wykonać również dzięki użyciu funkcji `ibe_miejsc()`, która tworzy dodatkowe zmienne, które używane są w pracy z danymi sondażowymi w IBE. Funkcji tej należy używać w następującej kolejności w pracy ze zbiorem RSPO przy użyciu pakietu `dplyr`:

```
library(rspo)
library(dplyr)

rspo = get_raw_rspo()

rspo = rspo %>% 
  clean_rspo() %>% 
  ibe_miejsc()

save(rspo, file = "rspo.RData")
```

Lub przy użyciu `base` R:

```
library(rspo)

rspo = get_raw_rspo()
rspo = clean_rspo(rspo)
rspo = ibe_miejsc(rspo)

save(rspo, file = "rspo.RData")
```

## Dalsze kierunki rozwoju

Planowane kolejne funkcjonalności pakietu:

1. Stworzenie testów
2. Dodanie możliwości okrojenia zapytania do kolumn potrzebnych użytkownikowi, co mogłoby przyspieszyć działanie funkcji tworzącej zrzut surowej bazy
3. Sprawdzanie zgodności wersji API dla której napisano funkcje pakietu z wersją API na stronie internetowej SIO RSPO
