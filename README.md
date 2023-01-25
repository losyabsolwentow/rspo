# Pakiet `rspo`

Pakiet `rspo` powstał w celu zwiększenia poziomu automatyzacji pobierania zrzutu z bazy RSPO (Rejestr Szkół i Placówek Oświatowych) dostępnej pod adresem: https://rspo.gov.pl/. A będzie to przydatne ze względu na potrzebę posiadania lokalnie kilku wersji bazy RSPO z różnych momentów w roku.

## Instalacja

Pakiet można zainstalować z githuba poniższymi komendami:

``` r
# install.packages("devtools")
devtools::install_github("bartplat/rspo")
```

## Przykład użycia

Na razie pakiet pozwala na pobranie bazy RSPO za pomocą wywołania jednej funkcji. Niestety ze względu na ograniczenia w API (limit pobierania 100 rekordów z bazy na stronę) trwa to długo - w zależności od prędkości łącza i parametrów komputera 1 do 2 godzin. Natomiast, zaletą tego rozwiązania jest to, że można pobieranie mieć gdzieś w tle zamiast robić to ręcznie.

### Pobeiranie surowej bazy RSPO

``` r
library(rspo)
rspo = get_raw_rspo()
save(rspo, file = "rspo.RData")
```

## Dalsze kierunki rozwoju

Planowane kolejne funkcjonalności pakietu:

1. Dołączenie do pakietu funkcji czyszczących zbiór tak, aby można było otrzymac gotowy do analiz/podpięć zbiór RSPO
2. Stworzenie testów
3. Dodanie możliwości okrojenia zapytania do kolumn potrzebnych użytkownikowi, co mogłoby przyspieszyć działanie funkcji tworzącej zrzut bazy
