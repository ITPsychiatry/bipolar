# confidence assignment and ground truth extrapolation

## notatki z ostatniego spotkania

* `confidence.ipynb`,
* `confidence.md`,
* `conf1.png`,
* `images/*`.

notebook `confidence.ipynb` zawiera notatki przedstawione
przeze mnie na ostatnim spotkaniu

poniewaz github nie wyswietla odpowiednio zalaczonych
zewnetrznie w notebooku plikow `png`, dodalem tez plik
`confidence.md` ktory jest po prostu w formacie markdown
i tam juz sie wyswietlaja te odreczne rysunki
	
## przyklad custom ground truth z artykulu

* artykul plik pdf na mojej stronie [link](https://www.kamilkmita.com/pdfs/KmitaCasalinoCastellanoHryniewiczKaczmarekMajer2022AcceptedPaper.pdf)
* `custom-ground-truth-from-wcci-article.png`

W pliku `png` zawarlem zrzut przedstawiajacy tabelke, przykladowy custom config
funkcji schodkowej w ground truth extrapolation.
Pelny artykul jest na mojej stronie domowej - powyzej link.

## przyklad funkcji ekstrapolujacej z mojego pakietu Python

* `fmatrix.py`

Ten plik jest z mojego projektu Python uzytego do artykulu na WCCI.
Zawiera w pelni udokumentowane funkcje, to co zrobisz w R de facto powinno
dawac ten sam efekt co funkcje z tego modulu.
Masz tez przyklady w dokumentacji funkcji z tym, jak powinien wygladac
efekt wykonania.

Funkcja `create_label_conf` pozwala na schodkowe przypisywanie.

Funkcja `create_label_conf_func` pozwala na dowolne wazenie funkcja ciagla,
np. gaussowska.
Wartosc `conf` przypisujemy na podstawie dnia, a nie datetime - 
to juz by byl troche `hardcore` zeby matchowac po datetime'ach.

**Ważne** 3cia funkcja `create_fmatrix_with_conf` to data wrapping, ale potrzebny:
do algorytmow ssfcm potrzebujemy reprezentacji wide, a nie long.

# implementacja Semi-Supervised Fuzzy C-Means w Python

* notebook `steps_of_ssfcm_explained.ipynb`,
* `X.csv`,
* kod `ssfcm_python_implementation.py`

Notebook pokazuje krok po kroku jakie operacje macierzowe sa potrzebne,
zeby zaimplementowac algorytm.
Wzory sa z nowego artykulu, moge sie nim podzielic w razie czego,
ale notebook jest self-explainable.

Poniewaz w notebooku losow generuje zbior danych $X$, zapisalem go do pliku `X.csv`
zebys mogl odtworzyc operacje i porowanc wyniki.

Plik `ssfcm_python_implementation.py` zawiera zwięzłą implementację klasy w Python,
której używam w artykułach do analiz.

