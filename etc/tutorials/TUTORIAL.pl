Kr�tki samouczek Emacsa.  Warunki kopiowania znajduj� sie na ko�cu pliku.

Polecenia Emacsa wymagaj� na og� wci�ni�cia klawisza CONTROL (oznaczanego
czasami Ctrl lub CTL) lub klawisza META (oznaczanego czasami EDIT
albo ALT). Dalej b�dziemy stosowa� nast�puj�ce skr�ty:

C-<znak> oznacza przytrzymanie klawisza CONTROL przy naciskaniu
	klawisza <znak>. Na przyk�ad C-f b�dzie odpowiada�o
	naci�ni�ciu f przy wci�ni�tym klawiszu CONTROL.
M-<znak> oznacza przytrzymanie klawisza META lub ALT przy naciskaniu
	klawisza <znak>. Zamiast tego mo�na nacisn�� i pu�ci� klawisz
	ESC, a potem nacisn�� klawisz <znak>.

Uwaga: aby zako�czy� sesj� Emacsa, naci�nij C-x C-c (kolejno dwa znaki).
Znaki ">>" na lewym marginesie oznaczaj� w dalszej cz�ci tego samouczka
�wiczenia dla Ciebie. Na przyk�ad:
<<Blank lines inserted around following line by help-with-tutorial>>
[Dodatkowe odst�py zosta�y zrobione w celach dydaktycznych.]
>> Teraz naci�nij C-v (nast�pny ekran), aby przej�� na nast�pny ekran
   samouczka (zr�b to naciskaj�c jednocze�nie klawisze CONTROL i v).
   Od tego momentu powiniene� robi� to zawsze, gdy dojdziesz
   do ko�ca ekranu.

Zwr�� uwag� na to, �e kilka linii si� powtarza, gdy przechodzisz z
ekranu na nast�pny; ma to zapewni� wra�enie ci�g�o�ci podczas przesuwania
si� w obr�bie pliku.

Pierwsz� umiej�tno�ci�, kt�ra powiniene� opanowa�, jest spos�b
przesuwania si� z miejsca na miejsce. Wiesz ju�, jak przesuwa� si�
o jeden ekran do przodu. Aby przesun�� si� o jeden ekran do ty�u,
wci�nij kombinacj� klawiszy M-v (to znaczy wci�nij i przytrzymaj
klawisz META lub Alt i jednocze�nie naci�nij v albo naci�nij kolejno
klawisze <ESC> v, je�li nie masz klawisza META lub Alt).

>> Spr�buj nacisn�� M-v, a potem C-v, by przesun�� si� w prz�d i w ty�
   kilka razy.


PODSUMOWANIE
------------

Nast�puj�ce polecenia s�u�� do przegl�dania tekstu po jednym ekranie:

	C-v Przesu� si� o jeden ekran do przodu
	M-v Przesu� si� o jeden ekran do ty�u
	C-l Wyczy�� ekran i wy�wietl go na nowo, umieszczaj�c
	tekst z okolic kursora w �rodku ekranu.
	(Ta kombinacja to CONTROL-L, a nie CONTROL-1.)

>> Znajd� kursor i zapami�taj, jaki tekst jest w jego pobli�u.
   Naci�nij nast�pnie C-l.
   Znajd� kursor jeszcze raz i zwr�� uwag�, �e znajduje si� on
   w pobli�u tego samego tekstu.

Mo�esz tak�e u�y� klawiszy PageUp i PageDn, je�li s� dost�pne na
Twojej klawiaturze, do przemieszczania si� mi�dzy stronami, ale u�ycie
C-v i M-v jest bardziej efektywne.

PODSTAWY KIEROWANIA KURSOREM
----------------------------

Przesuwanie si� z ekranu na ekran jest u�yteczne, ale jak przej�� do
okre�lonego miejsca w obr�bie jednego ekranu?

Mo�na to zrobi� na kilka sposob�w. Najprostszym jest u�ycie polece�
C-p, C-b, C-f oraz C-n. Ka�de z nich przesuwa kursor o jeden wiersz
albo kolumn� w okre�lonym kierunku. Oto schemat, kt�ry to obrazuje:

                 Poprzednia linia, C-p
                 (ang. previous line)
                         :
                         :
    Do ty�u, C-b .... Kursor .... Do przodu, C-f
             (ang. back) : (ang. forward)
                         :
                         :
                  Nast�pna linia, C-n
                   (ang. next line)

>> Przesu� kursor na �rodek tego schematu za pomoc� C-n lub C-p.
   Potem naci�nij C-l, by zobaczy� ca�y diagram na �rodku ekranu.

To s� podstawowe polecenia kieruj�ce po�o�eniem kursora, kt�rych
b�dziesz u�ywa� bardzo cz�sto, warto wi�c je zapami�ta�.

>> Naci�nij kilka razy C-n, by przesun�� kursor do tej linii.

>> Przesu� si� w g��b linii za pomoc� C-f, a potem do g�ry za pomoc�
   C-p. Zwr�� uwag� na zachowanie si� C-p, gdy kursor jest w �rodku
   linii.

Ka�da linia tekstu ko�czy si� znakiem nowej linii, kt�ry oddziela j�
od nast�pnej. Ka�dy Tw�j plik powinien si� ko�czy� znakiem nowej
linii (ale Emacs tego nie wymaga).

>> Spr�buj nacisn�� C-b na pocz�tku linii. Powinno Ci� to przenie��
   na koniec poprzedniej linii. Dzieje si� tak dlatego, �e kursor
   przechodzi w�wczas nad znakiem nowej linii.

C-f przechodzi nad znakiem nowej linii tak samo jak C-b.

>> Naci�nij kilka razy C-b i obserwuj po�o�enie kursora.
   Naciskaj potem C-f, by wr�ci� na koniec linii. W ko�cu naci�nij
   jeszcze raz C-f, by przej�� do nast�pnej linii.

Gdy przesuwasz kursor poza doln� kraw�d� ekranu, tekst po�o�ony
za kraw�dzi� przesuwa si� na ekran (ang. scrolling). Dzi�ki temu
Emacs mo�e przesun�� kursor do okre�lonego miejsca bez umieszczania
go poza ekranem.

>> Spr�buj przesun�� kursor poza doln� granic� ekranu za pomoc� C-n
   i zobacz, co si� stanie.

Je�li przesuwanie si� o jeden znak na raz jest dla Ciebie zbyt wolne,
to spr�buj przesuwa� si� o s�owa. M-f (Meta-f) przesuwa kursor o s�owo
do przodu, a M-b przesuwa go o s�owo do ty�u.

>> Naci�nij kilka razy M-f i M-b.

Gdy jeste� w �rodku s�owa, to M-f przesuwa kursor na jego koniec.
Je�li natomiast jeste� w przerwie miedzy s�owami, to M-f przesuwa
kursor na koniec nast�pnego s�owa. M-b zachowuje si� podobnie
dla ruchu do ty�u.

>> Naci�nij M-f i M-b kilka razy na przemian z C-f i C-b, tak by�
   m�g� zauwa�y� dzia�anie M-f i M-b naci�ni�tych w r�nych miejscach
   wewn�trz i mi�dzy s�owami.

Zauwa� podobie�stwo mi�dzy C-f i C-b oraz M-f i M-b. Bardzo cz�sto
kombinacje zawieraj�ce Meta (Alt) oznaczaj� operacje zwi�zane
z jednostkami j�zykowymi (s�owa, zdania, akapity), podczas gdy
kombinacje z klawiszem Control dzia�aj� na jednostkach podstawowych,
niezale�nych od tego, co edytujesz (znaki, linie, itd.).

Oto zale�no��, kt�ra stosuje si� do linii i zda�: C-a i C-e przesuwaj�
kursor na pocz�tek i koniec linii, a M-a i M-e przesuwaj� go na pocz�tek
i koniec zdania.

>> Naci�nij kilka razy C-a, a potem kilka razy C-e.
   Powt�rz to z M-a, a potem z M-e.

Czy zauwa�y�e�, �e powtarzanie C-a nic nie zmienia, natomiast powt�rne
M-a przesuwa Ci� o jedno zdanie? Chocia� nie ma tu pe�nej analogii,
wydaje si� to jednak naturalne.

Po�o�enie kursora w tek�cie jest okre�lane mianem "punktu".

Oto podsumowanie prostych polece� s�u��cych do przesuwania kursora,
w��cznie z operacjami dotycz�cymi s��w i zda�:

	C-f Do przodu o jeden znak
	C-b Do ty�u o jeden znak

	M-f Do przodu o s�owo
	M-b Do ty�u o s�owo

	C-n Nast�pna linia
	C-p Poprzednia linia

	C-a Pocz�tek linii
	C-e Koniec linii

	M-a Do ty�u na pocz�tek zdania
	M-e Do przodu na koniec zdania

>> Prze�wicz kilka razy dla wprawy wszystkie powy�sze polecenia.
   Nale�� one do najcz�ciej u�ywanych.

Dwa inne wa�ne polecenia przesuwaj�ce kursor to M-< (Meta lub Alt
i znak mniejszo�ci), kt�re przesuwa kursor na pocz�tek ca�ego tekstu
i M-> (Meta lub Alt i znak wi�kszo�ci), kt�re przesuwa kursor na koniec
ca�ego tekstu.

Na wi�kszo�ci klawiatur "<" jest nad przecinkiem, musisz wi�c u�y�
klawisza Shift, by nacisn�� "<", i podobnie musisz u�y� klawisza Shift,
by nacisn�� M-<. Bez Shift uzyska�by� M-przecinek.

>> Naci�nij M-<, by przej�� na pocz�tek samouczka, a potem kilka razy
   u�yj C-v, by powr�ci� do tego miejsca.

>> Teraz naci�nij M->, by przej�� na koniec samouczka, i wr�� do tego
   miejsca za pomoc� kilkakrotnego M-v.

Je�li Twoja klawiatura ma klawisze strza�ek, to mo�esz ich u�y� do
przesuwania kursora. Radzimy Ci nauczy� si� si� kombinacji C-b, C-f,
C-n i C-p z trzech powod�w. Po pierwsze, dzia�aj� one na wszystkich
typach terminali. Po drugie, gdy ju� zdob�dziesz pewn� praktyk� w
pos�ugiwaniu si� Emacsem, to b�dzie Ci szybciej nacisn�� te kombinacje
ni� klawisze strza�ek (poniewa� nie wymaga to przenoszenia d�oni z
miejsca, kt�re zajmuj� podczas szybkiego pisania za pomoc� 10 palc�w).
Po trzecie wreszcie, gdy ju� wyrobisz sobie zwyczaj pos�ugiwania si�
tymi poleceniami z klawiszem Control, to �atwo przyjdzie Ci nauczy� si�
bardziej zaawansowanych polece� przesuwaj�cych kursor.

Wi�kszo�� polece� Emacsa akceptuje argument liczbowy; dla wi�kszo�ci
polece� oznacza on liczb� powt�rze�. Aby okre�li� liczb� powt�rze�
polecenia, powiniene� je poprzedzi� naci�ni�ciem C-u a potem cyfr.
Je�li masz na klawiaturze klawisz META (lub EDIT albo ALT), to
alternatywnym sposobem wprowadzenia argumentu liczbowego jest u�ycie
tego klawisza i wciskanie cyfr argumentu. Radzimy jednak przyswoi�
sobie metod� z klawiszem C-u, poniewa� dzia�a ona na wszystkich
terminalach.

Na przyk�ad C-u 8 C-f przesuwa kursor do przodu o osiem znak�w.

>> Spr�buj u�y� C-n i C-p z argumentem liczbowym, by przesun�� kursor
   do jednej z linii w pobli�u tego zdania za pomoc� tylko jednego
   polecenia.

Wi�kszo�� polece� u�ywa argumentu liczbowego jako liczby powt�rze�.
Jest kilka polece�, kt�re u�ywaj� go w inny spos�b. Do takich wyj�tk�w
nale�� C-v i M-v. Je�li poda si� im argument, to przesuwaj� zawarto��
ekranu w g�r� lub w d� o podan� liczb� linii zamiast o tyle� ekran�w.
Na przyk�ad C-u 4 C-v przewija ekran o 4 linie.

>> Spr�buj nacisn�� C-u 8 C-v.

To powinno by�o przewin�� ekran do g�ry o 8 linii. Je�li chcia�by�
przewin�� ekran w d�, to powiniene� poda� argument przed poleceniem M-v.

Je�li pracujesz w systemie z okienkowym trybem graficznym, jak X11
lub MS-Windows, to prawdopodobnie po lewej stronie okna Emacsa znajduje
si� prostok�tny obszar nazywany po angielsku "scrollbar", a po polsku
suwakiem. Za jego pomoc� mo�esz przewija� tekst, u�ywaj�c do tego myszy.

>> Spr�buj nacisn�� �rodkowy klawisz myszy u g�ry pod�wietlonego
   obszaru na suwaku. To powinno przewin�� tekst do miejsca
   okre�lonego przez wysoko��, na kt�rej nacisn��e� klawisz myszy.

>> Przesu� mysz do miejsca oddalonego od g�rnego ko�ca suwaka o mniej
   wi�cej trzy linie i naci�nij lewy klawisz myszy kilka razy.


* GDY EMACS JEST ZABLOKOWANY
----------------------------

Je�li Emacs przestaje odpowiada� na Twoje polecenia, to mo�esz go
bezpiecznie zatrzyma�, przyciskaj�c C-g. Klawisza C-g mo�esz te� u�y� do
przerwania polecenia, kt�re zabiera zbyt wiele czasu.

Mo�esz tak�e u�y� C-g do anulowania argumentu liczbowego albo pocz�tku
polecenia, kt�rego nie zamierzasz doko�czy�.

>> Napisz C-u 100 jako argument liczbowy, po czym naci�nij C-g.
   Teraz naci�nij C-f. Powinno to przesun�� kursor zaledwie o
   jeden znak, poniewa� argument liczbowy anulowa�e� za pomoc� C-g.

Za pomoc� klawisza C-g mo�esz te� anulowa� skutki omy�kowego
wci�ni�cia klawisza <ESC>.


* ZABLOKOWANE POLECENIA
-----------------------

Pewne polecenia Emacsa s� ,,zablokowane'' -- po to, by pocz�tkuj�cy
u�ytkownicy nie mogli ich wywo�a� przez przypadek.

Je�li wywo�asz jedno z zablokowanych polece�, to Emacs wypisze komunikat
informuj�cy o tym, co to za polecenie, i zapyta Ci�, czy istotnie chcesz
je wywo�a�.

Je�li naprawd� chcesz wywo�a� to polecenie, to odpowiedz na pytanie,
naciskaj�c spacj�. Je�li nie chcesz wywo�a� zablokowanego polecenia,
to na pytanie odpowiedz, naciskaj�c n.

>> Napisz `C-x C-l' (co jest zablokowanym poleceniem) i odpowiedz n
   na zadane pytanie.


* OKNA
------

Emacs mo�e mie� otwartych kilka okien, z kt�rych ka�de wy�wietla
w�asny tekst. Poj�cie ,,okna'', je�li chodzi o Emacsa, nie odnosi
si� do osobnego okienka systemu okienkowego, lecz do pojedynczego
panelu wewn�trz okienka systemowego. (Emacs mo�e te� pracowa�
na kilku oknach systemowych (X-oknach); w terminologii Emacsa
nazywaj� si� one ramkami. Opisane jest to poni�ej.)

Na tym etapie lepiej jest si� nie zag��bia� w techniki wykorzystuj�ce
kilka okien. Powiniene� jedynie wiedzie�, w jaki spos�b pozby� si�
nadmiaru okien, kt�re mog� si� pojawi� w wyniku wywo�ania Emacsowego
systemu pomocy albo niekt�rych polece�. Robi si� to w prosty spos�b:

	C-x 1 Jedno okno (tzn. zlikwiduj wszystkie pozosta�e okna).

Kombinacja ta to klawisz Control-x, po kt�rym wyst�puje cyfra 1.
Powi�ksza ona okno, w kt�rym jest kursor tak, by wype�ni�o ono ekran,
kasuj�c zarazem pozosta�e okna Emacsa.

>> Przesu� kursor do tej linii i naci�nij C-u 0 C-l.

(C-l, jak pami�tasz od�wie�a zawarto�� ekranu. Je�li temu poleceniu
poda si� argument liczbowy, to b�dzie to oznacza�o ,,od�wie� zawarto��
ekranu i umie�� bie��ca lini� o tyle linii od g�ry ekranu''. Tak wi�c,
C-u 0 C-1 oznacza ,,od�wie� ekran, umieszczaj�c bie��ca lini� na samej
g�rze''.)

>> Naci�nij Control-x 2
   Zauwa�, �e okno si� kurczy, a jednocze�nie pojawia si� nowe,
   wy�wietlaj�ce ten sam tekst.

>> Naci�nij C-x 1, a nowe okno zniknie.


* WSTAWIANIE I USUWANIE
-----------------------

Je�li chcesz wstawi� nowy tekst, to po prostu go napisz. Znaki, kt�re da
si� wy�wietli�, takie jak A, 7, *, itd., Emacs traktuje jako tekst i
natychmiast wstawia do dotychczasowego tekstu. Aby wstawi� znak nowej
linii, trzeba nacisn�� klawisz <Return> (na maszynach do pisania tak
oznacza�o si� znak powrotu karetki).

Ostatnio napisany znak mo�esz skasowa�, naciskaj�c klawisz <Delback>.
Chodzi tu o klawisz, kt�rego normalnie u�ywasz do skasowania ostatnio
napisanego znaku. Na wi�kszo�ci klawiatur wyr�nia si� on wielko�ci�,
le�y nad klawiszem <Return> i jest oznaczony napisem "Delete", "Del"
albo "Backspace".

Je�li masz na klawiaturze klawisz oznaczony "Backspace", to w�a�nie on
jest wspomnianym <Delback>. Opr�cz niego mo�e jeszcze wyst�powa�
klawisz oznaczony s�owem "Delete", ale to nie on pe�ni rol� <Delback>.

M�wi�c bardziej og�lnie, <Delback> usuwa znak bezpo�rednio
poprzedzaj�cy bie��c� pozycj� kursora.

>> Sprawd� to teraz: wstaw kilka znak�w, po czym usu� je, kilka razy
   naciskaj�c <Delback>. Nie martw si�, �e zmieniasz w ten spos�b
   niniejszy plik, w istocie nie zmieniasz g��wnego pliku samouczka.
   Pracujesz teraz na jego kopii.

Gdy linia tekstu staje si� zbyt d�uga, by zmie�ci� si� w jednym
wierszu ekranu, to jest ona ,,kontynuowana'' w wierszu nast�pnym.
Znak ,,backslash'' (`\') (albo - je�li pracujesz w okienkowym
trybie graficznym - zagi�ta strza�ka) umieszczony na prawym marginesie
wskazuje, �e dana linia jest kontynuowana w nast�pnym wierszu ekranu.

>> Wpisuj jaki� tekst tak d�ugo, a� dojdziesz do prawego marginesu, i
   potem nie przestawaj. Zauwa�ysz, �e pojawi si� linia kontynuacji.

>> U�yj klawisza <Delback>, by usun�� znaki tekstu, tak by linia znowu
   mie�ci�a si� na ekranie; linia kontynuacji zniknie.

Znak nowej linii mo�na skasowa� tak jak ka�dy inny znak. Usuni�cie znaku
nowej linii mi�dzy dwiema liniami spowoduje ich po��czenie. Je�li powsta�a
w wyniku tego linia tekstu jest zbyt d�uga, by zmie�ci� si� na szeroko��
ekranu, to zostanie wy�wietlona z lini� kontynuacji.

>> Przesu� kursor na pocz�tek linii i naci�nij <Delback>. Bie��ca
   linia zostanie po��czona z poprzedni�.

>> Naci�nij <Return>, by z powrotem wstawi� znak nowej linii, kt�ry
   skasowa�e�.

Jak ju� wiesz, wi�kszo�� polece� Emacsa mo�na wywo�a� z parametrem
liczby powt�rze�; dotyczy to tak�e znak�w tekstu. Argument liczbowy
powoduje wstawienie znaku odpowiadaj�c� mu liczb� razy.

>> Wypr�buj to teraz -- naci�nij C-u 8 *, a uzyskasz ********.

Nauczy�e� si� ju� wi�kszej cz�ci podstawowych sposob�w pisania oraz
poprawiania b��d�w. W Emacsie mo�esz usuwa� r�wnie� ca�e s�owa lub
linie. Oto podsumowanie operacji usuwania znak�w:

	<Delback> usu� znak bezpo�rednio przed kursorem
	C-d usu� znak bezpo�rednio za kursorem

	M-<Delback> wytnij s�owo bezpo�rednio przed kursorem
	M-d wytnij s�owo bezpo�rednio za kursorem

	C-k wytnij zawarto�� linii od kursora do jej ko�ca
	M-k wytnij wszystkie znaki od kursora do ko�ca zdania

Warto zauwa�y�, �e stosunek <Delete> i C-d do M-<Delete> i M-d
rozszerza analogi� wyst�puj�c� w zestawieniu C-f i M-f (<Delete> tak
naprawd� nie jest znakiem steruj�cym, ale nie jest to tutaj
istotne). C-k i M-k s� podobne do C-e i M-e w tym sensie, �e linie s�
odpowiednikami zda�.


Oto metoda wycinania cz�ci tekstu. Umie�� kursor na pocz�tku fragmentu,
kt�ry chcesz wyci��, i naci�nij C-@ lub C-SPC (SPC-spacja). Teraz przejd�
na drugi koniec wybranego fragmentu i naci�nij C-w. To wytnie ca�y tekst
zawarty mi�dzy punktami pocz�tkowym i ko�cowym.

>> Przesu� kursor na liter� O na pocz�tku poprzedniego paragrafu.

>> Naci�nij C-SPC. Emacs wy�wietli "Mark set" (znacznik ustawiony)
   na dole ekranu.

>> Przesu� kursor do litery o w s�owie ,,kursor'' w drugim zdaniu.

>> Naci�nij C-w. Ta komenda wytnie ca�y fragment zaczynaj�cy si� od O,
   a ko�cz�cy tu� przed o.

Gdy usuwasz wi�cej ni� jeden znak naraz, Emacs zachowuje usuni�ty
tekst po to, by m�g� go z powrotem gdzie� wstawi�. Wstawianie
usuni�tego tekstu nazywa si� ,,wklejaniem''. Usuni�ty tekst
mo�esz wklei� zar�wno w to samo miejsce, z kt�rego zosta� usuni�ty,
b�d� te� w inne miejsca. Ten sam tekst mo�esz wklei� wielokrotnie,
w celu uzyskania wielu kopii. Poleceniem wklejenia tekstu jest C-y.

Zauwa� r�nic� mi�dzy ,,wycinaniem'' i ,,usuwaniem'', polegaj�c� na tym,
�e rzeczy wyci�te mo�na na nowo wkleja�, usuni�tych natomiast wkleja� nie
mo�na. Na og� polecenia Emacsa, kt�re kasuj� du�o tekstu, zachowuj� go,
podczas gdy polecenia, kt�re po prostu kasuj� jeden znak albo puste
linie lub odst�py, skasowanego tekstu nie zachowuj�.

>> Przesu� kursor na pocz�tek linii, kt�ra nie jest pusta. Naci�nij
   C-k, by wyci�� tekst z tej linii.

>> Naci�nij C-k jeszcze raz. Zauwa�, �e wycina to znak nowej linii,
   kt�ry znajduje si� za ta lini�.

Zwr�� uwag�, �e pojedyncze C-k wycina zawarto�� linii, a powt�rne C-k
wycina sam� lini�, tak �e pozosta�e linie przesuwaj� si� do g�ry. C-k
traktuje argument liczbowy w spos�b specjalny: wycina ono tyle linii,
ile wynosi warto�� argumentu, ORAZ ich zawarto��. To nie jest jedynie
powt�rzenie kilka razy C-k. C-u 2 C-k wycina dwie linie wraz z ich
znakami nowej linii; dwukrotne naci�niecie C-k nie zrobi�oby tego.

By odzyska� ostatnio wyci�ty tekst i wstawi� go w miejsce kursora,
naci�nij C-y.

>> Twoja kolej. Naci�nij C-y, by z powrotem wstawi� tekst.

Zwr�� uwag�, �e je�li naci�niesz C-k kilka razy z rz�du, to ca�y wyci�ty
tekst zostanie zachowywany w jednym kawa�ku, tak �e pojedyncze C-y wklei
wszystkie linie.

>> Naci�nij C-k kilka razy.

A by odzyska� ten wyci�ty tekst...

>> ...naci�nij C-y. Przesu� potem kursor o kilka linii w d� i
   naci�nij C-y jeszcze raz. Widzisz, �e wstawia to ten sam tekst.

Co zrobi�, je�li chcesz wstawi� tekst, kt�ry wcze�niej wyci��e�,
a potem wycinasz co� innego? C-y wstawia tekst ostatnio wyci�ty.
Poprzedni fragment nie jest jednak stracony. Mo�esz do niego wr�ci�,
u�ywaj�c polecenia M-y. Naciskaj�c C-y, wstawiasz tekst ostatnio
wyci�ty, a naciskaj�c M-y, zast�pujesz ten tekst wyci�tym uprzednio.
Dalsze naciskanie M-y przywo�uje coraz wcze�niejsze fragmenty tekstu.
Gdy dojdziesz do tekstu, kt�rego szuka�e�, po prostu kontynuuj edycj�
tekstu, pozostawiaj�c wklejony tekst tam, gdzie si� znajduje.

Naciskaj�c M-y wystarczaj�co wiele razy, dojdziesz do punktu,
z kt�rego wystartowa�e� (czyli tekstu wyci�tego ostatnio).

>> Wytnij jak�� lini�, zmie� pozycj� kursora i wytnij inn�. Naci�nij
   potem C-y, by wstawi� drug� z wyci�tych linii. Potem naci�nij M-y
   i linia ta zostanie zast�piona przez t� pierwsz�. Naci�nij M-y
   jeszcze kilka razy, by zobaczy�, co si� dzieje. Powtarzaj to a�
   do ponownego pojawienia si� drugiej z linii. Mo�esz te� wypr�bowa�,
   co si� stanie, gdy polecenie M-y poprzedzisz argumentem dodatnim
   albo ujemnym.


* COFNIJ
--------

Je�li wprowadzisz zmiany do tekstu, a potem dojdziesz do wniosku, �e
to by�a pomy�ka, to mo�esz cofn�� zmiany, wydaj�c polecenie ,,cofnij''
(ang. undo), C-x u.

C-x u cofa zmiany wprowadzone przez jedno polecenie; je�li powt�rzysz
C-x u kilka razy z rz�du, to ka�de powt�rzenie cofa kolejne polecenie.

Od tej regu�y s� dwa wyj�tki: polecenia, kt�re nie zmieniaj� tekstu nie
licz� si� jako polecenia, kt�re mo�na wycofa� (dotyczy to zar�wno
przesuni�� kursora, jak i przewijania tekstu), oraz znaki wstawiane do
tekstu (np. litery) ��czone s� w grupy do 20. (Redukuje to liczb�
naci�ni�� C-x u, kt�re musia�by� wykona�, by wycofa� si� z niechcianych
zmian.)

>> Wytnij t� lini� za pomoc� C-k, a potem naci�nij C-x u; linia
   powinna si� pojawi� ponownie.

C-_ jest innym sposobem wywo�ania polecenia "cofnij"; dzia�a to
dok�adnie tak samo jak C-x u, jest jednak �atwiejsze do naci�ni�cia
kilka razy z rz�du. Wad� kombinacji C-_ jest to, �e nie jest oczywiste
w jaki spos�b j� uzyska� na niekt�rych klawiaturach. To w�a�nie dlatego
dost�pna jest te� kombinacja C-x u. Na niekt�rych terminalach mo�esz
nacisn�� C-_ poprzez przytrzymanie Ctrl i naci�ni�cie /.

Argument liczbowy podany przed C-_ lub C-x u okre�la liczb� powt�rze�
tego polecenia.


* PLIKI
-------

Aby edytowany przez Ciebie tekst zosta� na trwa�e zachowany, musisz
umie�ci� go w pliku. Je�li tego nie zrobisz, to tekst zniknie, gdy
zamkni�ty zostanie Emacs, za pomoc� kt�rego go edytowa�e�. Aby zachowa�
tekst w pliku, najpierw musisz ten plik ,,znale��'', i to zanim
zaczniesz wprowadza� tekst. Czynno�� znajdowania pliku (ang. "file
finding") bywa te� nazywana ,,odwiedzaniem pliku'' (ang. "file
visiting").

Odwiedzanie pliku w Emacsie powoduje wy�wietlenie jego zawarto�ci.
Bardzo cz�sto jest to pocz�tek edycji pliku. Jednak�e zmiany, kt�re
wprowadzasz do pliku, nie s� w nim utrwalone, zanim go nie ,,zachowasz''
(ang. save). Ma to zapobiec pozostawieniu w systemie pliku, kt�ry zosta�
zmieniony tylko w po�owie, a tego chcesz unikn��. Gdy zachowujesz
zmieniony plik, Emacs zostawia orygina� (pod inna nazw�) na wypadek,
gdyby� doszed� do wniosku, �e wprowadzone zmiany by�y b��dne.

Je�li popatrzysz na d� ekranu, to zauwa�ysz lini�, kt�ra zaczyna si�
i ko�czy my�lnikami, a zawiera tekst ,,TUTORIAL''. W tej
cz�ci ekranu zawsze mo�esz znale�� nazw� pliku, kt�ry w�a�nie
odwiedzasz. W tej chwili odwiedzasz plik o nazwie TUTORIAL, kt�ry
jest Twoj� w�asn� kopi� samouczka Emacsa. Oboj�tnie, kt�ry plik
odwiedzisz, w�a�nie w tym miejscu pojawi si� jego nazwa.

Polecenia s�u��ce do odwiedzania i zachowywania plik�w r�ni� si�
od innych polece�, kt�re ju� pozna�e�, tym, �e sk�adaj� si� z dw�ch
znak�w. Obydwa zaczynaj� si� od znaku Control-x. Jest mn�stwo
polece�, kt�re zaczynaj� si� od tego w�a�nie znaku; wiele z nich
dotyczy plik�w, bufor�w oraz rzeczy z nimi zwi�zanych. Polecenia
te maj� d�ugo�� dw�ch, trzech lub czterech znak�w.

Kolejn� nowo�ci� odno�nie polecenia odwiedzania pliku jest to, �e
musisz mu poda� nazw� pliku, kt�ry chcesz znale��. M�wimy o tym, �e
polecenie ,,czyta argument z terminala'' (w tym wypadku argument jest
nazw� pliku). Po wpisaniu polecenia

	C-x C-f znajd� plik (ang. find a file)

Emacs poprosi Ci� o wpisanie nazwy pliku. Pojawia si� ona w dolnej linii
ekranu. Gdy ta linia jest u�ywana do wprowadzania tego typu danych,
nazywa si� j� ,,minibuforem'' (ang. "minibuffer"). Do edycji nazwy pliku
w minibuforze mo�esz u�ywa� zwyk�ych polece� Emacsa.

Wprowadzanie nazwy pliku (lub jakichkolwiek innych danych w
minibuforze) mo�na anulowa� klawiszem C-g.

>> Naci�nij C-x C-f, po czym naci�nij C-g. Na skutek tego zniknie
   minibufor oraz przerwane zostanie wykonanie polecenia C-x C-f, kt�re
   tego minibufora u�ywa�o. W rezultacie nie odwiedzisz �adnego pliku.

Gdy sko�czysz wpisywa� nazw� pliku, naci�nij <Return>. W�wczas
polecenie C-x C-f zabierze si� do roboty i znajdzie plik, kt�ry
wybra�e�. Z chwil� zako�czenia wykonywania polecenia C-x C-f
zniknie te� minibufor.

Zawarto�� znalezionego pliku po chwili pojawia si� na ekranie
i mo�esz j� edytowa�. Gdy chcesz zachowa� zmiany, by je utrwali�,
wydaj polecenie

	C-x C-s zachowaj plik (ang. save).

Kopiuje to tekst z Emacsa do pliku. Za pierwszym razem, gdy to
robisz, Emacs zmienia nazw� oryginalnego pliku, dodaj�c na
ko�cu jego nazwy znak ~. W ten spos�b powstaje zapasowa kopia
oryginalnego pliku.

Gdy zachowywanie pliku si� ko�czy, Emacs wypisuje jego nazw� u do�u
ekranu. Pliki powiniene� zachowywa� stosunkowo cz�sto, aby nie straci�
za du�o w wypadku za�amania systemu.

>> Naci�nij C-x C-s, by zachowa� dla siebie kopi� samouczka. Emacs
   powinien wypisa� "Wrote ...TUTORIAL" na dole ekranu.

Odwiedzi� w celu edycji lub odczytu mo�esz plik istniej�cy ju� w
systemie. Mo�esz te� odwiedzi� plik, kt�rego jeszcze nie ma w systemie i
w�a�nie w taki spos�b tworzy si� w Emacsie nowe pliki. Gdy poleceniem
C-x C-f odwiedzisz plik o nazwie nieistniej�cej w systemie, w�wczas
Emacs wy�wietli puste miejsce, do kt�rego b�dziesz m�g� zacz�� wpisywa�
tekst. Gdy za��dasz zachowania wpisanego tekstu, Emacs utworzy w
systemie plik z tym tekstem. Od tego momentu mo�esz uwa�a�, �e edytujesz
plik ju� istniej�cy.


* BUFORY
--------

Je�li za pomoc� C-x C-f odwiedzisz inny plik, to plik odwiedzony
poprzednio pozostanie w Emacsie. Mo�esz si� na niego prze��czy�,
odwiedzaj�c go jeszcze raz za pomoc� C-x C-f. W ten spos�b mo�esz
mie� w Emacsie odwiedzonych jednocze�nie wiele plik�w.

>> Utw�rz plik o nazwie "foo" za pomoc� C-x C-f foo <Return>.
   Wpisz w niego jaki� tekst i zachowaj "foo" za pomoc� C-x C-s.
   W ko�cu napisz C-x C-f TUTORIAL <Return>, by wr�ci� do samouczka.

Emacs przechowuje tekst ka�dego pliku w obiekcie, zwanym ,,buforem''.
Odwiedzenie pliku powoduje utworzenie nowego bufora wewn�trz Emacsa. By
zobaczy� list� bufor�w, kt�re istniej� w Twoim Emacsie, naci�nij

	C-x C-b lista bufor�w (ang. list buffers).

>> Naci�nij C-x C-b.

Zwr�� uwag�, �e ka�dy bufor ma w�asn� nazw�, mo�e te� mie� skojarzon� z
nim nazw� pliku, kt�ry odwiedza. KA�DY tekst, kt�ry ogl�dasz w Emacsie,
jest zawsze cz�ci� jednego z bufor�w.

>> Naci�nij C-x 1 by pozby� si� listy bufor�w.

Je�li masz kilka bufor�w to tylko jeden z nich jest aktualny, ten
kt�ry w�a�nie edytujesz. Je�li chcesz edytowa� inny bufer musisz si�
do niego "prze��czy�" (ang. switch). Je�li chcesz prze��czy� si� do
bufora, kt�ry odwiedza jaki� plik, mo�esz to zrobi� poprzez ponowne
odwiedzenie pliku za pomoc� C-x C-f. Ale istnieje tak�e �atwiejszy
spos�b: u�yj C-x b. U�ywaj�c tej komendy musisz poda� nazw� bufora, do
kt�rego zamierzasz si� prze��czy�.

>> Naci�nij C-x b foo <Return> by wr�ci� do bufora "foo", kt�ry
   przechowuje tekst pliku "foo". Nast�pnie naci�nij C-x b TUTORIAL
   <Return> by wr�ci� do samouczka.

Zwykle nazwa bufora odpowiada nazwie pliku (bez �cie�ki), cho� czasami
zdarza si� inaczej. Lista bufor�w, kt�r� tworzysz za pomoc� C-x C-b
pokazuje nazwy wszystkich bufor�w.

KA�DY tekst, kt�ry pojawia si� w oknie Emacsa jest cz�ci� jakiego�
bufora.  Niekt�re bufory nie odpowiadaj� �adnemu odwiedzanemu
plikowi. Na przyk�ad bufor "*Buffer List*" nie odwiedza �adnego pliku;
zawiera on list� bufor�w, utworzon� w reakcji na naci�ni�cie przez
Ciebie C-x C-b. Bufor "*Messages*" tak�e nie odwiedza �adnego pliku;
zawiera komunikaty, kt�re pojawia�y si� podczas Twojej sesji z
Emacsem.

>> Naci�nij C-x b *Messages* <Return> by obejrze� bufor zawieraj�cy
   komunikaty. Nast�pnie naci�nij C-x b TUTORIAL <Return> by wr�ci� do
   samouczka.

Je�li zmieniasz tekst w jakim� pliku, a potem odwiedzisz inny plik, to
zawarto�� tego pierwszego NIE jest automatycznie zachowywana. Zmiany,
kt�re wprowadzi�e�, pozostaj� w Emacsie, w buforze tego� pliku.
Tworzenie czy edytowanie innego bufora nie ma �adnego wp�ywu na
pozosta�e. Jest to bardzo przydatne, ale te� oznacza, �e potrzebny jest
Ci wygodny spos�b zachowywania zawarto�ci bufor�w. Niewygodne na
przyk�ad by�oby, aby zawsze w celu zachowania bufora trzeba by�o do
niego przechodzi� za pomoc� C-x C-f i dopiero potem wywo�ywa� C-x C-s.
Dlatego istnieje polecenie:

	C-x s Zachowaj bufory (ang. save some buffers)

W reakcji na polecenie C-x s Emacs dla ka�dego z bufor�w, w kt�rym
wyst�puj� nie zachowane do tej pory zmiany, zadaje pytanie, czy go
w tej chwili zachowa�.

>> Wstaw jak�� lini� tekstu, a potem naci�nij C-x s.
   Powiniene� zosta� zapytany o to, czy chcesz zachowa� bufor
   TUTORIAL. Odpowiedz na to pytanie twierdz�co, naciskaj�c y.


* ROZSZERZANIE ZESTAWU POLECE�
------------------------------

Polece� Emacsa jest znacznie, znacznie wi�cej, ni� mo�na by skojarzy�
z klawiszami klawiatury, uwzgl�dniaj�c nawet kombinacje z META lub Ctrl.
Emacs radzi sobie z tym problemem, udost�pniaj�c polecenia X (ang.
eXtend). Istniej� dwa rodzaje tych polece�:

	C-x Rozszerzenie o znak. Nast�puje po nim jeden znak.
	M-x Rozszerzenie o nazwane polecenie. Nast�puje po nim
	    pe�na, niekiedy d�uga nazwa polecenia.

Polecenia te s� u�yteczne, ale u�ywa si� ich nie tak cz�sto, jak tych,
kt�rych ju� si� nauczy�e�. Mia�e� ju� okazj� pozna� dwa z nich: C-x C-f,
s�u��ce do odwiedzania plik�w, oraz C-x C-s do ich zachowywania. Innym
przyk�adem mo�e by� polecenie C-x C-c, kt�re ko�czy sesj� Emacsa. (Nie
martw si�, �e w ten spos�b stracisz zmiany, kt�re wprowadzi�e� do
tekst�w; przed zamkni�ciem sesji Emacs proponuje Ci zachowania
ka�dego ze zmodyfikowanych plik�w.)

C-z jest poleceniem, kt�re wychodzi z Emacsa *na chwil�*, tak by� m�g�
wr�ci� do niej wr�ci� po jakim� czasie.

W systemach, w kt�rych jest to mo�liwe, C-z zawiesza proces Emacsa;
powoduje to powr�t do pow�oki (ang. shell), ale nie niszczy Emacsa.
W najpopularniejszych pow�okach mo�esz wr�ci� do Emacsa za pomoc�
polecenia `fg' lub `%emacs'.

W systemach, w kt�rych nie ma zawieszania proces�w, C-z tworzy proces
podpow�oki (ang. "subshell"), kt�ry dzia�a pod Emacsem i daje Ci szans�
uruchamiania innych program�w oraz powrotu do Emacsa po ich sko�czeniu; w
systemach tych C-z w istocie nie powoduje wyj�cia z Emacsa i w�wczas
normalnym poleceniem powrotu do Emacsa jest wyj�cie z podpow�oki za
pomoc� polecenia "exit".

Polecenia C-x C-c powiniene� u�ywa�, gdy masz zamiar si� wylogowa�.
Zalecane jest tak�e wychodzenie z Emacsa wystartowanego na przyk�ad przez
programy obs�uguj�ce poczt� elektroniczn� lub innego rodzaju narz�dzia,
poniewa� mog� one nie wiedzie�, jak sobie poradzi� z zawieszeniem
Emacsa. Jednak�e w zwyk�ych okoliczno�ciach, je�li nie musisz
wylogowywa� si� z systemu, korzystniej jest zawiesi� Emacsa za pomoc�
C-z, ni� z niego wyj��.

Istnieje wiele polece� zaczynaj�cych si� od C-x. Oto lista tych,
kt�rych ju� si� nauczy�e�:

	C-x C-f odwied� plik
	C-x C-s zachowaj plik
	C-x C-b wy�wietl list� bufor�w
	C-x C-c wyjd� z Emacsa
	C-x u cofnij

Polece� podawanych za pomoc� nazwy u�ywa si� jeszcze rzadziej lub u�ywa
si� tylko w niekt�rych trybach. Przyk�adem mo�e by� polecenie
replace-string, kt�re zast�puje jeden �a�cuch innym w ca�ym tek�cie. Gdy
naciskasz M-x, Emacs czeka na dalszy ci�g polecenia, wy�wietlaj�c na
dole ekranu (w minibuforze) napis "M-x". Powiniene� tam wpisa� nazw�
polecenia, w tym wypadku replace-string. Wystarczy przy tym, �e napisz
jedynie repl s<Tab>; Emacs doko�czy nazw� automatycznie. Wprowadzanie
nazwy zako�cz naci�ni�ciem klawisza <Return>.

Polecenie replace-string wymaga dw�ch argument�w: �a�cucha, kt�ry ma
zosta� zast�piony, i �a�cucha, kt�ry ma zosta� wstawiony w miejsce tego�.
Wpisywanie ka�dego z tych �a�cuch�w trzeba zako�czy� przyci�ni�ciem
klawisza <Return>.

>> Przesu� kursor do czystej linii, dwie linie poni�ej tej.
   Naci�nij M-x repl s<Return>zmieni<Return>zmodyfikuje<Return>.

   Zwr�� uwag�, jak ta linia si� zmieni�a: zast�pi�e� s�owem
   ,,zmodyfikuje'' ka�de wyst�pienie s�owa z-m-i-e-n-i poni�ej pocz�tkowej
   pozycji kursora.


* AUTOMATYCZNE ZACHOWYWANIE
---------------------------

Je�li zmian wprowadzonych do pliku nie zachowasz, to mo�esz je straci� w
wypadku, gdy Tw�j komputer przestanie dzia�a�. By Ci� przed tym
uchroni�, Emacs okresowo zachowuje wprowadzone zmiany w specjalnym
pliku, kt�ry ma znak # na pocz�tku i na ko�cu swojej nazwy. Przyjmijmy
na przyk�ad, �e Tw�j plik nazywa si� "hello.c". Odpowiadaj�cy mu plik
zachowywany automatycznie b�dzie nosi� nazw� "#hello.c#". Gdy
zachowasz plik w zwyk�y spos�b, Emacs skasuje plik
zachowany automatycznie.

Je�li Tw�j komputer przestanie dzia�a�, mo�esz odzyska� Twoje dane z
pliku automatycznie zachowanego przez zwyk�e odwiedzenie tego pliku,
kt�ry edytowa�e� (a nie pliku automatycznie zachowanego!) i napisanie
M-x recover file<Return>. Gdy Emacs zapyta o potwierdzenie, to
dane zachowane automatycznie odzyskasz, je�li odpowiesz yes<Return>.


* OBSZAR ECHA
-------------

Je�li polecenia dla Emacsa wpisujesz dostatecznie wolno, b�d� one
pokazywane w specjalnym obszarze na dole ekranu, zwanym obszarem echa
(ang. echo area). Obszar echa zawiera ostatni� doln� lini� ekranu.


* LINIA STANU
-------------

Linia, kt�ra znajduje si� bezpo�rednio nad obszarem echa, zwana jest
lini� trybu (ang. modeline). Pokazuje ona tekst podobny do
nast�puj�cego:

--:** TUTORIAL (Fundamental)--L670--58%----------------

Linia ta podaje u�yteczne informacje o stanie Emacsa i tekstu, kt�ry
edytujesz.

Wiesz ju�, jakie jest znaczenie nazwy: oznacza ona plik,
kt�ry odwiedzi�e�. --NN%-- informuje o bie��cej pozycji wewn�trz
tekstu; oznacza to, �e NN procent tekstu znajduje si� ponad g�rnym
brzegiem ekranu. Je�li pocz�tek pliku znajduje si� na pocz�tku
ekranu, to zamiast liczby --00%-- zobaczysz w tym miejscu --Top--.
Podobnie dla ko�ca tekstu pojawi si� tam napis --Bot-- (ang. bottom).
Je�li wy�wietlasz tekst na tyle kr�tki, �e mie�ci si� w
ca�o�ci na ekranie, to linia trybu b�dzie zawiera�a napis --All--.

Litera L, po kt�rej wyst�puj� cyfry, tak�e opisuje Twoj� bie��c�
pozycj�: cyfry oznaczaj� numer linii, na kt�rej obecnie ustawiony jest
kursor.

Gwiazdki blisko pocz�tku linii trybu oznaczaj�, �e wprowadzi�e� do
tekstu jakie� zmiany. Tu� po odwiedzeniu, a tak�e po zachowaniu pliku
nie b�dzie w tym miejscu gwiazdek, lecz my�lniki.

Wewn�trz nawias�w znajdziesz informacje na temat trybu edycji, w
kt�rym w�a�nie jest Emacs. Domy�lnym trybem edycji nazywa si�
podstawowym (ang. fundamental); jest to tryb u�ywanym w�a�nie w
tej chwili. Jest to przyk�ad ,,trybu g��wnego'' (ang. major mode).

Emacs mo�e dzia�a� w wielu trybach g��wnych. Zosta�y one zaprojektowane,
aby u�atwi� edycj� napis�w w rozmaitych j�zykach programowania, takich
jak tryb Lisp czy C, oraz rodzajach tekst�w, jak tryb tekstowy. W danej
chwili mo�e by� aktywny tylko jeden g��wny tryb pracy i to jego nazwa
jest wy�wietlana w linii trybu w miejscu, w kt�rym teraz jest
"Fundamental".

Ka�dy z g��wnych tryb�w edycyjnych mo�e zmieni� zachowanie niekt�rych
polece�. Na przyk�ad w Emacsie istniej� polecenia s�u��ce do tworzenia
komentarzy w programach. Skoro ka�dy j�zyk programowania sam okre�la,
jak powinien wygl�da� komentarz, to ka�dy z g��wnych tryb�w edycyjnych
musi wstawia� komentarze w odpowiedni spos�b. Trybowi edycyjnemu
odpowiada nazwa polecenia, kt�re mo�esz wykona�, by prze��czy� si� w ten
tryb lub go wy��czy�. Przyk�adem mo�e by� M-x fundamental-mode, kt�re
jest poleceniem prze��czaj�cym tryb podstawowy.

Je�li zamierzasz edytowa� tekst w j�zyku angielskim, taki jak na
przyk�ad oryginalna wersja tego samouczka, to prawdopodobnie
powiniene� u�y� trybu tekstowego (ang. text mode).

>> Napisz M-x text-mode<Return>.

Nie musisz si� martwi�, bo �adne z polece�, kt�re do tej pory pozna�e�,
nie zmienia Emacsa w powa�ny spos�b. Mo�esz jednak zauwa�y�, �e teraz
M-f i M-b traktuj� apostrofy jako cz�ci s��w. Poprzednio, w trybie
podstawowym, polecenia te traktowa�y apostrofy jako separatory s��w.

G��wne tryby edycji wprowadzaj� zwykle subtelne zmiany, takie jak
opisana powy�ej; wi�kszo�� polece� nadal robi ,,to samo'', chocia�
by� mo�e w troszeczk� inny spos�b.

By zobaczy� dokumentacj� na temat bie��cego g��wnego trybu edycji,
naci�nij C-h m.

>> Naci�nij C-u C-v raz lub wi�cej razy, tak by ta linia znalaz�a si�
   blisko g�ry ekranu.

>> Naci�nij C-h m, by odczyta� dokumentacj� na temat tego, czym tryb
   tekstowy r�ni si� od trybu podstawowego.

>> Naci�nij q, by usun�� dokumentacj� trybu z ekranu.

G��wne tryby edycji nazywaj� si� w�a�nie ,,g��wnymi'', gdy� wyst�puj�
tak�e ,,podrz�dne'' tryby edycji (ang. minor modes). Podrz�dne tryby
edycji nie s� alternatyw� dla tryb�w g��wnych, lecz jedynie ich
niewielk� modyfikacj�. Ka�dy podrz�dny tryb edycji mo�na w��czy� lub
wy��czy� niezale�nie od pozosta�ych tryb�w podrz�dnych, a tak�e
niezale�nie od trybu g��wnego. Mo�esz wiec u�ywa� jednego,
kombinacji dowolnych, albo nie u�ywa� �adnego trybu podrz�dnego.

Jednym z podrz�dnych tryb�w edycji, kt�ry jest bardzo u�yteczny,
szczeg�lnie do edycji tekstu angielskiego lub polskiego, jest tryb
automatycznego wype�niania (ang. auto fill mode). Je�li jest on
w��czony, to Emacs �amie linie pomi�dzy s�owami automatycznie, gdy
podczas wstawiania tekstu linia robi si� za szeroka.

Tryb automatycznego wstawiania w��cza si� na przyk�ad poleceniem M-x
auto-fill-mode<Return>. Powt�rzenie tego polecenie powoduje wy��czenie
trybu, ponowne powt�rzenie --- jego w��czenie, i tak dalej. M�wimy, �e
polecenie ,,prze��cza tryb''.

>> Napisz M-x auto-fill-mode<Return>. Wstaw potem wiele napis�w
   ,,asdf '' tak d�ugo, a� zobaczysz, �e linia podzieli na dwie.
   Mi�dzy literami musisz wstawia� spacje, poniewa� tryb
   automatycznego wype�niania �amie linie tylko tam, gdzie s� spacje.

Margines jest zazwyczaj ustawiony na 70 znak�w, ale mo�esz to zmieni�
poleceniem C-x f. Powiniene� poleceniu poda� argument liczbowy
m�wi�cy, w kt�rej kolumnie ma zosta� ustawiony margines.

>> Wywo�aj C-x f z argumentem r�wnym 20. (C-u 2 0 C-x f).
   Napisz potem jaki� tekst i zauwa�, �e Emacs wype�nia linie do
   d�ugo�ci co najwy�ej 20 znak�w. Ustaw margines z powrotem na
   70 znak�w, wywo�uj�c jeszcze raz C-x f z odpowiednim argumentem.

Je�li zmieniasz tekst wewn�trz akapitu, to tryb automatycznego
wype�niania sam z siebie nie wyr�wna marginesu. Mo�esz go wyr�wna�
samodzielnie, wydaj�c polecenie M-q (Meta-q) (kursor powinien si�
w�wczas znajdowa� wewn�trz akapitu).

>> Przesu� kursor do poprzedniego akapitu i naci�nij M-q.


* SZUKANIE
----------

Emacs potrafi szuka� �a�cuch�w (zwartych ci�g�w znak�w lub s��w)
zar�wno wstecz jak i do przodu. Szukanie �a�cucha jest poleceniem,
kt�re przesuwa kursor --- do nast�pnego miejsca, w kt�rym dany
�a�cuch wyst�puje.

Polecenie Emacsa "search" r�ni si� od podobnych polece� w innych
edytorach tym, �e jest przyrostowe. Znaczy to, �e szukanie odbywa
si� w trakcie, gdy wpisujesz kolejne znaki �a�cucha, kt�ry ma zosta�
znaleziony.

Poleceniami rozpoczynaj�cymi szukanie s�: C-s dla szukania w prz�d
oraz C-r dla szukania wstecz. POCZEKAJ PROSZ�! Nie pr�buj ich w tej
chwili.

Gdy naci�niesz C-s, zauwa�ysz, �e w obszarze echa pojawi si�
tekst "I-search". Jest to informacja, �e Emacs znajduje si� w trybie
"incremental search" i czeka, by� napisa� tekst, kt�ry ma znale��.
Naci�ni�cie <Return> ko�czy proces szukania.

>> Rozpocznij teraz szukanie, naciskaj�c C-s. POWOLI, litera po
   literze, napisz s�owo kursor, zatrzymuj�c si� po ka�dym znaku
   i obserwuj�c, gdzie zatrzymuje si� kursor. Gdy naci�niesz drugie
   r, b�dzie mo�na powiedzie�, �e szuka�e� s�owa kursor
   jednokrotnie. Naci�nij jeszcze raz C-s, by znale�� nast�pne
   wyst�pienie s�owa kursor. Naci�nij teraz cztery razy <Delback>
   i zobacz, co si� dzieje z kursorem. Naci�nij <Return>, by sko�czy�
   szukanie.

Widzia�e�, co si� dzia�o? Podczas szukania przyrostowego Emacs pr�buje
przej�� do miejsca wyst�pienia �a�cucha, kt�ry wpisa�e� do tej pory,
i pod�wietla go dla Twojej wygody. By znale�� nast�pne wyst�pienie
s�owa kursor, po prostu jeszcze raz naci�nij C-s. Je�li takiego
wyst�pienia nie ma, to Emacs zapiszczy i napisze, �e szukanie
,,sko�czy�o si� pora�k�''.

Kombinacja C-g przerywa proces szukania, podobnie jak to czyni
z innymi poleceniami.

UWAGA: W niekt�rych systemach naci�niecie C-s zamra�a ekran i w
rezultacie Emacs nie mo�e pokazywa� tekstu. Oznacza to, �e sk�adowa
systemu operacyjnego, zwana kontrol� przep�ywu (ang. "flow control"),
przechwyci�a znak C-s i nie pozwoli�a mu dotrze� do Emacsa. By odzyska�
kontrol� nad ekranem, naci�nij C-q. Dodatkowej pomocy poszukaj w
rozdziale "Spontaneous Entry to Incremental Search" w podr�czniku
Emacsa.

Je�li podczas szukania przyrostowego naci�niesz <Delback>, to zauwa�ysz,
�e w minibuforze znika ostatni znak wpisanego przez ciebie �a�cucha, a
kursor wraca do poprzedniego miejsca. Przypu��my na przyk�ad, �e
nacisn��e� k i znalaz�e� pierwsze wyst�pienie tej litery. Je�li teraz
naci�niesz u, to kursor przesunie si� tu� za najbli�sze litery
ku. Naci�nij teraz <Delback>. Spowoduje to skasowanie z wyszukiwanego
�a�cucha litery u, a kursor wr�ci do pierwszego wyst�pienia litery k.

Je�li podczas szukania naci�niesz jaki� klawisz w kombinacji z META lub
Ctrl (z nielicznymi wyj�tkami --- znakami, kt�re maj� specjalne
znaczenie podczas szukania, takimi jak C-s i C-r), to szukanie zostanie
przerwane.

C-s rozpoczyna proces szukania do przodu, czyli ZA bie��c� pozycj�
kursora. Je�li chcesz szuka� czego� po�o�onego w tek�cie wcze�niej,
to naci�nij C-r. Wszystko, co powiedzieli�my o poleceniu C-s, stosuje
si� te� do C-r, oczywi�cie w odniesieniu do szukania wstecz.


* WIELE OKIEN
-------------

Jedn� z u�ytecznych cech Emacsa jest mo�liwo�� wy�wietlania wi�cej ni�
jednego okna na raz.

>> Przesu� kursor do tej linii i naci�nij C-u 0 C-l.

>> Naci�nij teraz C-x 2, co podzieli ekran na dwa okna. Obydwa okna
   wy�wietlaj� ten samouczek. Kursor pozostaje w g�rnym oknie.

>> Naci�nij C-M-v by przewin�� dolne okno. (Je�li nie masz
   klawisza Meta lub Alt, to naci�nij ESC C-v.)

>> Naci�nij C-x o ("o" jak angielskie "other") by przesun�� kursor do
   dolnego okna. U�yj C-v i M-v w dolnym oknie, by przewin�� jego
   zawarto��. Polecenia, kt�re masz wykona�, odczytuj z g�rnego okna.

>> Naci�nij C-x o jeszcze raz tak, by kursor wr�ci� do g�rnego okna.
   Kursor w g�rnym oknie nie zmieni� po�o�enia.

Ka�de okno pami�ta po�o�enie swojego kursora, lecz w danej chwili
tylko jedno z okien wy�wietla kursor. Wszystkie polecenia edycyjne
stosuj� si� do okna, w kt�rym jest kursor. To okno nazywane jest
,,oknem wybranym''.

Polecenie C-M-v przyda Ci si�, gdy b�dziesz chcia� edytowa� tekst w
jednym oknie, a drugiego u�ywa� jako punktu odniesienia. Dzi�ki niemu
kursor mo�e zawsze znajdowa� si� w oknie, kt�rego zawarto�� edytujesz, a
Ty mo�esz przesuwa� drugie okno.

C-M-v to przyk�ad kombinacji, kt�ry uzyskuje si�, wciskaj�c jednocze�nie
klawisze Ctrl i Meta (Alt). Je�li masz prawdziwy klawisz META (Alt), to
C-M-v mo�esz uzyska� przytrzymuj�c jednocze�nie Ctrl oraz META (Alt) i
naciskaj�c v. Nie jest wa�ne, co zosta�o naci�ni�te wcze�niej, Ctrl czy
META, poniewa� obydwa te klawisze dzia�aj� jako modyfikatory znaczenia
znak�w.

Je�li nie masz klawisza META (Alt) i w jego zast�pstwie u�ywasz ESC, to
kolejno�� naciskania klawiszy ma znaczenie: musisz najpierw nacisn�� i
pu�ci� ESC, po czym nacisn�� Ctrl-v; kombinacja Ctrl-ESC v nie zadzia�a.
Wynika to z tego, �e ESC jest znakiem, a nie modyfikatorem.

>> Naci�nij C-x 1 (w g�rnym oknie), by pozby� si� okna dolnego.

(Je�li nacisn��by� C-x 1 w dolnym oknie, to g�rne by znik�o. Mo�esz
sobie to polecenie t�umaczy� jako ,,pozostaw tylko jedno okno --- to w
kt�rym w�a�nie jestem''.)

Nie musi by� tak, �e obydwa okna pokazuj� ten sam bufor. Je�li u�yjesz
C-x C-f, by odwiedzi� jaki� plik w jednym z nich, to zawarto�� drugiego
si� nie zmieni. Z zasady w r�nych oknach mo�esz niezale�nie wy�wietla�
r�ne pliki.

Oto inny spos�b u�ywania dw�ch okien do wy�wietlania dw�ch r�nych
rzeczy:

>> Naci�nij C-x 4 C-f i nazw� jednego z Twoich plik�w. Zako�cz
   wprowadzanie klawiszem <Return>. Podany plik pojawi si� w dolnym
   oknie razem z kursorem, kt�ry tam przeskakuje.

>> Naci�nij C-x o, by wr�ci� do g�rnego okna, oraz C-x 1 by usun��
   dolne okno.


* REKURSYWNE POZIOMY EDYCJI
---------------------------

Czasami mo�esz znale�� si� w czym�, co nazywa si� "rekursywnym
poziomem edycji". Mo�esz to rozpozna� po nawiasach kwadratowych w
linii trybu, obejmuj�cych nawiasy okr�g�e zawieraj�ce nazw� g��wnego
trybu edycji. M�g�by� na przyk�ad zobaczy� [(Fundamental)] zamiast
(Fundamental).

By wyj�� z rekursywnego poziomu edycji, naci�nij ESC ESC ESC. Jest to
og�lnego przeznaczenia polecenie ,,wychodzimy''. Mo�esz go u�y� tak�e,
by pozby� si� nadmiaru okien albo wyj�� z minibufora.

>> Naci�nij M-x by wej�� do minibufora, potem naci�nij ESC ESC ESC, by
   z niego wyj��.

Aby wyj�� z rekursywnego poziomu edycji, nie wystarczy u�y� C-g. Dzieje
si� tak dlatego, �e klawisz C-g jest u�ywany do anulowania polece� i
argument�w WEWN�TRZ pojedynczego rekursywnego poziomu edycji.


SZUKANIE POMOCY
---------------

W tym samouczku dostarczyli�my tylko tyle informacji, ile jest
niezb�dne, by� m�g� zacz�� u�ywa� Emacsa. Emacs jest istn� kopalni�
najr�niejszych rzeczy, kt�rych nie spos�b tutaj opisa�. B�dziesz
zapewne chcia� dowiedzie� si� o Emacsie wi�cej, poniewa� posiada on
wiele po�ytecznych cech, o kt�rych na razie nic nie wiesz. Mi�dzy innymi
jest w nim zaszyte mn�stwo wewn�trznej dokumentacji. Dotrze� do tej
dokumentacji mo�esz po naci�ni�ciu kombinacji C-h.

By uzyska� pomoc, naci�nij C-h, a potem znak, kt�ry okre�la jakiego
rodzaju pomocy oczekujesz. Je�li poczujesz si� NAPRAWD� zagubiony, to
napisz C-h?, a Emacs podpowie, jakiego rodzaju pomocy mo�e Ci
dostarczy�. Je�li naci�niesz C-h, a potem zadecydujesz, �e pomoc nie
jest Ci jednak potrzebna, to aby anulowa� zapocz�tkowane polecenie C-h,
po prostu wci�nij C-g.

Najprostsz� pomoc mo�esz uzyska� naciskaj�c C-h c. Naci�nij C-h a potem
c, po czym kombinacj� klawiszy, kt�rej znaczenie chcesz pozna�; Emacs
wy�wietli kr�tki opis polecenia odpowiadaj�cego tej kombinacji.

>> Naci�nij C-h c C-p.

Powinno to przywo�a� komunikat, o tre�ci podobnej do

	C-p runs the command previous-line

W ten spos�b mo�esz uzyska� ,,nazw� funkcji'' przypisanej kombinacji
klawiszy. Przydaje si� to podczas pisania kodu w Lispie, w kt�rym
zapisane s� rozszerzenia Emacsa; wystarcza to tak�e do przypomnienia
Ci, co dane polecenie robi, je�li widzia�e� je ju� wcze�niej, lecz
go nie zapami�ta�e�.

Jako dope�nienie polecenia C-h c Emacs dopuszcza te� wieloznakowe
kombinacje klawiszy, na przyk�ad C-x C-s albo (je�li nie masz klawisza
META lub Alt) <ESC>v.

By uzyska� wi�cej informacji na temat polecenia, naci�nij C-h k
zamiast C-h c.

>> Naci�nij C-h k C-p.

To polecenie wy�wietla dokumentacj� na temat danej funkcji oraz jej
nazw� w oknie Emacsa. Gdy sko�czysz �ledzi� wynik tego polecenia
naci�nij C-x 1, by pozby� si� tekstu pomocy. Nie musisz tego robi� od
razu. Mo�esz wykona� pewne operacje w oparciu o tekst pomocy zanim
naci�niesz C-x 1.

Oto kilka innych u�ytecznych wariant�w C-h:

	C-h f Opisz funkcje o podanej nazwie.

>> Napisz C-h f previous-line<Return>. Wypisze to na ekranie ca��
   informacje, jak� Emacs ma na temat funkcji, kt�ra implementuje
   polecenie C-p.

Podobnie komenda C-h v pokazuje na ekranie dokumentacj� zmiennych,
kt�rych warto�ci mo�esz zmieni�, aby dostosowa� Emacsa do swoich
preferencji. Wpisz nazw� zmiennej, gdy Emacs o ni� poprosi.


	C-h a 	Apropos. Wpisz s�owo, a Emacs wypisze list�
	      	wszystkich polece�, kt�rych nazwa zawiera to s�owo.
		Polecenia te mo�na wywo�ywa� za pomoc� Meta-x.
		Dla niekt�rych polece� Apropos wypisze jedno- lub
		dwuznakowe sekwencje, kt�re wywo�uj� te polecenia.

>> Napisz C-h a file<Return>.

Zobaczysz list� wszystkich polece�,
dost�pnych za pomoc� M-x, kt�re maja s�owo "file" w swojej nazwie.
Zauwa�ysz tam tak�e polecenia takie, jak C-x C-f oraz C-x C-w,
umieszczone obok nazw polece� "find-file" i "write-file".

>> Napisz C-M-v, aby przewin�� okno pomocy. Zr�b to kilka razy.
>> Napisz C-x 1, aby usun�� okno pomocy.

	C-h i 	Czytanie elektronicznych podr�cznik�w (w formacie Info). To
		polecenie prze��czy Ci� do specjalnego bufora o nazwie
		*info*, gdzie b�dziesz m�g� przeczyta� podr�czniki
		dotycz�ce pakiet�w zainstalowanych w Twoim
		systemie. Napisz m emacs <Return>, aby zapozna� si� z
		podr�cznikiem Emacsa. Je�eli nigdy wcze�niej nie u�ywa�e�
		trybu Info, to napisz ?, a Emacs przedstawi Ci mo�liwo�ci
		tego trybu. Po tym, jak zapoznasz si� z niniejszym kr�tkim
		samouczkiem, w dalszej pracy dost�p do dokumentacji
		b�dziesz uzyskiwa� w�a�nie za pomoc� Emacs Info.


DODATKOWE FUNKCJE
-----------------

Wi�cej o Emacsie mo�esz si� nauczy� czytaj�c jego podr�cznik, w formie
ksi��kowej lub on-line w postaci Info (u�yj menu Help lub naci�nij F10
h r). Dwie dodatkowe w�a�ciwo�ci, kt�re szczeg�lnie mog� si� przyda�
to dope�nianie wprowadzanych danych i dired u�atwiaj�ce zarz�dzanie
plikami.

Dope�nianie pozwala unikn�� niepotrzebnego wpisywania. Na przyk�ad
je�li chcesz si� prze��czy� do bufora *Messages*, mo�esz nacisn�� C-x
b *M<Tab> a Emacs dope�ni dalsz� cz�� nazwy za Ciebie na tyle, na ile
b�dzie w stanie ustali� na podstawie tego, co do tej pory wpisa�e�. Dope�nianie
jest opisane w Info w podr�czniku Emacsa w cz�ci zatytu�owanej
"Dop�nianie" (ang. Completion).

Dired umo�liwia Ci zrobienie wykazu plik�w w danym katalogu (dodatkowo
w podkatalogach), przemieszczanie si� wewn�trz tej listy, odwiedzanie
plik�w, zmienianie nazw, usuwanie i inne operacje na plikach. Dired
jest opisane w Info w podr�czniku Emacsa w cz�ci zatytu�owanej
"Dired".

Podr�cznik dodatkowo opisuje wiele innych w�a�ciwo�ci Emacsa.


* KIEROWANIE KURSOREM Z X TERMINALA (akapit dodany przez autor�w wersji polskiej)
-----------------------------------

Je�li pracujesz na terminalu graficznym, to do kierowania kursorem
prawdopodobnie �atwiej Ci b�dzie u�ywa� klawiszy strza�ek po prawej
stronie klawiatury. Klawisze strza�ek: w lewo, w prawo, w g�r� i w d�
dzia�aj� zgodnie z oczekiwaniem; odpowiadaj� one dok�adnie C-b, C-f, C-p
i C-n, ale s� �atwiejsze do zapami�tania. Mo�esz tak�e u�ywa� C-lewo i
C-prawo, by przesuwa� si� o s�owa, oraz C-g�ra i C-d�, by przesuwa� si�
o bloki (np. akapity, je�li edytujesz tekst). Je�li masz klawisze
oznaczone Home (lub Begin) oraz End, to przenios� Ci� one na pocz�tek i,
odpowiednio, na koniec linii, a C-Home i C-End na pocz�tek i koniec
pliku. Je�li na Twojej klawiaturze s� klawisze PgUp i PgDn, to mo�esz
ich u�y� do przesuwania si� o jeden ekran, tak jak M-v i C-v.

Wszystkie te polecenia akceptuj� argument liczbowy, tak jak to
opisano powy�ej. Wpisanie argumentu mo�esz sobie upro�ci�:
naci�nij i trzymaj CONTROL lub META i wpisz liczb�. Na
przyk�ad, aby przesun�� kursor o 12 s��w w prawo, naci�nij C-1 C-2
C-prawo. Zwr�� uwag�, �e jest to �atwe do wpisania, poniewa� nie
musisz puszcza� klawisza CONTROL podczas wpisywania cyfr.


* U�YWANIE MENU (akapit dodany przez autor�w wersji polskiej)
---------------

Je�li pracujesz na X-terminalu, to u g�ry okna Emacsa powiniene� zauwa�y�
pasek z menu. Tego menu mo�esz u�ywa�, by wywo�ywa� najcz�ciej
potrzebne polecenia Emacsa, takie jak "find file". Na pocz�tku b�dziesz
s�dzi�, �e jest to �atwiejsze ni� u�ywanie klawiatury, poniewa� nie
musisz si� na pami�� uczy� kombinacji klawiszy, kt�re uruchamiaj�
poszczeg�lne polecenia. Gdy ju� jednak poznasz Emacsa, to zaczniesz
sobie te kombinacje przyswaja� --- dla wygody przy pozycjach menu
pokazywane s� odpowiadaj�ce im kombinacje klawiszy.

Zwr�� uwag�, �e niekt�re pozycje wyst�puj�ce w menu nie maj�
odpowiednik�w klawiszowych. Na przyk�ad pozycja "Buffers" powoduje
wy�wietlenie listy wszystkich dost�pnych bufor�w. Do ka�dego z nich
mo�esz si� prze��czy�, wybieraj�c jego nazw�, wy�wietlon� pod pozycj�
Buffers.


PODSUMOWANIE
------------

Pami�taj, �e by wyj�� z Emacsa na sta�e, trzeba wyda� polecenie C-x C-c.
By wyj�� do pow�oki na chwil� tak, by jeszcze Do Emacsa wr�ci�, trzeba
u�y� C-z. (To nie dzia�a pod X-Windows, poniewa� tam nie ma prawdziwego
konceptu przej�cia na chwil� do pow�oki. Zamiast tego C-z ,,ikonizuje''
okno Emacsa.)

Ten samouczek by� pisany tak, by wszyscy nowi u�ytkownicy mogli go
zrozumie�. Je�li co� pozostawi� niejasnym, nie sied� cicho i nie
obwiniaj siebie, tylko daj nam zna�!


KOPIOWANIE
----------

Niniejszy samouczek jest potomkiem w d�ugiej linii samouczk�w
Emacsa, kt�ra rozpoczyna si� od tego, kt�ry zosta� napisany przez
Stuarta Cracrafta dla oryginalnego Emacsa. Zosta� on zmodyfikowany we
wrze�niu 1994 przez Bena Winga, kt�ry zaktualizowa� go w celu uwzgl�dnienia
pracy pod X-Windows.

Autorem pierwszego t�umaczenia na j�zyk polski by� Remek Trzaska
<remek@npac.syr.edu>, a pomaga� mu Ryszard Kubiak
<rysiek@ipipan.gda.pl>. Tamto t�umaczenie zosta�o uaktualnione dla
wersji GNU Emacs 21 przez Beat� Wierzcho�owsk� <beataw@orient.uw.edu.pl>
z pomoc� Ryszarda Kubiaka i Janusza S. Bienia <jsbien@mail.uw.edu.pl>.

Ta wersja samouczka, podobnie jak GNU Emacs, jest chroniona prawem
autorskim, ale wolno j� kopiowa� pod nast�puj�cymi warunkami:

Copyright (C) 1985, 1994, 2001-2012  Free Software Foundation, Inc.

Zezwala si� na wykonywanie lub rozpowszechnianie
wiernych kopii tego dokumentu w otrzymanej formie, na dowolnym
no�niku, pod warunkiem zachowania informacji o
prawach autorskich i niniejszym zezwoleniu oraz pod
warunkiem, �e dystrybutor udzieli odbiorcy pozwolenia na
dalsze rozpowszechnianie zgodnie z niniejszym zezwoleniem.


Zezwala si� r�wnie� na rozpowszechnianie na warunkach podanych
powy�ej zmodyfikowanych wersji tego dokumentu lub jego cz�ci,
pod warunkiem, �e zostan� wyra�nie uwidocznione
informacje o tym, kto dokona� modyfikacji jako ostatni.


Warunki kopiowania samego Emacsa s� bardziej skomplikowane, ale zgodne
z t� ide�. Prosz�, przeczytaj plik COPYING, po czym rozdaj swoim
znajomym kopie Emacsa. Pom� t�pi� obstrukcjonizm w informatyce,
u�ywaj�c, tworz�c i dziel�c si� oprogramowaniem swobodnym.

;;; Local Variables:
;;; mode: fundamental
;;; coding: latin-2
;;; sentence-end-double-space: nil
;;; End:

