Krótki samouczek Emacsa.  Warunki kopiowania znajduj± sie na koñcu pliku.

Polecenia Emacsa wymagaj± na ogó³ wci¶niêcia klawisza CONTROL (oznaczanego
czasami Ctrl lub CTL) lub klawisza META (oznaczanego czasami EDIT
albo ALT). Dalej bêdziemy stosowaæ nastêpuj±ce skróty:

C-<znak> oznacza przytrzymanie klawisza CONTROL przy naciskaniu
	klawisza <znak>. Na przyk³ad C-f bêdzie odpowiada³o
	naci¶niêciu f przy wci¶niêtym klawiszu CONTROL.
M-<znak> oznacza przytrzymanie klawisza META lub ALT przy naciskaniu
	klawisza <znak>. Zamiast tego mo¿na nacisn±æ i pu¶ciæ klawisz
	ESC, a potem nacisn±æ klawisz <znak>.

Uwaga: aby zakoñczyæ sesjê Emacsa, naci¶nij C-x C-c (kolejno dwa znaki).
Znaki ">>" na lewym marginesie oznaczaj± w dalszej czê¶ci tego samouczka
æwiczenia dla Ciebie. Na przyk³ad:
<<Blank lines inserted around following line by help-with-tutorial>>
[Dodatkowe odstêpy zosta³y zrobione w celach dydaktycznych.]
>> Teraz naci¶nij C-v (nastêpny ekran), aby przej¶æ na nastêpny ekran
   samouczka (zrób to naciskaj±c jednocze¶nie klawisze CONTROL i v).
   Od tego momentu powiniene¶ robiæ to zawsze, gdy dojdziesz
   do koñca ekranu.

Zwróæ uwagê na to, ¿e kilka linii siê powtarza, gdy przechodzisz z
ekranu na nastêpny; ma to zapewniæ wra¿enie ci±g³o¶ci podczas przesuwania
siê w obrêbie pliku.

Pierwsz± umiejêtno¶ci±, która powiniene¶ opanowaæ, jest sposób
przesuwania siê z miejsca na miejsce. Wiesz ju¿, jak przesuwaæ siê
o jeden ekran do przodu. Aby przesun±æ siê o jeden ekran do ty³u,
wci¶nij kombinacjê klawiszy M-v (to znaczy wci¶nij i przytrzymaj
klawisz META lub Alt i jednocze¶nie naci¶nij v albo naci¶nij kolejno
klawisze <ESC> v, je¶li nie masz klawisza META lub Alt).

>> Spróbuj nacisn±æ M-v, a potem C-v, by przesun±æ siê w przód i w ty³
   kilka razy.


PODSUMOWANIE
------------

Nastêpuj±ce polecenia s³u¿± do przegl±dania tekstu po jednym ekranie:

	C-v Przesuñ siê o jeden ekran do przodu
	M-v Przesuñ siê o jeden ekran do ty³u
	C-l Wyczy¶æ ekran i wy¶wietl go na nowo, umieszczaj±c
	tekst z okolic kursora w ¶rodku ekranu.
	(Ta kombinacja to CONTROL-L, a nie CONTROL-1.)

>> Znajd¼ kursor i zapamiêtaj, jaki tekst jest w jego pobli¿u.
   Naci¶nij nastêpnie C-l.
   Znajd¼ kursor jeszcze raz i zwróæ uwagê, ¿e znajduje siê on
   w pobli¿u tego samego tekstu.

Mo¿esz tak¿e u¿yæ klawiszy PageUp i PageDn, je¶li s± dostêpne na
Twojej klawiaturze, do przemieszczania siê miêdzy stronami, ale u¿ycie
C-v i M-v jest bardziej efektywne.

PODSTAWY KIEROWANIA KURSOREM
----------------------------

Przesuwanie siê z ekranu na ekran jest u¿yteczne, ale jak przej¶æ do
okre¶lonego miejsca w obrêbie jednego ekranu?

Mo¿na to zrobiæ na kilka sposobów. Najprostszym jest u¿ycie poleceñ
C-p, C-b, C-f oraz C-n. Ka¿de z nich przesuwa kursor o jeden wiersz
albo kolumnê w okre¶lonym kierunku. Oto schemat, który to obrazuje:

                 Poprzednia linia, C-p
                 (ang. previous line)
                         :
                         :
    Do ty³u, C-b .... Kursor .... Do przodu, C-f
             (ang. back) : (ang. forward)
                         :
                         :
                  Nastêpna linia, C-n
                   (ang. next line)

>> Przesuñ kursor na ¶rodek tego schematu za pomoc± C-n lub C-p.
   Potem naci¶nij C-l, by zobaczyæ ca³y diagram na ¶rodku ekranu.

To s± podstawowe polecenia kieruj±ce po³o¿eniem kursora, których
bêdziesz u¿ywa³ bardzo czêsto, warto wiêc je zapamiêtaæ.

>> Naci¶nij kilka razy C-n, by przesun±æ kursor do tej linii.

>> Przesuñ siê w g³±b linii za pomoc± C-f, a potem do góry za pomoc±
   C-p. Zwróæ uwagê na zachowanie siê C-p, gdy kursor jest w ¶rodku
   linii.

Ka¿da linia tekstu koñczy siê znakiem nowej linii, który oddziela j±
od nastêpnej. Ka¿dy Twój plik powinien siê koñczyæ znakiem nowej
linii (ale Emacs tego nie wymaga).

>> Spróbuj nacisn±æ C-b na pocz±tku linii. Powinno Ciê to przenie¶æ
   na koniec poprzedniej linii. Dzieje siê tak dlatego, ¿e kursor
   przechodzi wówczas nad znakiem nowej linii.

C-f przechodzi nad znakiem nowej linii tak samo jak C-b.

>> Naci¶nij kilka razy C-b i obserwuj po³o¿enie kursora.
   Naciskaj potem C-f, by wróciæ na koniec linii. W koñcu naci¶nij
   jeszcze raz C-f, by przej¶æ do nastêpnej linii.

Gdy przesuwasz kursor poza doln± krawêd¼ ekranu, tekst po³o¿ony
za krawêdzi± przesuwa siê na ekran (ang. scrolling). Dziêki temu
Emacs mo¿e przesun±æ kursor do okre¶lonego miejsca bez umieszczania
go poza ekranem.

>> Spróbuj przesun±æ kursor poza doln± granicê ekranu za pomoc± C-n
   i zobacz, co siê stanie.

Je¶li przesuwanie siê o jeden znak na raz jest dla Ciebie zbyt wolne,
to spróbuj przesuwaæ siê o s³owa. M-f (Meta-f) przesuwa kursor o s³owo
do przodu, a M-b przesuwa go o s³owo do ty³u.

>> Naci¶nij kilka razy M-f i M-b.

Gdy jeste¶ w ¶rodku s³owa, to M-f przesuwa kursor na jego koniec.
Je¶li natomiast jeste¶ w przerwie miedzy s³owami, to M-f przesuwa
kursor na koniec nastêpnego s³owa. M-b zachowuje siê podobnie
dla ruchu do ty³u.

>> Naci¶nij M-f i M-b kilka razy na przemian z C-f i C-b, tak by¶
   móg³ zauwa¿yæ dzia³anie M-f i M-b naci¶niêtych w ró¿nych miejscach
   wewn±trz i miêdzy s³owami.

Zauwa¿ podobieñstwo miêdzy C-f i C-b oraz M-f i M-b. Bardzo czêsto
kombinacje zawieraj±ce Meta (Alt) oznaczaj± operacje zwi±zane
z jednostkami jêzykowymi (s³owa, zdania, akapity), podczas gdy
kombinacje z klawiszem Control dzia³aj± na jednostkach podstawowych,
niezale¿nych od tego, co edytujesz (znaki, linie, itd.).

Oto zale¿no¶æ, która stosuje siê do linii i zdañ: C-a i C-e przesuwaj±
kursor na pocz±tek i koniec linii, a M-a i M-e przesuwaj± go na pocz±tek
i koniec zdania.

>> Naci¶nij kilka razy C-a, a potem kilka razy C-e.
   Powtórz to z M-a, a potem z M-e.

Czy zauwa¿y³e¶, ¿e powtarzanie C-a nic nie zmienia, natomiast powtórne
M-a przesuwa Ciê o jedno zdanie? Chocia¿ nie ma tu pe³nej analogii,
wydaje siê to jednak naturalne.

Po³o¿enie kursora w tek¶cie jest okre¶lane mianem "punktu".

Oto podsumowanie prostych poleceñ s³u¿±cych do przesuwania kursora,
w³±cznie z operacjami dotycz±cymi s³ów i zdañ:

	C-f Do przodu o jeden znak
	C-b Do ty³u o jeden znak

	M-f Do przodu o s³owo
	M-b Do ty³u o s³owo

	C-n Nastêpna linia
	C-p Poprzednia linia

	C-a Pocz±tek linii
	C-e Koniec linii

	M-a Do ty³u na pocz±tek zdania
	M-e Do przodu na koniec zdania

>> Przeæwicz kilka razy dla wprawy wszystkie powy¿sze polecenia.
   Nale¿± one do najczê¶ciej u¿ywanych.

Dwa inne wa¿ne polecenia przesuwaj±ce kursor to M-< (Meta lub Alt
i znak mniejszo¶ci), które przesuwa kursor na pocz±tek ca³ego tekstu
i M-> (Meta lub Alt i znak wiêkszo¶ci), które przesuwa kursor na koniec
ca³ego tekstu.

Na wiêkszo¶ci klawiatur "<" jest nad przecinkiem, musisz wiêc u¿yæ
klawisza Shift, by nacisn±æ "<", i podobnie musisz u¿yæ klawisza Shift,
by nacisn±æ M-<. Bez Shift uzyska³by¶ M-przecinek.

>> Naci¶nij M-<, by przej¶æ na pocz±tek samouczka, a potem kilka razy
   u¿yj C-v, by powróciæ do tego miejsca.

>> Teraz naci¶nij M->, by przej¶æ na koniec samouczka, i wróæ do tego
   miejsca za pomoc± kilkakrotnego M-v.

Je¶li Twoja klawiatura ma klawisze strza³ek, to mo¿esz ich u¿yæ do
przesuwania kursora. Radzimy Ci nauczyæ siê siê kombinacji C-b, C-f,
C-n i C-p z trzech powodów. Po pierwsze, dzia³aj± one na wszystkich
typach terminali. Po drugie, gdy ju¿ zdobêdziesz pewn± praktykê w
pos³ugiwaniu siê Emacsem, to bêdzie Ci szybciej nacisn±æ te kombinacje
ni¿ klawisze strza³ek (poniewa¿ nie wymaga to przenoszenia d³oni z
miejsca, które zajmuj± podczas szybkiego pisania za pomoc± 10 palców).
Po trzecie wreszcie, gdy ju¿ wyrobisz sobie zwyczaj pos³ugiwania siê
tymi poleceniami z klawiszem Control, to ³atwo przyjdzie Ci nauczyæ siê
bardziej zaawansowanych poleceñ przesuwaj±cych kursor.

Wiêkszo¶æ poleceñ Emacsa akceptuje argument liczbowy; dla wiêkszo¶ci
poleceñ oznacza on liczbê powtórzeñ. Aby okre¶liæ liczbê powtórzeñ
polecenia, powiniene¶ je poprzedziæ naci¶niêciem C-u a potem cyfr.
Je¶li masz na klawiaturze klawisz META (lub EDIT albo ALT), to
alternatywnym sposobem wprowadzenia argumentu liczbowego jest u¿ycie
tego klawisza i wciskanie cyfr argumentu. Radzimy jednak przyswoiæ
sobie metodê z klawiszem C-u, poniewa¿ dzia³a ona na wszystkich
terminalach.

Na przyk³ad C-u 8 C-f przesuwa kursor do przodu o osiem znaków.

>> Spróbuj u¿yæ C-n i C-p z argumentem liczbowym, by przesun±æ kursor
   do jednej z linii w pobli¿u tego zdania za pomoc± tylko jednego
   polecenia.

Wiêkszo¶æ poleceñ u¿ywa argumentu liczbowego jako liczby powtórzeñ.
Jest kilka poleceñ, które u¿ywaj± go w inny sposób. Do takich wyj±tków
nale¿± C-v i M-v. Je¶li poda siê im argument, to przesuwaj± zawarto¶æ
ekranu w górê lub w dó³ o podan± liczbê linii zamiast o tyle¿ ekranów.
Na przyk³ad C-u 4 C-v przewija ekran o 4 linie.

>> Spróbuj nacisn±æ C-u 8 C-v.

To powinno by³o przewin±æ ekran do góry o 8 linii. Je¶li chcia³by¶
przewin±æ ekran w dó³, to powiniene¶ podaæ argument przed poleceniem M-v.

Je¶li pracujesz w systemie z okienkowym trybem graficznym, jak X11
lub MS-Windows, to prawdopodobnie po lewej stronie okna Emacsa znajduje
siê prostok±tny obszar nazywany po angielsku "scrollbar", a po polsku
suwakiem. Za jego pomoc± mo¿esz przewijaæ tekst, u¿ywaj±c do tego myszy.

>> Spróbuj nacisn±æ ¶rodkowy klawisz myszy u góry pod¶wietlonego
   obszaru na suwaku. To powinno przewin±æ tekst do miejsca
   okre¶lonego przez wysoko¶æ, na której nacisn±³e¶ klawisz myszy.

>> Przesuñ mysz do miejsca oddalonego od górnego koñca suwaka o mniej
   wiêcej trzy linie i naci¶nij lewy klawisz myszy kilka razy.


* GDY EMACS JEST ZABLOKOWANY
----------------------------

Je¶li Emacs przestaje odpowiadaæ na Twoje polecenia, to mo¿esz go
bezpiecznie zatrzymaæ, przyciskaj±c C-g. Klawisza C-g mo¿esz te¿ u¿yæ do
przerwania polecenia, które zabiera zbyt wiele czasu.

Mo¿esz tak¿e u¿yæ C-g do anulowania argumentu liczbowego albo pocz±tku
polecenia, którego nie zamierzasz dokoñczyæ.

>> Napisz C-u 100 jako argument liczbowy, po czym naci¶nij C-g.
   Teraz naci¶nij C-f. Powinno to przesun±æ kursor zaledwie o
   jeden znak, poniewa¿ argument liczbowy anulowa³e¶ za pomoc± C-g.

Za pomoc± klawisza C-g mo¿esz te¿ anulowaæ skutki omy³kowego
wci¶niêcia klawisza <ESC>.


* ZABLOKOWANE POLECENIA
-----------------------

Pewne polecenia Emacsa s± ,,zablokowane'' -- po to, by pocz±tkuj±cy
u¿ytkownicy nie mogli ich wywo³aæ przez przypadek.

Je¶li wywo³asz jedno z zablokowanych poleceñ, to Emacs wypisze komunikat
informuj±cy o tym, co to za polecenie, i zapyta Ciê, czy istotnie chcesz
je wywo³aæ.

Je¶li naprawdê chcesz wywo³aæ to polecenie, to odpowiedz na pytanie,
naciskaj±c spacjê. Je¶li nie chcesz wywo³aæ zablokowanego polecenia,
to na pytanie odpowiedz, naciskaj±c n.

>> Napisz `C-x C-l' (co jest zablokowanym poleceniem) i odpowiedz n
   na zadane pytanie.


* OKNA
------

Emacs mo¿e mieæ otwartych kilka okien, z których ka¿de wy¶wietla
w³asny tekst. Pojêcie ,,okna'', je¶li chodzi o Emacsa, nie odnosi
siê do osobnego okienka systemu okienkowego, lecz do pojedynczego
panelu wewn±trz okienka systemowego. (Emacs mo¿e te¿ pracowaæ
na kilku oknach systemowych (X-oknach); w terminologii Emacsa
nazywaj± siê one ramkami. Opisane jest to poni¿ej.)

Na tym etapie lepiej jest siê nie zag³êbiaæ w techniki wykorzystuj±ce
kilka okien. Powiniene¶ jedynie wiedzieæ, w jaki sposób pozbyæ siê
nadmiaru okien, które mog± siê pojawiæ w wyniku wywo³ania Emacsowego
systemu pomocy albo niektórych poleceñ. Robi siê to w prosty sposób:

	C-x 1 Jedno okno (tzn. zlikwiduj wszystkie pozosta³e okna).

Kombinacja ta to klawisz Control-x, po którym wystêpuje cyfra 1.
Powiêksza ona okno, w którym jest kursor tak, by wype³ni³o ono ekran,
kasuj±c zarazem pozosta³e okna Emacsa.

>> Przesuñ kursor do tej linii i naci¶nij C-u 0 C-l.

(C-l, jak pamiêtasz od¶wie¿a zawarto¶æ ekranu. Je¶li temu poleceniu
poda siê argument liczbowy, to bêdzie to oznacza³o ,,od¶wie¿ zawarto¶æ
ekranu i umie¶æ bie¿±ca liniê o tyle linii od góry ekranu''. Tak wiêc,
C-u 0 C-1 oznacza ,,od¶wie¿ ekran, umieszczaj±c bie¿±ca liniê na samej
górze''.)

>> Naci¶nij Control-x 2
   Zauwa¿, ¿e okno siê kurczy, a jednocze¶nie pojawia siê nowe,
   wy¶wietlaj±ce ten sam tekst.

>> Naci¶nij C-x 1, a nowe okno zniknie.


* WSTAWIANIE I USUWANIE
-----------------------

Je¶li chcesz wstawiæ nowy tekst, to po prostu go napisz. Znaki, które da
siê wy¶wietliæ, takie jak A, 7, *, itd., Emacs traktuje jako tekst i
natychmiast wstawia do dotychczasowego tekstu. Aby wstawiæ znak nowej
linii, trzeba nacisn±æ klawisz <Return> (na maszynach do pisania tak
oznacza³o siê znak powrotu karetki).

Ostatnio napisany znak mo¿esz skasowaæ, naciskaj±c klawisz <Delback>.
Chodzi tu o klawisz, którego normalnie u¿ywasz do skasowania ostatnio
napisanego znaku. Na wiêkszo¶ci klawiatur wyró¿nia siê on wielko¶ci±,
le¿y nad klawiszem <Return> i jest oznaczony napisem "Delete", "Del"
albo "Backspace".

Je¶li masz na klawiaturze klawisz oznaczony "Backspace", to w³a¶nie on
jest wspomnianym <Delback>. Oprócz niego mo¿e jeszcze wystêpowaæ
klawisz oznaczony s³owem "Delete", ale to nie on pe³ni rolê <Delback>.

Mówi±c bardziej ogólnie, <Delback> usuwa znak bezpo¶rednio
poprzedzaj±cy bie¿±c± pozycjê kursora.

>> Sprawd¼ to teraz: wstaw kilka znaków, po czym usuñ je, kilka razy
   naciskaj±c <Delback>. Nie martw siê, ¿e zmieniasz w ten sposób
   niniejszy plik, w istocie nie zmieniasz g³ównego pliku samouczka.
   Pracujesz teraz na jego kopii.

Gdy linia tekstu staje siê zbyt d³uga, by zmie¶ciæ siê w jednym
wierszu ekranu, to jest ona ,,kontynuowana'' w wierszu nastêpnym.
Znak ,,backslash'' (`\') (albo - je¶li pracujesz w okienkowym
trybie graficznym - zagiêta strza³ka) umieszczony na prawym marginesie
wskazuje, ¿e dana linia jest kontynuowana w nastêpnym wierszu ekranu.

>> Wpisuj jaki¶ tekst tak d³ugo, a¿ dojdziesz do prawego marginesu, i
   potem nie przestawaj. Zauwa¿ysz, ¿e pojawi siê linia kontynuacji.

>> U¿yj klawisza <Delback>, by usun±æ znaki tekstu, tak by linia znowu
   mie¶ci³a siê na ekranie; linia kontynuacji zniknie.

Znak nowej linii mo¿na skasowaæ tak jak ka¿dy inny znak. Usuniêcie znaku
nowej linii miêdzy dwiema liniami spowoduje ich po³±czenie. Je¶li powsta³a
w wyniku tego linia tekstu jest zbyt d³uga, by zmie¶ciæ siê na szeroko¶æ
ekranu, to zostanie wy¶wietlona z lini± kontynuacji.

>> Przesuñ kursor na pocz±tek linii i naci¶nij <Delback>. Bie¿±ca
   linia zostanie po³±czona z poprzedni±.

>> Naci¶nij <Return>, by z powrotem wstawiæ znak nowej linii, który
   skasowa³e¶.

Jak ju¿ wiesz, wiêkszo¶æ poleceñ Emacsa mo¿na wywo³aæ z parametrem
liczby powtórzeñ; dotyczy to tak¿e znaków tekstu. Argument liczbowy
powoduje wstawienie znaku odpowiadaj±c± mu liczbê razy.

>> Wypróbuj to teraz -- naci¶nij C-u 8 *, a uzyskasz ********.

Nauczy³e¶ siê ju¿ wiêkszej czê¶ci podstawowych sposobów pisania oraz
poprawiania b³êdów. W Emacsie mo¿esz usuwaæ równie¿ ca³e s³owa lub
linie. Oto podsumowanie operacji usuwania znaków:

	<Delback> usuñ znak bezpo¶rednio przed kursorem
	C-d usuñ znak bezpo¶rednio za kursorem

	M-<Delback> wytnij s³owo bezpo¶rednio przed kursorem
	M-d wytnij s³owo bezpo¶rednio za kursorem

	C-k wytnij zawarto¶æ linii od kursora do jej koñca
	M-k wytnij wszystkie znaki od kursora do koñca zdania

Warto zauwa¿yæ, ¿e stosunek <Delete> i C-d do M-<Delete> i M-d
rozszerza analogiê wystêpuj±c± w zestawieniu C-f i M-f (<Delete> tak
naprawdê nie jest znakiem steruj±cym, ale nie jest to tutaj
istotne). C-k i M-k s± podobne do C-e i M-e w tym sensie, ¿e linie s±
odpowiednikami zdañ.


Oto metoda wycinania czê¶ci tekstu. Umie¶æ kursor na pocz±tku fragmentu,
który chcesz wyci±æ, i naci¶nij C-@ lub C-SPC (SPC-spacja). Teraz przejd¼
na drugi koniec wybranego fragmentu i naci¶nij C-w. To wytnie ca³y tekst
zawarty miêdzy punktami pocz±tkowym i koñcowym.

>> Przesuñ kursor na literê O na pocz±tku poprzedniego paragrafu.

>> Naci¶nij C-SPC. Emacs wy¶wietli "Mark set" (znacznik ustawiony)
   na dole ekranu.

>> Przesuñ kursor do litery o w s³owie ,,kursor'' w drugim zdaniu.

>> Naci¶nij C-w. Ta komenda wytnie ca³y fragment zaczynaj±cy siê od O,
   a koñcz±cy tu¿ przed o.

Gdy usuwasz wiêcej ni¿ jeden znak naraz, Emacs zachowuje usuniêty
tekst po to, by móg³ go z powrotem gdzie¶ wstawiæ. Wstawianie
usuniêtego tekstu nazywa siê ,,wklejaniem''. Usuniêty tekst
mo¿esz wkleiæ zarówno w to samo miejsce, z którego zosta³ usuniêty,
b±d¼ te¿ w inne miejsca. Ten sam tekst mo¿esz wkleiæ wielokrotnie,
w celu uzyskania wielu kopii. Poleceniem wklejenia tekstu jest C-y.

Zauwa¿ ró¿nicê miêdzy ,,wycinaniem'' i ,,usuwaniem'', polegaj±c± na tym,
¿e rzeczy wyciête mo¿na na nowo wklejaæ, usuniêtych natomiast wklejaæ nie
mo¿na. Na ogó³ polecenia Emacsa, które kasuj± du¿o tekstu, zachowuj± go,
podczas gdy polecenia, które po prostu kasuj± jeden znak albo puste
linie lub odstêpy, skasowanego tekstu nie zachowuj±.

>> Przesuñ kursor na pocz±tek linii, która nie jest pusta. Naci¶nij
   C-k, by wyci±æ tekst z tej linii.

>> Naci¶nij C-k jeszcze raz. Zauwa¿, ¿e wycina to znak nowej linii,
   który znajduje siê za ta lini±.

Zwróæ uwagê, ¿e pojedyncze C-k wycina zawarto¶æ linii, a powtórne C-k
wycina sam± liniê, tak ¿e pozosta³e linie przesuwaj± siê do góry. C-k
traktuje argument liczbowy w sposób specjalny: wycina ono tyle linii,
ile wynosi warto¶æ argumentu, ORAZ ich zawarto¶æ. To nie jest jedynie
powtórzenie kilka razy C-k. C-u 2 C-k wycina dwie linie wraz z ich
znakami nowej linii; dwukrotne naci¶niecie C-k nie zrobi³oby tego.

By odzyskaæ ostatnio wyciêty tekst i wstawiæ go w miejsce kursora,
naci¶nij C-y.

>> Twoja kolej. Naci¶nij C-y, by z powrotem wstawiæ tekst.

Zwróæ uwagê, ¿e je¶li naci¶niesz C-k kilka razy z rzêdu, to ca³y wyciêty
tekst zostanie zachowywany w jednym kawa³ku, tak ¿e pojedyncze C-y wklei
wszystkie linie.

>> Naci¶nij C-k kilka razy.

A by odzyskaæ ten wyciêty tekst...

>> ...naci¶nij C-y. Przesuñ potem kursor o kilka linii w dó³ i
   naci¶nij C-y jeszcze raz. Widzisz, ¿e wstawia to ten sam tekst.

Co zrobiæ, je¶li chcesz wstawiæ tekst, który wcze¶niej wyci±³e¶,
a potem wycinasz co¶ innego? C-y wstawia tekst ostatnio wyciêty.
Poprzedni fragment nie jest jednak stracony. Mo¿esz do niego wróciæ,
u¿ywaj±c polecenia M-y. Naciskaj±c C-y, wstawiasz tekst ostatnio
wyciêty, a naciskaj±c M-y, zastêpujesz ten tekst wyciêtym uprzednio.
Dalsze naciskanie M-y przywo³uje coraz wcze¶niejsze fragmenty tekstu.
Gdy dojdziesz do tekstu, którego szuka³e¶, po prostu kontynuuj edycjê
tekstu, pozostawiaj±c wklejony tekst tam, gdzie siê znajduje.

Naciskaj±c M-y wystarczaj±co wiele razy, dojdziesz do punktu,
z którego wystartowa³e¶ (czyli tekstu wyciêtego ostatnio).

>> Wytnij jak±¶ liniê, zmieñ pozycjê kursora i wytnij inn±. Naci¶nij
   potem C-y, by wstawiæ drug± z wyciêtych linii. Potem naci¶nij M-y
   i linia ta zostanie zast±piona przez t± pierwsz±. Naci¶nij M-y
   jeszcze kilka razy, by zobaczyæ, co siê dzieje. Powtarzaj to a¿
   do ponownego pojawienia siê drugiej z linii. Mo¿esz te¿ wypróbowaæ,
   co siê stanie, gdy polecenie M-y poprzedzisz argumentem dodatnim
   albo ujemnym.


* COFNIJ
--------

Je¶li wprowadzisz zmiany do tekstu, a potem dojdziesz do wniosku, ¿e
to by³a pomy³ka, to mo¿esz cofn±æ zmiany, wydaj±c polecenie ,,cofnij''
(ang. undo), C-x u.

C-x u cofa zmiany wprowadzone przez jedno polecenie; je¶li powtórzysz
C-x u kilka razy z rzêdu, to ka¿de powtórzenie cofa kolejne polecenie.

Od tej regu³y s± dwa wyj±tki: polecenia, które nie zmieniaj± tekstu nie
licz± siê jako polecenia, które mo¿na wycofaæ (dotyczy to zarówno
przesuniêæ kursora, jak i przewijania tekstu), oraz znaki wstawiane do
tekstu (np. litery) ³±czone s± w grupy do 20. (Redukuje to liczbê
naci¶niêæ C-x u, które musia³by¶ wykonaæ, by wycofaæ siê z niechcianych
zmian.)

>> Wytnij tê liniê za pomoc± C-k, a potem naci¶nij C-x u; linia
   powinna siê pojawiæ ponownie.

C-_ jest innym sposobem wywo³ania polecenia "cofnij"; dzia³a to
dok³adnie tak samo jak C-x u, jest jednak ³atwiejsze do naci¶niêcia
kilka razy z rzêdu. Wad± kombinacji C-_ jest to, ¿e nie jest oczywiste
w jaki sposób j± uzyskaæ na niektórych klawiaturach. To w³a¶nie dlatego
dostêpna jest te¿ kombinacja C-x u. Na niektórych terminalach mo¿esz
nacisn±æ C-_ poprzez przytrzymanie Ctrl i naci¶niêcie /.

Argument liczbowy podany przed C-_ lub C-x u okre¶la liczbê powtórzeñ
tego polecenia.


* PLIKI
-------

Aby edytowany przez Ciebie tekst zosta³ na trwa³e zachowany, musisz
umie¶ciæ go w pliku. Je¶li tego nie zrobisz, to tekst zniknie, gdy
zamkniêty zostanie Emacs, za pomoc± którego go edytowa³e¶. Aby zachowaæ
tekst w pliku, najpierw musisz ten plik ,,znale¼æ'', i to zanim
zaczniesz wprowadzaæ tekst. Czynno¶æ znajdowania pliku (ang. "file
finding") bywa te¿ nazywana ,,odwiedzaniem pliku'' (ang. "file
visiting").

Odwiedzanie pliku w Emacsie powoduje wy¶wietlenie jego zawarto¶ci.
Bardzo czêsto jest to pocz±tek edycji pliku. Jednak¿e zmiany, które
wprowadzasz do pliku, nie s± w nim utrwalone, zanim go nie ,,zachowasz''
(ang. save). Ma to zapobiec pozostawieniu w systemie pliku, który zosta³
zmieniony tylko w po³owie, a tego chcesz unikn±æ. Gdy zachowujesz
zmieniony plik, Emacs zostawia orygina³ (pod inna nazw±) na wypadek,
gdyby¶ doszed³ do wniosku, ¿e wprowadzone zmiany by³y b³êdne.

Je¶li popatrzysz na dó³ ekranu, to zauwa¿ysz liniê, która zaczyna siê
i koñczy my¶lnikami, a zawiera tekst ,,TUTORIAL''. W tej
czê¶ci ekranu zawsze mo¿esz znale¼æ nazwê pliku, który w³a¶nie
odwiedzasz. W tej chwili odwiedzasz plik o nazwie TUTORIAL, który
jest Twoj± w³asn± kopi± samouczka Emacsa. Obojêtnie, który plik
odwiedzisz, w³a¶nie w tym miejscu pojawi siê jego nazwa.

Polecenia s³u¿±ce do odwiedzania i zachowywania plików ró¿ni± siê
od innych poleceñ, które ju¿ pozna³e¶, tym, ¿e sk³adaj± siê z dwóch
znaków. Obydwa zaczynaj± siê od znaku Control-x. Jest mnóstwo
poleceñ, które zaczynaj± siê od tego w³a¶nie znaku; wiele z nich
dotyczy plików, buforów oraz rzeczy z nimi zwi±zanych. Polecenia
te maj± d³ugo¶æ dwóch, trzech lub czterech znaków.

Kolejn± nowo¶ci± odno¶nie polecenia odwiedzania pliku jest to, ¿e
musisz mu podaæ nazwê pliku, który chcesz znale¼æ. Mówimy o tym, ¿e
polecenie ,,czyta argument z terminala'' (w tym wypadku argument jest
nazw± pliku). Po wpisaniu polecenia

	C-x C-f znajd¼ plik (ang. find a file)

Emacs poprosi Ciê o wpisanie nazwy pliku. Pojawia siê ona w dolnej linii
ekranu. Gdy ta linia jest u¿ywana do wprowadzania tego typu danych,
nazywa siê j± ,,minibuforem'' (ang. "minibuffer"). Do edycji nazwy pliku
w minibuforze mo¿esz u¿ywaæ zwyk³ych poleceñ Emacsa.

Wprowadzanie nazwy pliku (lub jakichkolwiek innych danych w
minibuforze) mo¿na anulowaæ klawiszem C-g.

>> Naci¶nij C-x C-f, po czym naci¶nij C-g. Na skutek tego zniknie
   minibufor oraz przerwane zostanie wykonanie polecenia C-x C-f, które
   tego minibufora u¿ywa³o. W rezultacie nie odwiedzisz ¿adnego pliku.

Gdy skoñczysz wpisywaæ nazwê pliku, naci¶nij <Return>. Wówczas
polecenie C-x C-f zabierze siê do roboty i znajdzie plik, który
wybra³e¶. Z chwil± zakoñczenia wykonywania polecenia C-x C-f
zniknie te¿ minibufor.

Zawarto¶æ znalezionego pliku po chwili pojawia siê na ekranie
i mo¿esz j± edytowaæ. Gdy chcesz zachowaæ zmiany, by je utrwaliæ,
wydaj polecenie

	C-x C-s zachowaj plik (ang. save).

Kopiuje to tekst z Emacsa do pliku. Za pierwszym razem, gdy to
robisz, Emacs zmienia nazwê oryginalnego pliku, dodaj±c na
koñcu jego nazwy znak ~. W ten sposób powstaje zapasowa kopia
oryginalnego pliku.

Gdy zachowywanie pliku siê koñczy, Emacs wypisuje jego nazwê u do³u
ekranu. Pliki powiniene¶ zachowywaæ stosunkowo czêsto, aby nie straciæ
za du¿o w wypadku za³amania systemu.

>> Naci¶nij C-x C-s, by zachowaæ dla siebie kopiê samouczka. Emacs
   powinien wypisaæ "Wrote ...TUTORIAL" na dole ekranu.

Odwiedziæ w celu edycji lub odczytu mo¿esz plik istniej±cy ju¿ w
systemie. Mo¿esz te¿ odwiedziæ plik, którego jeszcze nie ma w systemie i
w³a¶nie w taki sposób tworzy siê w Emacsie nowe pliki. Gdy poleceniem
C-x C-f odwiedzisz plik o nazwie nieistniej±cej w systemie, wówczas
Emacs wy¶wietli puste miejsce, do którego bêdziesz móg³ zacz±æ wpisywaæ
tekst. Gdy za¿±dasz zachowania wpisanego tekstu, Emacs utworzy w
systemie plik z tym tekstem. Od tego momentu mo¿esz uwa¿aæ, ¿e edytujesz
plik ju¿ istniej±cy.


* BUFORY
--------

Je¶li za pomoc± C-x C-f odwiedzisz inny plik, to plik odwiedzony
poprzednio pozostanie w Emacsie. Mo¿esz siê na niego prze³±czyæ,
odwiedzaj±c go jeszcze raz za pomoc± C-x C-f. W ten sposób mo¿esz
mieæ w Emacsie odwiedzonych jednocze¶nie wiele plików.

>> Utwórz plik o nazwie "foo" za pomoc± C-x C-f foo <Return>.
   Wpisz w niego jaki¶ tekst i zachowaj "foo" za pomoc± C-x C-s.
   W koñcu napisz C-x C-f TUTORIAL <Return>, by wróciæ do samouczka.

Emacs przechowuje tekst ka¿dego pliku w obiekcie, zwanym ,,buforem''.
Odwiedzenie pliku powoduje utworzenie nowego bufora wewn±trz Emacsa. By
zobaczyæ listê buforów, które istniej± w Twoim Emacsie, naci¶nij

	C-x C-b lista buforów (ang. list buffers).

>> Naci¶nij C-x C-b.

Zwróæ uwagê, ¿e ka¿dy bufor ma w³asn± nazwê, mo¿e te¿ mieæ skojarzon± z
nim nazwê pliku, który odwiedza. KA¯DY tekst, który ogl±dasz w Emacsie,
jest zawsze czê¶ci± jednego z buforów.

>> Naci¶nij C-x 1 by pozbyæ siê listy buforów.

Je¶li masz kilka buforów to tylko jeden z nich jest aktualny, ten
który w³a¶nie edytujesz. Je¶li chcesz edytowaæ inny bufer musisz siê
do niego "prze³±czyæ" (ang. switch). Je¶li chcesz prze³±czyæ siê do
bufora, który odwiedza jaki¶ plik, mo¿esz to zrobiæ poprzez ponowne
odwiedzenie pliku za pomoc± C-x C-f. Ale istnieje tak¿e ³atwiejszy
sposób: u¿yj C-x b. U¿ywaj±c tej komendy musisz podaæ nazwê bufora, do
którego zamierzasz siê prze³±czyæ.

>> Naci¶nij C-x b foo <Return> by wróciæ do bufora "foo", który
   przechowuje tekst pliku "foo". Nastêpnie naci¶nij C-x b TUTORIAL
   <Return> by wróciæ do samouczka.

Zwykle nazwa bufora odpowiada nazwie pliku (bez ¶cie¿ki), choæ czasami
zdarza siê inaczej. Lista buforów, któr± tworzysz za pomoc± C-x C-b
pokazuje nazwy wszystkich buforów.

KA¯DY tekst, który pojawia siê w oknie Emacsa jest czê¶ci± jakiego¶
bufora.  Niektóre bufory nie odpowiadaj± ¿adnemu odwiedzanemu
plikowi. Na przyk³ad bufor "*Buffer List*" nie odwiedza ¿adnego pliku;
zawiera on listê buforów, utworzon± w reakcji na naci¶niêcie przez
Ciebie C-x C-b. Bufor "*Messages*" tak¿e nie odwiedza ¿adnego pliku;
zawiera komunikaty, które pojawia³y siê podczas Twojej sesji z
Emacsem.

>> Naci¶nij C-x b *Messages* <Return> by obejrzeæ bufor zawieraj±cy
   komunikaty. Nastêpnie naci¶nij C-x b TUTORIAL <Return> by wróciæ do
   samouczka.

Je¶li zmieniasz tekst w jakim¶ pliku, a potem odwiedzisz inny plik, to
zawarto¶æ tego pierwszego NIE jest automatycznie zachowywana. Zmiany,
które wprowadzi³e¶, pozostaj± w Emacsie, w buforze tego¿ pliku.
Tworzenie czy edytowanie innego bufora nie ma ¿adnego wp³ywu na
pozosta³e. Jest to bardzo przydatne, ale te¿ oznacza, ¿e potrzebny jest
Ci wygodny sposób zachowywania zawarto¶ci buforów. Niewygodne na
przyk³ad by³oby, aby zawsze w celu zachowania bufora trzeba by³o do
niego przechodziæ za pomoc± C-x C-f i dopiero potem wywo³ywaæ C-x C-s.
Dlatego istnieje polecenie:

	C-x s Zachowaj bufory (ang. save some buffers)

W reakcji na polecenie C-x s Emacs dla ka¿dego z buforów, w którym
wystêpuj± nie zachowane do tej pory zmiany, zadaje pytanie, czy go
w tej chwili zachowaæ.

>> Wstaw jak±¶ liniê tekstu, a potem naci¶nij C-x s.
   Powiniene¶ zostaæ zapytany o to, czy chcesz zachowaæ bufor
   TUTORIAL. Odpowiedz na to pytanie twierdz±co, naciskaj±c y.


* ROZSZERZANIE ZESTAWU POLECEÑ
------------------------------

Poleceñ Emacsa jest znacznie, znacznie wiêcej, ni¿ mo¿na by skojarzyæ
z klawiszami klawiatury, uwzglêdniaj±c nawet kombinacje z META lub Ctrl.
Emacs radzi sobie z tym problemem, udostêpniaj±c polecenia X (ang.
eXtend). Istniej± dwa rodzaje tych poleceñ:

	C-x Rozszerzenie o znak. Nastêpuje po nim jeden znak.
	M-x Rozszerzenie o nazwane polecenie. Nastêpuje po nim
	    pe³na, niekiedy d³uga nazwa polecenia.

Polecenia te s± u¿yteczne, ale u¿ywa siê ich nie tak czêsto, jak tych,
których ju¿ siê nauczy³e¶. Mia³e¶ ju¿ okazjê poznaæ dwa z nich: C-x C-f,
s³u¿±ce do odwiedzania plików, oraz C-x C-s do ich zachowywania. Innym
przyk³adem mo¿e byæ polecenie C-x C-c, które koñczy sesjê Emacsa. (Nie
martw siê, ¿e w ten sposób stracisz zmiany, które wprowadzi³e¶ do
tekstów; przed zamkniêciem sesji Emacs proponuje Ci zachowania
ka¿dego ze zmodyfikowanych plików.)

C-z jest poleceniem, które wychodzi z Emacsa *na chwilê*, tak by¶ móg³
wróciæ do niej wróciæ po jakim¶ czasie.

W systemach, w których jest to mo¿liwe, C-z zawiesza proces Emacsa;
powoduje to powrót do pow³oki (ang. shell), ale nie niszczy Emacsa.
W najpopularniejszych pow³okach mo¿esz wróciæ do Emacsa za pomoc±
polecenia `fg' lub `%emacs'.

W systemach, w których nie ma zawieszania procesów, C-z tworzy proces
podpow³oki (ang. "subshell"), który dzia³a pod Emacsem i daje Ci szansê
uruchamiania innych programów oraz powrotu do Emacsa po ich skoñczeniu; w
systemach tych C-z w istocie nie powoduje wyj¶cia z Emacsa i wówczas
normalnym poleceniem powrotu do Emacsa jest wyj¶cie z podpow³oki za
pomoc± polecenia "exit".

Polecenia C-x C-c powiniene¶ u¿ywaæ, gdy masz zamiar siê wylogowaæ.
Zalecane jest tak¿e wychodzenie z Emacsa wystartowanego na przyk³ad przez
programy obs³uguj±ce pocztê elektroniczn± lub innego rodzaju narzêdzia,
poniewa¿ mog± one nie wiedzieæ, jak sobie poradziæ z zawieszeniem
Emacsa. Jednak¿e w zwyk³ych okoliczno¶ciach, je¶li nie musisz
wylogowywaæ siê z systemu, korzystniej jest zawiesiæ Emacsa za pomoc±
C-z, ni¿ z niego wyj¶æ.

Istnieje wiele poleceñ zaczynaj±cych siê od C-x. Oto lista tych,
których ju¿ siê nauczy³e¶:

	C-x C-f odwied¼ plik
	C-x C-s zachowaj plik
	C-x C-b wy¶wietl listê buforów
	C-x C-c wyjd¼ z Emacsa
	C-x u cofnij

Poleceñ podawanych za pomoc± nazwy u¿ywa siê jeszcze rzadziej lub u¿ywa
siê tylko w niektórych trybach. Przyk³adem mo¿e byæ polecenie
replace-string, które zastêpuje jeden ³añcuch innym w ca³ym tek¶cie. Gdy
naciskasz M-x, Emacs czeka na dalszy ci±g polecenia, wy¶wietlaj±c na
dole ekranu (w minibuforze) napis "M-x". Powiniene¶ tam wpisaæ nazwê
polecenia, w tym wypadku replace-string. Wystarczy przy tym, ¿e napisz
jedynie repl s<Tab>; Emacs dokoñczy nazwê automatycznie. Wprowadzanie
nazwy zakoñcz naci¶niêciem klawisza <Return>.

Polecenie replace-string wymaga dwóch argumentów: ³añcucha, który ma
zostaæ zast±piony, i ³añcucha, który ma zostaæ wstawiony w miejsce tego¿.
Wpisywanie ka¿dego z tych ³añcuchów trzeba zakoñczyæ przyci¶niêciem
klawisza <Return>.

>> Przesuñ kursor do czystej linii, dwie linie poni¿ej tej.
   Naci¶nij M-x repl s<Return>zmieni<Return>zmodyfikuje<Return>.

   Zwróæ uwagê, jak ta linia siê zmieni³a: zast±pi³e¶ s³owem
   ,,zmodyfikuje'' ka¿de wyst±pienie s³owa z-m-i-e-n-i poni¿ej pocz±tkowej
   pozycji kursora.


* AUTOMATYCZNE ZACHOWYWANIE
---------------------------

Je¶li zmian wprowadzonych do pliku nie zachowasz, to mo¿esz je straciæ w
wypadku, gdy Twój komputer przestanie dzia³aæ. By Ciê przed tym
uchroniæ, Emacs okresowo zachowuje wprowadzone zmiany w specjalnym
pliku, który ma znak # na pocz±tku i na koñcu swojej nazwy. Przyjmijmy
na przyk³ad, ¿e Twój plik nazywa siê "hello.c". Odpowiadaj±cy mu plik
zachowywany automatycznie bêdzie nosi³ nazwê "#hello.c#". Gdy
zachowasz plik w zwyk³y sposób, Emacs skasuje plik
zachowany automatycznie.

Je¶li Twój komputer przestanie dzia³aæ, mo¿esz odzyskaæ Twoje dane z
pliku automatycznie zachowanego przez zwyk³e odwiedzenie tego pliku,
który edytowa³e¶ (a nie pliku automatycznie zachowanego!) i napisanie
M-x recover file<Return>. Gdy Emacs zapyta o potwierdzenie, to
dane zachowane automatycznie odzyskasz, je¶li odpowiesz yes<Return>.


* OBSZAR ECHA
-------------

Je¶li polecenia dla Emacsa wpisujesz dostatecznie wolno, bêd± one
pokazywane w specjalnym obszarze na dole ekranu, zwanym obszarem echa
(ang. echo area). Obszar echa zawiera ostatni± doln± liniê ekranu.


* LINIA STANU
-------------

Linia, która znajduje siê bezpo¶rednio nad obszarem echa, zwana jest
lini± trybu (ang. modeline). Pokazuje ona tekst podobny do
nastêpuj±cego:

--:** TUTORIAL (Fundamental)--L670--58%----------------

Linia ta podaje u¿yteczne informacje o stanie Emacsa i tekstu, który
edytujesz.

Wiesz ju¿, jakie jest znaczenie nazwy: oznacza ona plik,
który odwiedzi³e¶. --NN%-- informuje o bie¿±cej pozycji wewn±trz
tekstu; oznacza to, ¿e NN procent tekstu znajduje siê ponad górnym
brzegiem ekranu. Je¶li pocz±tek pliku znajduje siê na pocz±tku
ekranu, to zamiast liczby --00%-- zobaczysz w tym miejscu --Top--.
Podobnie dla koñca tekstu pojawi siê tam napis --Bot-- (ang. bottom).
Je¶li wy¶wietlasz tekst na tyle krótki, ¿e mie¶ci siê w
ca³o¶ci na ekranie, to linia trybu bêdzie zawiera³a napis --All--.

Litera L, po której wystêpuj± cyfry, tak¿e opisuje Twoj± bie¿±c±
pozycjê: cyfry oznaczaj± numer linii, na której obecnie ustawiony jest
kursor.

Gwiazdki blisko pocz±tku linii trybu oznaczaj±, ¿e wprowadzi³e¶ do
tekstu jakie¶ zmiany. Tu¿ po odwiedzeniu, a tak¿e po zachowaniu pliku
nie bêdzie w tym miejscu gwiazdek, lecz my¶lniki.

Wewn±trz nawiasów znajdziesz informacje na temat trybu edycji, w
którym w³a¶nie jest Emacs. Domy¶lnym trybem edycji nazywa siê
podstawowym (ang. fundamental); jest to tryb u¿ywanym w³a¶nie w
tej chwili. Jest to przyk³ad ,,trybu g³ównego'' (ang. major mode).

Emacs mo¿e dzia³aæ w wielu trybach g³ównych. Zosta³y one zaprojektowane,
aby u³atwiæ edycjê napisów w rozmaitych jêzykach programowania, takich
jak tryb Lisp czy C, oraz rodzajach tekstów, jak tryb tekstowy. W danej
chwili mo¿e byæ aktywny tylko jeden g³ówny tryb pracy i to jego nazwa
jest wy¶wietlana w linii trybu w miejscu, w którym teraz jest
"Fundamental".

Ka¿dy z g³ównych trybów edycyjnych mo¿e zmieniæ zachowanie niektórych
poleceñ. Na przyk³ad w Emacsie istniej± polecenia s³u¿±ce do tworzenia
komentarzy w programach. Skoro ka¿dy jêzyk programowania sam okre¶la,
jak powinien wygl±daæ komentarz, to ka¿dy z g³ównych trybów edycyjnych
musi wstawiaæ komentarze w odpowiedni sposób. Trybowi edycyjnemu
odpowiada nazwa polecenia, które mo¿esz wykonaæ, by prze³±czyæ siê w ten
tryb lub go wy³±czyæ. Przyk³adem mo¿e byæ M-x fundamental-mode, które
jest poleceniem prze³±czaj±cym tryb podstawowy.

Je¶li zamierzasz edytowaæ tekst w jêzyku angielskim, taki jak na
przyk³ad oryginalna wersja tego samouczka, to prawdopodobnie
powiniene¶ u¿yæ trybu tekstowego (ang. text mode).

>> Napisz M-x text-mode<Return>.

Nie musisz siê martwiæ, bo ¿adne z poleceñ, które do tej pory pozna³e¶,
nie zmienia Emacsa w powa¿ny sposób. Mo¿esz jednak zauwa¿yæ, ¿e teraz
M-f i M-b traktuj± apostrofy jako czê¶ci s³ów. Poprzednio, w trybie
podstawowym, polecenia te traktowa³y apostrofy jako separatory s³ów.

G³ówne tryby edycji wprowadzaj± zwykle subtelne zmiany, takie jak
opisana powy¿ej; wiêkszo¶æ poleceñ nadal robi ,,to samo'', chocia¿
byæ mo¿e w troszeczkê inny sposób.

By zobaczyæ dokumentacjê na temat bie¿±cego g³ównego trybu edycji,
naci¶nij C-h m.

>> Naci¶nij C-u C-v raz lub wiêcej razy, tak by ta linia znalaz³a siê
   blisko góry ekranu.

>> Naci¶nij C-h m, by odczytaæ dokumentacjê na temat tego, czym tryb
   tekstowy ró¿ni siê od trybu podstawowego.

>> Naci¶nij q, by usun±æ dokumentacjê trybu z ekranu.

G³ówne tryby edycji nazywaj± siê w³a¶nie ,,g³ównymi'', gdy¿ wystêpuj±
tak¿e ,,podrzêdne'' tryby edycji (ang. minor modes). Podrzêdne tryby
edycji nie s± alternatyw± dla trybów g³ównych, lecz jedynie ich
niewielk± modyfikacj±. Ka¿dy podrzêdny tryb edycji mo¿na w³±czyæ lub
wy³±czyæ niezale¿nie od pozosta³ych trybów podrzêdnych, a tak¿e
niezale¿nie od trybu g³ównego. Mo¿esz wiec u¿ywaæ jednego,
kombinacji dowolnych, albo nie u¿ywaæ ¿adnego trybu podrzêdnego.

Jednym z podrzêdnych trybów edycji, który jest bardzo u¿yteczny,
szczególnie do edycji tekstu angielskiego lub polskiego, jest tryb
automatycznego wype³niania (ang. auto fill mode). Je¶li jest on
w³±czony, to Emacs ³amie linie pomiêdzy s³owami automatycznie, gdy
podczas wstawiania tekstu linia robi siê za szeroka.

Tryb automatycznego wstawiania w³±cza siê na przyk³ad poleceniem M-x
auto-fill-mode<Return>. Powtórzenie tego polecenie powoduje wy³±czenie
trybu, ponowne powtórzenie --- jego w³±czenie, i tak dalej. Mówimy, ¿e
polecenie ,,prze³±cza tryb''.

>> Napisz M-x auto-fill-mode<Return>. Wstaw potem wiele napisów
   ,,asdf '' tak d³ugo, a¿ zobaczysz, ¿e linia podzieli na dwie.
   Miêdzy literami musisz wstawiaæ spacje, poniewa¿ tryb
   automatycznego wype³niania ³amie linie tylko tam, gdzie s± spacje.

Margines jest zazwyczaj ustawiony na 70 znaków, ale mo¿esz to zmieniæ
poleceniem C-x f. Powiniene¶ poleceniu podaæ argument liczbowy
mówi±cy, w której kolumnie ma zostaæ ustawiony margines.

>> Wywo³aj C-x f z argumentem równym 20. (C-u 2 0 C-x f).
   Napisz potem jaki¶ tekst i zauwa¿, ¿e Emacs wype³nia linie do
   d³ugo¶ci co najwy¿ej 20 znaków. Ustaw margines z powrotem na
   70 znaków, wywo³uj±c jeszcze raz C-x f z odpowiednim argumentem.

Je¶li zmieniasz tekst wewn±trz akapitu, to tryb automatycznego
wype³niania sam z siebie nie wyrówna marginesu. Mo¿esz go wyrównaæ
samodzielnie, wydaj±c polecenie M-q (Meta-q) (kursor powinien siê
wówczas znajdowaæ wewn±trz akapitu).

>> Przesuñ kursor do poprzedniego akapitu i naci¶nij M-q.


* SZUKANIE
----------

Emacs potrafi szukaæ ³añcuchów (zwartych ci±gów znaków lub s³ów)
zarówno wstecz jak i do przodu. Szukanie ³añcucha jest poleceniem,
które przesuwa kursor --- do nastêpnego miejsca, w którym dany
³añcuch wystêpuje.

Polecenie Emacsa "search" ró¿ni siê od podobnych poleceñ w innych
edytorach tym, ¿e jest przyrostowe. Znaczy to, ¿e szukanie odbywa
siê w trakcie, gdy wpisujesz kolejne znaki ³añcucha, który ma zostaæ
znaleziony.

Poleceniami rozpoczynaj±cymi szukanie s±: C-s dla szukania w przód
oraz C-r dla szukania wstecz. POCZEKAJ PROSZÊ! Nie próbuj ich w tej
chwili.

Gdy naci¶niesz C-s, zauwa¿ysz, ¿e w obszarze echa pojawi siê
tekst "I-search". Jest to informacja, ¿e Emacs znajduje siê w trybie
"incremental search" i czeka, by¶ napisa³ tekst, który ma znale¼æ.
Naci¶niêcie <Return> koñczy proces szukania.

>> Rozpocznij teraz szukanie, naciskaj±c C-s. POWOLI, litera po
   literze, napisz s³owo kursor, zatrzymuj±c siê po ka¿dym znaku
   i obserwuj±c, gdzie zatrzymuje siê kursor. Gdy naci¶niesz drugie
   r, bêdzie mo¿na powiedzieæ, ¿e szuka³e¶ s³owa kursor
   jednokrotnie. Naci¶nij jeszcze raz C-s, by znale¼æ nastêpne
   wyst±pienie s³owa kursor. Naci¶nij teraz cztery razy <Delback>
   i zobacz, co siê dzieje z kursorem. Naci¶nij <Return>, by skoñczyæ
   szukanie.

Widzia³e¶, co siê dzia³o? Podczas szukania przyrostowego Emacs próbuje
przej¶æ do miejsca wyst±pienia ³añcucha, który wpisa³e¶ do tej pory,
i pod¶wietla go dla Twojej wygody. By znale¼æ nastêpne wyst±pienie
s³owa kursor, po prostu jeszcze raz naci¶nij C-s. Je¶li takiego
wyst±pienia nie ma, to Emacs zapiszczy i napisze, ¿e szukanie
,,skoñczy³o siê pora¿k±''.

Kombinacja C-g przerywa proces szukania, podobnie jak to czyni
z innymi poleceniami.

UWAGA: W niektórych systemach naci¶niecie C-s zamra¿a ekran i w
rezultacie Emacs nie mo¿e pokazywaæ tekstu. Oznacza to, ¿e sk³adowa
systemu operacyjnego, zwana kontrol± przep³ywu (ang. "flow control"),
przechwyci³a znak C-s i nie pozwoli³a mu dotrzeæ do Emacsa. By odzyskaæ
kontrolê nad ekranem, naci¶nij C-q. Dodatkowej pomocy poszukaj w
rozdziale "Spontaneous Entry to Incremental Search" w podrêczniku
Emacsa.

Je¶li podczas szukania przyrostowego naci¶niesz <Delback>, to zauwa¿ysz,
¿e w minibuforze znika ostatni znak wpisanego przez ciebie ³añcucha, a
kursor wraca do poprzedniego miejsca. Przypu¶æmy na przyk³ad, ¿e
nacisn±³e¶ k i znalaz³e¶ pierwsze wyst±pienie tej litery. Je¶li teraz
naci¶niesz u, to kursor przesunie siê tu¿ za najbli¿sze litery
ku. Naci¶nij teraz <Delback>. Spowoduje to skasowanie z wyszukiwanego
³añcucha litery u, a kursor wróci do pierwszego wyst±pienia litery k.

Je¶li podczas szukania naci¶niesz jaki¶ klawisz w kombinacji z META lub
Ctrl (z nielicznymi wyj±tkami --- znakami, które maj± specjalne
znaczenie podczas szukania, takimi jak C-s i C-r), to szukanie zostanie
przerwane.

C-s rozpoczyna proces szukania do przodu, czyli ZA bie¿±c± pozycj±
kursora. Je¶li chcesz szukaæ czego¶ po³o¿onego w tek¶cie wcze¶niej,
to naci¶nij C-r. Wszystko, co powiedzieli¶my o poleceniu C-s, stosuje
siê te¿ do C-r, oczywi¶cie w odniesieniu do szukania wstecz.


* WIELE OKIEN
-------------

Jedn± z u¿ytecznych cech Emacsa jest mo¿liwo¶æ wy¶wietlania wiêcej ni¿
jednego okna na raz.

>> Przesuñ kursor do tej linii i naci¶nij C-u 0 C-l.

>> Naci¶nij teraz C-x 2, co podzieli ekran na dwa okna. Obydwa okna
   wy¶wietlaj± ten samouczek. Kursor pozostaje w górnym oknie.

>> Naci¶nij C-M-v by przewin±æ dolne okno. (Je¶li nie masz
   klawisza Meta lub Alt, to naci¶nij ESC C-v.)

>> Naci¶nij C-x o ("o" jak angielskie "other") by przesun±æ kursor do
   dolnego okna. U¿yj C-v i M-v w dolnym oknie, by przewin±æ jego
   zawarto¶æ. Polecenia, które masz wykonaæ, odczytuj z górnego okna.

>> Naci¶nij C-x o jeszcze raz tak, by kursor wróci³ do górnego okna.
   Kursor w górnym oknie nie zmieni³ po³o¿enia.

Ka¿de okno pamiêta po³o¿enie swojego kursora, lecz w danej chwili
tylko jedno z okien wy¶wietla kursor. Wszystkie polecenia edycyjne
stosuj± siê do okna, w którym jest kursor. To okno nazywane jest
,,oknem wybranym''.

Polecenie C-M-v przyda Ci siê, gdy bêdziesz chcia³ edytowaæ tekst w
jednym oknie, a drugiego u¿ywa³ jako punktu odniesienia. Dziêki niemu
kursor mo¿e zawsze znajdowaæ siê w oknie, którego zawarto¶æ edytujesz, a
Ty mo¿esz przesuwaæ drugie okno.

C-M-v to przyk³ad kombinacji, który uzyskuje siê, wciskaj±c jednocze¶nie
klawisze Ctrl i Meta (Alt). Je¶li masz prawdziwy klawisz META (Alt), to
C-M-v mo¿esz uzyskaæ przytrzymuj±c jednocze¶nie Ctrl oraz META (Alt) i
naciskaj±c v. Nie jest wa¿ne, co zosta³o naci¶niête wcze¶niej, Ctrl czy
META, poniewa¿ obydwa te klawisze dzia³aj± jako modyfikatory znaczenia
znaków.

Je¶li nie masz klawisza META (Alt) i w jego zastêpstwie u¿ywasz ESC, to
kolejno¶æ naciskania klawiszy ma znaczenie: musisz najpierw nacisn±æ i
pu¶ciæ ESC, po czym nacisn±æ Ctrl-v; kombinacja Ctrl-ESC v nie zadzia³a.
Wynika to z tego, ¿e ESC jest znakiem, a nie modyfikatorem.

>> Naci¶nij C-x 1 (w górnym oknie), by pozbyæ siê okna dolnego.

(Je¶li nacisn±³by¶ C-x 1 w dolnym oknie, to górne by znik³o. Mo¿esz
sobie to polecenie t³umaczyæ jako ,,pozostaw tylko jedno okno --- to w
którym w³a¶nie jestem''.)

Nie musi byæ tak, ¿e obydwa okna pokazuj± ten sam bufor. Je¶li u¿yjesz
C-x C-f, by odwiedziæ jaki¶ plik w jednym z nich, to zawarto¶æ drugiego
siê nie zmieni. Z zasady w ró¿nych oknach mo¿esz niezale¿nie wy¶wietlaæ
ró¿ne pliki.

Oto inny sposób u¿ywania dwóch okien do wy¶wietlania dwóch ró¿nych
rzeczy:

>> Naci¶nij C-x 4 C-f i nazwê jednego z Twoich plików. Zakoñcz
   wprowadzanie klawiszem <Return>. Podany plik pojawi siê w dolnym
   oknie razem z kursorem, który tam przeskakuje.

>> Naci¶nij C-x o, by wróciæ do górnego okna, oraz C-x 1 by usun±æ
   dolne okno.


* REKURSYWNE POZIOMY EDYCJI
---------------------------

Czasami mo¿esz znale¼æ siê w czym¶, co nazywa siê "rekursywnym
poziomem edycji". Mo¿esz to rozpoznaæ po nawiasach kwadratowych w
linii trybu, obejmuj±cych nawiasy okr±g³e zawieraj±ce nazwê g³ównego
trybu edycji. Móg³by¶ na przyk³ad zobaczyæ [(Fundamental)] zamiast
(Fundamental).

By wyj¶æ z rekursywnego poziomu edycji, naci¶nij ESC ESC ESC. Jest to
ogólnego przeznaczenia polecenie ,,wychodzimy''. Mo¿esz go u¿yæ tak¿e,
by pozbyæ siê nadmiaru okien albo wyj¶æ z minibufora.

>> Naci¶nij M-x by wej¶æ do minibufora, potem naci¶nij ESC ESC ESC, by
   z niego wyj¶æ.

Aby wyj¶æ z rekursywnego poziomu edycji, nie wystarczy u¿yæ C-g. Dzieje
siê tak dlatego, ¿e klawisz C-g jest u¿ywany do anulowania poleceñ i
argumentów WEWN¡TRZ pojedynczego rekursywnego poziomu edycji.


SZUKANIE POMOCY
---------------

W tym samouczku dostarczyli¶my tylko tyle informacji, ile jest
niezbêdne, by¶ móg³ zacz±æ u¿ywaæ Emacsa. Emacs jest istn± kopalni±
najró¿niejszych rzeczy, których nie sposób tutaj opisaæ. Bêdziesz
zapewne chcia³ dowiedzieæ siê o Emacsie wiêcej, poniewa¿ posiada on
wiele po¿ytecznych cech, o których na razie nic nie wiesz. Miêdzy innymi
jest w nim zaszyte mnóstwo wewnêtrznej dokumentacji. Dotrzeæ do tej
dokumentacji mo¿esz po naci¶niêciu kombinacji C-h.

By uzyskaæ pomoc, naci¶nij C-h, a potem znak, który okre¶la jakiego
rodzaju pomocy oczekujesz. Je¶li poczujesz siê NAPRAWDÊ zagubiony, to
napisz C-h?, a Emacs podpowie, jakiego rodzaju pomocy mo¿e Ci
dostarczyæ. Je¶li naci¶niesz C-h, a potem zadecydujesz, ¿e pomoc nie
jest Ci jednak potrzebna, to aby anulowaæ zapocz±tkowane polecenie C-h,
po prostu wci¶nij C-g.

Najprostsz± pomoc mo¿esz uzyskaæ naciskaj±c C-h c. Naci¶nij C-h a potem
c, po czym kombinacjê klawiszy, której znaczenie chcesz poznaæ; Emacs
wy¶wietli krótki opis polecenia odpowiadaj±cego tej kombinacji.

>> Naci¶nij C-h c C-p.

Powinno to przywo³aæ komunikat, o tre¶ci podobnej do

	C-p runs the command previous-line

W ten sposób mo¿esz uzyskaæ ,,nazwê funkcji'' przypisanej kombinacji
klawiszy. Przydaje siê to podczas pisania kodu w Lispie, w którym
zapisane s± rozszerzenia Emacsa; wystarcza to tak¿e do przypomnienia
Ci, co dane polecenie robi, je¶li widzia³e¶ je ju¿ wcze¶niej, lecz
go nie zapamiêta³e¶.

Jako dope³nienie polecenia C-h c Emacs dopuszcza te¿ wieloznakowe
kombinacje klawiszy, na przyk³ad C-x C-s albo (je¶li nie masz klawisza
META lub Alt) <ESC>v.

By uzyskaæ wiêcej informacji na temat polecenia, naci¶nij C-h k
zamiast C-h c.

>> Naci¶nij C-h k C-p.

To polecenie wy¶wietla dokumentacjê na temat danej funkcji oraz jej
nazwê w oknie Emacsa. Gdy skoñczysz ¶ledziæ wynik tego polecenia
naci¶nij C-x 1, by pozbyæ siê tekstu pomocy. Nie musisz tego robiæ od
razu. Mo¿esz wykonaæ pewne operacje w oparciu o tekst pomocy zanim
naci¶niesz C-x 1.

Oto kilka innych u¿ytecznych wariantów C-h:

	C-h f Opisz funkcje o podanej nazwie.

>> Napisz C-h f previous-line<Return>. Wypisze to na ekranie ca³±
   informacje, jak± Emacs ma na temat funkcji, która implementuje
   polecenie C-p.

Podobnie komenda C-h v pokazuje na ekranie dokumentacjê zmiennych,
których warto¶ci mo¿esz zmieniæ, aby dostosowaæ Emacsa do swoich
preferencji. Wpisz nazwê zmiennej, gdy Emacs o ni± poprosi.


	C-h a 	Apropos. Wpisz s³owo, a Emacs wypisze listê
	      	wszystkich poleceñ, których nazwa zawiera to s³owo.
		Polecenia te mo¿na wywo³ywaæ za pomoc± Meta-x.
		Dla niektórych poleceñ Apropos wypisze jedno- lub
		dwuznakowe sekwencje, które wywo³uj± te polecenia.

>> Napisz C-h a file<Return>.

Zobaczysz listê wszystkich poleceñ,
dostêpnych za pomoc± M-x, które maja s³owo "file" w swojej nazwie.
Zauwa¿ysz tam tak¿e polecenia takie, jak C-x C-f oraz C-x C-w,
umieszczone obok nazw poleceñ "find-file" i "write-file".

>> Napisz C-M-v, aby przewin±æ okno pomocy. Zrób to kilka razy.
>> Napisz C-x 1, aby usun±æ okno pomocy.

	C-h i 	Czytanie elektronicznych podrêczników (w formacie Info). To
		polecenie prze³±czy Ciê do specjalnego bufora o nazwie
		*info*, gdzie bêdziesz móg³ przeczytaæ podrêczniki
		dotycz±ce pakietów zainstalowanych w Twoim
		systemie. Napisz m emacs <Return>, aby zapoznaæ siê z
		podrêcznikiem Emacsa. Je¿eli nigdy wcze¶niej nie u¿ywa³e¶
		trybu Info, to napisz ?, a Emacs przedstawi Ci mo¿liwo¶ci
		tego trybu. Po tym, jak zapoznasz siê z niniejszym krótkim
		samouczkiem, w dalszej pracy dostêp do dokumentacji
		bêdziesz uzyskiwa³ w³a¶nie za pomoc± Emacs Info.


DODATKOWE FUNKCJE
-----------------

Wiêcej o Emacsie mo¿esz siê nauczyæ czytaj±c jego podrêcznik, w formie
ksi±¿kowej lub on-line w postaci Info (u¿yj menu Help lub naci¶nij F10
h r). Dwie dodatkowe w³a¶ciwo¶ci, które szczególnie mog± siê przydaæ
to dope³nianie wprowadzanych danych i dired u³atwiaj±ce zarz±dzanie
plikami.

Dope³nianie pozwala unikn±æ niepotrzebnego wpisywania. Na przyk³ad
je¶li chcesz siê prze³±czyæ do bufora *Messages*, mo¿esz nacisn±æ C-x
b *M<Tab> a Emacs dope³ni dalsz± czê¶æ nazwy za Ciebie na tyle, na ile
bêdzie w stanie ustaliæ na podstawie tego, co do tej pory wpisa³e¶. Dope³nianie
jest opisane w Info w podrêczniku Emacsa w czê¶ci zatytu³owanej
"Dop³nianie" (ang. Completion).

Dired umo¿liwia Ci zrobienie wykazu plików w danym katalogu (dodatkowo
w podkatalogach), przemieszczanie siê wewn±trz tej listy, odwiedzanie
plików, zmienianie nazw, usuwanie i inne operacje na plikach. Dired
jest opisane w Info w podrêczniku Emacsa w czê¶ci zatytu³owanej
"Dired".

Podrêcznik dodatkowo opisuje wiele innych w³a¶ciwo¶ci Emacsa.


* KIEROWANIE KURSOREM Z X TERMINALA (akapit dodany przez autorów wersji polskiej)
-----------------------------------

Je¶li pracujesz na terminalu graficznym, to do kierowania kursorem
prawdopodobnie ³atwiej Ci bêdzie u¿ywaæ klawiszy strza³ek po prawej
stronie klawiatury. Klawisze strza³ek: w lewo, w prawo, w górê i w dó³
dzia³aj± zgodnie z oczekiwaniem; odpowiadaj± one dok³adnie C-b, C-f, C-p
i C-n, ale s± ³atwiejsze do zapamiêtania. Mo¿esz tak¿e u¿ywaæ C-lewo i
C-prawo, by przesuwaæ siê o s³owa, oraz C-góra i C-dó³, by przesuwaæ siê
o bloki (np. akapity, je¶li edytujesz tekst). Je¶li masz klawisze
oznaczone Home (lub Begin) oraz End, to przenios± Ciê one na pocz±tek i,
odpowiednio, na koniec linii, a C-Home i C-End na pocz±tek i koniec
pliku. Je¶li na Twojej klawiaturze s± klawisze PgUp i PgDn, to mo¿esz
ich u¿yæ do przesuwania siê o jeden ekran, tak jak M-v i C-v.

Wszystkie te polecenia akceptuj± argument liczbowy, tak jak to
opisano powy¿ej. Wpisanie argumentu mo¿esz sobie upro¶ciæ:
naci¶nij i trzymaj CONTROL lub META i wpisz liczbê. Na
przyk³ad, aby przesun±æ kursor o 12 s³ów w prawo, naci¶nij C-1 C-2
C-prawo. Zwróæ uwagê, ¿e jest to ³atwe do wpisania, poniewa¿ nie
musisz puszczaæ klawisza CONTROL podczas wpisywania cyfr.


* U¯YWANIE MENU (akapit dodany przez autorów wersji polskiej)
---------------

Je¶li pracujesz na X-terminalu, to u góry okna Emacsa powiniene¶ zauwa¿yæ
pasek z menu. Tego menu mo¿esz u¿ywaæ, by wywo³ywaæ najczê¶ciej
potrzebne polecenia Emacsa, takie jak "find file". Na pocz±tku bêdziesz
s±dzi³, ¿e jest to ³atwiejsze ni¿ u¿ywanie klawiatury, poniewa¿ nie
musisz siê na pamiêæ uczyæ kombinacji klawiszy, które uruchamiaj±
poszczególne polecenia. Gdy ju¿ jednak poznasz Emacsa, to zaczniesz
sobie te kombinacje przyswajaæ --- dla wygody przy pozycjach menu
pokazywane s± odpowiadaj±ce im kombinacje klawiszy.

Zwróæ uwagê, ¿e niektóre pozycje wystêpuj±ce w menu nie maj±
odpowiedników klawiszowych. Na przyk³ad pozycja "Buffers" powoduje
wy¶wietlenie listy wszystkich dostêpnych buforów. Do ka¿dego z nich
mo¿esz siê prze³±czyæ, wybieraj±c jego nazwê, wy¶wietlon± pod pozycj±
Buffers.


PODSUMOWANIE
------------

Pamiêtaj, ¿e by wyj¶æ z Emacsa na sta³e, trzeba wydaæ polecenie C-x C-c.
By wyj¶æ do pow³oki na chwilê tak, by jeszcze Do Emacsa wróciæ, trzeba
u¿yæ C-z. (To nie dzia³a pod X-Windows, poniewa¿ tam nie ma prawdziwego
konceptu przej¶cia na chwilê do pow³oki. Zamiast tego C-z ,,ikonizuje''
okno Emacsa.)

Ten samouczek by³ pisany tak, by wszyscy nowi u¿ytkownicy mogli go
zrozumieæ. Je¶li co¶ pozostawi³ niejasnym, nie sied¼ cicho i nie
obwiniaj siebie, tylko daj nam znaæ!


KOPIOWANIE
----------

Niniejszy samouczek jest potomkiem w d³ugiej linii samouczków
Emacsa, która rozpoczyna siê od tego, który zosta³ napisany przez
Stuarta Cracrafta dla oryginalnego Emacsa. Zosta³ on zmodyfikowany we
wrze¶niu 1994 przez Bena Winga, który zaktualizowa³ go w celu uwzglêdnienia
pracy pod X-Windows.

Autorem pierwszego t³umaczenia na jêzyk polski by³ Remek Trzaska
<remek@npac.syr.edu>, a pomaga³ mu Ryszard Kubiak
<rysiek@ipipan.gda.pl>. Tamto t³umaczenie zosta³o uaktualnione dla
wersji GNU Emacs 21 przez Beatê Wierzcho³owsk± <beataw@orient.uw.edu.pl>
z pomoc± Ryszarda Kubiaka i Janusza S. Bienia <jsbien@mail.uw.edu.pl>.

Ta wersja samouczka, podobnie jak GNU Emacs, jest chroniona prawem
autorskim, ale wolno j± kopiowaæ pod nastêpuj±cymi warunkami:

Copyright (C) 1985, 1994, 2001-2012  Free Software Foundation, Inc.

Zezwala siê na wykonywanie lub rozpowszechnianie
wiernych kopii tego dokumentu w otrzymanej formie, na dowolnym
no¶niku, pod warunkiem zachowania informacji o
prawach autorskich i niniejszym zezwoleniu oraz pod
warunkiem, ¿e dystrybutor udzieli odbiorcy pozwolenia na
dalsze rozpowszechnianie zgodnie z niniejszym zezwoleniem.


Zezwala siê równie¿ na rozpowszechnianie na warunkach podanych
powy¿ej zmodyfikowanych wersji tego dokumentu lub jego czê¶ci,
pod warunkiem, ¿e zostan± wyra¼nie uwidocznione
informacje o tym, kto dokona³ modyfikacji jako ostatni.


Warunki kopiowania samego Emacsa s± bardziej skomplikowane, ale zgodne
z t± ide±. Proszê, przeczytaj plik COPYING, po czym rozdaj swoim
znajomym kopie Emacsa. Pomó¿ têpiæ obstrukcjonizm w informatyce,
u¿ywaj±c, tworz±c i dziel±c siê oprogramowaniem swobodnym.

;;; Local Variables:
;;; mode: fundamental
;;; coding: latin-2
;;; sentence-end-double-space: nil
;;; End:

