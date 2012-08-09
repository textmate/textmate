Tutoriál k Emacsu.  Podmínky viz na konci.
Do èe¹tiny pøelo¾il Milan Zamazal <pdm@zamazal.org>.

Máte pøed sebou tutoriál k Emacsu.

Pøíkazy Emacsu obecnì vyu¾ívají klávesu CONTROL (obèas oznaèovanou CTRL nebo
CTL) nebo klávesu META (obèas oznaèovanou EDIT nebo ALT).  Abychom tyto názvy
nemuseli stále psát v plném znìní, budeme pou¾ívat následující zkratky:

 C-<chr>  znamená pøidr¾et klávesu CONTROL a stisknout znak <chr>.
          Tedy C-f znamená: pøidr¾te klávesu CONTROL a stisknìte f.
 M-<chr>  znamená pøidr¾et klávesu META, EDIT nebo ALT a stisknout <chr>.
          Pokud ¾ádnou z kláves META, EDIT ani ALT nemáte, tak místo toho
          stisknìte a pus»te klávesu ESC a poté <chr>.  Klávesu ESC budeme
          znaèit <ESC>.

Dùle¾itá poznámka: práci s Emacsem ukonèíte stiskem C-x C-c (dva znaky).
Znaky ">>" na levém okraji znaèí místa, kde si máte vyzkou¹et pøíkaz.
Napøíklad:
<<Blank lines inserted here by startup of help-with-tutorial>>
[Prostøední èást obrazovky je prázdná zámìrnì. Text pokraèuje ní¾e.]
>>  Nyní stisknìte C-v (view next screen) pro posun na dal¹í obrazovku.
        (Smìle do toho, proveïte to pøidr¾ením klávesy CONTROL a stiskem v.)
	Od této chvíle byste toto mìli provádìt kdykoliv doètete zobrazenou
        obrazovku.

V¹imnìte si, ¾e pøi posuvu obrazovek v¾dy zùstávají zobrazeny dva øádky
z pøedchozí obrazovky; to poskytuje urèitou návaznost pøi postupném
ètení textu.

První vìc, kterou potøebujete vìdìt, je jak se v textu pohybovat
z jednoho místa na druhé.  U¾ víte, jak se posunout o jednu obrazovku
vpøed, pomocí C-v.  K pøechodu o obrazovku zpìt pou¾ijte M-v
(pøidr¾te klávesu META a stisknìte v nebo stisknìte <ESC>v, jestli¾e
nemáte ¾ádnou z kláves META, EDIT nebo ALT).

>>  Zkuste stisknout M-v a pak C-v, nìkolikrát to zopakujte.


* SHRNUTÍ
---------

K prohlí¾ení obrazovkových stránek jsou u¾iteèné následující pøíkazy:

	C-v	Posun o obrazovku vpøed
	M-v	Posun o obrazovku zpìt
	C-l	Smazání obrazovky a znovuzobrazení celého textu,
		 pøitom se text pod kurzorem pøesune ke støedu obrazovky.
		 (Jedná se o CONTROL-L a ne CONTROL-1.)

>> Najdìte kurzor a zapamatujte si, jaký text je kolem nìj.
   Pak stisknìte C-l.
   Najdìte kurzor znovu a v¹imnìte si, ¾e je kolem nìj tentý¾ text.


* ZÁKLADNÍ OVLÁDÁNÍ KURZORU
---------------------------

Pohyb mezi obrazovkami je u¾iteèný, ale jak se pøemístíte na konkrétní
místo v textu na obrazovce?

Je toho mo¾no dosáhnout nìkolika zpùsoby.  Nejzákladnìj¹ím zpùsobem je
pou¾ití pøíkazù C-p, C-b, C-f a C-n.  Ka¾dý z tìchto pøíkazù pøesune
kurzor na obrazovce o jeden øádek nebo sloupec v daném smìru.
Zde je tabulka znázoròující smìr posuvu kurzoru vyvolaný tìmito ètyømi
pøíkazy:

			  Pøedchozí øádek, C-p
				  :
				  :
      Dozadu, C-b .... Momentální pozice kurzoru .... Dopøedu, C-f
				  :
				  :
			 Následující øádek, C-n

>> Pøesuòte kurzor na prostøední øádek tohoto diagramu pomocí
   C-n nebo C-p.  Potom stisknìte C-l, abyste na obrazovce vidìli celý
   diagram vycentrován.

Pravdìpodobnì se vám budou tyto pøíkazy snadno pamatovat podle
poèáteèních písmen anglických názvù: P jako previous (pøedchozí),
N jako next (následující), B jako backward (zpìt), F jako forward (vpøed).
Jsou to základní pøíkazy pro pohyb kurzoru a budete je pou¾ívat
neustále, tak¾e by bylo velmi vhodné, kdybyste se je teï nauèili.

>> Proveïte nìkolikrát C-n, abyste kurzor pøesunuli na tento øádek.

>> Posuòte kurzor dovnitø øádku pomocí nìkolika C-f a pak nahoru stiskem C-p.
   Pozorujte, co C-p dìlá, kdy¾ je kurzor uprostøed øádku.

Ka¾dý øádek textu konèí znakem nového øádku, který jej oddìluje od øádku
následujícího.  Znakem nového øádku by mìl být ukonèen i poslední øádek
souboru (pøesto¾e to Emacs nevy¾aduje).

>> Vyzkou¹ejte C-b na zaèátku øádku.  Kurzor by se mìl pøesunout na konec
   pøedchozího øádku, nebo» jej tím pøesunete pøes znak nového øádku.

C-f funguje analogicky jako C-b, tj. na konci øádku dojde k pøesunu na
dal¹í øádek.

>> Proveïte nìkolik C-b, tak¾e uvidíte, kde se nachází kurzor.
   Pak provádìjte C-f, abyste se vrátili na konec øádku.
   Pak proveïte je¹tì jednou C-f, abyste se pøesunuli na následující
   øádek.

Kdy¾ kurzorem pøejdete pøes horní nebo dolní okraj obrazovky, posune se
text za pøíslu¹ným okrajem na obrazovku.  Tato vlastnost se nazývá
"scrollování".  Umo¾òuje pøemístit kurzor na libovolné místo v textu,
ani¾ by kurzor opustil obrazovku.

>> Zkuste posunout kurzor pod dolní okraj obrazovky pomocí C-n a pozorujte,
   co se stane.

Jestli¾e je posun po znacích pøíli¹ pomalý, mù¾ete se pohybovat po
slovech.  M-f (META-f) provádí posun o slovo vpøed a M-b provádí posun
o slovo zpìt.

>> Stisknìte nìkolikrát M-f a M-b.

Pokud se kurzor nachází uprostøed slova, M-f provede pøesun na konec
tohoto slova.  Nachází-li se kurzor v mezeøe mezi slovy, M-f provede
pøesun na konec následujícího slova.  M-b pracuje analogicky v opaèném
smìru.

>> Stisknìte nìkolikrát M-f a M-b prolo¾enì s C-f a C-b, abyste vidìli
   výsledky pøíkazù M-f a M-b provádìných z rùzných míst uvnitø slov a
   mezi nimi.

V¹imnìte si analogie mezi C-f a C-b na jedné stranì a M-f a M-b na
stranì druhé.  Znaky s klávesou META jsou velmi èasto vyu¾ívány pro operace
vztahující se k entitám definovaným jazykem (slova, vìty, odstavce),
zatímco znaky s klávesou CONTROL pracují na základních prvcích
nezávislých na tom, co zrovna editujete (znaky, øádky, apod.).

Tato analogie platí také pro øádky a vìty: C-a a C-e provádí pøesun
na zaèátek a konec øádku, M-a a M-e provádí pøesun na zaèátek a konec
vìty.

>> Zkuste nìkolikrát C-a a poté nìkolikrát C-e.
   Zkuste nìkolikrát M-a a poté nìkolikrát M-e.

V¹imnìte si, ¾e opakované C-a nedìlá nic, zatímco opakované M-a v¾dy
provádí posun o dal¹í vìtu. Principu analogie to sice pøíli¹
neodpovídá, ale pøesto je toto chování mo¾no pova¾ovat za pøirozené.

Pozice kurzoru v textu se také nazývá "bod" ("point").  Abychom to
parafrázovali, kurzor je vidìt na obrazovce v místì, kde je bod umístìn
v textu.

Zde je pøehled jednoduchých operací pro pohyb kurzoru vèetnì pøíkazù pro
pohyb mezi slovy a vìtami:

	C-f	Pøesun o znak vpøed
	C-b	Pøesun o znak zpìt

	M-f	Pøesun o slovo vpøed
	M-b	Pøesun o slovo zpìt

	C-n	Pøesun na následující øádek
	C-p	Pøesun na pøedchozí øádek

	C-a	Pøesun na zaèátek øádku
	C-e	Pøesun na konec øádku

	M-a	Pøesun zpìt na zaèátek vìty
	M-e	Pøesun vpøed na konec vìty

>> Vyzkou¹ejte si teï nìkolikrát v¹echny tyto pøíkazy pro procvièení.
   Jsou to nejpou¾ívanìj¹í pøíkazy.

Dal¹í dva dùle¾ité pøíkazy pro pohyb kurzoru jsou M-< (META men¹í-ne¾),
který provede pøesun na zaèátek celého textu, a M-> (META vìt¹í-ne¾),
který provede pøesun na konec celého textu.

Na vìt¹inì terminálù je "<" nad èárkou, tak¾e pro vyvolání tohoto znaku
musíte pou¾ít klávesu Shift.  Na tìchto terminálech je tedy nutno pou¾ít
klávesu Shift i v pøípadì pøíkazu M-<; bez klávesy Shift byste provedli
M-èárka.

>> Zkuste teï M-< pro pøesun na zaèátek tutoriálu.
   Pou¾ijte pak opakovanì C-v, abyste se opìt vrátili sem.

>> Zkuste teï M-> pro pøesun na konec tutoriálu.
   Pou¾ijte pak opakovanì M-v, abyste se opìt vrátili sem.

Kurzor mù¾ete pøesouvat také pomocí kurzorových kláves (klávesy
se ¹ipkami), pokud je vá¹ terminál má.  My v¹ak doporuèujeme nauèit se
C-b, C-f, C-n a C-p, a to ze tøí dùvodù.  Za prvé, tyto klávesy fungují
na v¹ech typech terminálù.  Za druhé, jakmile jednou získáte cvik
v pou¾ívání Emacsu, zjistíte, ¾e pou¾ívání tìchto CTRL znakù je
rychlej¹í ne¾ pou¾ívání kurzorových kláves (proto¾e nemusíte pøesouvat
ruku z psací pozice).  Za tøetí, zvyknete-li si pou¾ívat tyto CTRL-znak
pøíkazy, snadno se nauèíte pou¾ívat jiné pokroèilé pøíkazy pro pohyb
kurzoru.

Vìt¹ina pøíkazù Emacsu akceptuje numerický argument; ten pro vìt¹inu
pøíkazù slou¾í jako opakovaè.  Poèet opakování pøíkazu zadáte
prostøednictvím stisku C-u následovaného stiskem pøíslu¹ných èíslic pøed
vyvoláním pøíkazu.  Máte-li META (nebo EDIT èi ALT) klávesu, existuje
alternativní mo¾nost zadání numerického argumentu: pøidr¾te klávesu META
a stisknìte pøíslu¹né èíslice.  Doporuèujeme nauèit se C-u metodu,
proto¾e ta funguje na jakémkoliv terminálu.

Napøíklad C-u 8 C-f provede pøesun o osm znakù vpøed.

Vìt¹ina pøíkazù pou¾ívá numerický argument jako opakovaè.  Jisté
výjimeèné pøíkazy jej pou¾ívají jiným zpùsobem.  Mezi tyto výjimky patøí
C-v a M-v.  Dostanou-li numerický argument, posunou obrazovku nahoru
nebo dolù o odpovídající poèet øádkù místo obrazovek.  Napøíklad
C-u 4 C-v posune obrazovku o 4 øádky.

>> Zkuste teï stisknout C-u 8 C-v.

To by mìlo posunout obrazovku o 8 øádkù nahoru.  Pokud byste ji chtìli
posunout zpìt dolù, mù¾ete dát argument pøíkazu M-v.

Pou¾íváte-li X Window, mìli byste mít na levé stranì emacsového okna
vysokou obdélníkovou oblast, nazývanou scrollbar.  Mù¾ete pak text
posouvat klikáním my¹í na scrollbar.

>> Zkuste stisknout prostøední tlaèítko na vrcholu zvýraznìné oblasti
   uvnitø scrollbaru.  To by mìlo text posunout na pozici danou tím, jak
   vysoko nebo nízko jste kliknuli.

>> Zkuste pøi stisknutém prostøedním tlaèítku posouvat my¹í nahoru a
   dolù.  Uvidíte, jak se text posouvá nahoru a dolù podle toho, jak
   posouváte my¹í.


* KDY® EMACS NEREAGUJE
----------------------

Jestli¾e Emacs pøestane reagovat na va¹e pøíkazy, mù¾ete probíhající
èinnost bezpeènì zastavit pomocí C-g.  Pomocí C-g mù¾ete zastavit
pøíkaz, jeho¾ provádìní trvá pøíli¹ dlouho.

C-g mù¾ete pou¾ít také pro odstranìní numerického argumentu pøíkazu,
který nechcete dokonèit.

>> Stisknìte C-u 100 pro vytvoøení numerického argumentu 100 a pak
   stisknìte C-g.  Nyní stisknìte C-f.  Mìl by být proveden posun
   o právì jeden znak, proto¾e jste argument zru¹ili prostøednictvím
   C-g.

Pokud jste omylem stiskli <ESC>, mù¾ete se jej zbavit pomocí C-g.


* DEAKTIVOVANÉ PØÍKAZY
----------------------

Nìkteré pøíkazy Emacsu jsou "deaktivované" ("disabled"), aby je
zaèínající u¾ivatelé nemohli vyvolat náhodnì.

Pokud vyvoláte nìkterý z deaktivovaných pøíkazù, Emacs zobrazí hlá¹ení
oznamující, který pøíkaz to byl, s dotazem, zda chcete tento pøíkaz
provést.

Pokud opravdu chcete pøíkaz vyzkou¹et, stisknìte mezerník jako odpovìï
na tuto otázku.  Obyèejnì, jestli¾e nechcete deaktivovaný pøíkaz
provést, odpovìzte na tuto otázku pomocí "n".

>> Stisknìte C-x C-l (co¾ je deaktivovaný pøíkaz),
   pak na otázku odpovìzte n.


* OKNA
------

Emacs mù¾e mít nìkolik oken (windows), z nich¾ ka¾dé zobrazuje svùj
vlastní text.  Jak více oken pou¾ívat, objasníme pozdìji.  Nyní chceme
objasnit, jak se zbavit nadbyteèných oken a vrátit se do základní
jednookenní editace.  Je to jednoduché:

	C-x 1	Jedno okno (tj. zru¹ení v¹ech ostatních oken)

Tedy vlo¾ení CONTROL-x následované èíslicí 1.  C-x 1 roz¹íøí okno
obsahující kurzor pøes celou obrazovku.  Zru¹í to v¹echna ostatní okna.

>> Stisknìte C-h k C-f.
   Pozorujte, jak se aktuální okno zmen¹í a objeví se nové okno za
   úèelem zobrazení dokumentace k pøíkazu C-f.

>> Stisknìte C-x 1 a pozorujte, jak okno s dokumentací zmizí.


* VKLÁDÁNÍ A MAZÁNÍ
-------------------

Chcete-li vlo¾it text, prostì jej napi¹te.  Znaky, které vidíte,
jako A, 7, *, atd., jsou Emacsem chápány jako text a vkládány okam¾itì.
Pro vlo¾ení znaku nového øádku stisknìte <Return> (klávesu Enter).

Poslední znak, který jste napsali, mù¾ete smazat stiskem <Delete>.
<Delete> je klávesa, která mù¾e být na klávesnici oznaèena "Del".
V nìkterých pøípadech jako <Delete> slou¾í klávesa "Backspace", av¹ak ne
v¾dy!

Obecnìji, <Delete> ma¾e znak bezprostøednì pøed momentální pozicí
kurzoru.

>> Proveïte to teï -- napi¹te nìkolik znakù a pak je sma¾te nìkolika
   stisky <Delete>.  Nebojte se zmìn v tomto souboru; originální
   tutoriál se nezmìní.  Toto je va¹e osobní kopie.

Kdy¾ se øádek textu zvìt¹í natolik, ¾e pøesáhne jeden øádek obrazovky,
je zobrazen na více øádcích obrazovky.  Øádek textu, který pokraèuje na
dal¹ím øádku obrazovky, je indikován zpìtným lomítkem ("\") na pravém
okraji obrazovky.

>> Vkládejte text, a¾ dosáhnete pravého okraje, a pokraèujte ve vkládání.
   Objeví se vám pokraèovací øádek.

>> Pou¾ijte <Delete> pro smazání textu, a¾ se øádek textu opìt vejde na
   jeden øádek obrazovky.  Pokraèovací øádek zmizí.

Znak nového øádku mù¾ete smazat jako kterýkoliv jiný znak.  Smazání
znaku nového øádku mezi dvìma øádky zpùsobí jejich spojení do jediného
øádku.  Je-li výsledný øádek pøíli¹ dlouhý na to, aby se ve¹el na ¹íøku
obrazovky, bude zobrazen pokraèovacím øádkem.

>> Pøesuòte kurzor na zaèátek øádku a stisknìte <Delete>.  To tento
   øádek spojí s øádkem pøedchozím.

>> Stisknìte <Return> pro znovuvlo¾ení smazaného znaku nového øádku.

Vzpomeòte si, ¾e vìt¹ina pøíkazù Emacsu mù¾e dostat poèet opakování;
vèetnì textových znakù.  Opakování textových znakù je vlo¾í nìkolikrát.

>>  Vyzkou¹ejte si to teï -- stisknìte C-u 8 * pro vlo¾ení ********.

Teï u¾ znáte nejzákladnìj¹í zpùsoby, jak nìco v Emacsu napsat a jak
opravovat chyby.  Mù¾ete ov¹em také mazat po slovech nebo po øádcích.
Zde je shrnutí operací pro mazání textu:

	<Delete>     Smazání znaku bezprostøednì pøed kurzorem
	C-d   	     Smazání znaku následujícího za kurzorem

	M-<Delete>   Zru¹ení slova bezprostøednì pøed kurzorem
	M-d	     Zru¹ení slova následujícího za kurzorem

	C-k	     Zru¹ení textu od pozice kurzoru do konce øádku
	M-k	     Zru¹ení textu do konce aktuální vìty

V¹imnìte si, ¾e <Delete> a C-d, resp. M-<Delete> a M-d, roz¹iøují
paralelu zapoèatou C-f a M-f (pravda, <Delete> opravdu není CONTROL
znak, ale netrapme se tím).  C-k a M-k jsou jako C-e a M-e ve smyslu
vztahu øádkù k vìtám.

Libovolnou èást bufferu mù¾ete té¾ zru¹it následující metodou.
Pøesuòte se na jeden konec této èásti a stisknìte C-@ nebo C-SPC
(libovolnou z tìchto kombinací). (SPC oznaèuje mezerník.)  Pøesuòte
se na druhý konec této èásti a stisknìte C-w.  Text mezi tìmito
pozicemi bude zru¹en.

>> Pøesuòte kurzor na písmeno L na zaèátku pøedchozího odstavce.
>> Stisknìte C-SPC.  Emacs by mìl ve spodním øádku obrazovky
   zobrazit zprávu "Mark set".
>> Pøesuòte kurzor na písmeno c ve slovì "konec" na druhém øádku
   odstavce.
>> Stisknìte C-w.  Text zaèínající písmenem L a konèící pøed písmenem
   c bude zru¹en.

Uvìdomte si, ¾e rozdíl mezi "ru¹ením" ("killing") a "mazáním"
("deleting") je ten, ¾e "zru¹ené" vìci mohou být zpìt vhozeny, zatímco
"smazané" nikoliv.  Obecnì pøíkazy, které mohou smazat vìt¹í mno¾ství
textu, ukládají text, zatímco pøíkazy, které ma¾ou jediný znak nebo
pouze prázdné øádky a mezery, mazaný text neukládají.

>> Pøesuòte kurzor na zaèátek neprázdného øádku.
   Pak stisknìte C-k pro zru¹ení textu na tomto øádku.
>> Stisknìte C-k podruhé.  Uvidíte, ¾e to zru¹í znak nového øádku, který
   je za tímto øádkem.

V¹imnìte si, ¾e jedno C-k zru¹í obsah øádku a druhé C-k zru¹í øádek
samotný a posune v¹echny dal¹í øádky nahoru.  C-k zpracovává numerický
argument speciálnì: zru¹í odpovídající poèet øádkù VÈETNÌ jejich
obsahu.  To u¾ není opakování.  C-u 2 C-k zru¹í dva øádky a jejich
obsah; dvojitý stisk C-k by toto obvykle neudìlal.

Vracení textu zpìt se nazývá "vhazování" ("yanking").  (Pøedstavte
si opìtovné vhazování, vracení døíve odstranìného textu zpátky.)
Zru¹ený text mù¾ete vhodit buï na stejné místo, kde byl zru¹en,
nebo na jiné místo v bufferu, nebo dokonce i do jiného souboru.
Text mù¾ete vhodit i vícekrát, vytváøíte tak jeho dal¹í kopie.

Pøíkazem pro vhazování je C-y.  Tento pøíkaz vlo¾í poslední smazaný
text na pozici, na které se nachází kurzor.

>> Zkuste to; stisknìte C-y pro vhození textu zpìt.

Stisknete-li nìkolikrát C-k po sobì, v¹echen smazaný text je ulo¾en
spoleènì tak, aby bylo mo¾né vhodit zpìt v¹echny øádky najednou.

>> Stisknìte nìkolikrát C-k.

Nyní obnovte poslednì zru¹ený text:

>> Stisknìte C-y.  Pak posuòte kurzor o nìkolik øádkù ní¾e a stisknìte
   C-y znova.  Nyní vidíte, jak lze text kopírovat.

Co kdy¾ máte nìjaký text, který byste rádi vhodili zpìt a pak zru¹íte
nìco jiného?  C-y by vlo¾ilo poslední zru¹ený text.  Av¹ak pøedchozí
text není ztracen.  Mù¾ete jej získat zpìt pou¾itím pøíkazu M-y.  Poté,
co provedete C-y pro získání posledního zru¹eného textu, stisk M-y
vymìní tento vhozený text za pøedchozí zru¹ený text.  Dal¹ími a
dal¹ími stisky M-y dostáváte pøedcházející a pøedcházející zru¹ené
texty.  Kdy¾ dosáhnete textu, který hledáte, nemusíte s ním pro jeho
uchování nic dal¹ího provádìt.  Jednodu¹e vhozený text ponechejte, kde
je, a pokraèujte v editaci.

Pokud opakujete M-y dostateènì dlouho, dostanete se zpátky k výchozímu
bodu (poslednì zru¹enému textu).

>> Zru¹te øádek, pøesuòte kurzor nìkam jinam a zru¹te jiný øádek.
   Pak proveïte C-y pro vrácení druhého zru¹eného øádku.
   Pak proveïte M-y a vhozený øádek bude nahrazen prvním zru¹eným øádkem.
   Opakujte M-y a pozorujte, co dostáváte.  Pokraèujte v tom, dokud se
   znovu neobjeví druhý zru¹ený øádek a pak nìkolik dal¹ích.
   Chcete-li, mù¾ete zkusit pøedat M-y kladné a záporné argumenty.


* UNDO
------

Jestli¾e provedete v textu zmìnu a pak zjistíte, ¾e to byl omyl, mù¾ete
zmìnu vrátit pøíkazem undo, C-x u.

C-x u obvykle vrátí zmìny provedené jedním pøíkazem; pokud C-x u
zopakujete nìkolikrát za sebou, ka¾dé opakování vrátí jeden dal¹í
pøíkaz.

Jsou ale dvì výjimky: pøíkazy, které nemìní text, se nepoèítají (to
zahrnuje pøíkazy pro pohyb kurzoru a scrollování) a znaky vkládající
samy sebe jsou obvykle zpracovávány ve skupinách a¾ po 20.  (To je kvùli
tomu, aby se zredukoval poèet C-x u nutných pro vrácení vkládaného
textu.)

>> Zru¹te tento øádek pomocí C-k, stisknìte pak C-x u a øádek by se mìl
   znovu objevit.

Alternativní undo pøíkaz je C-_; pracuje stejnì jako C-x u, je v¹ak
ménì pracné jej aplikovat nìkolikrát za sebou.  Nevýhodou C-_ je, ¾e
na nìkterých klávesnicích není zøejmé, jak jej vyvolat.  To je dùvod,
proè nabízíme i C-x u.  Na nìkterých terminálech mù¾ete C-_ vyvolat
stiskem / pøi stisknutém CTRL.

Numerický argument pro C-_ a C-x u funguje jako poèet opakování.

Pomocí pøíkazu undo mù¾ete vrátit zru¹ený stejnì jako smazaný text.
Rozdíl mezi mazáním a ru¹ením textu ovlivòuje mo¾nost vhození tohoto
textu pomocí C-y, neovlivòuje mo¾nosti pøíkazu undo.


* SOUBORY
---------

Aby text, který editujete, zùstal trvale uchován, musíte jej ulo¾it do
souboru.  Jinak by byl po ukonèení Emacsu ztracen.  Svoji editaci
spojíte se souborem "vyhledáním" ("finding") souboru.  (Také se to
nazývá "nav¹tívení" ("visiting") souboru.)

Vyhledání souboru znamená, ¾e vidíte jeho obsah v Emacsu.  V mnoha
ohledech je to, jako byste editovali pøímo ten soubor.  Nicménì zmìny,
které prostøednictvím Emacsu èiníte, se nestanou trvalými, dokud tyto
zmìny do souboru "neulo¾íte" ("save").  Tím se zamezí nechtìnému ponechání
èásteènì zmìnìného souboru v systému.  Dokonce i kdy¾ soubor ulo¾íte,
Emacs uchová pùvodní soubor pod zmìnìným názvem pro pøípad, ¾e byste
zjistili, ¾e va¹e úpravy byly chybné.

Kdy¾ se podíváte do dolní èásti obrazovky, uvidíte øádek, který zaèíná a
konèí pomlèkami a na zaèátku má "2J:-- TUTORIAL.cs" nebo nìco podobného.
Tato èást obrazovky obvykle obsahuje jméno souboru, který je právì
nav¹tíven.  Zrovna teï máte nav¹tíven soubor nazvaný "TUTORIAL.cs",
který je va¹í osobní èmárací kopií tutoriálu Emacsu.  Kdy¾ v Emacsu
vyhledáte soubor, jeho jméno se objeví pøesnì na tom místì.

Pøíkazy pro vyhledávání a ukládání souborù se na rozdíl od ostatních
pøíkazù, které jste se zatím nauèili, skládají ze dvou znakù.  Oba
zaèínají znakem CONTROL-x.  Existuje celá øada pøíkazù zaèínajících na
CONTROL-x; mnoho z nich pracuje se soubory, buffery a podobnými vìcmi.
Tyto pøíkazy jsou dlouhé dva, tøi nebo ètyøi znaky.

Dal¹í vìcí ohlednì pøíkazu pro vyhledání souboru je to, ¾e musíte øíct,
které jméno souboru chcete.  Øíkáme, ¾e pøíkaz "ète argument
z terminálu" (v tomto pøípadì je argumentem jméno souboru).  Poté co
vyvoláte pøíkaz

	C-x C-f   Vyhledání souboru

Emacs se vás zeptá na jméno souboru.  Jméno souboru, které pí¹ete, se
objevuje ve spodním øádku obrazovky, který se v této situaci nazývá
minibuffer.  Pro editaci jména souboru mù¾ete pou¾ívat obvyklé editaèní
pøíkazy Emacsu.

Zadávání jména souboru (obecnì kterýkoliv vstup z minibufferu) mù¾ete
zru¹it pøíkazem C-g.

>> Stisknìte C-x C-f a pak C-g.  To minibuffer zru¹í a takté¾ to zru¹í
   pøíkaz C-x C-f, který minibuffer pou¾il.  Tak¾e nevyhledáte ¾ádný
   soubor.

Po napsání jména souboru stisknìte <Return>.
Pøíkaz C-x C-f pak zaène pracovat a vyhledá soubor, který jste zvolili.
Po skonèení pøíkazu C-x C-f minibuffer zmizí.

Po malé chvilce se obsah souboru objeví na obrazovce a mù¾ete jej
editovat.  Kdy¾ chcete zmìny trvale ulo¾it, pou¾ijte pøíkaz

	C-x C-s   Ulo¾ení souboru

To zkopíruje text z Emacsu do souboru.  Kdy¾ to provedete poprvé, Emacs
pøejmenuje pùvodní soubor na soubor s novým jménem, aby nebyl ztracen.
Nové jméno je vytvoøeno pøidáním "~" na konec pùvodního jména souboru.

Kdy¾ je ukládání dokonèeno, Emacs zobrazí jméno zapsaného souboru.
Mìli byste ukládat rozumnì èasto, abyste neztratili pøíli¹ mnoho práce
v pøípadì pádu systému.

>> Stisknìte C-x C-s pro ulo¾ení va¹í kopie tutoriálu.
   Mìlo by to zobrazit "Wrote ...TUTORIAL.cs" ve spodním øádku obrazovky.

Existující soubor mù¾ete vyhledat, abyste jej mohli prohlí¾et nebo
editovat.  Mù¾ete také vyhledat soubor, který je¹tì neexistuje.  To je
zpùsob, jakým lze vytvoøit soubor v Emacsu: vyhledejte soubor, který
bude na zaèátku prázdný a pak zaènìte vkládat text urèený pro tento
soubor.  Kdy¾ po¾ádáte o ulo¾ení, Emacs skuteènì vytvoøí soubor
s textem, který jste vlo¾ili.  Od té chvíle se pak mù¾ete cítit, jako
kdybyste editovali ji¾ existující soubor.


* BUFFERY
---------

Jestli¾e vyhledáte pomocí C-x C-f druhý soubor, první soubor v Emacsu
zùstává.  Mù¾ete se do nìj zpìt pøepnout jeho opìtovným vyhledáním
pomocí C-x C-f.  Tímto zpùsobem mù¾ete do Emacsu dostat pomìrnì hodnì
souborù.

>> Vytvoøte soubor pojmenovaný "foo" stiskem C-x C-f foo <Return>.
   Potom vlo¾te nìjaký text, zeditujte jej a ulo¾te "foo" stiskem C-x C-s.
   Nakonec stisknìte C-x C-f TUTORIAL.cs <Return>, èím¾ se vrátíte zpìt do
   tutoriálu.

Emacs ukládá text ka¾dého souboru do objektu nazývaného "buffer".
Vyhledání souboru vytvoøí v Emacsu nový buffer.  Chcete-li vidìt seznam
bufferù, které momentálnì existují ve va¹em procesu Emacs, stisknìte:

	C-x C-b   Seznam bufferù

>> Zkuste teï C-x C-b.

Podívejte se, ¾e ka¾dý buffer má v seznamu jméno a mù¾e tam mít také jméno
souboru, jeho¾ text obsahuje.  Nìkteré buffery neodpovídají souborùm.
Napøíklad buffer pojmenovaný "*Buffer List*" nemá ¾ádný soubor.  Je to
buffer, který obsahuje seznam bufferù vytvoøený pomocí C-x C-b.
JAKÝKOLIV text, který vidíte v emacsovém oknì, je v¾dy souèástí
nìjakého bufferu.

>> Stisknìte C-x 1, abyste se zbavili seznamu bufferù.

Pokud provedete zmìny textu jednoho souboru a pak vyhledáte jiný soubor,
nezpùsobí to ulo¾ení prvního souboru.  Jeho zmìny zùstávají v Emacsu
uchovány v jemu odpovídajícím bufferu.  Vytvoøení a editace druhého
souboru nemá ¾ádný vliv na buffer prvního souboru.  To je velmi
u¾iteèné, ale také to znamená, ¾e potøebujete vhodný zpùsob, jak ulo¾it
buffer prvního souboru.  Nutnost pøepnout se zpátky pomocí C-x C-f, aby
jej bylo mo¾no ulo¾it prostøednictvím C-x C-s, by byla nemístnì
obtì¾ující.  Tak¾e máme

	C-x s     Ulo¾ení nìkterých bufferù

C-x s se vás zeptá na ka¾dý buffer, který obsahuje zmìny, které jste
neulo¾ili.  Pro ka¾dý takový buffer se vás zeptá, zda jej má ulo¾it.

>> Vlo¾te øádek textu a pak stisknìte C-x s.
   Mìli byste být dotázáni, zda má být ulo¾en buffer nazvaný TUTORIAL.cs.
   Odpovìzte na tuto otázku ano (yes) stiskem "y".


* ROZ©IØOVÁNÍ SADY PØÍKAZÙ
--------------------------

Existuje mnohem, mnohem více pøíkazù Emacsu, ne¾ které by vùbec mohly
být rozmístìny na v¹echny CONTROL a META znaky.  Emacs tento problém
obchází prostøednictvím X (eXtend) pøíkazu.  Ten vzniká dvìma zpùsoby:

	C-x	Znakový eXtend.  Následován jedním znakem.
	M-x	Pojmenovaný pøíkaz eXtend.  Následován dlouhým názvem.

To jsou pøíkazy, které jsou obecnì u¾iteèné, av¹ak ménì èasto pou¾ívané
ne¾ ty, které jste se ji¾ nauèili.  U¾ jste vidìli dva z nich: souborové
pøíkazy C-x C-f pro vyhledání a C-x C-s pro ulo¾ení.  Jiný pøíklad je
pøíkaz pro ukonèení Emacsu -- tj. pøíkaz C-x C-c.  (Nemìjte obavy
o ztrátu zmìn, které jste provedli; C-x C-c nabídne ulo¾ení ka¾dého
zmìnìného souboru, ne¾ Emacs ukonèí.)

C-z je pøíkaz na *doèasné* opu¹tìní Emacsu -- mù¾ete se po nìm do
spu¹tìného Emacsu vrátit.

Na systémech, které to umo¾òují, C-z Emacs "pozastaví"; tzn. vrátí vás
do shellu, av¹ak Emacs neukonèí.  V nejbì¾nìj¹ích shellech se mù¾ete do
Emacsu vrátit pøíkazem `fg' nebo pomocí `%emacs'.

Na systémech, které pozastavování procesù nemají implementováno, C-z
vytvoøí subshell bì¾ící pod Emacsem, aby vám dal ¹anci spustit jiné
programy a pak se do Emacsu vrátit; neprovede tedy pravé opu¹tìní
Emacsu.  V tom pøípadì je obvyklou cestou návratu ze subshellu do Emacsu
shellovský pøíkaz `exit'.

Chvíle pro pou¾ití C-x C-c nastane, kdy¾ se chystáte odhlásit ze
systému.  Správné je to také pøi ukonèování Emacsu vyvolaného po¹tovním
programem a rùznými jinými utilitami, proto¾e ty nemusí vìdìt, jak si
poradit s pozastavením Emacsu.  Nicménì za normálních okolností, pokud
se nechystáte odlogovat, je lépe Emacs pozastavit pomocí C-z ne¾ jej
ukonèit.

Existuje mnoho C-x pøíkazù.  Zde je seznam tìch, které jste se ji¾ nauèili:

	C-x C-f		Vyhledání souboru
	C-x C-s		Ulo¾ení soubor
	C-x C-b		Seznam bufferù
	C-x C-c		Ukonèení Emacsu
	C-x u		Undo

Pojmenované eXtended pøíkazy jsou pøíkazy, které jsou pou¾ívány je¹tì
ménì, nebo pøíkazy, které jsou pou¾ívány jenom v jistých módech.
Pøíkladem je pøíkaz replace-string, který globálnì nahradí jeden øetìzec
jiným.  Kdy¾ stisknete M-x, vypí¹e se na spodním øádku obrazovky prompt
M-x a vy byste mìli zadat jméno pøíkazu; v tomto pøípadì
"replace-string".  Jednodu¹e napi¹te "repl s<TAB>" a Emacs název doplní.
Dokonèete zadávání jména pøíkazu pomocí <Return>.

Pøíkaz replace-string vy¾aduje dva argumenty -- øetìzec, který má být
nahrazen, a øetìzec, který jej má nahradit.  Ka¾dý argument musíte
ukonèit pomocí <Return>.

>> Pøesuòte kurzor na prázdný øádek dva øádky pod tímto.
   Pak napi¹te M-x repl s<Return>zmìnil<Return>modifikoval<Return>.

   V¹imnìte si, jak se tento øádek zmìnil: nahradili jste slovo
   z-m-ì-n-i-l slovem "modifikoval", kdekoliv se za aktuální pozicí
   kurzoru vyskytlo.


* AUTOMATICKÉ UKLÁDÁNÍ
----------------------

Jestli¾e jste provedli zmìny v souboru, ale nemáte je je¹tì ulo¾eny,
mohou být v pøípadì pádu systému ztraceny.  Aby vás Emacs od toho
uchránil, periodicky zapisuje "auto save" soubor pro ka¾dý soubor, který
editujete.  Jméno auto save souboru má na zaèátku a na konci #;
napøíklad jestli¾e se vá¹ soubor jmenuje "hello.c", jeho auto save
soubor se jmenuje "#hello.c#".  Kdy¾ soubor ulo¾íte normálním zpùsobem,
Emacs auto save soubor sma¾e.

Jestli¾e dojde k pádu systému, mù¾ete svoji editaci obnovit z auto-save
souboru, a to normálním vyhledáním souboru (toho, který jste editovali,
ne auto save souboru) a následnou aplikací M-x recover file<return>.
Na ¾ádost o potvrzení odpovìzte zadáním yes<return> pro pokraèování a
obnovení auto-save dat.


* ECHO OBLAST
-------------

Kdy¾ Emacs vidí, ¾e pí¹ete pøíkazy pomalu, ukazuje vám je ve spodní
èásti obrazovky v oblasti nazývané "echo oblast".  Echo oblast obsahuje
dolní øádek obrazovky.


* STAVOVÝ ØÁDEK
---------------

Øádek bezprostøednì nad echo oblastí se nazývá "stavový øádek" ("mode line").
Stavový øádek øíká nìco jako:

2J:** TUTORIAL.cs       (Fundamental)--L670--58%----------------

Tento øádek podává u¾iteènou informaci o stavu Emacsu a textu, který
editujete.

U¾ víte, co znamená jméno souboru -- je to soubor, který jste vyhledali.
-NN%-- oznaèuje va¹i aktuální pozici v textu; øíká, ¾e NN procent textu
je nad horním okrajem obrazovky.  Je-li zaèátek souboru na obrazovce, je
zde --Top-- a ne --00%--.  Je-li konec textu na obrazovce, je zde
--Bot--.  Jestli¾e se díváte na tak malý text, ¾e se celý vejde na
obrazovku, stavový øádek øíká --All--.

Hvìzdièky poblí¾ zaèátku znamenají, ¾e jste text zmìnili.  Tìsnì po
vyhledání nebo ulo¾ení souboru v této èásti stavového øádku nejsou ¾ádné
hvìzdièky, pouze pomlèky.

Èást stavového øádku v závorkách øíká, v jakých editaèních módech se
nacházíte.  Implicitní mód je Fundamental, co¾ je ten, který momentálnì
pou¾íváte.  Je pøíkladem hlavního módu ("major mode").

Emacs má celou øadu hlavních módù.  Nìkteré z nich jsou urèeny pro
editaci rùzných programovacích jazykù a/nebo textù jako tøeba Lisp mód,
Text mód, atd.  V libovolném okam¾iku je aktivní právì jeden hlavní mód a
jeho jméno lze nalézt ve stavovém øádku na místì, kde je teï
"Fundamental".

Ka¾dý hlavní mód mìní chování nìkterých pøíkazù.  Napøíklad existují
pøíkazy pro vytváøení komentáøù v programu, a proto¾e ka¾dý programovací
programovací jazyk má jinou pøedstavu o tom, jak má komentáø vypadat,
musí ka¾dý hlavní mód vkládat komentáøe jinak.  Ka¾dý hlavní mód je
vlastnì jméno extended pøíkazu, kterým se do tohoto módu mù¾ete
pøepnout.  Napøíklad M-x fundamental-mode je pøíkaz pro pøepnutí se do
Fundamental módu.

Chystáte-li se editovat èeský text, jako tøeba tento soubor,
pravdìpodobnì byste mìli pou¾ít Text mód.
>> Napi¹te M-x text-mode<Return>.

Nebojte se, ¾ádný z pøíkazù, které jste se nauèili, chování Emacsu nijak
významnì nezmìní.  Mù¾ete si ale v¹imnout, ¾e M-f a M-b nyní pracují
s apostrofy jako se souèástmi slov.  Pøedtím, ve Fundamental módu, M-f a
M-b pracovaly s apostrofy coby oddìlovaèi slov.

Hlavní módy obvykle dìlají men¹í zmìny, jako byla tato: pøíkazy vìt¹inou
dìlají "toté¾", ale v ka¾dém hlavním módu pracují tro¹ku jinak.

Dokumentaci k aktuálnímu hlavnímu módu si mù¾ete zobrazit stiskem C-h m.

>> Jednou nebo nìkolikrát pou¾ijte C-u C-v, abyste tento øádek dostali
   k vrcholu obrazovky.
>> Stisknìte C-h m, abyste vidìli, jak se Text mód li¹í od Fundamental
   módu.
>> Stisknìte C-x 1 pro odstranìní dokumentace z obrazovky.

Hlavní módy se nazývají hlavní proto, ¾e také existují vedlej¹í módy
(minor modes).  Vedlej¹í módy nejsou alternativou k hlavním módùm, nýbr¾
jejich malé modifikace.  Ka¾dý vedlej¹í mód mù¾e být zapnut nebo vypnut
sám o sobì nezávisle na v¹ech ostatních vedlej¹ích módech a nezávisle na
hlavním módu.  Tak¾e nemusíte pou¾ívat ¾ádný vedlej¹í mód nebo mù¾ete
pou¾ívat jeden vedlej¹í mód nebo libovolnou kombinaci nìkolika
vedlej¹ích módù.

Jedním z velmi u¾iteèných vedlej¹ích módù, zejména pro editaci èeských
textù, je Auto Fill mód.  Kdy¾ je tento mód zapnut, Emacs zalomí øádek
mezi dvìma slovy, kdykoliv vkládáte text a øádek se stane pøíli¹
dlouhým.

Auto Fill mód mù¾ete zapnout provedením M-x auto-fill-mode<Return>.
Je-li tento mód zapnut, mù¾ete jej vypnout provedením M-x
auto-fill-mode<Return>.  Je-li mód vypnut, tento pøíkaz jej zapíná,
a je-li mód zapnut, tak jej tento pøíkaz vypíná.  Øíkáme, ¾e tento
pøíkaz pøepíná ("toggles") tento mód.

>> Napi¹te teï M-x auto-fill-mode<Return>.  Pak vkládejte "asdf " stále
   dokola tak dlouho, a¾ uvidíte, jak se vkládaný øádek rozdìlí na dva
   øádky.  Do textu musíte vkládat mezery proto, ¾e Auto Fill mód
   zalamuje øádky pouze v mezerách.

Okraj je obvykle nastaven na 70 znakù, ale mù¾ete to zmìnit pøíkazem
C-x f.  Hodnotu okraje, kterou si pøejete, byste mìli pøedat jako
numerický argument.

>> Napi¹te C-x f s argumentem 20.  (C-u 2 0 C-x f).
   Pak pi¹te nìjaký text a pozorujte, jak Emacs vyplòuje øádky po
   20 znacích.  Pak nastavte okraj zpátky na 70 opìtovným pou¾itím
   C-x f.

Jestli¾e provedete zmìny uprostøed odstavce, Auto Fill mód jej
nepøeformátuje.
Pro pøeformátování odstavce stisknìte M-q (META-q) s kurzorem uvnitø
odstavce.

>> Pøesuòte kurzor do pøedchozího odstavce a stisknìte M-q.


* VYHLEDÁVÁNÍ
-------------

Emacs umí v textu vyhledávat øetìzce (tj. skupiny spojených znakù nebo
slov) smìrem vpøed nebo vzad.  Hledání øetìzce je pøíkaz pøesunující
kurzor; pøesune kurzor na nejbli¾¹í místo, kde se tento øetìzec nachází.

Vyhledávací pøíkaz Emacsu se li¹í od vyhledávacích pøíkazù vìt¹iny
editorù v tom smyslu, ¾e je "inkrementální".  To znamená, ¾e vyhledávání
se provádí u¾ v okam¾iku, kdy zadáváte vyhledávací øetìzec.

Pøíkaz pro zahájení hledání vpøed je C-s a pro hledání vzad C-r.
ALE POZOR!  Nezkou¹ejte to je¹tì.

Kdy¾ stisknete C-s, uvidíte v echo oblasti prompt "I-search".  To vám
øíká, ¾e Emacs se nachází ve stavu, který se nazývá inkrementální hledání,
a èeká, a¾ mu zadáte, co chcete hledat.  <RET> hledání ukonèí.

>> Nyní zahajte hledání stiskem C-s.  POMALU, písmeno po písmenu, pi¹te
   slovo 'kurzor'.  Po ka¾dém písmenu si v¹imnìte, co se dìje s kurzorem.
   Teï jste vyhledali "kurzor" poprvé.
>> Stisknìte C-s znovu, abyste nalezli dal¹í výskyt "kurzor".
>> Nyní ètyøikrát stisknìte <Delete> a pozorujte, jak se kurzor
   pøesunuje.
>> Stisknìte <RET> pro ukonèení hledání.

Vidìli jste, co se stalo?  Emacs se v inkrementálním hledání pokou¹í
pøejít na dal¹í výskyt øetìzce, který jste dosud napsali.  Chcete-li
pøejít na dal¹í výskyt 'kurzor', jednodu¹e stisknìte C-s znovu.
Jestli¾e u¾ ¾ádný takový výskyt není, Emacs pípne a øekne vám, ¾e
hledání momentálnì "selhává", C-g hledání ukonèí.

POZNÁMKA: Na nìkterých systémech stisk C-s zpùsobí ztuhnutí
obrazovky a nevidíte ¾ádný dal¹í výstup z Emacsu.  To znamená, ¾e
"vlastnost" operaèního systému zvaná "flow control" zachycuje C-s a
nepropustí jej k Emacsu.  Pro odtuhnutí obrazovky stisknìte C-q.  Pak
v sekci "Spontaneous Entry to Incremental Search" v manuálu Emacsu
vyhledejte radu, jak se vypoøádat s touto "vlastností".

Jestli¾e uprostøed inkrementálního hledání stisknete <Delete>, uvidíte,
¾e poslední znak v hledaném øetìzci zmizí a hledání se vrací na poslední
místo hledání.  Pøedpokládejme napøíklad, ¾e jste napsali "c", abyste
na¹li první výskyt "k".  Jestli¾e nyní stisknete "u", kurzor se pøesune na
první výskyt "ku".  Teï stisknìte <Delete>.  To vyma¾e "u" z hledaného
øetìzce a kurzor se pøesune zpìt na první výskyt "k".

Jestli¾e uprostøed hledání stisknete CONTROL nebo META znak (s nìkolika
výjimkami -- znaky, které jsou speciální v hledání, jako C-s a C-r),
hledání se ukonèí.

C-s zahajuje hledání, které hledá jakýkoliv výskyt hledaného øetìzce ZA
aktuální pozicí kurzoru.  Chcete-li nìco hledat v pøedcházejícím textu,
stisknìte C-r místo C-s.  V¹e, co jsme øekli o C-s, platí také o C-r
kromì toho, ¾e smìr hledání je opaèný.


* VÍCE OKEN
-----------

Jednou z pìkných vlastností Emacsu je to, ¾e mù¾e na obrazovce zobrazit
více oken souèasnì.

>> Pøesuòte kurzor na tento øádek a stisknìte C-u 0 C-l.

>> Teï stisknìte C-x 2, co¾ rozdìlí obrazovku na dvì okna.
   Obì okna zobrazují tento tutoriál.  Kurzor zùstává navrchu okna.

>> Tisknìte C-M-v pro scrollování spodního okna.
   (Nemáte-li skuteènou klávesu META, stisknìte ESC C-v.)

>> Stisknìte C-x o ("o" jako "other") pro pøesun kurzoru do dolního okna.

>> Pou¾ijte C-v a M-v ve spodním oknì pro jeho scrollování.
   Pokraèujte ve ètení tìchto instrukcí v horním oknì.

>> Znovu stisknìte C-x o pro pøesun kurzoru zpìt do horního okna.
   Kurzor v horním oknì je pøesnì na místì, kde byl pùvodnì.

Mù¾ete dále pou¾ívat C-x o pro pøepínání mezi okny.  Ka¾dé okno má svoji
vlastní pozici kurzoru, ale jenom jedno okno kurzor skuteènì zobrazuje.
V¹echny obvyklé editaèní pøíkazy platí pro okno, ve kterém se nachází
kurzor.  Toto okno nazýváme "aktivní okno" ("selected window").

Pøíkaz C-M-v je velmi u¾iteèný, jestli¾e v jednom oknì editujete text a
druhé okno pou¾íváte pouze pro pøehled.  Mù¾ete kurzor nechávat stále
v oknì, kde editujete, a postupovat po druhém oknì pomocí C-M-v.

C-M-v je pøíkladem CONTROL-META znaku.  Máte-li skuteènou META klávesu,
mù¾ete vyvolat C-M-v pøidr¾ením obou kláves CTRL a META pøi stisku v.
Nezále¾í na tom, zda je prvnì stisknuta CTRL nebo META, proto¾e obì tyto
klávesy fungují jako modifikátory kláves, které tisknete.

Pokud nemáte skuteènou META klávesu, mù¾ete místo ní pou¾ít ESC, na
poøadí zále¾í: musíte stisknout ESC a následnì CTRL-v; CTRL-ESC v by
nefungovalo.  To proto, ¾e ESC je samostatný znak, nikoliv modifikátor.

>> Stisknìte C-x 1 (v horním oknì), abyste se zbavili dolního okna.

(Kdybyste C-x 1 stiskli v dolním oknì, odstranilo by to horní okno.
Chápejte tento pøíkaz jako "ponechej právì jedno okno -- to, ve kterém
zrovna jsem".)

Nemusíte v obou oknech zobrazovat tentý¾ buffer.  Jestli¾e pou¾ijete
C-x C-f pro vyhledání souboru v jednom z oken, druhé okno se nezmìní.
Mù¾ete vyhledávat soubory v obou oknech nezávisle.

Zde je dal¹í zpùsob, jak vyu¾ít dvì okna ke zobrazení dvou rùzných vìcí:

>> Stisknìte C-x 4 C-f následované jménem nìkterého z va¹ich souborù.
   Dokonèete to pomocí <Return>.  Vidíte zadaný soubor v dolním oknì.
   Pøesunul se tam i kurzor.

>> Stisknìte C-x o pro pøesun zpìt do horního okna a C-x 1 pro smazání
   dolního okna.


* REKURZIVNÍ EDITAÈNÍ ÚROVNÌ
----------------------------

Obèas se dostanete do nìèeho, co se nazývá "rekurzivní editaèní úroveò"
("recursive editing level").  To je indikováno hranatými závorkami ve
stavovém øádku obklopujícími závorky okolo jména hlavního módu.
Napøíklad mù¾ete vidìt [(Fundamental)] místo (Fundamental).

Abyste se dostali z rekurzivní editaèní úrovnì, stisknìte ESC ESC ESC.
To je obecný "vyskakovací" pøíkaz.  Mù¾ete jej pou¾ít té¾ pro odstranìní
nìkterých oken a vyskoèení z minibufferu.

>> Stisknìte M-x, abyste se dostali do minibufferu; pak stisknìte
   ESC ESC ESC, abyste se z nìj dostali ven.

Z rekurzivní editaèní úrovnì nemù¾ete vyskoèit pomocí C-g.  To proto, ¾e
C-g je vyu¾íváno pro ru¹ení pøíkazù a argumentù UVNITØ rekurzivní
editaèní vrstvy.


* ZÍSKÁNÍ DAL©Í NÁPOVÌDY
------------------------

V tomto tutoriálu jsme se pokusili poskytnout vám dostatek informací,
abyste mohli zaèít Emacs pou¾ívat.  V Emacsu je toho tolik, ¾e by bylo
nemo¾né to zde v¹echno objasnit.  Nicménì se o Emacsu mù¾ete nauèit
více, proto¾e má mnoho u¾iteèných vlastností.  Emacs nabízí pøíkazy pro
ètení dokumentace svých pøíkazù.  V¹echny tyto "help" pøíkazy
zaèínají znakem CONTROL-h, který se nazývá "help znak".

Pro pou¾ití vlastností nápovìdy stisknìte znak C-h a pak znak øíkající,
jaký druh nápovìdy ¾ádáte.  Jste-li OPRAVDU ztraceni, stisknìte C-h ? a
Emacs vám sdìlí, jaké druhy nápovìdy vám mù¾e poskytnout.  Jestli¾e
jste stiskli C-h a pak jste se rozhodli, ¾e ¾ádnou nápovìdu nechcete,
jednodu¹e to zru¹te stiskem C-g.

(Na nìkterých poèítaèích je význam znaku C-h zmìnìn.  To by opravdu
nemìlo být obecným nastavením pro v¹echny u¾ivatele, tak¾e máte právo
stì¾ovat si systémovému administrátorovi.  Do té doby, jestli¾e C-h
nezobrazuje hlá¹ení o nápovìdì v dolní èásti obrazovky, zkuste místo
toho pou¾ívat klávesu F1 nebo M-x help RET.)

Nejzákladnìj¹í help pøíkaz je C-h c.  Stisknìte C-h, znak c a klávesový
pøíkaz; Emacs pak zobrazí velmi struèný popis pøíkazu.

>> Stisknìte C-h c C-p.
   Hlá¹ení by mìlo vypadat asi takto

	C-p runs the command previous-line

To vám sdìluje "jméno funkce".  Jména funkcí jsou pou¾ívána zejména pro
konfiguraci a roz¹iøování Emacsu.  Ale proto¾e jména funkcí jsou volena
tak, aby naznaèovala, co odpovídající pøíkaz dìlá, mohou slou¾it také
jako velmi struèná dokumentace -- dostateèná k tomu, aby vám pøipomenula
pøíkazy, které jste se ji¾ nauèili.

Víceznakové pøíkazy jako C-x C-s a (pokud nemáte META, EDIT ani ALT
klávesu) <ESC>v jsou po C-h c povoleny také.

K získání více informací o pøíkazu místo C-h c pou¾ijte C-h k.

>> Stisknìte C-h k C-p.

To zobrazí dokumentaci k funkci a její jméno v emacsovém oknì.  A¾
výstup pøeètete, stisknìte C-x 1, abyste se textu nápovìdy zbavili.
Nemusíte to dìlat hned.  Mù¾ete chvíli editovat a nahlí¾et do textu
nápovìdy a teprve pak stisknout C-x 1.

Zde jsou dal¹í u¾iteèné C-h volby:

   C-h f	Popis funkce.  Zadáváte jméno funkce.

>> Zkuste napsat C-h f previous-line<Return>.
   To vypí¹e ve¹keré informace, které Emacs má o funkci implementující
   pøíkaz C-p.

Podobný pøíkaz C-h v zobrazí dokumentaci promìnné, její¾ hodnotu
mù¾ete nastavit a zmìnit tím chování Emacsu.  Jméno promìnné zadáte, a¾
se na nì Emacs zeptá.

   C-h a	Pøíkazové apropos.  Zadejte klíèové slovo a Emacs vypí¹e
		v¹echny pøíkazy, jejich¾ jména obsahují toto klíèové
		slovo.  V¹echny tyto pøíkazy mohou být vyvolány pomocí
		META-x.  Pro nìkteré pøíkazy pøíkazové apropos vypí¹e
		také jedno nebo dvouznakové sekvence, které provádìjí
		tentý¾ pøíkaz.

>> Napi¹te C-h a file<Return>.

To zobrazí v druhém oknì seznam v¹ech M-x pøíkazù obsahujících "file" ve
svém názvu.  Znakové pøíkazy jako C-x C-f uvidíte vypsané vedle
odpovídajících jmen pøíkazù jako find-file.

>> Stisknìte C-M-v pro posun okna s nápovìdou.  Proveïte to nìkolikrát.

>> Stisknìte C-x 1 pro smazání okna s nápovìdou.

   C-h i	Ètení on-line manuálù (té¾ Info).  Tento pøíkaz
		vás pøepne do speciálního bufferu s názvem `*info*',
		ve kterém mù¾ete èíst on-line manuály pro balíky
		nainstalované na va¹em systému.  Pokud stisknete
		m emacs <Return> mù¾ete si napøíklad pøeèíst manuál
		k Emacsu.  Pokud jste dosud nikdy nepou¾ívali Info,
		stisknìte ? a Emacs vám pøedstaví hlavní mo¾nosti
		módu pro Info.  A¾ si tyto mo¾nosti prostudujete,
		mìli byste pou¾ívat Info manuál Emacsu jako svoji
		primární dokumentaci.


* ZÁVÌR
-------

Nezapomeòte, Emacs ukonèíte provedením pøíkazu C-x C-c.  Pro doèasný
odskok do shellu, ze kterého se do Emacsu mù¾ete opìt vrátit,
pou¾ijte C-z.

Zámìrem tohoto tutoriálu je být srozumitelný v¹em novým u¾ivatelùm, tak¾e
narazíte-li na nìco nejasného, tak neusedejte a neklaïte to za vinu sobì
-- stì¾ujte si!


KOPÍROVÁNÍ
----------

Tento tutoriál vychází z dlouhé øady emacsových tutoriálù zahájené
tutoriálem napsaným Stuartem Cracraftem pro pùvodní Emacs.

Tato verze tutoriálu je, podobnì jako GNU Emacs, chránìna copyrightem a
je ¹íøena se svolením distribuovat kopie za jistých podmínek:

Copyright (C) 1985, 1996, 1998, 2001-2012  Free Software Foundation, Inc.

   Ka¾dému je zaruèeno právo vytváøet a distribuovat pøesné kopie tohoto
   dokumentu tak, jak jej obdr¾el, na jakémkoliv médiu, s tím, ¾e bude
   zachována tato poznámka o autorství a poznámka o svolení a ¾e
   distributor zaruèuje pøíjemci právo na dal¹í redistribuci povolenou
   touto poznámkou.

   Je zaruèeno právo distribuovat modifikované verze tohoto dokumentu
   nebo jeho èástí pod vý¹e uvedenými podmínkami za pøedpokladu, ¾e
   obsahuje jasné poznámky uvádìjící, kdo provedl poslední modifikace.

Podmínky pro kopírování Emacsu samotného jsou slo¾itìj¹í, av¹ak ve
stejném duchu.  Pøeètìte si prosím soubor COPYING a pak pøedávejte kopie
GNU Emacsu svým pøátelùm.  Pomáhejte potírat softwarovou obstrukci
("vlastnictví") pou¾íváním, psaním a sdílením free softwaru!

;;; Local Variables:
;;; coding: iso-latin-2
;;; End:

