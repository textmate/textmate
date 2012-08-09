Prvo berilo za Emacs. Pogoji uporabe in razširjanja so navedeni na koncu.

Ukazi v Emacsu v splošnem vključujejo tipki CONTROL (včasih označeni
CTRL ali CTL) in META (včasih označena EDIT ali ALT). Namesto, da bi ju
vedno izpisali s celim imenom, bomo uporabili naslednji okrajšavi:

 C-<znak> pomeni, da moramo držati pritisnjeno tipko CONTROL, ko
 	  vtipkamo <znak>. Oznaka C-f tako pomeni: držimo pritisnjeno
 	  tipko CONTROL in pritisnemo tipko f.
 M-<znak> pomeni, da moramo držati pritisnjeno tipko META, EDIT ali
 	  ALT, ko vtipkamo <znak>. Če na tipkovnici ni tipk META, EDIT
 	  ali ALT, pritisnemo tipko ESC, jo spustimo in zatem
 	  pritisnemo tipko <chr>. Tipko ESC bomo označevali z <ESC>.

Pomembno: Emacs zapustimo z ukazom C-x C-c (dva znaka).
Delno vnešen ukaz prekinete s C-g.
V učbeniku so vaje, s katerimi preskusite nove ukaze. Označujeta jih
znaka »>>« ob levem robu. Zgled:
<<Blank lines inserted here by startup of help-with-tutorial>>
[Sredina strani je iz didaktičnih razlogov prazna. Besedilo se nadaljuje spodaj]
>> Vtipkajte zdaj ukaz C-v (View next screen, Prikaži naslednji zaslon),
	da se premaknete na naslednji zaslon (kar poskusite, pritisnite
	hkrati tipko CONTROL in V). Od zdaj naprej boste morali to
	napraviti sami vsakič, ko pridete do konca zaslona.

Ste opazili, da sta se dve vrstici s prejšnjega zaslona ponovili? Ta
kontinuiteta olajša branje pri skakanju s strani na stran.

Prva stvar, ki si jo morate zapomniti, je, kako se premikate po
datoteki. Zdaj že veste, da se premaknete za cel zaslon naprej z
ukazom C-v. Za cel zaslon nazaj pa se premaknete z ukazom M-v
(pritisnite tipko META in jo držite ter pritisnite tipko v, ali pa
pritisnite in spustite <ESC> ter zatem pritisnite tipko v, če tipke
META, EDIT ali ALT na vaši tipkovnici ni).

>>  Nekajkrat pritisnite M-v in C-v, da vidite, kako ukaza delujeta.


* povzetek
----------

Za pregled celega zaslona besedila so uporabni naslednji ukazi:

	C-v	Premik se za cel zaslon naprej
	M-v	Premik se za cel zaslon nazaj
	C-l	Cel zaslon premaknemo tako, da je zdaj po vertikali
		 osredninjen okoli besedila, kjer se nahaja kazalček
		 (znak v C-l je črka L, ne števka 1)

>> Poiščite kazalček na zaslonu in si zapomnite besedilo okoli njega.
   Zatem vtipkajte C-l. Ponovno poiščite kazalček. Opazili boste, da
   je besedilo okoli njega ostalo isto, vendar se je pomaknilo na sredo
   zaslona. Če še enkrat pritisnite C-l, se bo ta vrstica pomaknila na
   vrh zaslona.  Pritisnite C-l še enkrat, in vrstica se bo pomaknila
   na dno zaslona.

Za premikanje za cel zaslon naprej ali nazaj lahko tipkovnicah, ki
imajo ti tipki, uporabljate tudi PageUp in PageDown. Opisan postopek s
C-v in M-v pa deluje povsod.


* PREMIKANJE KAZALČKA
---------------------

Premiki za celo stran naprej in nazaj so sicer uporabni, ampak kako pa
pridemo do izbranega mesta na zaslonu?

Načinov je več. Najosnovnejši je uporaba ukazov C-p, C-b, C-f in
C-n. Ti po vrsti premaknejo kazalček v prejšnjo vrstico, znak nazaj,
znak naprej, in v naslednjo vrstico. Ti štirje ukazi so enakovredni
kurzorskim tipkam:

			  prejšnja vrstica, C-p
				  :
				  :
       nazaj, C-b .... trenutni položaj kazalčka .... naprej, C-f
				  :
				  :
			  naslednja vrstica, C-n

>> S pritiski na C-n ali C-p premaknite kazalček v sredinsko vrstico
   na diagramu zgoraj. Zatem pritisnite C-l. S tem diagram postavite na
   sredino zaslona.

V angleščini ima izbor tipk nazoren pomen. P kot »previous«
(prejšnji), N kot »next« (naslednji), B kot »backward« (nazaj) in F
kot »forward« (naprej). Te osnovne ukaze za premikanje kazalčka boste
uporabljali ves čas.

>> Nekajkrat pritisnite C-n, da pride kazalček do te vrstice.

>> Z nekaj C-f se pomaknite na desno na sredo vrstice, nato pa nekajkrat
   pritisnite C-p. Opazujte, kaj se dogaja s kazalčkom na sredini
   vrstice.

Vsaka vrstice v besedilu je zaključena z znakom za novo vrstico
(angl. Newline). Ta ločuje vrstico v besedilu od naslednje. (Tudi
zadnja vrstica v datoteki je po navadi zaključena z znakom za novo
vrstico, čeprav Emacs tega ne zahteva.)

>> Poskusite ukaz C-b, ko je kazalček na začetku vrstice. Kazalček se
   mora premakniti na konec prejšnje vrstice. To je zato, ker se je
   ravnokar premaknil prek znaka za konec vrstice.

Ukaz C-f premika kazalček prek znaka za novo vrstico enako kot C-b.

>> Poskusite še nekajkrat pritisniti C-b, da dobite občutek za
   premikanje kazalčka. Potem nekajkrat poskusite C-f, da pridete do konca
   vrstice. Še enkrat pritisnite C-f, da skočite v naslednjo vrstico.

Ko s kazalčkom dosežete zgornji ali spodnji rob zaslona, se besedilo
toliko premakne, da kazalček ostane na zaslonu. V angleščini se temu
pravi »scrolling«. To omogoča, da lahko premaknemo kazalček na
katerokoli mesto v besedilu, a vseeno ostanemo na zaslonu.

>> Poskusite kazalček pripeljati s C-n čisto do dna zaslona in si oglejte,
   kaj se zgodi.

Če se vam zdi premikanje po en znak prepočasno, se lahko premikate za
celo besedo. M-f (META-f) premakne kazalček za eno besedo naprej, M-b
pa za besedo nazaj.

>> Poskusite nekajkrat M-f in M-b.

Če je kazalček sredi besede, ga M-f prestavi na konec besede. Če je v
belini med besedami, ga M-f premakne na konec naslednje besede. M-b
deluje podobno, a v nasprotni smeri.

>> Nekajkrat poskusite M-f in M-b, vmes pa še nekaj C-f in
   C-b. Opazujte učinke M-f in M-b, ko je kazalček sredi besede ali
   med besedami.

Ste opazili paralelo med C-f in C-b na eni strani ter M-f in M-b na
drugi? V Emacsu se dostikrat ukazi Meta nanašajo na operacije nad
enotami jezika (besede, stavki, odstavki), medtem ko se ukazi Control
nanašajo na operacije, neodvisne od zvrsti besedila (znaki, vrstice
ipd.).

Podobna zveza je tudi med vrsticami in stavki: ukaza C-a in C-e
premakneta kazalček na začetek oz. konec vrstice, M-a in M-e pa na
začetek oz. konec stavka.

>> Poskusite nekaj ukazov C-a, potem pa nekaj ukazov C-e.
   Poskusite nekaj ukazov M-a, potem pa nekaj ukazov M-e.

Ste opazili, da ponovljeni C-a ne napravijo nič, ponovljeni M-a pa se
premikajo naprej? Čeprav se ne obnašata enako, pa je vendar obnašanje
enega in drugega po svoje naravno.

Položaju kazalčka na zaslonu pravimo tudi »point«, točka.
Parafrazirano: kazalček kaže na zaslonu, kje je točka v besedilu.

Povzetek preprostih ukazov za premikanje kazalčka, vključno s premiki
po besedo in stavek:

	C-f	Premik za znak naprej
	C-b	Premik za znak nazaj

	M-f	Premik za besedo naprej
	M-b	Premik za besedo nazaj

	C-n	Premik v naslednjo vrstico
	C-p	Premik v prejšnjo vrstico

	C-a	Premik na začetek vrstice
	C-e	Premik na konec vrstice

	M-a	Premik na začetek stavka
	M-e	Premik na konec stavka

>> Za vajo nekajkrat poskusite vsakega od teh ukazov.
   To so najpogosteje uporabljani ukazi.

Še dva pomembna ukaza za premikanje kazalčka sta M-< (META-manjši od),
ki ga premakne na začetek datoteke, in M-> (META-večji od), ki ga
premakne na konec datoteke.

Na ameriških tipkovnicah najdete znak < nad vejico in morate
pritisniti tipko Shift, da pridete do njega. Z ukazom M-< je enako -
prav tako morate pritisniti tipko Shift, sicer moste izvedli drug
ukaz, Meta-vejica. Na naših tipkovnicah sta oba znaka na isti tipko,
in za ukaz M-> morate pritisniti še tipko Shift.

>> Poskusite zdaj M-<, skok na začetek tega učbenika.
   Potem se vrnite nazaj z zaporednimi C-v.

>> Poskusite zdaj M->, skok na konec tega učbenika.
   Potem se vrnite nazaj z zaporednimi M-v.

Če ima vaša tipkovnica kurzorske tipke, lahko premikate kazalček po
zaslonu tudi z njimi. Vseeno priporočamo, da se privadite ukazov C-b,
C-f, C-n in C-p, in to iz treh razlogov. Prvič, delujejo na čisto vseh
terminalih. Drugič, z nekaj prakse v Emacsu boste opazili, da je
tipkanje ukazov s CONTROL hitrejše od tipkanja s kurzorskimi tipkami, ker
ni treba ves čas premikati desnice s tipkovnice na kurzorske tipke in
nazaj. In tretjič, ko se enkrat navadite teh ukazov s CONTROL, se boste
enostavneje naučili tudi bolj zapletenih ukazov za premikanje kazalčka.

Večini ukazov v Emacsu lahko podamo številčni argument; največkrat ta
pove, kolikokrat zapovrstjo naj se ukaz izvede. Večkratno ponovitev
ukaza izvedemo tako, da najprej vtipkamo C-u, zatem število,
kolikokrat naj se ukaz ponovi, in nazadnje željeni ukaz. Če ima vaša
tipkovnica tipko META (ali EDIT ali ALT), lahko izpustite ukaz C-u in
namesto tega vtipkate število ponovitev, medtem ko držite pritisnjeno
tipko META. Druga metoda je sicer krajša, priporočamo pa prvo, ker
deluje na vseh terminalih. Takšen številčni argument je »prefiksni«
argument, ker vnesemo argument pred ukazom, na katerega se nanaša.

Zgled: C-u 8 C-f premakne kazalček za osem znakov naprej.

>> Poskusite s primernim argumentom za število ponovitev ukaza
   C-n ali C-p priti čim bliže tej vrstici v enem samem skoku.

Večina ukazov, ne pa vsi, uporablja številčni argument kot število
ponovitev ukaza. Nekateri ukazi - nobeden od tistih, ki smo si jih
ogledali do zdaj - ga uporabljajo kot stikalo: s podanim prefiksnim
argumentom napravi ukaz nekaj drugega kot običajno.

Ukaza C-v in M-v sta tudi izjemi, a drugačni. Če jima podamo argument,
premakneta zaslon za navedeno število vrstic, ne pa zaslonov. Ukaz C-u
8 C-v, na primer, premakne zaslon navzgor za 8 vrstic.

>> Poskusite zdaj C-u 8 C-v

To bi moralo zaslon premakniti navzgor za osem vrstic. Če bi ga radi
premaknili nazaj, poskusite M-v z istim argumentom.

Če uporabljate grafični vmesnik, denimo X ali MS Windows, imate
verjetno ob robu Emacsovega okna pokončno pravokotno ploskev,
imenovano drsnik. Pogled na besedilo lahko premikate tudi tako, da z
miško kliknete na drsnik.


* ČE SE EMACS PRENEHA ODZIVATI
------------------------------

Če se Emacs preneha odzivati na vaše ukaze, ga lahko varno prekinete z
ukazom C-g. Z njim lahko prekinete ukaze, za katere bi trajalo
predolgo, da bi se izvedli.

Isti ukaz, C-g, lahko uporabite tudi, da prekličete številčni
argument, ali pa začetek ukaza, ki ga ne želite izvesti.

>> Vtipkajte C-u 100, s čimer ste izbrali številčni argument 100,
   zatem pa vtipkajte C-g. Vtipkajte zdaj C-f. Kazalček se je
   premaknil le za en znak, ker ste številčni argument vmes preklicali
   s C-g.

Tudi če ste po nesreči vtipkali <ESC>, se ga lahko znebite s C-g.


* ONEMOGOČENI UKAZI
-------------------

Nekaj ukazov v Emacsu je namenoma »onemogočenih«, da bi jih
začetniki ne izvedli po nesreči.

Če vtipkate tak onemogočen ukaz, se bo na zaslonu pojavilo novo okno z
obvestilom, kateri ukaz ste skušali izvesti, in vas vprašalo, če ga
res želite izvesti.

Če v resnici želite poskusiti ukaz, pritisnite preslednico kot odgovor
na vprašanje. Normalno verjetno ukaza ne želite izvesti, zato na
vprašanje odgovorite z »n«.

>> Vtipkajte C-x C-l (ki je onemogočen ukaz),
   zatem na vprašanje odgovorite n.


* OKNA
------

Emacs lahko prikaže več »oken« in v vsakem svoje besedilo. Kasneje
bomo razložili, kako uporabljamo več oken hkrati. Zaenkrat bomo
povedali le, kako se znebite dodatnih oken, ki jih lahko odpre
vgrajena pomoč ali pa izpis kakšnega drugega programa. Preprosto je:

	C-x 1   Eno okno (torej, zaprimo vsa ostala).

To je CONTROL-x, ki mu sledi števka 1. Ukaz C-x 1 raztegne čez cel
zaslon okno, v katerem se nahaja kazalček, ostala pa zapre.

>> Premaknite kazalček do te vrstice in vtipkajte C-u 0 C-l
>> Vtipkajte C-h k C-f.
   Vidite, kako se je to okno skrčilo in odstopilo prostor oknu,
   ki pojasnjuje ukaz C-f?

>> Vtipkajte C-x 1 in spodnje okno se bo zaprlo.

Za razliko od ukazov, ki smo se jih naučili do zdaj, je ta ukaz
sestavljen iz dveh znakov. Začne se z znakom CONTROL-x. Cela vrsta
ukazov se začne enako, in mnogi od njih zadevajo delo z datotekami,
delovnimi področji in podobnim. Vsem tem ukazom je skupno, da se
začnejo s CONTROL-x, ki mu sledi še en, dva ali trije znaki.


* VRIVANJE IN BRISANJE
----------------------

Če želite v obstoječe besedilo vriniti novo, preprosto premaknite
kazalček na želeno mesto in začnite tipkati. Vidne znake, na primer A,
7, * in podobno, Emacs vrine takoj, ko jih vtipkate. S pritiskom na
tipko <Return> (ali <Enter>) vrinete znak za skok v novo vrstico.

Zadnji vtipkani znak lahko izbrišete s pritiskom na tipko <DEL>. Ta
tipka je na tipkovnici običajno označena z »Backspace« - skratka, to
je ista tipka, ki jo tudi v drugih programih uporabljate za brisanje
nazadnje natipkanega znaka.

Najverjetneje imate na tipkovnici še tipko »Delete«. Naj vas to ne
zmede - z <DEL> mislimo tipko »Backspace«.

>> Poskusite zdaj! Vtipkajte zdaj nekaj znakov in jih zatem s tipko
   <DEL> pobrišite. Nič naj vas ne skrbi, če se je ta vrstica
   spremenila. Izvirnika tega učbenika ne boste pokvarili -- tole je
   samo vaša osebna delovna kopija.

Ko vrstica postane predolga za zaslon, se »nadaljuje« v naslednji
vrstici na zaslonu. Če uporabljate grafično okolje, boste opazili
zaviti puščici ob levem in desnem robu, ki označujeta vrstico, ki se
nadaljuje v naslednji zaslonski vrstici. Če uporabljate terminalski
vmesnik, je vrstica, ki se nadaljuje v naslednji zaslonski vrstici,
označena z obrnjeno poševnico (znak »\«) v skrajnem desnem stolpcu.

>> Zdaj začnite tipkati besedilo, dokler ne dosežete desnega roba, in
   še naprej. Opazili boste, da se pojavi znak za nadaljevanje.

>> S tipko <DEL> pobrišite toliko znakov, da vrstica ne sega
   več čez širino zaslona. Znak za nadaljevanje v naslednji
   vrstici je izginil.

Znak za novo vrstico lahko pobrišemo enako kot vsak drug znak. S tem,
ko pobrišemo znak za novo vrstico, združimo vrstici v eno samo.  Če bo
nova vrstica predolga, da bi cela prišla na zaslon, bo razdeljena v
več zaslonskih vrstic.

>> Premaknite kazalček na začetek vrstice in pritisnite <DEL>. To
   združi vrstico s prejšnjo.

>> Pritisnite <Return>. S tem ste ponovno vrinili znak za skok v novo
   vrstico, ki ste ga malo prej zbrisali.

Spomnimo se, da lahko za večino ukazov v Emacsu določimo, naj se
izvedejo večkrat zaporedoma; to vključuje tudi vnos teksta. Ponovitev
običajnega znaka ga večkrat vrine v besedilo.

>> Poskusite zdaj tole: da vnesete osem zvezdic, vtipkajte C-u 8 *

Zdaj ste se naučili najpreprostejši način, da v Emacsu nekaj natipkate
in popravite. Brišete lahko tudi besede ali vrstice. Tu je povzetek
ukazov za brisanje:

	<DEL>        pobriše znak tik pred kazalčkom (levo od
	             oznake za kazalček)
	C-d   	     pobriše znak tik za kazalčkom (»pod« oznako
		     za kazalček)

	M-<DEL>      pobriše besedo tik pred kazalčkom
	M-d	     pobriše besedo tik za kazalčkom

	C-k          zavrže besedilo desno od kazalčka do konca vrstice
	M-k          zavrže besedilo od položaja kazalčka do konca stavka

Črka »d« je iz angleške besede »delete« (pobrisati), črka »k« pa iz
besede »kill« (pobiti). Ste opazili, da <DEL> in C-d na eni, ter
M-<DEL> in M-d na drugi strani nadaljujeta paralelo, ki sta jo začela
C-f in M-f (<DEL> pravzaprav ni kontrolni znak, kar pa naj nas ne
moti).  C-k in M-k sta v enakem sorodu s C-e in M-e: prvi deluje na
vrstice, drugi na stavke.

Obstaja tudi splošen postopek za brisanje kateregakoli dela delovnega
področja. Kazalček postavimo na en konec področja, ki ga želimo
izbrisati, in pritisnemo C-@ ali C-<SPC> (<SPC> je preslednica).
Katerikoli od obeh ukazov deluje. Premaknite kazalček na drug konec
področja, ki ga želite izbrisati. Med premikanjem Emacs z barvo
označuje področje med kazalčkom in mestom, kjer ste pritisnili
C-<SPC>. Končno pritisnite C-w. S tem ste zavrgli vse besedilo med
obema mejama.

>> Premaknite kazalček na črko O, s katero se začenja prejšnji
   odstavek.
>> Vtipkajte C-SPC. Emacs prikaže sporočilo »Mark set« (slov. »oznaka
   postavljena«) na dnu ekrana.
>> Premaknite kazalček na črko V v »postavimo« v drugi vrstici istega
   odstavka.
>> Vtipkajte C-w. S tem zavržemo vse besedilo začenši z O in vse do
   črke V.

Razlika med tem, če zavržete cel odstavek besedila (angl. »kill«,
pobiti) ali pa če pobrišete znak (angl. »delete«), je ta, da lahko
prvega povrnete - na katerokoli mesto v besedilu - z ukazom C-y,
drugega pa ne (seveda pa lahko prekličete brisanje - glejte nižje). Na
splošno ukazi, ki lahko povzročijo veliko škode (pobrišejo veliko
besedila), shranijo pobrisano besedilo; tisti, ki pobrišejo samo
posamezni znak, ali samo prazne vrstice in presledke, pa ne.

>> Postavite kazalček na začetek neprazne vrstice. Pritisnite C-k, da
   pobrišete vsebino vrstice.
>> Še enkrat pritisnite C-k. To pobriše še znak za novo vrstico.

Ste opazili, da prvi C-k pobriše vsebino vrstice, naslednji C-k pa še
vrstici samo, s čimer se vse besedilo pod bivšo vrstico premakne za
eno vrstico navzgor? Ukaz C-k obravnava številčni argument malo
drugače: pobriše toliko in toliko vrstic z vsebinami vred. To ni zgolj
ponovitev. C-u 2 C-k pobriše dve polni vrstici besedila, kar je nekaj
drugega, kot če dvakrat vtipkate C-k.

Besedilo, ki ste ga prej pobili, lahko povrnete (angl. »yank« -
potegniti). Predstavljajte si, kot da potegnete nazaj nekaj, kar vam
je nekdo odnesel. Pobito besedilo lahko potegnete nazaj na isti ali pa
na kakšen drug kraj v besedilu, ali pa celo v kaki drugi datoteki.
Isto besedilo lahko večkrat potegnete nazaj, tako da je v delovnem
področju povečterjeno. Nekateri drugi urejevalniki uporabljajo namesto
»kill« in »yank« izraza »cut« in »paste« (glejte glosar v priročniku
za Emacs).

Ukaz za vračanje pobitega besedila je C-y.

>> Poskusite z ukazom C-y povrniti pobrisano besedilo.

Če ste uporabili več zaporednih ukazov C-k, je vse pobrisano besedilo
shranjeno skupaj, in en sam C-y bo vrnil vse tako pobrisane vrstice.

>> Poskusite, nekajkrat vtipkajte C-k.

Zdaj pa vrnimo pobrisano besedilo:

>> Vtipkajte C-y. Zdaj pa premaknite kazalček za nekaj vrstic navzdol
   in še enkrat vtipkajte C-y. Vidite zdaj, kako se kopira dele
   besedila?

Kaj pa, če ste pobrisali nekaj besedila, ki bi ga radi vrnili, vendar
ste za iskanim odlomkom pobrisali še nekaj? C-y vrne samo nazadnje
pobrisan odlomek. Vendar tudi prejšnje besedilo ni izgubljeno. Do
njega lahko pridete z ukazom M-y. Ko ste vrnili nazadnje zbrisano
besedilo s C-y, pritisnite M-y, ki ga zamenja s predzanje pobrisanim
besedilom. Vsak naslednji M-y prikaže še eno prej. Ko ste končno
prišli do iskanega besedila, ni treba napraviti nič posebnega, da bi
ga obdržali. Preprosto nadaljujte z urejanjem, in vrnjeno besedilo bo
ostalo, kamor ste ga odložili.

Če pritisnete M-y dovolj velikokrat, se boste vrnili na začete, torej
spet na zadnje pobrisano besedilo.

>> Pobrišite vrstico, premaknite se nekam drugam, in pobrišite še
   eno vrstico.
   Z ukazom C-y dobite nazaj to drugo vrstico.
   Z ukazom M-y pa jo zamenjate s prvo vrstico.
   Ponovite ukaz M-y še nekajkrat in si oglejte, kaj dobite na
   zaslon. Ponavljajte ga, dokler se ne prikaže ponovno nazadnje
   pobrisana vrstica, in še naprej. Če želite, lahko tudi ukazu
   M-y podate pozitivno ali negativno število ponovitev.


* PREKLIC UKAZA (UNDO)
----------------------

Če ste besedilo spremenili, a ste se kasneje premislili, lahko
besedilo vrnete v prvotno stanje z ukazom Undo, C-/.

Običajno C-/ prekliče spremembo besedila, ki jo izvede en ukaz; če
ukaz C-/ ponovimo, prekličemo še spremembo, ki jo je izvedel
predzadnji ukaz, in vsaka nadaljnja ponovitev C-/ seže še eno
spremembo globlje v zgodovino.

Emacs hrani bolj ali manj celotno zgodovino naših ukazov, z dvema
izjemama: ukazov, ki niso napravili nobene spremembe v besedilu (npr.
premik kazalčka), ne shranjuje, in zaporedje do 20 vrinjenih znakov
shrani kot en sam ukaz. Slednje prihrani nekaj ukazov C-/, ki bi jih
morali vtipkati.

>> Pobrišite to vrstico z ukazom C-k, potem jo prikličite nazaj s C-/.

C-_ je alternativni ukaz za preklic zadnjega ukaza. Deluje povsem
enako kot C-/. Na nekaterih besedilnih terminalih v resnici pritisk
C-/ pošlje Emacsu ukaz C-_. Še tretja možnost je C-x u, ki tudi deluje
povsem enako kot C-/, le z nekaj več tipkanja.

Če podamo ukazu C-/, C-_ ali C-x u numerični argument, je to enako,
kot če bi ukaz ročno ponovili tolikokrat, kot pravi argument.

Ukaz za brisanje besedila lahko prekličete in besedilo povrnete,
enako, kot če bi besedilo pobili. Razlika med brisanjem in pobijanjem
besedila je le ta, da le slednje lahko povrnete z ukazom C-y. Preklic
ukaza pa velja za eno in drugo.


* DATOTEKE
----------

Da bi bile spremembe v besedilu trajne, morate besedilo shraniti v
datoteko. V nasprotnem primeru jih boste za vedno izgubili tisti hip,
ko boste zapustili Emacs. Besedilo postavimo v datoteko tako, da
na disku »poiščemo« (angl. find) datoteko, preden začnemo tipkati
(pravimo tudi, da »obiščemo« datoteko).

Poiskati datoteko pomeni, da v Emacsu vidimo vsebino datoteke. To je
bolj ali manj tako, kot da z Emacsom urejamo datoteko samo. Vendar pa
spremembe ne postanejo trajne, dokler datoteke ne shranimo
(angl. save) na disk. Tako imamo možnost, da se izognemo temu, da bi
nam na pol spremenjene datoteke ležale po disku, kadar tega ne
želimo. Ker pa Emacs ohrani izvorno datoteko pod spremenjenim imenom,
lahko prvotno datoteko prikličemo nazaj celo še potem, ko smo datoteko
že shranili na disk.

V predzadnji vrstici na dnu zaslona vidite vrstico, ki se začne z
vezaji, na začetku pa vsebuje niz znakov »--:---  TUTORIAL« ali nekaj
podobnega. Ta del zaslona navadno vsebuje ime datoteke, ki smo jo
obiskali. Zdajle je to »TUTORIAL«, vaša delovna kopija učbenika
Emacsa. Ko boste poiskali kakšno drugo datoteko, bo na tem mestu
izpisano ime te datoteke.

Posebnost ukaza za iskanje datoteke je, da moramo povedati, katero
datoteko iščemo. Pravimo, da ukaz »prebere argument« (v tem primeru je
argument ime datoteke). Ko vtipkate ukaz

	C-x C-f   (poišči datoteko)

vas Emacs povpraša po imenu datoteke. Kar vtipkate, se sproti vidi v
vrstici na dnu zaslona. Temu delovnemu področju pravimo pogovorni
vmesnik (minibuffer), kadar se uporablja za tovrstni vnos. Znotraj
pogovornega vmesnika lahko uporabljate običajne ukaze za urejanje, če
ste se na primer pri tipkanju zmotili.

Sredi tipkanja imena datoteke (ali katerega koli drugega opravila v
pogovornem vmesniku) lahko ukaz prekličete s C-g.

>> Vtipkajte C-x C-f, zatem pa še C-g. Zadnji ukaz od treh je
   zaprl pogovorni vmesnik in tudi preklical ukaz C-x C-f, ki je
   uporabljal pogovorni vmesnik. Konec z iskanjem datoteke.

Ko ste dokončali ime, ga vnesete s pritiskom na <Return>. Pogovorni
vmesnik izgine, ko je ukaz izveden.

Vsebina datoteke se pojavi na zaslonu. Zdaj lahko dopolnjujete,
urejate ali kako drugače spreminjate vsebino. Ko želite, da ostanejo
spremembe trajne, izvedete ukaz:

	C-x C-s   (shrani datoteko)

Besedilo se s tem shrani iz pomnilnika računalnika na datoteko na
disk. Ko prvič izvedete ta ukaz, se izvorna datoteka preimenuje, tako
da ni izgubljena. Najdete jo pod novim imenom, ki se od starega
razlikuje po tem, da ima na koncu pripet znak »~«.

Ko je Emacs shranil datoteko, izpiše njeno ime. Shranjujte raje
pogosteje kot ne, da v primeru, če gre z računalnikom kaj narobe, ne
izgubite veliko (oglejte si tudi razdelek o samodejnem shranjevanju
nižje).

>> Vtipkajte C-x C-s TUTORIAL <Return>.
   S tem boste shranili svojo kopijo tega učbenika. Emacs bo v vrstici
   na dnu zaslona izpisal »Wrote ...TUTORIAL«.

Poiščete lahko lahko že obstoječo datoteko, da si jo ogledate ali
popravite, ali pa tudi datoteko, ki še ne obstaja. To je način, kako z
Emacsom ustvarimo novo datoteko: poiščite datoteko z izbranim imenom,
ki bo sprva prazna, in začnite pisati. Ko jo boste prvič shranili, bo
Emacs ustvaril datoteko z vnešenim besedilom. Od tod dalje delate na
že obstoječi datoteki.


* DELOVNA PODROČJA
------------------

Tudi če ste z ukazom C-x C-f poiskali in odprli drugo datoteko, prva
ostane v Emacsu. Nanjo se vrnete tako, da jo še enkrat »poiščete« z
ukazom C-x C-f. Tako imate lahko v Emacsu hkrati kar precej datotek.

Emacs hrani besedilo vsake datoteke v takoimenovanem »delovnem
področju« (angl. buffer). Ko poiščemo datoteko, Emacs ustvari zanjo
novo delovno področje. Vsa obstoječa delovna področja v Emacsu vidimo
z ukazom:

	C-x C-b   Seznam delovnih področij.

>> Poskusite C-x C-b zdaj.

Vidite, da ima vsako delovno področje svoje ime, pri nekaterih pa piše
tudi ime datoteke, katere vsebina se hrani v njem. Vsako besedilo, ki
ga vidite v katerem od Emacsovih oken, je vedno del kakšnega delovnega
področja.

>> Z ukazom C-x 1 se znebite seznama delovnih področij.

Tudi če imate več delovnih področij, pa je vedno le eno od njih
trenutno dejavno. To je tisto delovno področje, ki ga popravljate. Če
želite popravljati drugo delovno področje, morate »preklopiti«
nanj. Če bi radi preklopili na delovno področje, ki pripada kakšni
datoteki, že poznate en način, kako to storiti: ponovno »obiščete«
(odprete) to datoteko z ukazom C-x C-f. Obstaja pa še lažji način: z
ukazom C-x b. Pri tem ukazu morate navesti ime delovnega področja.

>> Ustvarite datoteko z imenom »bla« tako, da vtipkate C-x C-f bla
   <Return>. Zatem se vrnite v ta učbenik z ukazom C-x C-f TUTORIAL
   <Return>.

Večinoma se ime delovnega področja kar ujema z imenom datoteke (brez
poti do datoteke), ne pa vedno. Seznam delovnih področij, ki ga
prikaže ukaz C-x C-b, prikaže imena vseh delovnih področij in
pripadajoča imena datotek.

Vsako besedilo, ki ga vidite v katerem od Emacsovih oken, je vedno del
kakšnega delovnega področja. Nekatera delovna področja ne pripadajo
nobeni datoteki. Področje »*Buffer List*«, na primer, je že eno takih.
To delovno področje smo ustvarili ravnokar, ko smo pognali ukaz C-x
C-b, in vsebuje seznam delovnih področij. Temu delovnemu področju
TUTORIAL sprva ni pripadala datoteka, zdaj pa mu, ker smo v prejšnjem
razdelku vtipkali C-x C-s in ga shranili v datoteko.

Tudi delovno področje »Messages« ne pripada nobeni datoteki, ampak
vsebuje sporočila, ki jih je Emacs izpisoval v odzivnem področju na
dnu zaslona.

>> Vtipkajte C-x b *Messages* <Return> in si oglejte delovno področje
   s sporočili, zatem pa vtipkajte C-x b TUTORIAL <Return> in se tako
   vrnite v učbenik.

Če ste spreminjali besedilo ene datoteke, potem pa poiskali drugo, to
ne shrani spremeb v prvo datoteko. Te ostanejo znotraj Emacsa, na
delovnem področju, ki pripada prvi datoteki. Ustvarjenje ali
spreminjanje delovnega področja druge datoteke nima nobenega vpliva na
področje prve. To je zelo uporabno, pomeni pa tudi, da potrebujemo
udobno pot, da shranimo delovno področje prve datoteke. Nerodno bi
bilo preklapljanje na prvo področje s C-x C-f, da bi shranili s C-x
C-s. Namesto tega imamo:

	C-x s     Shrani nekatera delovna področja

Ukaz C-x poišče delovna področja, katerih vsebina je bila spremenjena,
odkar je bila zadnjič shranjena na datoteko. Za vsako tako delovno
področje C-x s vpraša, če ga želite shraniti.


* RAZŠIRJEN NABOR UKAZOV
------------------------

Še mnogo, mnogo je ukazov Emacsa, ki bi zaslužili, da jih obesimo na
razne kontrolne in meta znake. Emacs se temu izogne z ukazom X (iz angl.
eXtend - razširiti), ki uvede ukaz iz razširjenega nabora. Dveh vrst je:

	C-x	Znakovna razširitev (angl. Character eXtend).
		Sledi mu en sam znak.
	M-x	Razširitev s poimenovanim ukazom. Sledi mu dolgo ime
		ukaza.

Tudi ti ukazi so na splošno uporabni, ne uporabljamo pa jih tako
pogosto kot tiste, ki ste se jih že naučili. Dva ukaza iz razširjenega
nabora že poznamo: C-x C-f, s katerim poiščemo datoteko, in C-x C-s, s
katerim datoteko shranimo. Še en primer je ukaz, s katerim Emacsu
povemo, da želimo končati z delom iz iziti iz Emacsa. Ta ukaz je C-x
C-c (ne skrbite: preden konča, Emacs ponudi, da shrani vse spremenjene
datoteke).

Če uporabljate grafični vmesnik, ne potrebujete posebnega ukaza za
preklop iz Emacsa v katerikoli drug program, ampak to opravite z miško
ali ukazom upravljalnika oken. Če pa uporabljate besedilni terminal,
ki lahko prikazuje le en program naenkrat, morate začasno zapustiti
Emacs, da preklopite na drug program.

Z ukazom C-z Emacs zapustimo samo *začasno*, tako da lahko ob vrnitvi
nadaljujemo z delom, kjer smo ostali. Na sistemih, ki to dopuščajo,
ukaz C-z izide iz Emacsa v ukazno lupino, a ga ne konča - če
uporabljate ukazno lupino C, se lahko vrnete z ukazom »fg« ali
splošneje z ukazom »%emacs«.

Drugod ukaz C-z požene sekundarno ukazno lupino, tako da lahko
poženete kakšen drug program in se kasneje vrnete v Emacs. V tem
primeru pravzaprav Emacsa ne zapustimo. Ukaz »exit« v ukazni lupini
je navadno način, da zapremo sekundarno lupino in se vrnemo v Emacs.

Ukaz C-x C-c uporabimo, če se nameravamo odjaviti s sistema. To je
tudi pravilen način za izhod iz Emacsa, če je tega pognal program za
delo s pošto ali kak drug program.

Ukazov C-x je veliko. Zaenkrat smo spoznali naslednje:

	C-x C-f		Poišči datoteko.
	C-x C-s		Shrani datoteko.
	C-x C-b		Prikaži seznam delovnih področij.
	C-x C-c		Končaj Emacs.
	C-x 1		Zapri vsa okna razen enega.
	C-x u		Preklic zadnjega ukaza.

Poimenovani razširjeni ukazi so ukazi, ki se uporabljajo še bolj
poredko, ali pa se uporabljajo samo v nekaterih načinih dela.  Eden
takih je na primer ukaz replace-string, ki po vsem besedilu zamenja en
niz znakov z drugim. Ko vtipkate M-x, se to izpiše v pogovornem
vmesniku na dnu zaslona, Emacs pa čaka, da vtipkate ime ukaza, ki ga
želite priklicati; v tem primeru je to »replace-string«. Vtipkajte
samo »repl s<TAB>« in Emacs bo dopolnil ime (<TAB> je tabulatorska
tipka; navadno jo najdemo nad tipko Caps Lock ali Shift na levi strani
tipkovnice). Ukaz vnesete s pritiskom na <Return>.

Ukaz replace-string potrebuje dva argumenta -- niz, ki ga želite
zamenjati, in niz, s katerim bi radi zamenjali prvega. Vsakega posebej
vnesete in zaključite s pritiskom na tipko Return.

>> Premaknite kazalček na prazno vrstico dve vrstici pod to, zatem
   vtipkajte M-x repl s<Return>zamenjala<Return>spremenila<Return>.

   Opazite, kako se je ta vrstica zamenjala? Vse besede
   z-a-m-e-n-j-a-l-a od tod do konca besedila ste nadomestili z besedo
   »spremenila«.


* AVTOMATIČNO SHRANJEVANJE
--------------------------

Spremembe v datoteki, ki jih še niste shranili na disk, so izgubljene,
če medtem denimo zmanjka elektrike. Da bi vas zavaroval pred tem,
Emacs periodično avtomatično shrani vse datoteke, ki jih
urejate. Avtomatično shranjena datoteka se od izvorne razlikuje po
znaku »#« na začetku in koncu imena: če se je vaša datoteka imenovala
»hello.c«, se avtomatično shranjena datoteka imenuje
»#hello.c#«. Ko normalno shranite datoteko, avtomatično shranjena
datoteka ni več potrebna, in Emacs jo pobriše.

Če res pride do izgube podatkov v pomnilniku, lahko povrnete avtomatično
shranjeno besedilo tako, da normalno poiščete datoteko (pravo ime
datoteke, ne ime avtomatično shranjene datoteke), zatem pa vtipkate M-x
recover-file <Return>. Ko vas vpraša za potrditev, vtipkajte yes<Return>
za nadaljevanje in povrnitev avtomatično shranjenenih podatkov.


* ODZIVNO PODROČJE
------------------

Kadar Emacs opazi, da počasi vtipkavate ukaz, odpre v zadnji vrstici
na dnu zaslona odzivno področje in v njem sproti prikazuje natipkano.


* STATUSNA VRSTICA
------------------

Vrstica nad odzivnim področjem je statusna vrstica. Ta kaže verjetno
nekaj podobnega kot:

--:**- TUTORIAL          (Fundamental)--L670--58%----------------------

V njej so izpisani pomembni podatki o stanju Emacsa in besedilu, ki ga
urejate.

Zdaj že veste, kaj pomeni ime datoteke -- to je datoteka, ki ste jo
poiskali. Oznaka --NN%-- pomeni, da je nad vrhom zaslona še NN
odstotkov celotne datoteke. Če je začetek datoteke na zaslonu, bo
namesto »0%« pisalo »Top«. Podobno bo pisalo »Bot«, če je
zadnja vrstica datoteke na zaslonu. Če je datoteka, ki jo ogledujete,
tako kratka, da gre vsa na en zaslon, pa bo pisalo »All«.

Črka L in številke za njo kažejo položaj še drugače, kot zaporedno
številko vrstice, v kateri je kazalček.

Zvezdice na začetku vrstice pomenijo, da ste datoteko že spreminjali.
Tik po tem, ko ste odprli ali shranili datoteko, ni nobenih zvezdic,
so samo črtice.

Del statusne vrstice znotraj oklepajev vam pove, v kakšnem načinu dela
Emacs. Privzeti način je osnovni način (Fundamental), v katerem ste
sedaj. Fundamental je eden od glavnih načinov (angl. major
mode). Emacs pozna veliko različnih glavnih načinov. Nekateri od njih
so namenjeni pisanju programov, kot na primer Lisp, ali pisanju
besedil, kot npr. Text. Naenkrat je lahko aktiven le en glavni način,
njegovo ime pa je vedno izpisano v statusni vrstici, kjer zdaj piše
Fundamental.

Glavni načini lahko spremenijo pomen nekaterim ukazom. Obstajajo,
denimo, ukazi za pisanje komentarjev v programu, in ker ima vsak
programski jezik svoje predstave o tem, kako mora komentar izgledati,
mora vsak glavni način vnesti komentarje drugače. Ker je vsak glavni
način ime razširjenega ukaza, lahko tako tudi izbiramo glavni
način. Na primer, M-x fundamental-mode vas postavi v način
Fundamental.

Če nameravate popravljati slovensko (ali angleško) besedilo, kot je na
primer tole, boste verjetno izbrali tekstovni način (Text).
>> Vtipkajte M-x text-mode <Return>.

Brez skrbi, noben od ukazov Emacsa, ki ste se jih naučili, se s tem ne
spremeni kaj dosti. Lahko pa opazite, da Emacs zdaj jemlje opuščaje za
dele besed, ko se premikate z M-f ali M-b. V osnovnem načinu jih je
obravnaval kot meje med besedami.

Glavni načini navadno počenjajo majhne spremembe, kot je ta: večina
ukazov »opravi isti posel«, vendar pa to počnejo na različen način.

Dokumentacijo o trenutno aktivnem glavnem načinu dobite z ukazom C-h m.

>> Vtipkajte C-l C-l, da postavite to vrstico na vrh zaslona.
>> Vtipkajte C-h m, da vidite, v čem se tekstovni način (Text) razlikuje
   od osnovnega (Fundamental).
>> Vtipkajte C-x 1, da umaknete dokumentacijo z zaslona.

Glavnim načinom pravimo glavni načini zato, ker obstajajo tudi
podnačini (angl. minor modes). Podnačini ne nadomeščajo glavnih
načinom, ampak le spreminjajo njihovo obnašanje. Podnačine lahko
aktiviramo ali deaktiviramo neodvisno od glavnega načina in neodvisno
od ostalih podnačinov. Tako lahko ne uporabljate nobenega podnačina,
en podnačin, ali kombinacijo večih podnačinov.

Podnačin, ki je zelo uporaben posebno za pisanje besedil, je Auto
Fill. Ko je vklopljen, Emacs med pisanjem avtomatično deli vrstice na
presledkih med besedami, tako da vrstice niso predolge.

Vklopite ga lahko z ukazom M-x auto-fill-mode <Return>. Ko je
vklopljen, ga lahko izklopite z istim ukazom, M-x auto-fill-mode
<Return>. Z istim ukazom torej preklapljamo (angl. toggle) med
vklopljenim in izklopljenim stanjem.

>> Vtipkajte zdaj M-x auto-fill-mode <Return>. Potem začnite tipkati
   »asdf asdkl sdjf sdjkf«... dokler ne opazite, da je Emacs razbil
   vrstico na dve.  Med tipkanjem mora biti dovolj presledkov, saj
   Auto Fill prelamlja vrstice samo na presledkih.

Širina besedila je navadno postavljena na 70 znakov, kar pa lahko
spremenite z ukazom C-x f. Novo širino morate podati kot številčni
argument.

>> Vtipkajte C-x f in argument 20. (C-u 2 0 C-x f). Zatem vtipkajte
   nekaj besedila in poglejte, če bo Emacs res delil vrstice pri 20
   znakih. Potem z ukazom C-x f postavite mejo nazaj na 70.

Auto Fill deluje le, kadar pišete novo besedilo, ne pa,
kadar popravljate že napisan odstavek.
Tak odstavek lahko poravnate tako, da kazalček premaknete nekam
znotraj odstavka in ukažete M-q (META-q).

>> Premaknite kazalček v prejšnji odstavek in izvedite M-q.


* ISKANJE
---------

Emacs lahko v besedilu poišče niz znakov (»niz« je zaporedje soslednih
znakov), naprej ali nazaj po besedilu. Iskanje spada v skupino ukazov
za premikanje kazalčka, saj premakne kazalček na kraj v besedilu, kjer
je našel iskani niz.

Iskanje v Emacsu je »inkrementalno«. To pomeni, da se iskanje odvija
hkrati s tem, ko tipkate iskani niz.

Ukaza za iskanje sta C-s za iskanje naprej po datoteki in C-r za
iskanje nazaj po datoteki. POČAKAJTE! Ne preizkušajte jih še ta hip!

Ko boste natipkali C-s, boste opazili niz »I-search« kot pozivnik
v pogovornem vmesniku. To vam pove, da je Emacs v inkrementalnem iskanju
in vas čaka, da začnete tipkati, kar iščete. <Return> zaključi iskanje.

>> Pritisnite zdaj C-s. POČASI, črko za črko, vtipkajte besedo
   »kazalček«. Za vsako vtipkano črko se ustavite in si oglejte, kaj
   se je zgodilo s kazalčkom.
>> Še enkrat pritisnite C-s, da poiščete naslednji »kazalček«.
>> Šestkrat pritisnite <DEL> in opazujte, kako se premika kazalček.
>> Končajte iskanje s tipko <Return>.

Ste videli, kaj se je zgodilo? Emacs pri inkrementalnem iskanju skuša
poiskati niz, ki ste ga natipkali do tistega hipa. Da poiščete
naslednje mesto, kjer se pojavi »kazalček«, samo še enkrat
pritisnete C-s. Če takega mesta ni, Emacs čivkne in vam sporoči, da
iskanje ni uspelo. Tudi C-g prekine iskanje.

Če sredi inkrementalnega iskanja pritisnete <DEL>, boste opazili,
da to pobriše zadnji znak v iskanem nizu, kazalček pa se premakne
nazaj na mesto v besedilu, kjer je našel krajši niz. Na primer,
predpostavimo, da ste do zdaj natipkali »ka« in je kazalček na
mestu, kjer se prvič pojavi »ka«. Če zdaj pritisnete <DEL>, boste
s tem v pogovornem vmesniku izbrisali »a«, hkrati pa se bo kazalček
postavil na mesto, kjer je prvič našel »k«, preden ste natipkali še
»a«.

Če sredi iskanja vtipkate katerikoli kontrolni znaki ali metaznak
(razen tistih, ki imajo poseben pomen pri iskanju, to sta C-s in C-r),
se iskanje prekine.

C-s začne iskati na mestu v datoteki, kjer trenutno stoji kazalček, in
išče do konca datoteke. Če bi radi iskali proti začetku datoteke,
namesto C-s vtipkamo C-r.  Vse, kar smo povedali o ukazu C-s, velja
tudi za C-r, le smer iskanja je obrnjena.


* VEČ OKEN NA ZASLONU
---------------------

Ena simpatičnih lastnosti Emacsa je, da zna hkrati prikazati več oken
na zaslonu, tudi če ne delamo v grafičnem načinu. (Opozorimo naj, da
Emacs uporablja izraz »okvir« (angl. »frame«) - razložen je v
naslednjem razdelku - za tisto, čemur nekateri drugi programi pravijo
»okno« (angl. »window«). Priročnik za Emacs vsebuje glosar
uporabljenih izrazov.)

>> Premaknite kazalček v to vrstico in vtipkajte C-l C-l.
>> Zdaj vtipkajte C-x 2, da razdelite zaslon na dve okni.
   V obeh oknih imate odprt ta priročnik. Kazalček je ostal v zgornjem
   oknu.
>> Pritisnite C-M-v za listanje v spodnjem oknu.
   (Če nimate tipke META, tipkajte ESC C-v).
>> Vtipkajte C-x o (o kot »other«, drugi), da preselite kazalček v
   spodnje okno.
>> Z ukazoma C-v in M-v se v spodnjem oknu premikate po vsebini
   datoteke. Zgornje okno še vedno kaže ta navodila.
>> Ponovni C-x o vas vrne v zgornje okno. Kazalček se je vrnil na
   mesto, kjer je bil, preden smo skočili v spodnje okno.

Z ukazom C-x o lahko preklapljamo med okni. Izbrano okno, torej tisto,
v katerem urejamo besedilo, je tisto z zelo opaznim kazalčkom, ki
utripa, kadar ne tipkamo. Tudi ostala okna pa si zapomnijo, kje je
ostal kazalček. Če poganjate Emacs v grafičnem načinu, je položaj
kazalčka v teh oknih prikazan kot ne-utripajoč črtni pravokotnik.

Ukaz C-M-v je zelo uporaben, kadar urejamo besedilo v enem oknu,
drugega pa uporabljamo samo za pomoč. Ne da bi zapustili izbrano okno,
se lahko premikamo po vsebini drugega okna z ukazon C-M-v.

C-M-v je primer znaka CONTROL-META. Če imate v resnici tipko META (na
PC navadno levi Alt), lahko vtipkate C-M-v tako, da držite pritisnjeni
tako CONTROL kot META, medtem ko vtipkate v. Ni pomembno, katero od
tipk, CONTROL ali META, pritisnete prvo, saj obe delujeta šele, ko
pritisnete znak, ki sledi (v zgornjem primeru »v«).

Nasprotno pa je vrstni red pritiskanja pomemben, če nimate tipke META
in namesto nje uporabljate <ESC>. V tem primeru morate najprej
pritisniti <ESC>, potem pa Control-v. Obratna kombinacija,
CONTROL-<ESC> ne deluje. To je zato, ker je <ESC> znak sam po sebi, ne
pa modifikator, kot sta CONTROL in META.

>> V zgornjem oknu vtipkajte C-x 1, da se znebite spodnjega okna.

(Če bi vtipkali C-x 1 v spodnjem oknu, bi se znebili
zgornjega. Razmišljajte o tem ukazu kot »Obdrži samo eno okno, in
sicer tisto, v katerem sem zdaj.«)

Seveda ni nujno, da obe okni kažeta isto delovno področje. Če v enem
oknu izvedete C-x C-f in poiščete novo datoteko, se vsebina drugega
okna ne spremeni. V vsakem oknu lahko neodvisno obdelujete drugo
datoteko.

Pa še ena pot, kako v dveh oknih prikažete dve različni datoteki:

>> Vtipkajte C-x 4 C-f, in na pozivnik vtipkajte ime ene vaših
   datotek. Končajte z <Return>. Odpre se še eno okno in izbrana
   datoteka se pojavi v drugem oknu. Tudi kazalček se preseli v drugo
   okno.

>> Vtipkajte C-x o, da se vrnete nazaj v zgornje okno, in C-x 1, da
   zaprete spodnje okno.


* VEČ HKRATNIH OKVIROV
----------------------

Emacs lahko ustvari tudi več »okvirov«. Okvir je zbirka oken, skupaj z
menuji, drsniki, pogovornim vmesnikom ipd. V grafičnem načinu je
Emacsov »okvir« tisto, čemur večina drugih programov pravi »okno«. Če
delate v grafičnem načinu, je lahko več okvirov hkrati prikazanih na
zaslonu. V besedilnem terminalu imamo seveda na voljo le en okvir.

>> Vtipkajte M-x make-frame <Return>
   Opazite, kako se je na zaslonu pojavil nov okvir.

Vse, kar ste počeli v prvotnem okviru, lahko počnete tudi v novem.
Prvi okvir ni v ničemer poseben.

>> Vtipkajte M-x delete-frame <Return>
   Ukaz izbriše izbrani okvir.

Okvir lahko izbrišete tudi z običajnim načinom, ki ga ponuja grafični
sistem - pogosto s klikom na simbol »X« v enem od zgornjih kotov okna.
Če zaprete zadnji okvir, s tem obenem zaprete tudi Emacs.


* REKURZIVNI NIVOJI UREJANJA
----------------------------

Včasih boste prišli v nekaj, čemur se pravi »rekurzivni nivo
urejanja«. To se vidi po tem, da v statusni vrstici oglati oklepaji
oklepajo ime glavnega načina. V osnovnem načinu bi, na primer, videli
[(Fundamental)] namesto (Fundamental).

Iz rekurzivnega nivoja urejanja se rešite, če vtipkate ESC ESC ESC. To
zaporedje je vsenamenski ukaz »pojdi ven«. Uporabite ga lahko tudi
za ukinjanje odvečnih oken, ali vrnitev iz pogovornega vmesnika.

>> Pritisnite M-x, da odprete pogovorni vmesnik, zatem pa vtipkajte
   ESC ESC ESC, da pridete ven iz njega.

Z ukazom C-g ne morete iz rekurzivnega nivoja urejanja, ker C-g
prekliče ukaze ali argumente ZNOTRAJ rekurzivnega nivoja.


* DODATNA POMOČ
---------------

V tem uvodu smo poskušali zbrati dovolj informacij, da lahko začnete
Emacs uporabljati. Emacs ponuja toliko, da bi bilo nemogoče vse to
zbrati tukaj. Verjetno pa bi se vseeno radi naučili kaj o številnih
koristnih možnostih, ki jih še ne poznate. Emacs ima že vgrajene
veliko dokumentacije, do katere lahko pridete s pritiskom na CONTROL-h
(h kot »help«, pomoč).

Za pomoč pritisnete C-h, potem pa vtipkate znak, ki pove, kakšno pomoč
želite. Če ste poplnoma izgubljeni, vtipkajte C-h ? in Emacs vam bo
povedal, kakšna pomoč je sploh na voljo. Če ste vtipkali C-h, pa ste
si premislili, lahko ukaz prekličete s C-g.

(Če C-h ne prikaže sporočila o pomoči na dnu zaslona, poskusite
namesto tega pritisniti tipko F1 ali pa vtipkajte M-x help <Return>.)

Najosnovnejši tip pomoči prikaže C-h c. Pritisnite C-h, tipko c, zatem
pa ukazni znak ali zaporedje ukaznih znakov, in Emacs bo izpisal
kratek opis ukaza.

>> Vtipkajte C-h c C-p.
   Izpiše se nekaj takega kot

	C-p runs the command previous-line

Ukaz je izpisal ime funkcije, ki izvede ukaz. Ker so navadno imena
funkcij izbrana tako, da kaj povedo o tem, kaj funkcija počne, bo
verjetno to tudi dovolj za kratko osvežitev, če ste se z ukazom že
kdaj srečali.

Ukazu C-h lahko sledi tudi zaporedje znakov, kot na primer C-x C-s,
ali, če nimate tipke META, <Esc>v.

Za več informacij o ukazu vtipkajte C-h k namesto C-h c.

>> Vtipkajte C-h k C-p.

To odpre novo okno in v njem prikaže dokumentacijo o funkciji, obenem
z njenim imenom. Ko ste opravili, vtipkajte C-x 1, da se znebite okna
z pomočjo. Tega ni potrebno napraviti ta hip. Namesto tega lahko
urejate, medtem ko imate odprto okno s pomočjo, in ga zaprete, ko ste
končali.

Sledi še nekaj uporabnih možnosti, ki jih ponuja pomoč:

   C-h f	Opiši funkcijo. Kot argument morate podati ime
		funkcije.

>> Poskusite C-h f previous-line <Return>.
   To izpiše vse podatke, ki jih ima Emacs o funkciji, ki izvede ukaz C-p.

Podoben ukaz C-h v izpiše dokumentacijo za spremenljivke, vključno s
tistimi, s katerimi lahko nastavite obnašanje Emacsa. Ob pozivniku
morate vpisati ime spremenljivke.

   C-h a	 Apropos. Vtipkajte ključno besedo in Emacs bo izpisal
		 vse ukaze, ki vsebujejo to ključno besedo. Vse te
		 ukaze lahko prikličete z META-x. Pri nekaterih ukazih
		 bo Apropos izpisal tudi eno ali dvoznakovno
		 zaporedje, s katerim dosežete isti učinek.

>> Vtipkajte C-h a file <Return>.

To odpre novo okno, v katerem so vsa dolga imena ukazov, ki vsebujejo
»file« v imenu. Izvedete jih lahko z M-x. Pri nekaterih se izpiše
tudi kratek ukaz, npr. C-x C-f ali C-x C-w pri ukazih find-file in
write-file.

>> Pritisnite C-M-v, da se sprehajate po oknu s pomočjo. Poskusite
   nekajkrat.

>> Vtipkajte C-x 1, da zaprete okno s pomočjo.

   C-h i         Priročniki z navodili za uporabo (tkim. datoteke
		 »info«). Ta ukaz vas prestavi v posebno delovno
		 področje, imenovano »*info*«. V njem lahko prebirate
		 priročnike za programe, ki so nameščeni v sistemu. Z
		 ukazom m emacs<Return> denimo dobite priročnik za
		 urejevalnik Emacs. Če sistema Info še niste
		 uporabljali, vtipkajte ? in Emacs vas bo popeljal na
		 vódeni izlet po načinu Info in možnostih, ki jih
		 ponuja. Ko boste zaključili z branjem tega prvega
		 berila, bo priročnik za Emacs v sistemu Info vaš
		 glavni vir dokumentacije.


* DRUGE MOŽNOSTI
----------------

Še več se lahko naučite o Emacsu z branjem priročnika, bodisi
natisnjenega, bodisi znotraj samega Emacsa (uporabite menu Help ali
vtipkajte C-h r). Dve možnosti, ki vam bosta morda posebej všeč, sta
samodejno zaključevanje vrstice, s katerim prihranite nekaj tipkanja,
in dired, s katerim poenostavimo delo z datotekami.

Samodejno zaključevanje vrstic je način, s katerim prihranimo nekaj
tipkanja. Če želite denimo preklopiti v delovno področje *Messages*,
je dovolj, da vtipkate C-x b *M<Tab> in Emacs bo sam dopolnil
preostanek imena delovnega področja. Samodejno zaključevanje deluje
tudi za imena ukazov in imena datotek. Samodejno zaključevanje je
opisano v priročniku za Emacs, razdelek »Completion«.

Dired omogoča izpis seznama datotek v imeniku (in po možnosti tudi
podimenikih), premikanje po seznamu, obiskovanje (odpiranje),
preimenovanje, brisanje in druge operacije z datotekami. Dired je
opisav v priročniku za Emacs, razdelek »Dired«.

Priročnik opisuje tudi mnoge druge možnosti Emacsa.


* ZAKLJUČEK
-----------

Emacs zapustite z ukazom C-x C-c.

Ta učbenik je napisan z namenom, da bi bil razumljiv vsem novincem v
Emacsu. Če se vam kaj ne zdi jasno napisano, ne valite krivde nase -
pritožite se!


* RAZMNOŽEVANJE IN RAZŠIRJANJE
------------------------------

Angleški izvirnik tega uvoda v Emacs je naslednik dolge vrste tovrstnih
besedil, začenši s tistim, ki ga je Stuart Cracraft napisal za izvorni
Emacs. V slovenščino ga je prevedel Primož Peterlin.

To besedilo, kot sam GNU Emacs, je avtorsko delo, in njegovo
razmnoževanje in razširjanje je dovoljeno pod naslednjimi pogoji:

Copyright © 1985, 1996, 1998, 2001-2012  Free Software Foundation, Inc.

  Ta datoteka je del paketa GNU Emacs.

  GNU Emacs je prost program; lahko ga redistribuirate in/ali prirejate
  po pogojih, določenih v dovoljenju za rabo »GNU General Public License«,
  izdanem pri Free Software Foundation, bodisi 3. izdaje tega dovoljenja,
  bodisi katerekoli kasnejše izdaje, ki je na voljo.

  GNU Emacs je ponujen v dobri veri, da je uporaben, vendar zanj NI
  NOBENEGA JAMSTVA, niti implicitnih jamstev PRIMERNOSTI ZA PRODAJO
  ali USTREZNOSTI ZA DOLOČEN NAMEN. Podrobnosti so na voljo v »GNU
  General Public License«.

  Kopijo »GNU General Public License« bi morali prejeti skupaj s paketom
  GNU Emacs. Če je niste, je na voljo na <http://www.gnu.org/licenses/>.

Prosimo, preberite datoteko COPYING in potem ponudite kopijo programa
GNU Emacs svojim prijateljem. Pomagajte zatreti obstrukcionizem
(»lastništvo«) v programju tako, da uporabljate, pišete in delite
prosto programje!

;;; Local Variables:
;;; coding: utf-8
;;; sentence-end-double-space: nil
;;; End:
