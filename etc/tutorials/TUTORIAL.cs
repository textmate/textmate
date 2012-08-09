Tutori�l k Emacsu.  Podm�nky viz na konci.
Do �e�tiny p�elo�il Milan Zamazal <pdm@zamazal.org>.

M�te p�ed sebou tutori�l k Emacsu.

P��kazy Emacsu obecn� vyu��vaj� kl�vesu CONTROL (ob�as ozna�ovanou CTRL nebo
CTL) nebo kl�vesu META (ob�as ozna�ovanou EDIT nebo ALT).  Abychom tyto n�zvy
nemuseli st�le ps�t v pln�m zn�n�, budeme pou��vat n�sleduj�c� zkratky:

 C-<chr>  znamen� p�idr�et kl�vesu CONTROL a stisknout znak <chr>.
          Tedy C-f znamen�: p�idr�te kl�vesu CONTROL a stiskn�te f.
 M-<chr>  znamen� p�idr�et kl�vesu META, EDIT nebo ALT a stisknout <chr>.
          Pokud ��dnou z kl�ves META, EDIT ani ALT nem�te, tak m�sto toho
          stiskn�te a pus�te kl�vesu ESC a pot� <chr>.  Kl�vesu ESC budeme
          zna�it <ESC>.

D�le�it� pozn�mka: pr�ci s Emacsem ukon��te stiskem C-x C-c (dva znaky).
Znaky ">>" na lev�m okraji zna�� m�sta, kde si m�te vyzkou�et p��kaz.
Nap��klad:
<<Blank lines inserted here by startup of help-with-tutorial>>
[Prost�edn� ��st obrazovky je pr�zdn� z�m�rn�. Text pokra�uje n�e.]
>>  Nyn� stiskn�te C-v (view next screen) pro posun na dal�� obrazovku.
        (Sm�le do toho, prove�te to p�idr�en�m kl�vesy CONTROL a stiskem v.)
	Od t�to chv�le byste toto m�li prov�d�t kdykoliv do�tete zobrazenou
        obrazovku.

V�imn�te si, �e p�i posuvu obrazovek v�dy z�st�vaj� zobrazeny dva ��dky
z p�edchoz� obrazovky; to poskytuje ur�itou n�vaznost p�i postupn�m
�ten� textu.

Prvn� v�c, kterou pot�ebujete v�d�t, je jak se v textu pohybovat
z jednoho m�sta na druh�.  U� v�te, jak se posunout o jednu obrazovku
vp�ed, pomoc� C-v.  K p�echodu o obrazovku zp�t pou�ijte M-v
(p�idr�te kl�vesu META a stiskn�te v nebo stiskn�te <ESC>v, jestli�e
nem�te ��dnou z kl�ves META, EDIT nebo ALT).

>>  Zkuste stisknout M-v a pak C-v, n�kolikr�t to zopakujte.


* SHRNUT�
---------

K prohl�en� obrazovkov�ch str�nek jsou u�ite�n� n�sleduj�c� p��kazy:

	C-v	Posun o obrazovku vp�ed
	M-v	Posun o obrazovku zp�t
	C-l	Smaz�n� obrazovky a znovuzobrazen� cel�ho textu,
		 p�itom se text pod kurzorem p�esune ke st�edu obrazovky.
		 (Jedn� se o CONTROL-L a ne CONTROL-1.)

>> Najd�te kurzor a zapamatujte si, jak� text je kolem n�j.
   Pak stiskn�te C-l.
   Najd�te kurzor znovu a v�imn�te si, �e je kolem n�j tent�� text.


* Z�KLADN� OVL�D�N� KURZORU
---------------------------

Pohyb mezi obrazovkami je u�ite�n�, ale jak se p�em�st�te na konkr�tn�
m�sto v textu na obrazovce?

Je toho mo�no dos�hnout n�kolika zp�soby.  Nejz�kladn�j��m zp�sobem je
pou�it� p��kaz� C-p, C-b, C-f a C-n.  Ka�d� z t�chto p��kaz� p�esune
kurzor na obrazovce o jeden ��dek nebo sloupec v dan�m sm�ru.
Zde je tabulka zn�zor�uj�c� sm�r posuvu kurzoru vyvolan� t�mito �ty�mi
p��kazy:

			  P�edchoz� ��dek, C-p
				  :
				  :
      Dozadu, C-b .... Moment�ln� pozice kurzoru .... Dop�edu, C-f
				  :
				  :
			 N�sleduj�c� ��dek, C-n

>> P�esu�te kurzor na prost�edn� ��dek tohoto diagramu pomoc�
   C-n nebo C-p.  Potom stiskn�te C-l, abyste na obrazovce vid�li cel�
   diagram vycentrov�n.

Pravd�podobn� se v�m budou tyto p��kazy snadno pamatovat podle
po��te�n�ch p�smen anglick�ch n�zv�: P jako previous (p�edchoz�),
N jako next (n�sleduj�c�), B jako backward (zp�t), F jako forward (vp�ed).
Jsou to z�kladn� p��kazy pro pohyb kurzoru a budete je pou��vat
neust�le, tak�e by bylo velmi vhodn�, kdybyste se je te� nau�ili.

>> Prove�te n�kolikr�t C-n, abyste kurzor p�esunuli na tento ��dek.

>> Posu�te kurzor dovnit� ��dku pomoc� n�kolika C-f a pak nahoru stiskem C-p.
   Pozorujte, co C-p d�l�, kdy� je kurzor uprost�ed ��dku.

Ka�d� ��dek textu kon�� znakem nov�ho ��dku, kter� jej odd�luje od ��dku
n�sleduj�c�ho.  Znakem nov�ho ��dku by m�l b�t ukon�en i posledn� ��dek
souboru (p�esto�e to Emacs nevy�aduje).

>> Vyzkou�ejte C-b na za��tku ��dku.  Kurzor by se m�l p�esunout na konec
   p�edchoz�ho ��dku, nebo� jej t�m p�esunete p�es znak nov�ho ��dku.

C-f funguje analogicky jako C-b, tj. na konci ��dku dojde k p�esunu na
dal�� ��dek.

>> Prove�te n�kolik C-b, tak�e uvid�te, kde se nach�z� kurzor.
   Pak prov�d�jte C-f, abyste se vr�tili na konec ��dku.
   Pak prove�te je�t� jednou C-f, abyste se p�esunuli na n�sleduj�c�
   ��dek.

Kdy� kurzorem p�ejdete p�es horn� nebo doln� okraj obrazovky, posune se
text za p��slu�n�m okrajem na obrazovku.  Tato vlastnost se naz�v�
"scrollov�n�".  Umo��uje p�em�stit kurzor na libovoln� m�sto v textu,
ani� by kurzor opustil obrazovku.

>> Zkuste posunout kurzor pod doln� okraj obrazovky pomoc� C-n a pozorujte,
   co se stane.

Jestli�e je posun po znac�ch p��li� pomal�, m��ete se pohybovat po
slovech.  M-f (META-f) prov�d� posun o slovo vp�ed a M-b prov�d� posun
o slovo zp�t.

>> Stiskn�te n�kolikr�t M-f a M-b.

Pokud se kurzor nach�z� uprost�ed slova, M-f provede p�esun na konec
tohoto slova.  Nach�z�-li se kurzor v meze�e mezi slovy, M-f provede
p�esun na konec n�sleduj�c�ho slova.  M-b pracuje analogicky v opa�n�m
sm�ru.

>> Stiskn�te n�kolikr�t M-f a M-b prolo�en� s C-f a C-b, abyste vid�li
   v�sledky p��kaz� M-f a M-b prov�d�n�ch z r�zn�ch m�st uvnit� slov a
   mezi nimi.

V�imn�te si analogie mezi C-f a C-b na jedn� stran� a M-f a M-b na
stran� druh�.  Znaky s kl�vesou META jsou velmi �asto vyu��v�ny pro operace
vztahuj�c� se k entit�m definovan�m jazykem (slova, v�ty, odstavce),
zat�mco znaky s kl�vesou CONTROL pracuj� na z�kladn�ch prvc�ch
nez�visl�ch na tom, co zrovna editujete (znaky, ��dky, apod.).

Tato analogie plat� tak� pro ��dky a v�ty: C-a a C-e prov�d� p�esun
na za��tek a konec ��dku, M-a a M-e prov�d� p�esun na za��tek a konec
v�ty.

>> Zkuste n�kolikr�t C-a a pot� n�kolikr�t C-e.
   Zkuste n�kolikr�t M-a a pot� n�kolikr�t M-e.

V�imn�te si, �e opakovan� C-a ned�l� nic, zat�mco opakovan� M-a v�dy
prov�d� posun o dal�� v�tu. Principu analogie to sice p��li�
neodpov�d�, ale p�esto je toto chov�n� mo�no pova�ovat za p�irozen�.

Pozice kurzoru v textu se tak� naz�v� "bod" ("point").  Abychom to
parafr�zovali, kurzor je vid�t na obrazovce v m�st�, kde je bod um�st�n
v textu.

Zde je p�ehled jednoduch�ch operac� pro pohyb kurzoru v�etn� p��kaz� pro
pohyb mezi slovy a v�tami:

	C-f	P�esun o znak vp�ed
	C-b	P�esun o znak zp�t

	M-f	P�esun o slovo vp�ed
	M-b	P�esun o slovo zp�t

	C-n	P�esun na n�sleduj�c� ��dek
	C-p	P�esun na p�edchoz� ��dek

	C-a	P�esun na za��tek ��dku
	C-e	P�esun na konec ��dku

	M-a	P�esun zp�t na za��tek v�ty
	M-e	P�esun vp�ed na konec v�ty

>> Vyzkou�ejte si te� n�kolikr�t v�echny tyto p��kazy pro procvi�en�.
   Jsou to nejpou��van�j�� p��kazy.

Dal�� dva d�le�it� p��kazy pro pohyb kurzoru jsou M-< (META men��-ne�),
kter� provede p�esun na za��tek cel�ho textu, a M-> (META v�t��-ne�),
kter� provede p�esun na konec cel�ho textu.

Na v�t�in� termin�l� je "<" nad ��rkou, tak�e pro vyvol�n� tohoto znaku
mus�te pou��t kl�vesu Shift.  Na t�chto termin�lech je tedy nutno pou��t
kl�vesu Shift i v p��pad� p��kazu M-<; bez kl�vesy Shift byste provedli
M-��rka.

>> Zkuste te� M-< pro p�esun na za��tek tutori�lu.
   Pou�ijte pak opakovan� C-v, abyste se op�t vr�tili sem.

>> Zkuste te� M-> pro p�esun na konec tutori�lu.
   Pou�ijte pak opakovan� M-v, abyste se op�t vr�tili sem.

Kurzor m��ete p�esouvat tak� pomoc� kurzorov�ch kl�ves (kl�vesy
se �ipkami), pokud je v� termin�l m�.  My v�ak doporu�ujeme nau�it se
C-b, C-f, C-n a C-p, a to ze t�� d�vod�.  Za prv�, tyto kl�vesy funguj�
na v�ech typech termin�l�.  Za druh�, jakmile jednou z�sk�te cvik
v pou��v�n� Emacsu, zjist�te, �e pou��v�n� t�chto CTRL znak� je
rychlej�� ne� pou��v�n� kurzorov�ch kl�ves (proto�e nemus�te p�esouvat
ruku z psac� pozice).  Za t�et�, zvyknete-li si pou��vat tyto CTRL-znak
p��kazy, snadno se nau��te pou��vat jin� pokro�il� p��kazy pro pohyb
kurzoru.

V�t�ina p��kaz� Emacsu akceptuje numerick� argument; ten pro v�t�inu
p��kaz� slou�� jako opakova�.  Po�et opakov�n� p��kazu zad�te
prost�ednictv�m stisku C-u n�sledovan�ho stiskem p��slu�n�ch ��slic p�ed
vyvol�n�m p��kazu.  M�te-li META (nebo EDIT �i ALT) kl�vesu, existuje
alternativn� mo�nost zad�n� numerick�ho argumentu: p�idr�te kl�vesu META
a stiskn�te p��slu�n� ��slice.  Doporu�ujeme nau�it se C-u metodu,
proto�e ta funguje na jak�mkoliv termin�lu.

Nap��klad C-u 8 C-f provede p�esun o osm znak� vp�ed.

V�t�ina p��kaz� pou��v� numerick� argument jako opakova�.  Jist�
v�jime�n� p��kazy jej pou��vaj� jin�m zp�sobem.  Mezi tyto v�jimky pat��
C-v a M-v.  Dostanou-li numerick� argument, posunou obrazovku nahoru
nebo dol� o odpov�daj�c� po�et ��dk� m�sto obrazovek.  Nap��klad
C-u 4 C-v posune obrazovku o 4 ��dky.

>> Zkuste te� stisknout C-u 8 C-v.

To by m�lo posunout obrazovku o 8 ��dk� nahoru.  Pokud byste ji cht�li
posunout zp�t dol�, m��ete d�t argument p��kazu M-v.

Pou��v�te-li X Window, m�li byste m�t na lev� stran� emacsov�ho okna
vysokou obd�ln�kovou oblast, naz�vanou scrollbar.  M��ete pak text
posouvat klik�n�m my�� na scrollbar.

>> Zkuste stisknout prost�edn� tla��tko na vrcholu zv�razn�n� oblasti
   uvnit� scrollbaru.  To by m�lo text posunout na pozici danou t�m, jak
   vysoko nebo n�zko jste kliknuli.

>> Zkuste p�i stisknut�m prost�edn�m tla��tku posouvat my�� nahoru a
   dol�.  Uvid�te, jak se text posouv� nahoru a dol� podle toho, jak
   posouv�te my��.


* KDY� EMACS NEREAGUJE
----------------------

Jestli�e Emacs p�estane reagovat na va�e p��kazy, m��ete prob�haj�c�
�innost bezpe�n� zastavit pomoc� C-g.  Pomoc� C-g m��ete zastavit
p��kaz, jeho� prov�d�n� trv� p��li� dlouho.

C-g m��ete pou��t tak� pro odstran�n� numerick�ho argumentu p��kazu,
kter� nechcete dokon�it.

>> Stiskn�te C-u 100 pro vytvo�en� numerick�ho argumentu 100 a pak
   stiskn�te C-g.  Nyn� stiskn�te C-f.  M�l by b�t proveden posun
   o pr�v� jeden znak, proto�e jste argument zru�ili prost�ednictv�m
   C-g.

Pokud jste omylem stiskli <ESC>, m��ete se jej zbavit pomoc� C-g.


* DEAKTIVOVAN� P��KAZY
----------------------

N�kter� p��kazy Emacsu jsou "deaktivovan�" ("disabled"), aby je
za��naj�c� u�ivatel� nemohli vyvolat n�hodn�.

Pokud vyvol�te n�kter� z deaktivovan�ch p��kaz�, Emacs zobraz� hl�en�
oznamuj�c�, kter� p��kaz to byl, s dotazem, zda chcete tento p��kaz
prov�st.

Pokud opravdu chcete p��kaz vyzkou�et, stiskn�te mezern�k jako odpov��
na tuto ot�zku.  Oby�ejn�, jestli�e nechcete deaktivovan� p��kaz
prov�st, odpov�zte na tuto ot�zku pomoc� "n".

>> Stiskn�te C-x C-l (co� je deaktivovan� p��kaz),
   pak na ot�zku odpov�zte n.


* OKNA
------

Emacs m��e m�t n�kolik oken (windows), z nich� ka�d� zobrazuje sv�j
vlastn� text.  Jak v�ce oken pou��vat, objasn�me pozd�ji.  Nyn� chceme
objasnit, jak se zbavit nadbyte�n�ch oken a vr�tit se do z�kladn�
jednookenn� editace.  Je to jednoduch�:

	C-x 1	Jedno okno (tj. zru�en� v�ech ostatn�ch oken)

Tedy vlo�en� CONTROL-x n�sledovan� ��slic� 1.  C-x 1 roz���� okno
obsahuj�c� kurzor p�es celou obrazovku.  Zru�� to v�echna ostatn� okna.

>> Stiskn�te C-h k C-f.
   Pozorujte, jak se aktu�ln� okno zmen�� a objev� se nov� okno za
   ��elem zobrazen� dokumentace k p��kazu C-f.

>> Stiskn�te C-x 1 a pozorujte, jak okno s dokumentac� zmiz�.


* VKL�D�N� A MAZ�N�
-------------------

Chcete-li vlo�it text, prost� jej napi�te.  Znaky, kter� vid�te,
jako A, 7, *, atd., jsou Emacsem ch�p�ny jako text a vkl�d�ny okam�it�.
Pro vlo�en� znaku nov�ho ��dku stiskn�te <Return> (kl�vesu Enter).

Posledn� znak, kter� jste napsali, m��ete smazat stiskem <Delete>.
<Delete> je kl�vesa, kter� m��e b�t na kl�vesnici ozna�ena "Del".
V n�kter�ch p��padech jako <Delete> slou�� kl�vesa "Backspace", av�ak ne
v�dy!

Obecn�ji, <Delete> ma�e znak bezprost�edn� p�ed moment�ln� pozic�
kurzoru.

>> Prove�te to te� -- napi�te n�kolik znak� a pak je sma�te n�kolika
   stisky <Delete>.  Nebojte se zm�n v tomto souboru; origin�ln�
   tutori�l se nezm�n�.  Toto je va�e osobn� kopie.

Kdy� se ��dek textu zv�t�� natolik, �e p�es�hne jeden ��dek obrazovky,
je zobrazen na v�ce ��dc�ch obrazovky.  ��dek textu, kter� pokra�uje na
dal��m ��dku obrazovky, je indikov�n zp�tn�m lom�tkem ("\") na prav�m
okraji obrazovky.

>> Vkl�dejte text, a� dos�hnete prav�ho okraje, a pokra�ujte ve vkl�d�n�.
   Objev� se v�m pokra�ovac� ��dek.

>> Pou�ijte <Delete> pro smaz�n� textu, a� se ��dek textu op�t vejde na
   jeden ��dek obrazovky.  Pokra�ovac� ��dek zmiz�.

Znak nov�ho ��dku m��ete smazat jako kter�koliv jin� znak.  Smaz�n�
znaku nov�ho ��dku mezi dv�ma ��dky zp�sob� jejich spojen� do jedin�ho
��dku.  Je-li v�sledn� ��dek p��li� dlouh� na to, aby se ve�el na ���ku
obrazovky, bude zobrazen pokra�ovac�m ��dkem.

>> P�esu�te kurzor na za��tek ��dku a stiskn�te <Delete>.  To tento
   ��dek spoj� s ��dkem p�edchoz�m.

>> Stiskn�te <Return> pro znovuvlo�en� smazan�ho znaku nov�ho ��dku.

Vzpome�te si, �e v�t�ina p��kaz� Emacsu m��e dostat po�et opakov�n�;
v�etn� textov�ch znak�.  Opakov�n� textov�ch znak� je vlo�� n�kolikr�t.

>>  Vyzkou�ejte si to te� -- stiskn�te C-u 8 * pro vlo�en� ********.

Te� u� zn�te nejz�kladn�j�� zp�soby, jak n�co v Emacsu napsat a jak
opravovat chyby.  M��ete ov�em tak� mazat po slovech nebo po ��dc�ch.
Zde je shrnut� operac� pro maz�n� textu:

	<Delete>     Smaz�n� znaku bezprost�edn� p�ed kurzorem
	C-d   	     Smaz�n� znaku n�sleduj�c�ho za kurzorem

	M-<Delete>   Zru�en� slova bezprost�edn� p�ed kurzorem
	M-d	     Zru�en� slova n�sleduj�c�ho za kurzorem

	C-k	     Zru�en� textu od pozice kurzoru do konce ��dku
	M-k	     Zru�en� textu do konce aktu�ln� v�ty

V�imn�te si, �e <Delete> a C-d, resp. M-<Delete> a M-d, roz�i�uj�
paralelu zapo�atou C-f a M-f (pravda, <Delete> opravdu nen� CONTROL
znak, ale netrapme se t�m).  C-k a M-k jsou jako C-e a M-e ve smyslu
vztahu ��dk� k v�t�m.

Libovolnou ��st bufferu m��ete t� zru�it n�sleduj�c� metodou.
P�esu�te se na jeden konec t�to ��sti a stiskn�te C-@ nebo C-SPC
(libovolnou z t�chto kombinac�). (SPC ozna�uje mezern�k.)  P�esu�te
se na druh� konec t�to ��sti a stiskn�te C-w.  Text mezi t�mito
pozicemi bude zru�en.

>> P�esu�te kurzor na p�smeno L na za��tku p�edchoz�ho odstavce.
>> Stiskn�te C-SPC.  Emacs by m�l ve spodn�m ��dku obrazovky
   zobrazit zpr�vu "Mark set".
>> P�esu�te kurzor na p�smeno c ve slov� "konec" na druh�m ��dku
   odstavce.
>> Stiskn�te C-w.  Text za��naj�c� p�smenem L a kon��c� p�ed p�smenem
   c bude zru�en.

Uv�domte si, �e rozd�l mezi "ru�en�m" ("killing") a "maz�n�m"
("deleting") je ten, �e "zru�en�" v�ci mohou b�t zp�t vhozeny, zat�mco
"smazan�" nikoliv.  Obecn� p��kazy, kter� mohou smazat v�t�� mno�stv�
textu, ukl�daj� text, zat�mco p��kazy, kter� ma�ou jedin� znak nebo
pouze pr�zdn� ��dky a mezery, mazan� text neukl�daj�.

>> P�esu�te kurzor na za��tek nepr�zdn�ho ��dku.
   Pak stiskn�te C-k pro zru�en� textu na tomto ��dku.
>> Stiskn�te C-k podruh�.  Uvid�te, �e to zru�� znak nov�ho ��dku, kter�
   je za t�mto ��dkem.

V�imn�te si, �e jedno C-k zru�� obsah ��dku a druh� C-k zru�� ��dek
samotn� a posune v�echny dal�� ��dky nahoru.  C-k zpracov�v� numerick�
argument speci�ln�: zru�� odpov�daj�c� po�et ��dk� V�ETN� jejich
obsahu.  To u� nen� opakov�n�.  C-u 2 C-k zru�� dva ��dky a jejich
obsah; dvojit� stisk C-k by toto obvykle neud�lal.

Vracen� textu zp�t se naz�v� "vhazov�n�" ("yanking").  (P�edstavte
si op�tovn� vhazov�n�, vracen� d��ve odstran�n�ho textu zp�tky.)
Zru�en� text m��ete vhodit bu� na stejn� m�sto, kde byl zru�en,
nebo na jin� m�sto v bufferu, nebo dokonce i do jin�ho souboru.
Text m��ete vhodit i v�cekr�t, vytv���te tak jeho dal�� kopie.

P��kazem pro vhazov�n� je C-y.  Tento p��kaz vlo�� posledn� smazan�
text na pozici, na kter� se nach�z� kurzor.

>> Zkuste to; stiskn�te C-y pro vhozen� textu zp�t.

Stisknete-li n�kolikr�t C-k po sob�, v�echen smazan� text je ulo�en
spole�n� tak, aby bylo mo�n� vhodit zp�t v�echny ��dky najednou.

>> Stiskn�te n�kolikr�t C-k.

Nyn� obnovte posledn� zru�en� text:

>> Stiskn�te C-y.  Pak posu�te kurzor o n�kolik ��dk� n�e a stiskn�te
   C-y znova.  Nyn� vid�te, jak lze text kop�rovat.

Co kdy� m�te n�jak� text, kter� byste r�di vhodili zp�t a pak zru��te
n�co jin�ho?  C-y by vlo�ilo posledn� zru�en� text.  Av�ak p�edchoz�
text nen� ztracen.  M��ete jej z�skat zp�t pou�it�m p��kazu M-y.  Pot�,
co provedete C-y pro z�sk�n� posledn�ho zru�en�ho textu, stisk M-y
vym�n� tento vhozen� text za p�edchoz� zru�en� text.  Dal��mi a
dal��mi stisky M-y dost�v�te p�edch�zej�c� a p�edch�zej�c� zru�en�
texty.  Kdy� dos�hnete textu, kter� hled�te, nemus�te s n�m pro jeho
uchov�n� nic dal��ho prov�d�t.  Jednodu�e vhozen� text ponechejte, kde
je, a pokra�ujte v editaci.

Pokud opakujete M-y dostate�n� dlouho, dostanete se zp�tky k v�choz�mu
bodu (posledn� zru�en�mu textu).

>> Zru�te ��dek, p�esu�te kurzor n�kam jinam a zru�te jin� ��dek.
   Pak prove�te C-y pro vr�cen� druh�ho zru�en�ho ��dku.
   Pak prove�te M-y a vhozen� ��dek bude nahrazen prvn�m zru�en�m ��dkem.
   Opakujte M-y a pozorujte, co dost�v�te.  Pokra�ujte v tom, dokud se
   znovu neobjev� druh� zru�en� ��dek a pak n�kolik dal��ch.
   Chcete-li, m��ete zkusit p�edat M-y kladn� a z�porn� argumenty.


* UNDO
------

Jestli�e provedete v textu zm�nu a pak zjist�te, �e to byl omyl, m��ete
zm�nu vr�tit p��kazem undo, C-x u.

C-x u obvykle vr�t� zm�ny proveden� jedn�m p��kazem; pokud C-x u
zopakujete n�kolikr�t za sebou, ka�d� opakov�n� vr�t� jeden dal��
p��kaz.

Jsou ale dv� v�jimky: p��kazy, kter� nem�n� text, se nepo��taj� (to
zahrnuje p��kazy pro pohyb kurzoru a scrollov�n�) a znaky vkl�daj�c�
samy sebe jsou obvykle zpracov�v�ny ve skupin�ch a� po 20.  (To je kv�li
tomu, aby se zredukoval po�et C-x u nutn�ch pro vr�cen� vkl�dan�ho
textu.)

>> Zru�te tento ��dek pomoc� C-k, stiskn�te pak C-x u a ��dek by se m�l
   znovu objevit.

Alternativn� undo p��kaz je C-_; pracuje stejn� jako C-x u, je v�ak
m�n� pracn� jej aplikovat n�kolikr�t za sebou.  Nev�hodou C-_ je, �e
na n�kter�ch kl�vesnic�ch nen� z�ejm�, jak jej vyvolat.  To je d�vod,
pro� nab�z�me i C-x u.  Na n�kter�ch termin�lech m��ete C-_ vyvolat
stiskem / p�i stisknut�m CTRL.

Numerick� argument pro C-_ a C-x u funguje jako po�et opakov�n�.

Pomoc� p��kazu undo m��ete vr�tit zru�en� stejn� jako smazan� text.
Rozd�l mezi maz�n�m a ru�en�m textu ovliv�uje mo�nost vhozen� tohoto
textu pomoc� C-y, neovliv�uje mo�nosti p��kazu undo.


* SOUBORY
---------

Aby text, kter� editujete, z�stal trvale uchov�n, mus�te jej ulo�it do
souboru.  Jinak by byl po ukon�en� Emacsu ztracen.  Svoji editaci
spoj�te se souborem "vyhled�n�m" ("finding") souboru.  (Tak� se to
naz�v� "nav�t�ven�" ("visiting") souboru.)

Vyhled�n� souboru znamen�, �e vid�te jeho obsah v Emacsu.  V mnoha
ohledech je to, jako byste editovali p��mo ten soubor.  Nicm�n� zm�ny,
kter� prost�ednictv�m Emacsu �in�te, se nestanou trval�mi, dokud tyto
zm�ny do souboru "neulo��te" ("save").  T�m se zamez� necht�n�mu ponech�n�
��ste�n� zm�n�n�ho souboru v syst�mu.  Dokonce i kdy� soubor ulo��te,
Emacs uchov� p�vodn� soubor pod zm�n�n�m n�zvem pro p��pad, �e byste
zjistili, �e va�e �pravy byly chybn�.

Kdy� se pod�v�te do doln� ��sti obrazovky, uvid�te ��dek, kter� za��n� a
kon�� poml�kami a na za��tku m� "2J:-- TUTORIAL.cs" nebo n�co podobn�ho.
Tato ��st obrazovky obvykle obsahuje jm�no souboru, kter� je pr�v�
nav�t�ven.  Zrovna te� m�te nav�t�ven soubor nazvan� "TUTORIAL.cs",
kter� je va�� osobn� �m�rac� kopi� tutori�lu Emacsu.  Kdy� v Emacsu
vyhled�te soubor, jeho jm�no se objev� p�esn� na tom m�st�.

P��kazy pro vyhled�v�n� a ukl�d�n� soubor� se na rozd�l od ostatn�ch
p��kaz�, kter� jste se zat�m nau�ili, skl�daj� ze dvou znak�.  Oba
za��naj� znakem CONTROL-x.  Existuje cel� �ada p��kaz� za��naj�c�ch na
CONTROL-x; mnoho z nich pracuje se soubory, buffery a podobn�mi v�cmi.
Tyto p��kazy jsou dlouh� dva, t�i nebo �ty�i znaky.

Dal�� v�c� ohledn� p��kazu pro vyhled�n� souboru je to, �e mus�te ��ct,
kter� jm�no souboru chcete.  ��k�me, �e p��kaz "�te argument
z termin�lu" (v tomto p��pad� je argumentem jm�no souboru).  Pot� co
vyvol�te p��kaz

	C-x C-f   Vyhled�n� souboru

Emacs se v�s zept� na jm�no souboru.  Jm�no souboru, kter� p�ete, se
objevuje ve spodn�m ��dku obrazovky, kter� se v t�to situaci naz�v�
minibuffer.  Pro editaci jm�na souboru m��ete pou��vat obvykl� edita�n�
p��kazy Emacsu.

Zad�v�n� jm�na souboru (obecn� kter�koliv vstup z minibufferu) m��ete
zru�it p��kazem C-g.

>> Stiskn�te C-x C-f a pak C-g.  To minibuffer zru�� a takt� to zru��
   p��kaz C-x C-f, kter� minibuffer pou�il.  Tak�e nevyhled�te ��dn�
   soubor.

Po naps�n� jm�na souboru stiskn�te <Return>.
P��kaz C-x C-f pak za�ne pracovat a vyhled� soubor, kter� jste zvolili.
Po skon�en� p��kazu C-x C-f minibuffer zmiz�.

Po mal� chvilce se obsah souboru objev� na obrazovce a m��ete jej
editovat.  Kdy� chcete zm�ny trvale ulo�it, pou�ijte p��kaz

	C-x C-s   Ulo�en� souboru

To zkop�ruje text z Emacsu do souboru.  Kdy� to provedete poprv�, Emacs
p�ejmenuje p�vodn� soubor na soubor s nov�m jm�nem, aby nebyl ztracen.
Nov� jm�no je vytvo�eno p�id�n�m "~" na konec p�vodn�ho jm�na souboru.

Kdy� je ukl�d�n� dokon�eno, Emacs zobraz� jm�no zapsan�ho souboru.
M�li byste ukl�dat rozumn� �asto, abyste neztratili p��li� mnoho pr�ce
v p��pad� p�du syst�mu.

>> Stiskn�te C-x C-s pro ulo�en� va�� kopie tutori�lu.
   M�lo by to zobrazit "Wrote ...TUTORIAL.cs" ve spodn�m ��dku obrazovky.

Existuj�c� soubor m��ete vyhledat, abyste jej mohli prohl�et nebo
editovat.  M��ete tak� vyhledat soubor, kter� je�t� neexistuje.  To je
zp�sob, jak�m lze vytvo�it soubor v Emacsu: vyhledejte soubor, kter�
bude na za��tku pr�zdn� a pak za�n�te vkl�dat text ur�en� pro tento
soubor.  Kdy� po��d�te o ulo�en�, Emacs skute�n� vytvo�� soubor
s textem, kter� jste vlo�ili.  Od t� chv�le se pak m��ete c�tit, jako
kdybyste editovali ji� existuj�c� soubor.


* BUFFERY
---------

Jestli�e vyhled�te pomoc� C-x C-f druh� soubor, prvn� soubor v Emacsu
z�st�v�.  M��ete se do n�j zp�t p�epnout jeho op�tovn�m vyhled�n�m
pomoc� C-x C-f.  T�mto zp�sobem m��ete do Emacsu dostat pom�rn� hodn�
soubor�.

>> Vytvo�te soubor pojmenovan� "foo" stiskem C-x C-f foo <Return>.
   Potom vlo�te n�jak� text, zeditujte jej a ulo�te "foo" stiskem C-x C-s.
   Nakonec stiskn�te C-x C-f TUTORIAL.cs <Return>, ��m� se vr�t�te zp�t do
   tutori�lu.

Emacs ukl�d� text ka�d�ho souboru do objektu naz�van�ho "buffer".
Vyhled�n� souboru vytvo�� v Emacsu nov� buffer.  Chcete-li vid�t seznam
buffer�, kter� moment�ln� existuj� ve va�em procesu Emacs, stiskn�te:

	C-x C-b   Seznam buffer�

>> Zkuste te� C-x C-b.

Pod�vejte se, �e ka�d� buffer m� v seznamu jm�no a m��e tam m�t tak� jm�no
souboru, jeho� text obsahuje.  N�kter� buffery neodpov�daj� soubor�m.
Nap��klad buffer pojmenovan� "*Buffer List*" nem� ��dn� soubor.  Je to
buffer, kter� obsahuje seznam buffer� vytvo�en� pomoc� C-x C-b.
JAK�KOLIV text, kter� vid�te v emacsov�m okn�, je v�dy sou��st�
n�jak�ho bufferu.

>> Stiskn�te C-x 1, abyste se zbavili seznamu buffer�.

Pokud provedete zm�ny textu jednoho souboru a pak vyhled�te jin� soubor,
nezp�sob� to ulo�en� prvn�ho souboru.  Jeho zm�ny z�st�vaj� v Emacsu
uchov�ny v jemu odpov�daj�c�m bufferu.  Vytvo�en� a editace druh�ho
souboru nem� ��dn� vliv na buffer prvn�ho souboru.  To je velmi
u�ite�n�, ale tak� to znamen�, �e pot�ebujete vhodn� zp�sob, jak ulo�it
buffer prvn�ho souboru.  Nutnost p�epnout se zp�tky pomoc� C-x C-f, aby
jej bylo mo�no ulo�it prost�ednictv�m C-x C-s, by byla nem�stn�
obt�uj�c�.  Tak�e m�me

	C-x s     Ulo�en� n�kter�ch buffer�

C-x s se v�s zept� na ka�d� buffer, kter� obsahuje zm�ny, kter� jste
neulo�ili.  Pro ka�d� takov� buffer se v�s zept�, zda jej m� ulo�it.

>> Vlo�te ��dek textu a pak stiskn�te C-x s.
   M�li byste b�t dot�z�ni, zda m� b�t ulo�en buffer nazvan� TUTORIAL.cs.
   Odpov�zte na tuto ot�zku ano (yes) stiskem "y".


* ROZ�I�OV�N� SADY P��KAZ�
--------------------------

Existuje mnohem, mnohem v�ce p��kaz� Emacsu, ne� kter� by v�bec mohly
b�t rozm�st�ny na v�echny CONTROL a META znaky.  Emacs tento probl�m
obch�z� prost�ednictv�m X (eXtend) p��kazu.  Ten vznik� dv�ma zp�soby:

	C-x	Znakov� eXtend.  N�sledov�n jedn�m znakem.
	M-x	Pojmenovan� p��kaz eXtend.  N�sledov�n dlouh�m n�zvem.

To jsou p��kazy, kter� jsou obecn� u�ite�n�, av�ak m�n� �asto pou��van�
ne� ty, kter� jste se ji� nau�ili.  U� jste vid�li dva z nich: souborov�
p��kazy C-x C-f pro vyhled�n� a C-x C-s pro ulo�en�.  Jin� p��klad je
p��kaz pro ukon�en� Emacsu -- tj. p��kaz C-x C-c.  (Nem�jte obavy
o ztr�tu zm�n, kter� jste provedli; C-x C-c nab�dne ulo�en� ka�d�ho
zm�n�n�ho souboru, ne� Emacs ukon��.)

C-z je p��kaz na *do�asn�* opu�t�n� Emacsu -- m��ete se po n�m do
spu�t�n�ho Emacsu vr�tit.

Na syst�mech, kter� to umo��uj�, C-z Emacs "pozastav�"; tzn. vr�t� v�s
do shellu, av�ak Emacs neukon��.  V nejb�n�j��ch shellech se m��ete do
Emacsu vr�tit p��kazem `fg' nebo pomoc� `%emacs'.

Na syst�mech, kter� pozastavov�n� proces� nemaj� implementov�no, C-z
vytvo�� subshell b��c� pod Emacsem, aby v�m dal �anci spustit jin�
programy a pak se do Emacsu vr�tit; neprovede tedy prav� opu�t�n�
Emacsu.  V tom p��pad� je obvyklou cestou n�vratu ze subshellu do Emacsu
shellovsk� p��kaz `exit'.

Chv�le pro pou�it� C-x C-c nastane, kdy� se chyst�te odhl�sit ze
syst�mu.  Spr�vn� je to tak� p�i ukon�ov�n� Emacsu vyvolan�ho po�tovn�m
programem a r�zn�mi jin�mi utilitami, proto�e ty nemus� v�d�t, jak si
poradit s pozastaven�m Emacsu.  Nicm�n� za norm�ln�ch okolnost�, pokud
se nechyst�te odlogovat, je l�pe Emacs pozastavit pomoc� C-z ne� jej
ukon�it.

Existuje mnoho C-x p��kaz�.  Zde je seznam t�ch, kter� jste se ji� nau�ili:

	C-x C-f		Vyhled�n� souboru
	C-x C-s		Ulo�en� soubor
	C-x C-b		Seznam buffer�
	C-x C-c		Ukon�en� Emacsu
	C-x u		Undo

Pojmenovan� eXtended p��kazy jsou p��kazy, kter� jsou pou��v�ny je�t�
m�n�, nebo p��kazy, kter� jsou pou��v�ny jenom v jist�ch m�dech.
P��kladem je p��kaz replace-string, kter� glob�ln� nahrad� jeden �et�zec
jin�m.  Kdy� stisknete M-x, vyp�e se na spodn�m ��dku obrazovky prompt
M-x a vy byste m�li zadat jm�no p��kazu; v tomto p��pad�
"replace-string".  Jednodu�e napi�te "repl s<TAB>" a Emacs n�zev dopln�.
Dokon�ete zad�v�n� jm�na p��kazu pomoc� <Return>.

P��kaz replace-string vy�aduje dva argumenty -- �et�zec, kter� m� b�t
nahrazen, a �et�zec, kter� jej m� nahradit.  Ka�d� argument mus�te
ukon�it pomoc� <Return>.

>> P�esu�te kurzor na pr�zdn� ��dek dva ��dky pod t�mto.
   Pak napi�te M-x repl s<Return>zm�nil<Return>modifikoval<Return>.

   V�imn�te si, jak se tento ��dek zm�nil: nahradili jste slovo
   z-m-�-n-i-l slovem "modifikoval", kdekoliv se za aktu�ln� pozic�
   kurzoru vyskytlo.


* AUTOMATICK� UKL�D�N�
----------------------

Jestli�e jste provedli zm�ny v souboru, ale nem�te je je�t� ulo�eny,
mohou b�t v p��pad� p�du syst�mu ztraceny.  Aby v�s Emacs od toho
uchr�nil, periodicky zapisuje "auto save" soubor pro ka�d� soubor, kter�
editujete.  Jm�no auto save souboru m� na za��tku a na konci #;
nap��klad jestli�e se v� soubor jmenuje "hello.c", jeho auto save
soubor se jmenuje "#hello.c#".  Kdy� soubor ulo��te norm�ln�m zp�sobem,
Emacs auto save soubor sma�e.

Jestli�e dojde k p�du syst�mu, m��ete svoji editaci obnovit z auto-save
souboru, a to norm�ln�m vyhled�n�m souboru (toho, kter� jste editovali,
ne auto save souboru) a n�slednou aplikac� M-x recover file<return>.
Na ��dost o potvrzen� odpov�zte zad�n�m yes<return> pro pokra�ov�n� a
obnoven� auto-save dat.


* ECHO OBLAST
-------------

Kdy� Emacs vid�, �e p�ete p��kazy pomalu, ukazuje v�m je ve spodn�
��sti obrazovky v oblasti naz�van� "echo oblast".  Echo oblast obsahuje
doln� ��dek obrazovky.


* STAVOV� ��DEK
---------------

��dek bezprost�edn� nad echo oblast� se naz�v� "stavov� ��dek" ("mode line").
Stavov� ��dek ��k� n�co jako:

2J:** TUTORIAL.cs       (Fundamental)--L670--58%----------------

Tento ��dek pod�v� u�ite�nou informaci o stavu Emacsu a textu, kter�
editujete.

U� v�te, co znamen� jm�no souboru -- je to soubor, kter� jste vyhledali.
-NN%-- ozna�uje va�i aktu�ln� pozici v textu; ��k�, �e NN procent textu
je nad horn�m okrajem obrazovky.  Je-li za��tek souboru na obrazovce, je
zde --Top-- a ne --00%--.  Je-li konec textu na obrazovce, je zde
--Bot--.  Jestli�e se d�v�te na tak mal� text, �e se cel� vejde na
obrazovku, stavov� ��dek ��k� --All--.

Hv�zdi�ky pobl� za��tku znamenaj�, �e jste text zm�nili.  T�sn� po
vyhled�n� nebo ulo�en� souboru v t�to ��sti stavov�ho ��dku nejsou ��dn�
hv�zdi�ky, pouze poml�ky.

��st stavov�ho ��dku v z�vork�ch ��k�, v jak�ch edita�n�ch m�dech se
nach�z�te.  Implicitn� m�d je Fundamental, co� je ten, kter� moment�ln�
pou��v�te.  Je p��kladem hlavn�ho m�du ("major mode").

Emacs m� celou �adu hlavn�ch m�d�.  N�kter� z nich jsou ur�eny pro
editaci r�zn�ch programovac�ch jazyk� a/nebo text� jako t�eba Lisp m�d,
Text m�d, atd.  V libovoln�m okam�iku je aktivn� pr�v� jeden hlavn� m�d a
jeho jm�no lze nal�zt ve stavov�m ��dku na m�st�, kde je te�
"Fundamental".

Ka�d� hlavn� m�d m�n� chov�n� n�kter�ch p��kaz�.  Nap��klad existuj�
p��kazy pro vytv��en� koment��� v programu, a proto�e ka�d� programovac�
programovac� jazyk m� jinou p�edstavu o tom, jak m� koment�� vypadat,
mus� ka�d� hlavn� m�d vkl�dat koment��e jinak.  Ka�d� hlavn� m�d je
vlastn� jm�no extended p��kazu, kter�m se do tohoto m�du m��ete
p�epnout.  Nap��klad M-x fundamental-mode je p��kaz pro p�epnut� se do
Fundamental m�du.

Chyst�te-li se editovat �esk� text, jako t�eba tento soubor,
pravd�podobn� byste m�li pou��t Text m�d.
>> Napi�te M-x text-mode<Return>.

Nebojte se, ��dn� z p��kaz�, kter� jste se nau�ili, chov�n� Emacsu nijak
v�znamn� nezm�n�.  M��ete si ale v�imnout, �e M-f a M-b nyn� pracuj�
s apostrofy jako se sou��stmi slov.  P�edt�m, ve Fundamental m�du, M-f a
M-b pracovaly s apostrofy coby odd�lova�i slov.

Hlavn� m�dy obvykle d�laj� men�� zm�ny, jako byla tato: p��kazy v�t�inou
d�laj� "tot�", ale v ka�d�m hlavn�m m�du pracuj� tro�ku jinak.

Dokumentaci k aktu�ln�mu hlavn�mu m�du si m��ete zobrazit stiskem C-h m.

>> Jednou nebo n�kolikr�t pou�ijte C-u C-v, abyste tento ��dek dostali
   k vrcholu obrazovky.
>> Stiskn�te C-h m, abyste vid�li, jak se Text m�d li�� od Fundamental
   m�du.
>> Stiskn�te C-x 1 pro odstran�n� dokumentace z obrazovky.

Hlavn� m�dy se naz�vaj� hlavn� proto, �e tak� existuj� vedlej�� m�dy
(minor modes).  Vedlej�� m�dy nejsou alternativou k hlavn�m m�d�m, n�br�
jejich mal� modifikace.  Ka�d� vedlej�� m�d m��e b�t zapnut nebo vypnut
s�m o sob� nez�visle na v�ech ostatn�ch vedlej��ch m�dech a nez�visle na
hlavn�m m�du.  Tak�e nemus�te pou��vat ��dn� vedlej�� m�d nebo m��ete
pou��vat jeden vedlej�� m�d nebo libovolnou kombinaci n�kolika
vedlej��ch m�d�.

Jedn�m z velmi u�ite�n�ch vedlej��ch m�d�, zejm�na pro editaci �esk�ch
text�, je Auto Fill m�d.  Kdy� je tento m�d zapnut, Emacs zalom� ��dek
mezi dv�ma slovy, kdykoliv vkl�d�te text a ��dek se stane p��li�
dlouh�m.

Auto Fill m�d m��ete zapnout proveden�m M-x auto-fill-mode<Return>.
Je-li tento m�d zapnut, m��ete jej vypnout proveden�m M-x
auto-fill-mode<Return>.  Je-li m�d vypnut, tento p��kaz jej zap�n�,
a je-li m�d zapnut, tak jej tento p��kaz vyp�n�.  ��k�me, �e tento
p��kaz p�ep�n� ("toggles") tento m�d.

>> Napi�te te� M-x auto-fill-mode<Return>.  Pak vkl�dejte "asdf " st�le
   dokola tak dlouho, a� uvid�te, jak se vkl�dan� ��dek rozd�l� na dva
   ��dky.  Do textu mus�te vkl�dat mezery proto, �e Auto Fill m�d
   zalamuje ��dky pouze v mezer�ch.

Okraj je obvykle nastaven na 70 znak�, ale m��ete to zm�nit p��kazem
C-x f.  Hodnotu okraje, kterou si p�ejete, byste m�li p�edat jako
numerick� argument.

>> Napi�te C-x f s argumentem 20.  (C-u 2 0 C-x f).
   Pak pi�te n�jak� text a pozorujte, jak Emacs vypl�uje ��dky po
   20 znac�ch.  Pak nastavte okraj zp�tky na 70 op�tovn�m pou�it�m
   C-x f.

Jestli�e provedete zm�ny uprost�ed odstavce, Auto Fill m�d jej
nep�eform�tuje.
Pro p�eform�tov�n� odstavce stiskn�te M-q (META-q) s kurzorem uvnit�
odstavce.

>> P�esu�te kurzor do p�edchoz�ho odstavce a stiskn�te M-q.


* VYHLED�V�N�
-------------

Emacs um� v textu vyhled�vat �et�zce (tj. skupiny spojen�ch znak� nebo
slov) sm�rem vp�ed nebo vzad.  Hled�n� �et�zce je p��kaz p�esunuj�c�
kurzor; p�esune kurzor na nejbli��� m�sto, kde se tento �et�zec nach�z�.

Vyhled�vac� p��kaz Emacsu se li�� od vyhled�vac�ch p��kaz� v�t�iny
editor� v tom smyslu, �e je "inkrement�ln�".  To znamen�, �e vyhled�v�n�
se prov�d� u� v okam�iku, kdy zad�v�te vyhled�vac� �et�zec.

P��kaz pro zah�jen� hled�n� vp�ed je C-s a pro hled�n� vzad C-r.
ALE POZOR!  Nezkou�ejte to je�t�.

Kdy� stisknete C-s, uvid�te v echo oblasti prompt "I-search".  To v�m
��k�, �e Emacs se nach�z� ve stavu, kter� se naz�v� inkrement�ln� hled�n�,
a �ek�, a� mu zad�te, co chcete hledat.  <RET> hled�n� ukon��.

>> Nyn� zahajte hled�n� stiskem C-s.  POMALU, p�smeno po p�smenu, pi�te
   slovo 'kurzor'.  Po ka�d�m p�smenu si v�imn�te, co se d�je s kurzorem.
   Te� jste vyhledali "kurzor" poprv�.
>> Stiskn�te C-s znovu, abyste nalezli dal�� v�skyt "kurzor".
>> Nyn� �ty�ikr�t stiskn�te <Delete> a pozorujte, jak se kurzor
   p�esunuje.
>> Stiskn�te <RET> pro ukon�en� hled�n�.

Vid�li jste, co se stalo?  Emacs se v inkrement�ln�m hled�n� pokou��
p�ej�t na dal�� v�skyt �et�zce, kter� jste dosud napsali.  Chcete-li
p�ej�t na dal�� v�skyt 'kurzor', jednodu�e stiskn�te C-s znovu.
Jestli�e u� ��dn� takov� v�skyt nen�, Emacs p�pne a �ekne v�m, �e
hled�n� moment�ln� "selh�v�", C-g hled�n� ukon��.

POZN�MKA: Na n�kter�ch syst�mech stisk C-s zp�sob� ztuhnut�
obrazovky a nevid�te ��dn� dal�� v�stup z Emacsu.  To znamen�, �e
"vlastnost" opera�n�ho syst�mu zvan� "flow control" zachycuje C-s a
nepropust� jej k Emacsu.  Pro odtuhnut� obrazovky stiskn�te C-q.  Pak
v sekci "Spontaneous Entry to Incremental Search" v manu�lu Emacsu
vyhledejte radu, jak se vypo��dat s touto "vlastnost�".

Jestli�e uprost�ed inkrement�ln�ho hled�n� stisknete <Delete>, uvid�te,
�e posledn� znak v hledan�m �et�zci zmiz� a hled�n� se vrac� na posledn�
m�sto hled�n�.  P�edpokl�dejme nap��klad, �e jste napsali "c", abyste
na�li prvn� v�skyt "k".  Jestli�e nyn� stisknete "u", kurzor se p�esune na
prvn� v�skyt "ku".  Te� stiskn�te <Delete>.  To vyma�e "u" z hledan�ho
�et�zce a kurzor se p�esune zp�t na prvn� v�skyt "k".

Jestli�e uprost�ed hled�n� stisknete CONTROL nebo META znak (s n�kolika
v�jimkami -- znaky, kter� jsou speci�ln� v hled�n�, jako C-s a C-r),
hled�n� se ukon��.

C-s zahajuje hled�n�, kter� hled� jak�koliv v�skyt hledan�ho �et�zce ZA
aktu�ln� pozic� kurzoru.  Chcete-li n�co hledat v p�edch�zej�c�m textu,
stiskn�te C-r m�sto C-s.  V�e, co jsme �ekli o C-s, plat� tak� o C-r
krom� toho, �e sm�r hled�n� je opa�n�.


* V�CE OKEN
-----------

Jednou z p�kn�ch vlastnost� Emacsu je to, �e m��e na obrazovce zobrazit
v�ce oken sou�asn�.

>> P�esu�te kurzor na tento ��dek a stiskn�te C-u 0 C-l.

>> Te� stiskn�te C-x 2, co� rozd�l� obrazovku na dv� okna.
   Ob� okna zobrazuj� tento tutori�l.  Kurzor z�st�v� navrchu okna.

>> Tiskn�te C-M-v pro scrollov�n� spodn�ho okna.
   (Nem�te-li skute�nou kl�vesu META, stiskn�te ESC C-v.)

>> Stiskn�te C-x o ("o" jako "other") pro p�esun kurzoru do doln�ho okna.

>> Pou�ijte C-v a M-v ve spodn�m okn� pro jeho scrollov�n�.
   Pokra�ujte ve �ten� t�chto instrukc� v horn�m okn�.

>> Znovu stiskn�te C-x o pro p�esun kurzoru zp�t do horn�ho okna.
   Kurzor v horn�m okn� je p�esn� na m�st�, kde byl p�vodn�.

M��ete d�le pou��vat C-x o pro p�ep�n�n� mezi okny.  Ka�d� okno m� svoji
vlastn� pozici kurzoru, ale jenom jedno okno kurzor skute�n� zobrazuje.
V�echny obvykl� edita�n� p��kazy plat� pro okno, ve kter�m se nach�z�
kurzor.  Toto okno naz�v�me "aktivn� okno" ("selected window").

P��kaz C-M-v je velmi u�ite�n�, jestli�e v jednom okn� editujete text a
druh� okno pou��v�te pouze pro p�ehled.  M��ete kurzor nech�vat st�le
v okn�, kde editujete, a postupovat po druh�m okn� pomoc� C-M-v.

C-M-v je p��kladem CONTROL-META znaku.  M�te-li skute�nou META kl�vesu,
m��ete vyvolat C-M-v p�idr�en�m obou kl�ves CTRL a META p�i stisku v.
Nez�le�� na tom, zda je prvn� stisknuta CTRL nebo META, proto�e ob� tyto
kl�vesy funguj� jako modifik�tory kl�ves, kter� tisknete.

Pokud nem�te skute�nou META kl�vesu, m��ete m�sto n� pou��t ESC, na
po�ad� z�le��: mus�te stisknout ESC a n�sledn� CTRL-v; CTRL-ESC v by
nefungovalo.  To proto, �e ESC je samostatn� znak, nikoliv modifik�tor.

>> Stiskn�te C-x 1 (v horn�m okn�), abyste se zbavili doln�ho okna.

(Kdybyste C-x 1 stiskli v doln�m okn�, odstranilo by to horn� okno.
Ch�pejte tento p��kaz jako "ponechej pr�v� jedno okno -- to, ve kter�m
zrovna jsem".)

Nemus�te v obou oknech zobrazovat tent�� buffer.  Jestli�e pou�ijete
C-x C-f pro vyhled�n� souboru v jednom z oken, druh� okno se nezm�n�.
M��ete vyhled�vat soubory v obou oknech nez�visle.

Zde je dal�� zp�sob, jak vyu��t dv� okna ke zobrazen� dvou r�zn�ch v�c�:

>> Stiskn�te C-x 4 C-f n�sledovan� jm�nem n�kter�ho z va�ich soubor�.
   Dokon�ete to pomoc� <Return>.  Vid�te zadan� soubor v doln�m okn�.
   P�esunul se tam i kurzor.

>> Stiskn�te C-x o pro p�esun zp�t do horn�ho okna a C-x 1 pro smaz�n�
   doln�ho okna.


* REKURZIVN� EDITA�N� �ROVN�
----------------------------

Ob�as se dostanete do n��eho, co se naz�v� "rekurzivn� edita�n� �rove�"
("recursive editing level").  To je indikov�no hranat�mi z�vorkami ve
stavov�m ��dku obklopuj�c�mi z�vorky okolo jm�na hlavn�ho m�du.
Nap��klad m��ete vid�t [(Fundamental)] m�sto (Fundamental).

Abyste se dostali z rekurzivn� edita�n� �rovn�, stiskn�te ESC ESC ESC.
To je obecn� "vyskakovac�" p��kaz.  M��ete jej pou��t t� pro odstran�n�
n�kter�ch oken a vysko�en� z minibufferu.

>> Stiskn�te M-x, abyste se dostali do minibufferu; pak stiskn�te
   ESC ESC ESC, abyste se z n�j dostali ven.

Z rekurzivn� edita�n� �rovn� nem��ete vysko�it pomoc� C-g.  To proto, �e
C-g je vyu��v�no pro ru�en� p��kaz� a argument� UVNIT� rekurzivn�
edita�n� vrstvy.


* Z�SK�N� DAL�� N�POV�DY
------------------------

V tomto tutori�lu jsme se pokusili poskytnout v�m dostatek informac�,
abyste mohli za��t Emacs pou��vat.  V Emacsu je toho tolik, �e by bylo
nemo�n� to zde v�echno objasnit.  Nicm�n� se o Emacsu m��ete nau�it
v�ce, proto�e m� mnoho u�ite�n�ch vlastnost�.  Emacs nab�z� p��kazy pro
�ten� dokumentace sv�ch p��kaz�.  V�echny tyto "help" p��kazy
za��naj� znakem CONTROL-h, kter� se naz�v� "help znak".

Pro pou�it� vlastnost� n�pov�dy stiskn�te znak C-h a pak znak ��kaj�c�,
jak� druh n�pov�dy ��d�te.  Jste-li OPRAVDU ztraceni, stiskn�te C-h ? a
Emacs v�m sd�l�, jak� druhy n�pov�dy v�m m��e poskytnout.  Jestli�e
jste stiskli C-h a pak jste se rozhodli, �e ��dnou n�pov�du nechcete,
jednodu�e to zru�te stiskem C-g.

(Na n�kter�ch po��ta��ch je v�znam znaku C-h zm�n�n.  To by opravdu
nem�lo b�t obecn�m nastaven�m pro v�echny u�ivatele, tak�e m�te pr�vo
st�ovat si syst�mov�mu administr�torovi.  Do t� doby, jestli�e C-h
nezobrazuje hl�en� o n�pov�d� v doln� ��sti obrazovky, zkuste m�sto
toho pou��vat kl�vesu F1 nebo M-x help RET.)

Nejz�kladn�j�� help p��kaz je C-h c.  Stiskn�te C-h, znak c a kl�vesov�
p��kaz; Emacs pak zobraz� velmi stru�n� popis p��kazu.

>> Stiskn�te C-h c C-p.
   Hl�en� by m�lo vypadat asi takto

	C-p runs the command previous-line

To v�m sd�luje "jm�no funkce".  Jm�na funkc� jsou pou��v�na zejm�na pro
konfiguraci a roz�i�ov�n� Emacsu.  Ale proto�e jm�na funkc� jsou volena
tak, aby nazna�ovala, co odpov�daj�c� p��kaz d�l�, mohou slou�it tak�
jako velmi stru�n� dokumentace -- dostate�n� k tomu, aby v�m p�ipomenula
p��kazy, kter� jste se ji� nau�ili.

V�ceznakov� p��kazy jako C-x C-s a (pokud nem�te META, EDIT ani ALT
kl�vesu) <ESC>v jsou po C-h c povoleny tak�.

K z�sk�n� v�ce informac� o p��kazu m�sto C-h c pou�ijte C-h k.

>> Stiskn�te C-h k C-p.

To zobraz� dokumentaci k funkci a jej� jm�no v emacsov�m okn�.  A�
v�stup p�e�tete, stiskn�te C-x 1, abyste se textu n�pov�dy zbavili.
Nemus�te to d�lat hned.  M��ete chv�li editovat a nahl�et do textu
n�pov�dy a teprve pak stisknout C-x 1.

Zde jsou dal�� u�ite�n� C-h volby:

   C-h f	Popis funkce.  Zad�v�te jm�no funkce.

>> Zkuste napsat C-h f previous-line<Return>.
   To vyp�e ve�ker� informace, kter� Emacs m� o funkci implementuj�c�
   p��kaz C-p.

Podobn� p��kaz C-h v zobraz� dokumentaci prom�nn�, jej� hodnotu
m��ete nastavit a zm�nit t�m chov�n� Emacsu.  Jm�no prom�nn� zad�te, a�
se na n� Emacs zept�.

   C-h a	P��kazov� apropos.  Zadejte kl��ov� slovo a Emacs vyp�e
		v�echny p��kazy, jejich� jm�na obsahuj� toto kl��ov�
		slovo.  V�echny tyto p��kazy mohou b�t vyvol�ny pomoc�
		META-x.  Pro n�kter� p��kazy p��kazov� apropos vyp�e
		tak� jedno nebo dvouznakov� sekvence, kter� prov�d�j�
		tent�� p��kaz.

>> Napi�te C-h a file<Return>.

To zobraz� v druh�m okn� seznam v�ech M-x p��kaz� obsahuj�c�ch "file" ve
sv�m n�zvu.  Znakov� p��kazy jako C-x C-f uvid�te vypsan� vedle
odpov�daj�c�ch jmen p��kaz� jako find-file.

>> Stiskn�te C-M-v pro posun okna s n�pov�dou.  Prove�te to n�kolikr�t.

>> Stiskn�te C-x 1 pro smaz�n� okna s n�pov�dou.

   C-h i	�ten� on-line manu�l� (t� Info).  Tento p��kaz
		v�s p�epne do speci�ln�ho bufferu s n�zvem `*info*',
		ve kter�m m��ete ��st on-line manu�ly pro bal�ky
		nainstalovan� na va�em syst�mu.  Pokud stisknete
		m emacs <Return> m��ete si nap��klad p�e��st manu�l
		k Emacsu.  Pokud jste dosud nikdy nepou��vali Info,
		stiskn�te ? a Emacs v�m p�edstav� hlavn� mo�nosti
		m�du pro Info.  A� si tyto mo�nosti prostudujete,
		m�li byste pou��vat Info manu�l Emacsu jako svoji
		prim�rn� dokumentaci.


* Z�V�R
-------

Nezapome�te, Emacs ukon��te proveden�m p��kazu C-x C-c.  Pro do�asn�
odskok do shellu, ze kter�ho se do Emacsu m��ete op�t vr�tit,
pou�ijte C-z.

Z�m�rem tohoto tutori�lu je b�t srozumiteln� v�em nov�m u�ivatel�m, tak�e
naraz�te-li na n�co nejasn�ho, tak neusedejte a nekla�te to za vinu sob�
-- st�ujte si!


KOP�ROV�N�
----------

Tento tutori�l vych�z� z dlouh� �ady emacsov�ch tutori�l� zah�jen�
tutori�lem napsan�m Stuartem Cracraftem pro p�vodn� Emacs.

Tato verze tutori�lu je, podobn� jako GNU Emacs, chr�n�na copyrightem a
je ���ena se svolen�m distribuovat kopie za jist�ch podm�nek:

Copyright (C) 1985, 1996, 1998, 2001-2012  Free Software Foundation, Inc.

   Ka�d�mu je zaru�eno pr�vo vytv��et a distribuovat p�esn� kopie tohoto
   dokumentu tak, jak jej obdr�el, na jak�mkoliv m�diu, s t�m, �e bude
   zachov�na tato pozn�mka o autorstv� a pozn�mka o svolen� a �e
   distributor zaru�uje p��jemci pr�vo na dal�� redistribuci povolenou
   touto pozn�mkou.

   Je zaru�eno pr�vo distribuovat modifikovan� verze tohoto dokumentu
   nebo jeho ��st� pod v��e uveden�mi podm�nkami za p�edpokladu, �e
   obsahuje jasn� pozn�mky uv�d�j�c�, kdo provedl posledn� modifikace.

Podm�nky pro kop�rov�n� Emacsu samotn�ho jsou slo�it�j��, av�ak ve
stejn�m duchu.  P�e�t�te si pros�m soubor COPYING a pak p�ed�vejte kopie
GNU Emacsu sv�m p��tel�m.  Pom�hejte pot�rat softwarovou obstrukci
("vlastnictv�") pou��v�n�m, psan�m a sd�len�m free softwaru!

;;; Local Variables:
;;; coding: iso-latin-2
;;; End:

