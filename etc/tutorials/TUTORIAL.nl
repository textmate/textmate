De Emacs-inleiding.  De kopieervoorwaarden staan onderaan.

De meeste Emacs-commando's gebruiken de CONTROL-toets (soms CTRL of CTL
genaamd) en/of de META-toets (soms genaamd EDIT of ALT).  In plaats van
steeds de volledige naam te noemen, gebruiken we de volgende afkortingen:

 C-<ltr>  betekent: houd de CONTROL-toets ingedrukt en tik de toets <ltr>
	  Dus C-f wordt: houd de CONTROL-toets ingedrukt en tik f.
 M-<ltr>  betekent: houd de META-, EDIT- of ALT-toets ingedrukt en tik
	  de toets <ltr>.  Als er geen toets META, EDIT of ALT is, kun
	  je ook eerst de ESC-toets tikken, gevolgd door <ltr>.  We
	  verwijzen naar de ESC-toets als <ESC>.

BELANGRIJK: om Emacs te verlaten, tik C-x C-c (twee tekens).
Om een commando halverwege af te breken, tik C-g.
De tekens ">>" tegen de linkerkantlijn nodigen je uit om een bepaald
commando te proberen.  Bijvoorbeeld:
<<Blank lines inserted around following line by help-with-tutorial>>
[Lege regels om didactische redenen.  Hieronder gaat het verder.]
>> Tik nu C-v (volgend scherm) om naar het volgende scherm te gaan.
	(Geef nu het commando door de CONTROL-toets ingedrukt te
	houden terwijl je de v tikt.)  Vanaf nu moet je dit steeds
	herhalen als je klaar bent met het lezen van een scherm.

Merk op dat er een overlapping van twee regels is als je van een
scherm naar het volgende gaat; dat zorgt voor continuïteit bij het
lezen van de tekst.

Het eerste wat je moet weten, is hoe je je naar verschillende plaatsen
in de tekst kan bewegen.  Je weet al hoe je een scherm vooruit moet
gaan: met C-v.  Om een scherm terug te gaan, tik je M-v (houd de
META-toets ingedrukt en tik v, of tik <ESC> v als je geen META-, EDIT-
of ALT-toets hebt).

>> Probeer nu een paar keer M-v, steeds gevolgd door C-v.


* SAMENVATTING
--------------

De volgende commando's zijn handig om volledige schermen te bekijken:

	C-v	Ga een scherm vooruit
	M-v	Ga een scherm terug
	C-l	Maak het scherm schoon en teken alle tekst opnieuw,
		 waarbij de regel waarop de cursor staat, op het
		 midden van het scherm terecht komt.  (C-l is
		 CONTROL-L, niet CONTROL-1.)

>> Kijk waar de cursor staat, en onthoud de tekst er omheen.  Tik C-l.
   Zoek de cursor en merk op dat hij nog steeds bij dezelfde tekst
   staat, maar nu in het midden van het scherm.
   Als je weer C-l tikt dan gaat hij naar de bovenkant van het scherm.
   Nog een keer C-l en hij gaat naar de onderkant.

Als je toetsenbord PageUp- en PageDn-toetsen heeft dan kun je deze ook
gebruiken om een scherm terug dan wel vooruit te gaan, maar het werken
met C-v en M-v is efficiënter.


* BASISCOMMANDO'S CURSORBEWEGINGEN
----------------------------------

Het is handig om je per scherm te bewegen, maar hoe beweeg je je nu
naar een specifieke plaats op het scherm?

Er is een aantal manieren waarop je dit kan doen.  Je kan de
pijltjestoetsen gebruiken, maar het is efficiënter om je handen in de
standaardhouding te laten, en de commando's C-p, C-b, C-f en C-n te
gebruiken.  Deze commando's komen overeen met de pijltjestoetsen, als
in onderstaande figuur:

			  vorige regel, C-p
				  :
				  :
    achteruit, C-b .... huidige cursorpositie .... vooruit, C-f
				  :
				  :
			 volgende regel, C-n

>> Verplaats, met C-n of C-p, de cursor naar de middelste regel van de
   figuur.  Tik dan C-l om de hele figuur in het midden van het
   scherm te plaatsen.

Met een beetje kennis van het Engels zijn deze commando's gemakkelijk
te onthouden: de p komt van "previous" (vorige), de n van "next"
(volgende), de b van "backward" (achteruit) en de f van "forward"
(vooruit).  Dit zijn de basiscommando's om de cursor te bewegen, die
je VOORTDUREND zal gebruiken.

>> Tik een paar keer C-n om de cursor op deze regel te zetten.

>> Beweeg de cursor binnen de regel met een paar keer C-f en terug
   omhoog met C-p.  Let op wat C-p doet als de cursor midden in een
   regel staat.

Elke regel eindigt met een Newline-teken (het Engelse "new line"
betekent "nieuwe regel"); dit teken scheidt elke regel van de
volgende.  (De laatste regel in een bestand heeft meestal ook een
Newline aan het eind maar dat is geen vereiste voor Emacs.)

>> Probeer C-b aan het begin van een regel.  De cursor zal zich naar
   het eind van de vorige regel bewegen, omdat je achteruit over het
   Newline-teken gaat.

Net als C-b kan ook C-f zich over Newline-tekens heen bewegen.

>> Tik nog een aantal keren het commando C-b, zodat je een gevoel
   krijgt waar de cursor is.  Tik dan enkele keren C-f om de cursor
   naar het eind van de regel te bewegen.  Nog een C-f commando
   beweegt de cursor dan naar de volgende regel.

Wanneer je de cursor voorbij het begin of het eind van het scherm
beweegt, zal de tekst over het scherm bewegen.  Dit heet "scrollen".
Door te scrollen zorgt Emacs ervoor dat de cursor naar de gewenste
plaats in de tekst kan gaan zonder de cursor van het scherm te laten
verdwijnen.

>> Probeer de cursor voorbij de onderkant van het scherm te bewegen
   met C-n en zie wat er gebeurt.

Als de beweging per teken te langzaam gaat, kan je de cursor ook per
woord bewegen.  M-f (META-f) beweegt de cursor een woord vooruit en
M-b een woord achteruit.

>> Tik enkele keren M-f en M-b.

Als je midden in een woord staat, beweegt M-f de cursor naar het eind
van het woord.  Als je op witruimte tussen twee woorden staat, beweegt
M-f de cursor naar het eind van het volgende woord.  Het commando M-b
werkt op dezelfde manier de andere kant op.

>> Tik enkele keren M-f en M-b en tussendoor een paar maal C-f en C-b,
   zodat je ziet wat M-f en M-b doen vanaf verschillende plaatsen in
   een woord en tussen twee woorden.

Merk op dat er een analogie bestaat tussen enerzijds C-f en C-b en
anderzijds M-f en M-b.  Het is bij veel commando's zo dat META-tekens
gebruikt worden om iets te doen in eenheden van de taal (woorden,
zinnen, alinea's) terwijl CONTROL-tekens te maken hebben met de
bouwstenen die onafhankelijk zijn van wat je aan het bewerken bent
(tekens, regels, enz.).

Deze analogie gaat ook op voor regels en zinnen: C-a en C-e bewegen de
cursor naar het begin of eind van een regel, terwijl met M-a,
respectievelijk M-e, de cursor naar het begin, respectievelijk het
eind, van een zin gaat.

>> Probeer een paar maal C-a, en dan enkele keren C-e.
   Probeer een paar maal M-a, en dan enkele keren M-e.

Bemerk hoe herhaalde C-a commando's niets doen, terwijl herhaalde M-a
commando's de cursor steeds een zin achteruit bewegen.  Alhoewel ze
niet volledig overeenkomen, lijkt het gedrag van beide heel natuurlijk.

De plaats van de cursor in de tekst wordt "punt" genoemd (zonder
lidwoord, "point" in het Engels).  Anders gezegd: de cursor laat op
het scherm de plek zien waar punt in de tekst staat.

Nu volgt een samenvatting van eenvoudige cursorbewegingen, inclusief
de commando's die de cursor per woord of zin bewegen:

	C-f	Ga een teken vooruit
	C-b	Ga een teken achteruit

	M-f	Ga een woord vooruit
	M-b	Ga een woord achteruit

	C-n	Ga naar de volgende regel
	C-p	Ga naar de vorige regel

	C-a	Ga naar het begin van de regel
	C-e	Ga naar het eind van de regel

	M-a	Ga terug naar het begin van de zin
	M-e	Ga vooruit naar het eind van de zin

>> Probeer al deze commando's een paar keer als oefening.
   Dit zijn de meestgebruikte commando's.

Er zijn nog twee belangrijke cursorbewegingen: M-< (META kleiner-dan)
beweegt de cursor naar het begin van de tekst, en M-> (META
groter-dan) beweegt hem naar het eind.

Op de meeste toetsenborden zit de '<' boven de komma, zodat je de
Shift-toets (ook wel bekend als de hoofdlettertoets) moet gebruiken om
het '<'-teken in te tikken.  Op deze toetsenborden moet je ook de
shift gebruiken om M-< in te tikken: zonder shift zou je M-komma tikken.

>> Tik nu M-< om naar het begin van dit bestand te gaan.
   Gebruik daarna C-v om hier weer terug te komen.

>> Tik nu M-> om naar het eind van het bestand te springen.
   Gebruik daarna M-v om hier weer terug te komen.

Als je toetsenbord pijltjestoetsen heeft, kan je die ook gebruiken om
de cursor te verplaatsen.  We raden je aan om C-b, C-f, C-n en C-p te
leren, om drie redenen.  Ten eerste werken ze op alle soorten
toetsenborden.  Ten tweede zul je merken dat wanneer je eenmaal wat
ervaring hebt opgedaan in de omgang met Emacs, het gebruik van de
CONTROL-tekens sneller is dan werken met de pijltjestoetsen (omdat je
handen in de normale tikpositie kunnen blijven).  Ten derde, als je
eenmaal gewend bent aan deze commando's met CONTROL-tekens, kan je
makkelijk andere gevorderde cursorbewegingscommando's leren.

De meeste Emacs-commando's accepteren een numeriek argument.  Voor de
meeste commando's is dit argument het aantal keren dat het commando
herhaald moet worden.  Je geeft dit numerieke argument aan met C-u en
vervolgens de cijfers van het getal, vóór het commando.  Als je
toetsenbord een META- (of EDIT- of ALT-) toets heeft, is er ook een
andere manier om het getal aan te geven: tik de cijfers terwijl je de
META toets ingedrukt houdt.  We raden je aan de C-u manier te leren
omdat die beschikbaar is op elke terminal.  Het numerieke argument
wordt ook wel het "prefix-argument" genoemd omdat je het typt voor
het commando waar het bij hoort.

Bijvoorbeeld, C-u 8 C-f beweegt de cursor 8 plaatsen naar voren.

>> Probeer eens om met C-n of C-p en een numeriek argument de cursor
   met slechts een commando naar een regel in de buurt van deze zin te
   bewegen.

Voor de meeste commando's is het numerieke argument het aantal keren
dat het commando herhaald moet worden.  Voor sommige commando's
betekent het echter iets anders.  Verschillende commando's (die je
totnogtoe niet geleerd hebt) gebruiken het als een vlag -- de
aanwezigheid van een prefix-argument, ongeacht zijn waarde, maakt dat
het commando iets anders doet.

C-v en M-v vormen een andere uitzondering.  Met een numeriek argument
verschuiven deze commando's de tekst het aangegeven aantal regels in
plaats van (bijna) een heel scherm.  Bijvoorbeeld, C-u 4 C-v
verschuift de tekst 4 regels.

>> Probeer nu C-u 8 C-v.

Daarmee zou de tekst 8 regels opgeschoven moeten zijn.  Als je terug
omlaag wil scrollen, kan je M-v een argument geven.

Als je een grafisch scherm gebruikt, zoals X of MS-Windows, dan zou je
een hoge rechthoek moeten zien aan een katn van het Emacs-venster.
Deze rechthoek heet de schuifbalk ("scroll bar" in het Engels).  Je
kan de tekst scrollen door met de muis in de schuifbalk te klikken.

Als je muis een scrollwiel heeft, dan kan je die gebruiken om te scrollen.


* ALS EMACS HANGT
-----------------

Als Emacs niet meer op commando's reageert, kan je het gerust
onderbreken door C-g te tikken.  Je kan C-g gebruiken om een commando
af te breken als het te lang duurt om uit te voeren.

Je kan C-g ook gebruiken om een numeriek argument te verwijderen of om
het begin van een commando dat je niet wilt afmaken, af te breken.

>> Tik nu C-u 100 om een numeriek argument te maken met de waarde 100,
   en tik dan C-g.  Tik vervolgens C-f.  Het zou de cursor maar één
   positie mogen verplaatsen, omdat je het argument verwijderd hebt
   met C-g.

Als je per ongeluk een <ESC> tikt, kan je dat ongedaan maken met het
commando C-g.


* UITGESCHAKELDE COMMANDO'S
---------------------------

Sommige Emacs-commando's zijn uitgeschakeld zodat beginnende
gebruikers ze niet per ongeluk kunnen uitvoeren.

Als je een van de uitgeschakelde commando's intikt, laat Emacs uitleg
zien over het commando dat je gegeven hebt, en vraagt of je het
werkelijk wil uitvoeren.

Wanneer je het commando echt wil uitvoeren, tik dan <SPC> (de
spatiebalk) als antwoord op de vraag.  Normaal wil je het commando
niet uitvoeren en beantwoord je de vraag met "n" (van "no" of "nee").

>> Tik C-x C-l (een uitgeschakeld commando), en tik dan n als antwoord
   op de vraag.


* VENSTERS
----------

Emacs kan meerdere vensters laten zien, elk venster met zijn eigen
tekst.  We zullen later uitleggen hoe je met meerdere vensters kan
werken.  Op dit moment willen we slechts uitleggen hoe je van extra
vensters af kunt komen en terug kan keren naar het werken met één
venster.  Het is eenvoudig:

	C-x 1	Een enkel venster (dat wil zeggen: verwijder alle
		andere vensters).

Het commando is CONTROL-x gevolgd door het cijfer 1.  C-x 1 vergroot
het venster waar de cursor in staat tot het hele scherm.  Alle andere
vensters worden verwijderd.

>> Zet de cursor op deze regel en tik C-u 0 C-l.
>> Tik nu C-h k C-f.
   Zie hoe dit venster kleiner is geworden, terwijl een nieuw venster
   verschijnt om de documentatie van het C-f commando te laten zien.

>> Tik nu C-x 1 en zie het documentatievenster verdwijnen.

Dit commando is anders dan de commando's die je tot nu toe geleerd
hebt aangezien het uit twee tekens bestaat.  Het begint met het teken
CONTROL-x.  Er zijn een heleboel commando's die met CONTROL-x
beginnen.  Veel van die commando's hebben te maken met vensters,
bestanden, buffers, en soortgelijke dingen.  Dergelijke commando's
bestaan uit twee, drie of vier tekens.


* TOEVOEGEN EN WEGHALEN
-----------------------

Als je tekst toe wil voegen, tik je die gewoon in.  Tekens die je kan
zien, zoals A, 7, * en dergelijke, worden door Emacs als tekst
geïnterpreteerd en meteen toegevoegd.  Tik <Return> (de "volgende
regel"-toets) om een Newline toe te voegen en dus een nieuwe regel te
beginnen.

Om het teken dat dat voor de cursor staat te verwijderen, tik <DEL>.
<DEL> is de toets op het toetsenbord die vaak "Backspace" heet --
dezelfde toets die je normaal gesproken, buiten Emacs, gebruikt om het
laatst ingetikte teken te wissen.

Er kan ook nog een toets op het toetsenbord zijn waarop "Delete"
staat, maar dat is niet de knop die we <DEL> noemen.

>> Probeer dit nu: tik een paar letters en haal ze weer weg door een
   paar keer <DEL> te tikken.  Maak je niet druk over het feit dat dit
   bestand verandert; je zal niets veranderen aan de originele versie
   van deze inleiding.  Je bewerkt je eigen kopie.

Als een regel tekst te lang wordt om helemaal op het scherm getoond te
worden, dan gaat hij verder op de volgende schermregel.  Als je een
grafisch scherm gebruikt verschijnen kleine gebogen pijltjes links en
rechts van het tekstgebied om aan te geven waar een regel voortgezet
is.  In een tekstvenster of terminal geeft een backslash ("\") in de
laatste kolom een vervolgregel aan.

>> Voeg nu tekst toe totdat je de rechter kantlijn raakt, en blijf
   toevoegen.  Je zal zien dat er een vervolgregel verschijnt.

>> Tik weer enkele keren <DEL> om zoveel tekens weg te halen dat
   de regel weer op een schermregel past.  De vervolgregel zal
   verdwijnen.

Je kan een Newline zoals elk ander teken verwijderen.  Als je een
Newline verwijdert, voeg je de twee regels waar de Newline tussen
staat samen tot een enkele regel.  Als de regel die het resultaat is
niet op een enkele schermregel past, zal hij getoond worden met een
vervolgregel.

>> Beweeg de cursor naar het begin van een regel en tik <DEL>.
   Dit voegt die regel en de vorige regel samen.

>> Tik <Return> om de Newline die je net verwijderd hebt weer toe te
   voegen.

Je herinnert je dat je bij de meeste Emacs-commando's het aantal keren
op kan geven, dat ze herhaald moeten worden.  Dit geldt ook voor
gewone tekens.  Als je een gewoon teken herhaalt, wordt dat teken
meerdere keren toegevoegd.

>> Probeer dat nu: tik C-u 8 * om ******** toe te voegen.

Je hebt nu de basismanier geleerd om iets in Emacs te tikken en fouten
te verbeteren.  Je kan tekst ook per woord of regel verwijderen.  Hier
volgt een samenvatting van de commando's om tekst te verwijderen:

	<DEL>        Haal het teken weg dat voor de cursor staat
	C-d   	     Haal het teken weg dat achter de cursor staat

	M-<DEL>      Verwijder het woord dat voor de cursor staat
	M-d	     Verwijder het woord dat achter de cursor staat

	C-k	     Verwijder alles van de cursor tot het eind van de regel
	M-k	     Verwijder alles van de cursor tot het eind van de zin

Merk op dat <DEL> en C-d, met M-<DEL> en M-d de analogie verder
trekken, die begon met C-f en M-f (waarbij we voor het gemak even
vergeten dat <DEL> niet echt een CONTROL-teken is).  C-k en M-k lijken
enigzins op C-e en M-e in hun relatie tot regels en zinnen.

Je kunt ook op één uniforme manier een willekeurig deel van de tekst
verwijderen.  Beweeg daartoe naar één kant van het gedeelte dat je
wilt verwijderen en tik C-<SPC>.  (<SPC> is de spatiebalk.)  Beweeg nu
naar de andere kant van de tekst die je wilt verwijderen.  Terwijl je
beweegt, markeert Emacs zichtbaar de tekst tussen de cursor en de
plaats waar je C-<SPC> tikte.  Tik C-w.  Dit verwijdert alle tekst
tussen beide posities.

>> Beweeg de cursor naar de J aan het begin van de vorige alinea.
>> Tik C-<SPC>.  Emacs toont nu de mededeling "Mark set" ("Markering
   geplaatst") onderaan het scherm.
>> Plaats de cursor op de n van "kant" op de tweede regel van de
   alinea.
>> Tik C-w.  Dit zal de tekst vanaf de J tot aan de n verwijderen.

Er is een verschil tussen iets verwijderen ("kill") en iets weghalen
("delete"): iets dat je hebt verwijderd ("killed"), kan je
terugbrengen, maar iets dat je hebt weggehaald ("deleted") niet.  (Je
kan het weghalen wel herstellen, zie verderop.)  Verwijderde tekst
invoegen heet "yanken".  In het algemeen geldt dat de commando's die
veel tekst kunnen verwijderen, deze tekst bewaren zodat hij geyankt
kan worden, terwijl dat niet geldt voor commando's die slechts een
enkel teken weghalen.  <DEL> en C-d zijn de eenvoudigste commando's om
tekst weg te halen, zonder argument.  Met argument verwijderen ze en
kan de verwijderde tekst geyankt worden.

>> Zet de cursor op het begin van een regel die niet leeg is.
   Tik C-k om de tekst op die regel te verwijderen.
>> Tik C-k een tweede keer.  Nu verwijdert dit commando het
   Newline-teken.

Merk op hoe een enkel C-k commando de inhoud van een regel verwijdert,
een tweede C-k commando de regel zelf zodat alle volgende regels een
regel omhoog komen.  Het numerieke argument is voor C-k bijzonder: het
aangegeven aantal regels zal worden verwijderd, inclusief de inhoud.
Dit is meer dan simpelweg herhaling: C-u 2 C-k verwijdert twee regels,
terwijl tweemaal C-k tikken dat niet doet.

Het invoegen van de laatst verwijderde tekst heet yanken ("yanking").
Je kan de tekst yanken op de plek waar je het verwijderde, op een
andere plek of zelfs in een ander bestand.  Je kan dezelfde tekst
meerdere keren yanken; op deze manier maak je meerdere kopieën van
dezelfde tekst.  Verwijderen ("killing") en yanken worden in andere
programma's ook wel knip ("cut") en plak ("paste") genoemd (zie ook de
Glossary in de Emacs-handleiding).

Het commando om te yanken is C-y.  Het voegt de laatst verwijderde
tekst in op de huidige cursorpositie.

>> Probeer het nu: tik C-y om de tekst te yanken.

Als je meerdere keren C-k achter elkaar hebt gedaan, dan worden alle
verwijderde tekstregels samen onthouden, zodat een enkele C-y al die
regels in één keer invoegt.

>> Probeer het nu: tik C-k een paar keer.

Om de verwijderde tekst terug te halen:

>> Tik C-y.  Beweeg de cursor enkele regels naar beneden en tik weer
   C-y.  Je ziet nu hoe je tekst kan kopiëren.

Wat moet je doen als je wat tekst terug wilt brengen, maar je intussen
al iets anders verwijderd hebt?  C-y zou datgene terugbrengen wat je
het recentst hebt verwijderd.  Gelukkig is de voorgaande tekst niet
verloren gegaan.  Je kunt die tekst terughalen met M-y.  Nadat je C-y
hebt getikt om de recentst weggegooide tekst terug te halen, vervangt
M-y die tekst met de tekst die je daarvoor had weggegooid.  Je kunt
M-y herhalen om tekst terug te halen die je al langer geleden hebt
weggegooid.  Als je de tekst te pakken hebt die je zocht, hoef je
niets te doen om die daar te houden.  Je kan gewoon verder werken en
de teruggehaalde tekst met rust laten.

Als je M-y vaak genoeg tikt kom je terug waar je begon, bij de laatst
verwijderde tekst.

>> Verwijder een regel, beweeg de cursor wat, en verwijder nog een
   regel.  Tik C-y om de tweede regel die je verwijderde, terug te
   halen.  Tik M-y en die regel wordt vervangen door de eerste
   regel die je verwijderde.  Tik nog enkele keren M-y en zie wat er
   langs komt.  Herhaal dit tot de tweede regel weer langs komt, en
   dan nog een paar keer.  Je kan ook experimenteren met positieve en
   negatieve argumenten bij M-y.


* HERSTELLEN
------------

Als je de tekst veranderd hebt en je daar toch niet tevreden mee bent,
dan kan je de verandering ongedaan maken met het herstelcommando, C-/.

Normaal gesproken herstelt C-/ de veranderingen die het gevolg zijn
van een enkel commando; door herhaaldelijk C-/ te tikken, worden
steeds eerdere commando's hersteld.

Er zijn echter twee uitzonderingen: commando's die de tekst niet
wijzigen, zoals cursorbewegingen en scrollen, worden overgeslagen, en
commando's die simpelweg het ingetikte teken aan de tekst toevoegen,
worden meestal samengenomen in groepjes van maximaal 20 tekens.
(Hierdoor hoef je minder vaak C-/ te tikken om teksttoevoegingen te
herstellen.)

>> Gooi deze regel weg met C-k; met C-/ zal hij weer verschijnen.

C-_ is een alternatief herstelcommando; het doet exact hetzelfde als
C-/.  Op sommige terminals stuurt het tikken van C-/ in werkelijkheid
een C-_ naar Emacs.  Nog een alternatief commando is C-x u, maar dit
is minder makkelijk te tikken.

Een numeriek argument bij C-/, C-_ of C-x u duidt het aantal
herhalingen aan.

Je kan het weghalen van tekst herstellen, net zoals je het verwijderen
ervan herstelt.  Het verschil tussen iets verwijderen of weghalen is
of je het kan yanken met C-y.  Voor het herstellen maakt het geen
verschil.



* BESTANDEN
-----------

Om een tekst die je gemaakt of veranderd hebt op te slaan, moet je de
tekst in een bestand opslaan ("to save a file" in het Engels).  Als je
dat niet doet, ben je die veranderingen kwijt op het moment dat je
Emacs verlaat.  Je kan een bestand veranderen door het bestand te
"bezoeken".  (Ook wel "vinden"; "finding" of "visiting" in het
Engels.)

Een bestand bezoeken betekent dat je de inhoud van dat bestand in
Emacs ziet.  Het lijkt er dan op alsof je het bestand aan het
veranderen bent.  Deze veranderingen zijn echter slechts tijdelijk
zolang je het bestand niet opslaat.  Op deze manier kan je nooit per
ongeluk een half gewijzigd bestand op het systeem achterlaten.  Zelfs
als je het bestand opslaat, zorgt Emacs ervoor dat het originele
bestand onder een gewijzigde naam nog steeds beschikbaar is, voor het
geval je later besluit dat de veranderingen toch niet zo goed waren.

Bij de onderkant van het scherm zie je een regel die begint 
met streepjes, met aan het begin "-:--  TUTORIAL.nl" of iets
dergelijks.  Dit deel van het scherm laat normaal de naam van het
bestand zien dat je op dat moment bezoekt.  Op dit moment bezoek je
een bestand dat "TUTORIAL.nl" heet; het is je eigen kopie van de
Nederlandstalige Emacs-inleiding ("tutorial" in het Engels).  Als je
in Emacs een bestand bezoekt dan staat de naam van het bestand altijd
op deze plaats.

Iets bijzonders aan het commando om een bestand te bezoeken, is dat je
aan moet geven welk bestand je wil.  Dit heet dat het commando "een
argument inleest"; in dit geval de naam van het bestand.  Nadat je het
commando

	C-x C-f		Bezoek bestand (met de f van "find file").

hebt getikt vraagt Emacs om de naam van het bestand.  De naam die je
intikt verschijnt op de onderste regel van het scherm.  Wanneer die
regel voor dit soort invoer gebruikt wordt, heet hij de minibuffer.
Je kan gewone Emacs commando's gebruiken om de bestandsnaam in te
geven.

Tijdens het invoeren van de bestandsnaam (of om het even welke invoer
in de minibuffer) kan je het commando afbreken met C-g.

>> Tik C-x C-f gevolgd door C-g.  Dit commando breekt de minibuffer af
   en ook het C-x C-f commando dat van de minibuffer gebruik maakte.
   Het resultaat is dat je geen bestand bezoekt.

Als je de naam van een bestand hebt ingevoerd, tik dan <Return> om het
commando af te sluiten.  Hierna verdwijnt de minibuffer en gaat het
C-x C-f commando aan het werk: het haalt het bestand op dat je
aangegeven hebt.

De inhoud van het bestand verschijnt nu op het scherm en je kan de
inhoud wijzigen.  Als je de wijzigingen op wilt slaan, tik dan het
commando

	C-x C-s   Sla bestand op (met de s van "save file").

Dit commando slaat de tekst zoals Emacs die nu heeft op in het
bestand.  De eerste keer dat je dit doet, slaat Emacs het originele
bestand onder een andere naam op, zodat het niet verloren gaat.  De
nieuwe naam bestaat uit de oude bestandsnaam gevolgd door een "~".

Als Emacs het bestand heeft opgeslagen, laat het de naam van het
bestand zien.  Het is een goede gewoonte een bestand regelmatig op te
slaan zodat er niet teveel werk verloren gaat als het systeem crasht
(zie ook "Automatisch bewaren" hieronder).

>> Tik C-x C-s TUTORIAL.nl <Return>
   Op deze manier sla je deze inleiding op in een bestand genaamd
   TUTORIAL.nl.  Als het goed is verschijnt "Wrote ...TUTORIAL.nl" op de
   onderste schermregel.

Je kan een bestand dat al bestaat bezoeken om het te bekijken of het
te wijzigen.  Je kan ook een bestand bezoeken dat nog niet bestaat.
Dit is de manier om met Emacs een nieuw bestand te maken: bezoek het
bestand, dat eerst leeg zal zijn, en voeg tekst toe.  Zodra je de
tekst opslaat, wordt het bestand werkelijk gecreëerd, met de nieuwe
tekst als inhoud.  Vanaf dat moment ben je dus bezig met een bestand
dat al bestaat.


* BUFFERS
---------

Als je een tweede bestand bezoekt met C-x C-f, blijft het eerste
bestand gewoon in Emacs.  Je kan naar dat bestand terug door het
gewoon nog een keer te bezoeken met C-x C-f.  Op deze manier kan je
een behoorlijk aantal bestanden in Emacs hebben.

Emacs onthoudt de tekst van elk bestand in een ding dat een "buffer"
heet.  Als je een bestand bezoekt maakt Emacs een nieuwe buffer aan.
Om een lijst van de huidige buffers te zien, tik

	C-x C-b   Toon de bufferlijst

>> Probeer C-x C-b nu.

Merk op dat elke buffer een naam heeft en mogelijk ook een
bestandsnaam: de naam van het bestand waarvan de inhoud in de buffer
zit.  ALLE tekst die je in een Emacs venster ziet is altijd onderdeel
van een of andere buffer.

>> Tik C-x 1 om de bufferlijst uit het zicht krijgen.

Wanneer je met meerdere buffers werkt, dan is op elk moment slechts
één van die buffers "actueel".  De actuele buffer is degene die je aan
het bewerken bent.  Als je een andere buffer wilt bewerken, dan moet
je daarnaar "omschakelen".  Als je wilt omschakelen naar een buffer
die overeenkomt met een bestand, dan kun je dit doen door dat bestand
opnieuw te bezoeken met C-x C-f.  Er is ook een makkelijkere manier:
gebruik het commando C-x b.  Dit commando vraagt je naar de naam van
de buffer.

>> Bezoek een bestand met de naam "foo" door te tikken: C-x C-f foo
   <Return>.  Tik vervolgens C-x b TUTORIAL <Return> om terug te komen
   in deze Emacs-inleiding.

Meestal is de naam van de buffer gelijk aan de naam van het bestand
(minus de naam van de directory).  Dit klopt echter niet altijd.  De
lijst met buffers die je maakt met C-x C-b laat je zowel de naam van
buffer als de bestandsnaam van alle buffers zien.

ALLE tekst die je ziet in een venster van Emacs is altijd onderdeel
van een of andere buffer.  Sommige buffers komen niet overeen met een
bestand.  De buffer genaamd "*Buffer List*" heeft bijvoorbeeld geen
bijbehorend bestand (deze buffer bevat de lijst met buffers die je
gemaakt hebt met C-x C-b).  Deze TUTORIAL.nl-buffer had in het begin
ook geen bijbehorend bestand; nu heeft hij die wel omdat je eerder in
deze inleiding C-x C-s tikte om hem in een bestand op te slaan.

Ook de buffer "*Messages*" hoort niet bij een bestand; deze buffer
bevat de mededelingen die Emacs op de onderste regel toonde tijdens
deze Emacs-sessie.

>> Tik C-x b *Messages* <Return> om de buffer met mededelingen te
   bekijken.  Tik daarna weer C-x b TUTORIAL.nl <Return> om terug te
   keren naar deze Emacs-inleiding.

Als je de tekst van het ene bestand verandert en dan een ander bestand
bezoekt, wordt het eerste bestand niet opgeslagen.  De wijzigingen
blijven in Emacs, in de buffer die bij het bestand hoort.  Het creëren
of veranderen van de buffer van het tweede bestand heeft geen effect
op de eerste buffer.  Dit is erg nuttig, maar betekent ook dat er een
eenvoudige manier nodig is om het eerste bestand te bewaren.  Het zou
erg vervelend zijn om er eerst naar terug te moeten gaan met C-x C-f
om het dan te kunnen bewaren met C-x C-s.  Dus hebben we het commando:

	C-x s	  Sla een paar buffers op

C-x s vraagt voor elke buffer die veranderingen heeft die nog niet
opgeslagen zijn, of je de buffer wilt bewaren.

>> Voeg wat tekst toe en tik C-x s.
   Emacs vraagt nu of je de buffer die TUTORIAL.nl heet wilt bewaren.
   Beantwoord deze vraag positief door een "y" in te tikken (de y van
   "yes", Engels voor "ja").


* UITGEBREIDE COMMANDO'S
------------------------

Er zijn veel meer Emacs commando's dan er op de toetsen van het
toetsenbord passen, zelfs als we hun aantal vergroten door de CONTROL-
of de META-toets te gebruiken.  Emacs lost dit probleem op met het X
commando (met de X van eXtensie of uitbreiding).  Het X commando kent
twee smaken:

	C-x	Tekenuitbreiding.  Wordt gevolgd door een teken.
	M-x	Commando-naam-uitbreiding.  Wordt gevolgd door een naam.

Deze commando's zijn in het algemeen nuttig, maar worden minder
gebruikt dan de commando's die je tot nu toe al geleerd hebt.  Je hebt
al enkele van deze commando's gezien: C-x C-f om een bestand te
bezoeken en C-x C-s om het te bewaren, bijvoorbeeld.  Een ander
voorbeeld is het commando om Emacs te verlaten: dit is C-x C-c.  (Maak
je geen zorgen over het verloren gaan van veranderingen die niet
opgeslagen zijn; C-x C-c vraagt of je veranderde buffers wilt bewaren
voordat Emacs helemaal stopt.)

Als je een grafisch scherm gebruikt heb je geen commando's nodig om
van Emacs naar een andere applicatie te gaan.  Je gebruikt dat de muis
of commando's van de vensterbeheerder.  Als je Emacs gebruikt in een
tekstvenster of terminal, die maar één applicatie tegelijkertijd kan
laten zien, moet je Emacs tijdelijk verlaten om naar een andere
applicatie te gaan.

C-z is het commando om Emacs *tijdelijk* te verlaten, zodat je daarna
weer terug kan keren naar dezelfde Emacs-sessie.  Als je Emacs in een
tekstvenster op terminal gebruikt, zet C-z Emacs stil: je komt weer
terug in de shell, maar Emacs is nog aanwezig.  In de meeste shells
kan je Emacs weer activeren met het "fg" commando, of met "%emacs".

Het moment om C-x C-c te gebruiken is wanneer je uit gaat loggen.  Het
is ook het juiste commando om Emacs te beëindigen wanneer Emacs
opgestart was door een mail-programma of iets dergelijks.

Er bestaan vele C-x commando's.  Hier is een lijst van degene die je
nu al kent:

	C-x C-f		Bezoek bestand
	C-x C-s		Sla bestand op
	C-x s		Sla een paar buffers op
	C-x C-b		Laat bufferlijst zien
	C-x b		Schakel naar een buffer
	C-x C-c		Verlaat Emacs
	C-x 1		Een enkel venster
	C-x u		Herstel

Commando-naam-commando's worden nog minder vaak gebruikt, of alleen
onder bepaalde omstandigheden.  Een voorbeeld is het commando
replace-string, dat in de hele tekst een string vervangt door een
andere string ("to replace" betekent "vervangen").  Als je M-x tikt,
toont Emacs onderaan het scherm "M-x" en moet je de naam van het
commando intikken, in dit geval "replace-string".  Als je gewoon
"repl s<TAB>" tikt maakt Emacs de naam zelf af.  (<TAB> is de
Tab-toets, die meestal boven de CapsLock of Shift-toets zit aan de
linkerkant van het toetsenbord.)  Beëindig het commando met <Return>.

Het replace-string commando heeft twee argumenten nodig: de string die
vervangen moet worden en de string waarmee die vervangen moet worden.
Je sluit elk argument af met <Return>.

>> Plaats de cursor op de lege regel twee regels onder deze regel.
   Tik dan M-x repl s<Return>gewijzigd<Return>veranderd<Return>.

   Zie hoe deze regel daardoor gewijzigd is.  Je hebt elk voorkomen
   van het woord g-e-w-i-j-z-i-g-d vervangen door "veranderd", te
   beginnen op de plek waar de cursor stond.


* AUTOMATISCH BEWAREN
---------------------

Als je een bestand veranderd hebt maar je hebt het nog niet opgeslagen,
zouden de veranderingen verloren kunnen gaan als het systeem zou
hangen of herstarten.  Om je hiertegen te beschermen, slaat Emacs
regelmatig de veranderde tekst automatisch op.  De naam van het
bestand waarin de tekst automatisch wordt opgeslagen begint en eindigt
met een #.  Bijvoorbeeld, als je het bestand "hello.c" aan het
bewerken bent, wordt de tekst automatisch opgeslagen in een bestand
dat "#hello.c#" heet.  Zodra je het bestand werkelijk opslaat, wordt
het automatisch opgeslagen bestand verwijderd.

Als de computer crasht, kan je de automatisch opgeslagen tekst
terugkrijgen door het bestand gewoon te bezoeken (het originele
bestand, niet het automatisch opgeslagen), gevolgd door M-x
recover-file<Return>.  Als Emacs vraagt om bevestiging, antwoord dan
met yes<Return> en de automatisch opgeslagen informatie wordt
teruggehaald.


* ECHO-GEBIED
-------------

Als je een commando dat uit meerdere tekens bestaat langzaam intikt,
toont Emacs de tekens onderin het scherm in een deel dat het
"echo-gebied" genoemd wordt.  Dit gebied omvat de onderste regel van
het scherm.


* MODUS-REGEL
-------------

De regel direct boven het echo-gebied heet de "modusregel".  De
modusregel ziet er ongeveer zo uit:

-1:**  TUTORIAL.nl    63% L776    (Fundamental)-----------------------

Deze regel geeft nuttige informatie over Emacs en de tekst die je aan
het bewerken bent.

Je weet al wat de bestandsnaam betekent: het is de naam van het
bestand dat je bezoekt.  NN% geeft je huidige positie in de tekst aan:
NN procent van de tekst bevindt zich boven het scherm.  Als het
bestand vanaf het begin op het scherm staat, staat er "Top" in plaats
van " 0%".  Als het laatste stuk tekst op het scherm staat, zal er
"Bot" staan (van "bottom", "onderkant" in het Nederlands).  Als de
tekst zo klein is dat hij volledig op het scherm past staat "All" in
de modus-regel.

De L gevolgd door een getal geeft het regelnummer aan waar punt zich
bevindt.

De sterretjes aan het begin betekenen dat je de tekst veranderd hebt.
Direct na het bezoeken of opslaan staan er gewoon streepjes.

In de modusregel staat tussen haakjes in welke modus je aan het werken
bent.  De standaardmodus is de "Fundamental" modus, die je nu gebruikt
("fundamental" is "basis" in het Nederlands).  Een dergelijke modus
heet een hoofdmodus ("major mode" in het Engels).

Emacs heeft verschillende hoofdmodi.  Sommige daarvan zijn bedoeld
voor het bewerken van verschillende talen of soorten tekst, zoals
bijvoorbeeld Lisp-modus, Text-modus, etc.  Op elk moment is er altijd
precies één modus actief, en de naam daarvan staat in de modusregel,
op de plaats waar nu "Fundamental" staat.

Elke hoofdmodus zorgt ervoor dat sommige commando's zich anders
gedragen.  Zo bestaat er een commando om een commentaar in een
programma te tikken, en aangezien elke programmeertaal een ander idee
heeft over hoe commentaar eruit moet zien, moet elke hoofdmodus op een
andere manier het commentaar beginnen.  Elke hoofdmodus is de naam van
een uitgebreid commando, en met dat commando schakel je om naar die
hoofdmodus.  Zo is bijvoorbeeld M-x fundamental-mode het commando om
naar de basismodus om te schakelen.

Als je Nederlandse of Engelse tekst gaat bewerken, zoals bijvoorbeeld
dit bestand, kan je beter "Text mode" gebruiken, de modus om tekst in
een gewone taal te bewerken:

>> Tik M-x text-mode <Return>.

Wees gerust; geen van de commando's die je geleerd hebt gaan zich nu
echt anders gedragen.  Een van de dingen die je kan merken, is
bijvoorbeeld dat M-f en M-b nu apostrofs als onderdeel van een woord
beschouwen.  In de vorige modus (Fundamental) behandelen M-f en M-b de
apostrof als ruimte tussen twee woorden.

Het is gebruikelijk dat hoofdmodi dergelijke subtiele verschillen
hebben.  De meeste commando's doen dus min of meer hetzelfde in elke
hoofdmodus.

Met het commando C-h m kan je de documentatie over de huidige
hoofdmodus lezen.

>> Gebruik C-l C-l om deze regel bovenin het scherm te krijgen.
>> Tik C-h m om te zien hoe de tekstmodus verschilt van de basismodus.
>> Tik C-x 1 om de documentatie van het scherm te verwijderen.

Hoofdmodi heten zo omdat er ook bijmodi zijn.  Bijmodi zijn geen
alternatieven voor hoofdmodi; het zijn slechts kleine aanpassingen
daarvan.  Elke bijmodus kan aan- of uitgezet worden, onafhankelijk van
andere bijmodi en onafhankelijk van de hoofdmodus.  Het is dus
mogelijk geen bijmodi, één bijmodus of een willekeurige combinatie van
bijmodi te gebruiken.

Een nuttige bijmodus voor het bewerken van tekst in een natuurlijke
taal, zoals het Nederlands, is Auto Fill modus ("auto fill" betekent
automatisch uitvullen).  Wanneer deze modus aanstaat, breekt Emacs
automatisch een regel tussen twee woorden af als de regel te lang
wordt.

Je kan Auto Fill modus aanzetten met M-x auto-fill-mode <Return>.  Als
deze modus al aanstaat, kan je hem uitzetten met
M-x auto-fill-mode <Return>.  Als de modus uitstaat, zet dit commando
de modus aan; als ze aanstaat, zet dit commando de modus uit.  We
zeggen dat het commando de modus "schakelt" ("to toggle" in het
Engels).

>> Tik nu M-x auto-fill-mode<Return>.  Tik nu vele malen "asdf " op
   een regel totdat je ziet dat de regel in tweeën gesplitst wordt.
   Er moeten wel spaties tussen de woorden staan, omdat de Auto Fill
   modus de regel alleen op spaties breekt.

De rechterkantlijn staat meestal op 70 tekens, maar die kan je
veranderen met het C-x f commando.  Dit commando accepteert de
gewenste kantlijn als numeriek argument.

>> Tik C-x f met 20 als argument (C-u 20 C-x f).
   Tik wat tekst en zie dat Emacs de regels afbreekt bij 20 tekens.
   Zet de kantlijn nu terug op 70, met C-u 70 C-x f.

Als je de tekst midden in een regel verandert, vult Auto Fill modus de
regel niet opnieuw.
Om een alinea opnieuw te vullen, tik M-q (META-q) terwijl de cursor in
de alinea staat.

>> Plaats de cursor in de voorgaande alinea en tik M-q.


* ZOEKEN
--------

Emacs kan tekenreeksen ("strings") zoeken, zowel volgend op de
cursorpositie, als eraan voorafgaand.  Het zoeken naar een string
verplaatst de cursor naar de volgende plaats waar de gezochte string
voorkomt.

Het zoekcommando van Emacs zoekt incrementeel.  Dit betekent dat het
zoeken gebeurt tijdens het intikken van de gezochte string.

Het commando om het voorwaarts zoeken te starten is C-s (met de "s"
van "to search", zoeken); C-r start het achterwaarts zoeken (met de
"r" van "reverse" of achteruit).  WACHT!  Probeer ze nu nog niet.

Als je C-s tikt verschijnt de string "I-search" in het echo-gebied.
Dit betekent dat Emacs bezig is met een "incremental search"
(incrementele zoekopdracht) en wacht op het intikken van de
zoekstring.  <Return> beëindigt het zoeken.

>> Tik nu C-s om het zoeken te starten.  Tik nu, LANGZAAM, één letter
   per keer, het woord "cursor", met een pauze na elke letter zodat je
   kan zien wat er met de cursor gebeurt.  Je hebt nu eenmaal naar het
   woord "cursor" gezocht.
>> Tik nogmaals C-s, om naar het volgende voorkomen van het woord
   "cursor" te zoeken.
>> Tik nu viermaal <Del> en let op de cursorbewegingen.
>> Tik <Return> om het zoeken te beëindigen.

Zag je wat er gebeurde?  Tijdens incrementeel zoeken probeert Emacs
naar de eerste plek te gaan waar de string staat die je tot dan toe
getikt hebt.  Om naar de volgende plek te gaan, tik je C-s nog een
keer.  Als er geen volgende plek is gevonden, biept Emacs en vertelt
je dat de zoekopdracht niets gevonden heeft ("failing" in het Engels).
C-g zou het zoeken ook afbreken.

Als je tijdens incrementeel zoeken <DEL> tikt, dan gaat het zoeken
terug naar de vorige plek.  Als je <DEL> tikt nadat je C-s hebt getikt
om naar een volgende plaats te gaan waar de zoekstring voorkomt, zal
<DEL> de cursor terug laten gaan naar de vorige plaats.  Als er geen
vorige plaats is verwijdert <DEL> het laatste karakter van de
zoekstring.  Als je bijvoorbeeld begint met zoeken en je tikt een "c",
dan ga je naar de plaats waar de "c" het eerst voorkomt.  Tik je
vervolgens een "u", dan gaat de cursor naar de eerstvolgende plaats
waar de string "cu" het eerst voorkomt.  Tik nu <DEL> en de "u" wordt
van de zoekstring afgehaald en de cursor gaat terug naar de plaats
waar "c" het eerst voorkwam.

Als je tijdens een zoekoperatie een CONTROL- of META-teken intikt, dan
wordt het zoeken beëindigd.  Er zijn een paar uitzonderingen, namelijk
tekens die tijdens zoeken een speciale betekenis hebben, zoals C-s en
C-r.

Met C-s begin je te zoeken naar de plaats waar de zoekstring voor het
eerst voorkomt NA de huidige cursorpositie.  Als je iets wilt zoeken
dat eerder in de tekst moet voorkomen, gebruik dan C-r.  Alles wat we
nu weten over C-s geldt ook voor C-r, alleen is de zoekrichting
omgedraaid.


* MEERDERE VENSTERS
-------------------

Een van Emacs' aardige eigenschappen is dat je meerdere vensters op
het scherm kan laten zien.  (Merk op dat wat Emacs "frames" noemt in
andere systemen "vensters" genoemd wordt.  Zie de Woordenlijst van
Emacs-termen (Glossary of Emacs terms) in de Emacs-handleiding.)

>> Zet de cursor op deze regel en tik C-l C-l.

>> Tik C-x 2 om het scherm in twee vensters op te splitsen.
   Beide vensters laten deze inleiding zien; de cursor blijft in het
   bovenste venster.

>> Tik C-M-v om de tekst in het onderste venster te verschuiven.
   (Als je geen META-toets hebt, tik dan <ESC> C-v.)

>> Tik C-x o (met de o van "other"; "ander" in het Nederlands) om de
   cursor naar het andere venster te verplaatsen.
>> Verschuif de tekst in het onderste venster, met C-v en M-v.
   Deze inleiding kan je blijven lezen  in het bovenste venster.

>> Tik weer C-x o om de cursor weer in het bovenste venster te zetten.
   De cursor staat weer precies op de plaats waar hij stond toen je
   het venster verliet.

Je kan C-x o blijven gebruiken om van venster naar venster te gaan.
Het "geselecteerde venster" ("selected windows" in het Engels), waar
de meeste bewerkingen plaatsvinden, is die met die vette cursor die
knippert als je niet aan het tikken bent.  De andere vensters hebben
hun eigen cursorposities.  Als je Emacs gebruikt op een grafisch
scherm, dan zijn de cursors in die andere venters niet-gevulde
rechthoekjes die niet knipperen.

Het C-M-v commando is erg nuttig wanneer je tekst aan het bewerken
bent in het ene venster, terwijl je het andere venster als referentie
gebruikt.  Je kan de cursor dan altijd in het venster houden waarin je
bezig bent, terwijl je met C-M-v door de tekst in het andere venster
loopt.

C-M-v is een voorbeeld van een CONTROL-META teken.  Als je een
META-toets (of Alt-toets) hebt kan je C-M-v intikken door zowel
CONTROL als META ingedrukt te houden terwijl je v tikt.  Het maakt
niet uit in welke volgorde je CONTROL en META indrukt; het gaat erom
welke toetsen ingedrukt zijn terwijl je tikt.

Als je geen echte META-toets hebt kan je <ESC> gebruiken; de volgorde
is dan wel belangrijk.  Je moet dan eerst <ESC> tikken, gevolgd door
CONTROL-v; CONTROL-<ESC> v zal niet werken.  Dit komt doordat <ESC>
zelf een teken is, terwijl CONTROL en META dat niet zijn: dat zijn
"modifiers" (Engels).

>> Tik C-x 1 (in het bovenste venster) om het onderste venster te
   laten verdwijnen.

(Als je C-x 1 tikt in het onderste venster laat je het bovenste
verdwijnen.  C-x 1 betekent zoveel als "ik wil maar 1 venster,
en wel het venster waar ik nu ben.")

Je hoeft niet dezelfde buffer in beide vensters te hebben.  Wanneer je
C-x C-f gebruikt om een bestand in één van de vensters te bezoeken,
verandert het andere venster niet.  Je kunt de vensters onafhankelijk
van elkaar gebruiken om bestanden te bezoeken.

Hier is nog een manier om twee venster te krijgen die elk een andere
tekst laten zien:

>> Tik C-x 4 C-f gevolgd door de naam van een van je bestanden,
   gevolgd door <Return>.  Het opgegeven bestand zal in het onderste
   venster verschijnen, en de cursor zal in dat venster staan.

>> Tik C-x o om terug naar het bovenste venster te gaan, en C-x 1 om
   het onderste venster te laten verdwijnen.


* MEERDERE FRAMES
-----------------

Emacs kan meerdere zogeheten frames maken.  Een frame bestaat uit
vensters, menu's, scrollbalken, echo-gebied, etc.  Op grafische
schermen is een Emacs-frame wat andere applicaties meestal een venter
(of een "window" in het Engels, vgl. Windows) noemen.  Meerdere
grafische frames kunnen tegelijk op het scherm getoond worden.  Een
tekstterminal kan maar één frame tegelijkertijd tonen.

>> Tik M-x make-frame <Return>.
   Een nieuw frame verschijnt op het scherm.

In het nieuwe frame kan je alles doen wat je ook in het eerste frame
kon doen.  Het eerste frame is niet speciaal.

>> Tik M-x delete-frame <Return>.
   Het actieve frame verdwijnt.

Je kan een frame ook laten verdwijnen op de manier die gebruikelijk is
voor het grafische systeem dat je gebruikt, vaak door de button te
klikken in een van de bovenhoek van het frame die gemarkeerd is met
een "X".  Als je Emacs' laatste frame op deze manier laat verdwijnen,
dan sluit je Emacs af.


* RECURSIEVE BEWERKINGSNIVEAUS
------------------------------

Soms kom je in Emacs in een recursief bewerkingsniveau terecht
(Engels: "recursive editing level").  Dit is te zien in de modusregel
aan de vierkante haken die om de haakjes van de naam van de hoofdmodus
staan.  Dan staat er bijvoorbeeld [(Fundamental)] in plaats van
(Fundamental).

Tik <ESC> <ESC> <ESC> om uit een recursief bewerkingsniveau te komen.
Dit is een algemeen "ontsnappingscommando".  Je kan het ook gebruiken
om extra vensters te verwijderen of om uit de minibuffer te komen.

>> Tik M-x om in een minibuffer te komen; tik dan <ESC> <ESC> <ESC>
   om er weer uit te komen.

C-g is niet bruikbaar om uit een recursief bewerkingsniveau te komen.
De reden hiervoor is dat C-g gebruikt wordt om commando's af te breken
BINNEN het recursieve bewerkingsniveau.


* MEER INFORMATIE
-----------------

We hebben geprobeerd je met deze inleiding precies genoeg informatie
te leveren om met Emacs te beginnen werken.  De mogelijkheden van
Emacs zijn zo groot dat het onmogelijk is nu alles uit te leggen.  Het
kan zijn dat je meer over Emacs wil leren omdat het zoveel nuttige
mogelijkheden heeft.  Emacs heeft commando's om documentatie te lezen
over Emacs commando's.  Deze "helpcommando's" beginnen allemaal met
C-h: "het Hulpteken".

Om hulp te krijgen tik je C-h, gevolgd door een teken om aan te duiden
welke hulp je wilt.  Als je het echt niet meer weet, tik C-h ? en
Emacs vertelt welke hulp het allemaal te bieden heeft.  Als je C-h
hebt getikt maar van gedachten veranderd bent, tik je gewoon C-g om
het af te breken.

(Als C-h niet een bericht onderaan het scherm laat zien over mogelijke
hulp, probeer dan functietoets F1 of gebruik M-x help <Return>.)

De eenvoudigste hulp is C-h c.  Tik C-h, het teken "c" en een teken of
uitgebreid commando, en Emacs laat een zeer korte beschrijving van het
commando zien.

>> Tik C-h c C-p.

De beschrijving die getoond wordt, zou zoiets moeten zijn als:

	C-p runs the command previous-line

   (Nederlands: C-p voert het commando previous-line uit.)

Dit commando vertelt je "de naam van de functie".  Aangezien
functienamen gekozen zijn om aan te geven wat de functie doet, zijn ze
ook geschikt als heel korte documentatie; genoeg om je te herinneren
aan wat de commando's die je al geleerd hebt betekenen.

Uitgebreide commando's zoals C-x C-s en (als je geen META-, EDIT- of
ALT-toets hebt) <ESC>v kunnen ook getikt worden na C-h c.

Om meer informatie over een commando te krijgen, tik C-h k in plaats
van C-h c.

>> Tik C-h k C-p.

Dit laat de documentatie van de functie, inclusief de naam van de
functie, in een apart venster zien.  Als je klaar bent met lezen, tik
C-x 1 om van dat venster af te komen.  Je hoeft dat natuurlijk niet
meteen te doen.  Je kan ook eerst wat tekst bewerken (en de helptekst
lezen) voordat je C-x 1 tikt.

Hier zijn nog wat nuttige mogelijkheden van C-h:

   C-h f	Beschrijf een functie.  Je moet de naam van de functie
		intikken.

>> Tik C-h f previous-line <Return>
   Dit laat alle informatie zien die Emacs heeft over de functie die
   het C-p commando implementeert.

Een vergelijkbaar commando C-h v toont de documentatie van variabelen
die je kunt instellen om het gedrag van Emacs naar wens aan te passen.
Het commando vraagt je om de naam van een variabele.

   C-h a	Commando Apropos.  Tik een woord in en Emacs zal een
		lijst van alle commando's laten zien waarin dat woord
		voorkomt.  Al deze commando's kunnen aangeroepen
		worden met M-x.  Bij sommige commando's staat met
		welke tekens dit commando direct uitgevoerd kan
		worden.

>> Tik C-h a file <Return>.

Dit laat in een ander venster alle M-x commando's zien met "file" in
hun naam.  Je zal teken-commando's zien als C-x C-f naast de
overeenkomende commandonaam zoals find-file.

>> Tik C-M-v herhaaldelijk om de tekst in het hulpvenster te
   scrollen.

>> Tik C-x 1 om het hulpvenster te verwijderen.

   C-h i	Lees de handleidingen (ook wel Info genoemd).
		Dit commando zet je in een speciale buffer genaamd
		"*info*" waar je handleidingen kunt lezen van
		software die op je computer is geïnstalleerd.
		Tik m Emacs <Return> om de handleiding van Emacs te
		lezen.  Als je nog nooit Info hebt gebruikt dan kun je
		? tikken zodat Emacs je een rondleiding geeft langs de
		mogelijkheden van het Info systeem.  Wanneer je klaar
		bent met deze Emacs-inleiding dan kun je de
		Emacs-Info-handleiding gebruiken als je primaire bron
		van informatie.


* MEER MOGELIJKHEDEN
--------------------

Je kunt meer over Emacs leren door haar handleiding te lezen.  Deze is
zowel als boek als in in Emacs beschikbaar (gebruik het Help menu of
tik C-h r).  Kijk bijvoorbeeld eens naar "completion", wat minder
tikwerk oplevert, of "dired" wat het omgaan met bestanden
vereenvoudigt.

"Completion" ("afmaken" in het Nederlands) is een manier om onnodig
tikwerk te voorkomen.  Als je bijvoorbeeld naar de "*Messages*" buffer
wilt omschakelen, dan kun je C-x b *M<Tab> tikken en dan zal Emacs de
rest van de buffernaam invullen voor zover dit mogelijk is gegeven wat
je al getikt had.  Completion staat beschreven in de node "Completion"
in de Emacs-Info-handleiding.

"Dired" toont je een lijst van bestanden in een directory (en als je
wilt ook subdirectories), waarmee je gemakkelijk bestanden kunt
bezoeken, van naam kunt veranderen, kunt wissen, of andere acties op
uit kunt voeren.  Informatie over Dired kun je vinden in de node
"Dired" van de Emacs-Info-handleiding.

De handleiding beschrijft ook vele andere Emacs-features.


* CONCLUSIE
-----------

Denk eraan dat je Emacs verlaat met C-x C-c.

De bedoeling van deze inleiding is dat ze begrijpelijk is voor alle
nieuwe Emacs-gebruikers.  Als je dus iets onduidelijks bent
tegengekomen, blijf dan niet zitten en maak jezelf geen verwijten.
Doe je beklag!


* KOPIËREN
-----------

(De Engelse versie van) deze inleiding is voorafgegaan door een lange
reeks van Emacs-inleidingen, die begon met de inleiding die Stuart
Cracraft schreef voor de originele Emacs.  Deze Nederlandse vertaling
is gemaakt door Pieter Schoenmakers <tiggr@tiggr.net> met
verbeteringen en correcties door Frederik Fouvry en Lute Kamstra.

(Wat nu volgt is een vertaling naar het Nederlands van de condities
voor gebruik en verspreiding van deze inleiding.  Deze vertaling is
niet gecontroleerd door een jurist.  Er kunnen derhalve geen rechten
aan de vertaling worden ontleend.  Na de vertaling volgt het Engelse
origineel.)

Deze versie van de inleiding is onderdeel van GNU Emacs.  Het valt
onder copyright.  Je mag deze inleiding verspreiden onder bepaalde
voorwaarden:

  Copyright (C) 1985, 1996, 1998, 2001-2012  Free Software Foundation, Inc.

  Dit bestand is onderdeel van GNU Emacs.

  GNU Emacs is vrije software: iedereen mag het verspreiden en/of
  modificeren onder de voorwaarden van de GNU General Public License
  ("algemene publieke licentie") zoals die gepubliceerd wordt door de
  Free Software Foundation, versie 3 of, zo je wilt, een latere
  versie.

  GNU Emacs wordt verspreid met de bedoeling dat het nuttig zal zijn,
  maar ZONDER ENIGE GARANTIE; zonder zelfs de impliciete garantie van
  verkoopbaarheid of geschiktheid voor een specifiek doel.  De GNU
  General Public License bevat meer informatie.

  Je zou de GNU General Public License moeten hebben ontvangen als
  onderdeel van GNU Emacs.  Als dat niet het geval is, ga naar
  www.gnu.org/licenses.

Lees het bestand COPYING en geef daarna kopieën van Emacs aan al je
vrienden.  Help bij het uitroeien van softwarebeschermingspolitiek
("eigendom") door vrije software te gebruiken, te schrijven en te
delen!

Engels origineel van de copyrightmelding en condities:

This version of the tutorial is a part of GNU Emacs.  It is copyrighted
and comes with permission to distribute copies on certain conditions:

  Copyright (C) 1985, 1996, 1998, 2001-2012 Free Software Foundation, Inc.

  This file is part of GNU Emacs.

  GNU Emacs is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  GNU Emacs is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

Please read the file COPYING and then do give copies of GNU Emacs to
your friends.  Help stamp out software obstructionism ("ownership") by
using, writing, and sharing free software!

;;; Local Variables:
;;;   coding: latin-1
;;; End:
