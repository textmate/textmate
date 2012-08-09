Emacs användarhandledning. I slutet finns kopieringsvillkoren.

Emacs-kommandon innebär ofta användning av kontrolltangenten (vanligen
märkt CTRL eller CTL) eller META-tangenten (på vissa tangentbord märkt
ALT eller EDIT). Vi använder här följande förkortningar:

 C-<chr> håll ner kontrolltangenten samtidigt som du skriver bokstaven
         <chr>. C-f betyder: håll ner kontrolltangenten och tryck f.
 M-<chr> håll ner META-tangenten samtidigt som du skriver <chr>. Om
         META-tangent saknas trycker du <ESC>, ESC-tangenten, släpper
         den och trycker sedan <chr>.

Viktigt: För att avsluta Emacs trycker du C-x C-c (två tecken).
För att avsluta kommandon som inte skrivits in fullt, tryck C-g.  
Tecknen ">>" i vänstermarginalen anger att du kan prova ett
kommando. Till exempel:
<<Tomma rader sätts in runt nästa rad när help-with-tutorial aktiveras>>
[Tomma rader av pedagogiska skäl. Texten fortsätter nedanför.]
>> Tryck C-v (View next screen) för att hoppa till nästa skärmbild.
        Prova nu. Håll ned kontrolltangenten och tryck v. Gör så i
        fortsättningen när du är färdig med en skärmbild.

Notera att det är ett överlapp på två rader när du byter från
skärmbild till skärmbild. Detta är för att behålla sammanhanget när du
bläddrar framåt i filen.

Det första du behöver veta är hur du manövrerar från plats till plats
i texten. Du har redan lärt dig hur du flyttar en skärmbild framåt,
med C-v. För att flytta dig en skärmbild bakåt trycker du M-v. (Håll
ned META-tangenten och tryck v eller tryck <ESC>v om du inte har
META-, EDIT- eller ALT-tangent.)

>> Prova att trycka M-v och C-v några gånger.


* SAMMANFATTNING
----------------

Följande kommandon är bra för att se hela skärmbilder:

        C-v     Flytta en skärmbild framåt.
        M-v     Flytta en skärmbild bakåt.
        C-l     Rita om skärmen och placera texten där markören står
                mitt på skärmbilden. (Det är KONTROLL-L, inte
                KONTROLL-1.)

>> Leta reda på markören och se vad som står där. Tryck sedan C-l.
   Hitta markören igen och notera att det är samma text som står kring
   markören nu, men nu mitt på skärmen. Om du trycker C-l igen så
   flyttas texten högst upp på skärmen. Tryck C-l igen och den flyttas
   ner till botten.

Du kan också använda PageUp och PageDn tangenterna, om din terminal
har dem, för att flytta en hel skärmbild åt gången, men du redigerar
effektivare om du använder C-v och M-v.


* GRUNDLÄGGANDE MARKÖRRÖRELSER
------------------------------

Att flytta sig från skärmbild till skärmbild kan vara bra, men hur
förflyttar man sig till en speciell plats på skärmen?

Det finns flera sätt att göra detta. Du kan använda piltangenterna,
men det är mer effektivt att ha händerna i standardläget och använda
kommandona C-p, C-b, C-f och C-n. Dessa tecken är likvärdiga med de
fyra piltangenterna. Så här:

			 Föregående rad, C-p
                                  :
                                  :
   Bakåt, C-b .... Nuvarande markörposition .... Framåt, C-f
                                  :
                                  :
                           Nästa rad, C-n

>> Flytta markören till linjen mitt i diagrammet genom att använda C-n
   och C-p. Använd sedan C-l för att centrera diagrammet på
   skärmbilden.

Detta är enklare att komma ihåg om du tänker på dessa förkortningar: P
för föregående (previous), N för nästa (next), B för bakåt (backward)
och F för framåt (forward). Du kommer att använda dessa grundläggande
kommandona hela tiden.

>> Gör några C-n så att du kommer ned till den här raden.

>> Flytta dig in i raden med hjälp av några C-f och sedan uppåt
   med några C-p. Lägg märke till vad C-p gör när markören står mitt
   på en rad.

Textrader är åtskilda med radslutstecken. Den sista raden i filen
avslutas också vanligtvis med ett radslut men Emacs kräver inte att
den gör det.

>> Prova med C-b i början av en rad. Detta gör att markören
   flyttas till slutet av den tidigare raden. Detta är för att den
   flyttar markören över radslutstecknet.

C-f flyttar också över radslut, precis som C-b.

>> Gör några fler C-b så att du får en känsla för var markören
   är. Tryck sedan några C-f tills du kommer till slutet av
   raden. Tryck ytterligare en C-f så att du flyttar markören till
   nästa rad.

När du flyttar markören förbi toppen eller botten av skärmbilden
kommer texten utanför skärmen att komma fram. Detta kallas "rullning"
och gör det möjligt för Emacs att flytta markören utan att den
försvinner ut ur skärmbilden.

>> Prova att flytta markören förbi skärmbildens nederkant med hjälp av
   C-n och se vad som händer.

Om det går för sakta att flytta markören ett tecken i taget kan du
flytta den ett ord. M-f flyttar markören ett ord framåt och M-b
flyttar den ett ord bakåt.

>> Prova några M-f och M-b.

Om markören står mitt i ett ord kommer M-f att flytta markören till
slutet av ordet. Om du står mitt emellan två ord kommer M-f att flytta
markören till slutet av nästa ord. M-b fungerar på samma sätt men i
motsatt riktning.

>> Tryck M-f och M-b några gånger och skifta markörposition med några
   C-f och C-b så att du ser hur M-f och M-b uppför sig vid olika
   placeringar av markören både i och mellan ord.

Lägg märke till likheten mellan C-f och C-b å ena sidan och M-f och
M-b å den andra. Ofta används META-kommandon till språkrelaterade
operationer (ord, stycken, avsnitt), medan kontrollkommandon används
till grundläggande operationer som inte beror av vad man redigerar
(bokstäver, rader, etc.).

Denna likhet finns också mellan rader och stycken: C-a och C-e flyttar
markören till början av en rad eller till slutet av en rad, medan M-a
och M-e flyttar den till början respektive slutet av ett stycke.

>> Prova några C-a och sedan några C-e.
   Prova också några M-a och sedan några M-e.

Se hur efterföljande C-a efter varandra inte gör något, medan flera
M-a fortsätter att flytta markören till nästa stycke. Även om detta
inte verkar självklart är det ganska naturligt.

Platsen där markören är i texten kallas också för "arbetspunkt"
(point). Eller omskrivet: Markören visar på skärmen var arbetspunkten
är i texten.

Här är en kort sammanfattning av de enklaste markörförflyttnings-
kommandona, inklusive ord- och styckesförflyttningskommandon:

        C-f     Flytta markören ett steg framåt
        C-b     Flytta markören ett steg bakåt

        M-f     Flytta markören ett ord framåt
        M-b     Flytta markören ett ord bakåt

        C-n     Flytta markören till nästa rad
        C-p     Flytta markören till föregående rad

        C-a     Flytta markören till början av raden
        C-e     Flytta markören till slutet av raden

        M-a     Flytta markören till början av meningen
        M-e     Flytta markören till slutet av meningen

>> Prova alla dessa kommandon några gånger för tränings skull.
   Dessa är de kommandon som används mest.

Två andra viktiga markörrörelsekommandon är M-< (META mindre-än), som
flyttar markören till början av texten, och M-> (META större-än), som
flyttar den till slutet av texten.

På en del tangentbord är "<" placerad över komma, så att man måste
använda skift för att få fram den. På dessa tangentbord måste man
också använda skift för att skriva M-<. Utan skifttangenten skulle det
bli M-komma.

>> Prova M-< nu för att flytta markören till början av vägledningen.
   Använd sedan C-v för att flytta markören tillbaka hit igen.

>> Prova också M-> för att flytta markören till slutet av vägledningen.
   Använd sedan M-v för att flytta markören tillbaka hit igen.

Du kan också flytta markören med hjälp av piltangenterna, om
terminalen har piltangenter. Vi föreslår att du lär dig C-b, C-f, C-n
och C-p av tre skäl. För det första kommer de att fungera på alla
slags terminaler. För det andra kommer du att finna, när du har fått
lite träning i att använda Emacs, att det går mycket snabbare att
använda kontrollfunktionerna än piltangenterna (för att du undviker
att ändra fingersättningen). Den tredje anledningen är att när man har
lärt sig att använda kontrolltangenten blir det lättare att lära sig
de mer avancerade kontrollfunktionerna.

De flesta kommandon i Emacs tar ett numeriskt argument och för de
flesta kommandon leder detta till att de repeteras. Ett numeriskt
argument anges genom att du skriver C-u och sedan talet, innan du
skriver kommandot. Om du har en META- (eller EDIT- eller ALT-) tangent
så finns det ett annat alternativ för att ge numeriska argument: skriv
talet medan du håller ned META-tangenten. Vi föreslår att du använder
C-u för det fungerar på alla slags terminaler. Det numeriska
argumentet kallas också för "prefixargument" eftersom det skrivs före
kommandot.

Till exempel: C-u 8 C-f flyttar markören åtta steg framåt.

>> Prova C-n eller C-p med ett numeriskt argument så att du
   kommer så nära den här raden som möjligt med ett enda kommando.

De flesta kommandon använder det numeriska argumentet för ett
repeterat utförande men det finns kommandon som använder det
annorlunda. Flera kommandon, men inga av dem du lärt dig hittills,
använder det som en flagga. Med ett prefixargument, och oberoende av
dess värde, gör kommandot något annat.

C-v och M-v finns med bland dessa undantag. Om man ger ett argument
till ett av dessa kommandon kommer skärmbilden flytta sig upp eller
ned så många rader som argumentet anger, istället för så många
skärmbilder. Till exempel kommer C-u 8 C-v flytta skärmbilden 8 rader
uppåt.

>> Prova C-u 8 C-v nu.

Detta borde ha flyttat skärmbilden 8 rader uppåt. Om du önskar flytta
tillbaka igen är det bara att ge samma argument till M-v.

Om du använder ett fönstersystem, som X eller MS-Windows, finns det
troligen ett rektangulärt område på sidan av Emacs-fönstret, en så
kallad rullningslist. Genom att klicka i den med musen kan du rulla
texten.

Om din mus har ett rullningshjul kan även den användas för att rulla
texten.

* OM EMACS SLUTAR SVARA
-----------------------

Om Emacs slutar att reagera på kommandon kan du lugnt stoppa dem genom
att trycka C-g. Du kan också använda C-g för att stoppa ett kommando
som tar för lång tid att utföra.

Det är också möjligt att använda C-g för att avbryta ett numeriskt
argument eller början på ett kommando som du inte önskar att utföra.

>> Skriv C-u 100 för att ge ett numeriskt argument på 100 och tryck
   C-g. Tryck nu C-f. Markören skall nu flytta sig bara ett steg, för att
   du avbröt argumentet med C-g.

Om du av misstag slår <ESC> blir du kvitt detta med ett C-g.


* SPÄRRADE KOMMANDON
--------------------

En del Emacs-kommandon är "spärrade" så att nybörjare inte skall
använda dem av misstag.

Om du provar ett av dessa spärrade kommandon kommer Emacs ge ett
meddelande som berättar vilket kommando det är och kommer att fråga om
du verkligen vill fortsätta och utföra detta kommando.

Om du verkligen önskar att utföra kommandot skriver du <SPC>,
(mellanslagstangenten) som svar på frågan. Normalt, om du inte önskar
att utföra detta kommando, svarar du "n" på frågan.

>> Skriv C-x C-l (som är ett spärrat kommando).
   Skriv n som svar på frågan.


* FÖNSTER
---------

Emacs kan ha flera "fönster" där varje kan visa sin egen text. Vi
kommer förklara senare hur man använder flera fönster. Här skall vi
förklara hur man blir av med extra fönster för att komma tillbaka till
det grundläggande läget med endast ett fönster. Det är enkelt:

        C-x 1      Ett fönster (dvs. ta bort alla andra fönster).

Det är KONTROLL-x följt av siffran 1. C-x 1 utvidgar fönstret där
markören står så att det fyller hela skärmbilden. Alla andra fönster
tas bort.

>> Flytta markören till den här raden och tryck C-u 0 C-l.
>> Tryck C-h k C-f.
   Se hur det här fönstret krymper samtidigt som ett nytt uppträder
   för att visa dokumentationen av C-f-kommandot.

>> Slå C-x 1 och se hur dokumentationsfönstret nu försvinner.

Kommandot skiljer sig lite från andra kommandon du har lärt dig
eftersom det består av två tecken. Det startar med tecknet
KONTROLL-x. Det är faktisk många kommandon som startar med KONTROLL-x
och många av dem har med filer, skärmbilder och liknande saker att
göra. Dessa kommandon är två, tre eller fyra tecken långa.


* SKRIVA OCH TA BORT TEXT
-------------------------

Om du önskar att sätta in text är det bara att skriva in texten. 
Vanliga tecken, som A, 7, *, etc., sätts in direkt när du skriver dem. 
Tryck på <Return> för att sätta in en radbrytning. (Det är den tangent
på tangentbordet som ibland är märkt med "Enter")

För att radera <DEL> tecknet omedelbart före aktuell markörposition,
skriv <DEL>. Det är tangenten på tangentbordet som vanligtvis är
markerad med "Backspace" -- det är samma tangent som du normal
använder för att radera det sist inmatade tecknet utanför Emacs. 

Det kan finnas en annan tangent på ditt tangentbordet som är märkt med
"Delete", men det är inte den vi menar med <DEL>.

>>  Gör detta nu: Skriv in några tecken och ta bort dem genom att
    använda <DEL>. Var inte rädd för att skriva i den här filen, du
    kommer inte att kunna förändra originalet till vägledningen. Detta
    är bara en lokal kopia.

När en rad blir för lång för att rymmas på en skärmbredd så fortsätter
den på raden under. Om du använder ett fönstersystem, visas små böjda
pilar i det lilla utrymmet på bägge sidor om textmassan (i vänster och
höger marginal) för att ange var en rad fortsätter, Om du använder
en textterminal anges med ett bakstreck ("\") i kolumnen längst till
höger att raden fortsätter.

>>  Skriv in lite text så att du kommer till slutet av raden och
    fortsätt att skriva lite till. Du kommer då att se hur
    fortsättningstecknet ser ut.

>>  Använd <DEL> för att radera texten tills raden ryms på en
    skärmbredd igen. Fortsättningstecknet kommer då att försvinna.

Du kan radera radbrytning precis som andra tecken. Genom att radera
radbrytningen mellan två rader slås dessa samman till en. Om
resultatet av denna sammanslagning blir för stor för att passa inom en
skärmbredd, så kommer den att visas med ett fortsättningstecken.

>> Flytta markören till början av en rad och tryck <DEL>.
   Detta kommer att klistra ihop raden med raden över.

>> Tryck <Return> för att sätta in radbrytningen du tog bort.

Tänk på att de flesta Emacs-kommandon kan ta numeriska argument. Detta
gäller också texttecken. Genom att repetera ett texttecken kommer det
skrivas flera gånger.

>> Prova det nu: Skriv C-u 8 * för att sätta in ********.

Du har nu lärt dig de mest grundläggande sätten att skriva något i
Emacs och att rätta fel. Du kan radera ord och rader också. Här är en
översikt över kommandon för radering:

        <DEL>        Raderar tecknet som står precis före markören
        C-d          Raderar tecknet som står precis under markören

        M-<DEL>      Raderar ordet precis före markören
        M-d          Raderar ordet precis efter markören

        C-k          Raderar från markören till slutet av raden
        M-k          Raderar till slutet av stycket

Lägg märke till att <DEL> och C-d kontra M-<DEL> och M-d följer
mönstret som började med C-f och M-f. (<DEL> är inte precis ett
kontrolltecken men låt oss inte bry oss om det.) C-k och M-k fungerar
på liknande sätt som C-e och M-e när det gäller rader respektive
meningar.

Du kan också ta bort en del av en texten med hjälp av följande
allmänna metod. Flytta till ena änden av det område du vill ta bort
och tryck C-<SPC>. (<SPC> är mellanslagstangenten.) Flytta sedan till
andra änden av området du vill ta bort. När du gör det markerar Emacs
texten mellan markören och den plats där du tryckte C-<SPC>. Slutligen,
tryck C-w. Detta tar bort texten mellan de två positionerna.

>> Flytta markören till bokstaven D i föregående stycke.
>> Tryck C-<SPC>. Emacs skall nu visa meddelandet "Mark set"
   längst ner på skärmen.
>> Flytta markören till bokstaven o i ordet metod på andra raden i
   stycket.
>> Tryck C-w. Detta tar bort texten från och med D fram till just före
   o.

Skillnaden mellan att "ta bort" (killing) och "radera" (deleting) text
är att "borttagen" text kan sättas tillbaka (var som helst), medan
raderad text inte kan det på det sättet. (Du kan dock ångra en
radering--se nedan.) Återinsättning av borttagen text kallas
"återhämtning" (yanking).  Generellt kan man säga att kommandon som
tar bort fler än ett tecken sparar undan texten (så att den kan
återhämtas) medan kommandon som bara raderar ett tecken, eller bara
raderar tomma rader och mellanrum inte sparar någonting (och den
texten kan alltså inte återhämtas). <DEL> och C-d raderar i det enkla
fallet utan argument. Med argument så tar de bort i stället.

>> Flytta markören till början av en rad som inte är tom.
   Tryck C-k för att ta bort texten på raden.
>> Tryck C-k en gång till. Du kommer nu se att den raderar den tomma
   raden som var kvar.

Lägg märke till att ett enstaka C-k bara raderar texten på raden och
att det andra C-k raderar själva raden och flyttar upp texten på raden
under ett steg. C-k hanterar numeriska argument lite speciellt. Den
raderar så många rader OCH innehållet i dem. Detta är alltså inte bara
en repetition av kommandot. C-u 2 C-k raderar två rader samt de tomma
raderna, medan C-k två gånger inte kommer att göra det.

Att sätta in borttagen text kallas att "återhämta" den (yanking).
(Tänk på det som att du rycker, eller drar, tillbaka någon text som
tagits bort.) Du kan antingen hämta tillbaka borttagen text till samma
plats som där den blev borttagen, eller så kan du sätta in den på en
annan plats i texten du redigerar eller till och med i en helt annan
fil. Du kan också hämta tillbaka samma text flera gånger så att du får
flera kopior av den. Några andra textredigerare kallar "ta bort" och
"återhämta" att "klippa ut" respektive "klistra in" (Se ordlistan i
Emacs-manualen)

Kommandot för att hämta tillbaka text är C-y. Kommandot hämtar
tillbaka den sist borttagna texten och placerar den där markören är.

>> Prova: Gör C-y för att få tillbaka texten.

Om du gör flera C-k i rad så kommer all bortagen text att sparas
samlat så att ett C-y återhämtar alla raderna på en gång.

>> Prova detta. Tryck C-k ett par gånger.

Och hämta så tillbaka igen:

>> Tryck C-y. Flytta markören några rader ned och tryck C-y igen.
   Så kopierar man text.

Men vad gör du om du har en text du önskar att hämta tillbaka men du
har redan tagit bort något nytt? C-y skulle hämta tillbaka den senaste
texten som blev borttagen men tidigare bortagen text är inte
förlorad. Du kan få tillbaka den med kommandot M-y. Efter att du har
använt C-y för att hämta tillbaka den sist borttagna texten kommer M-y
ersätta denna text med tidigare borttagen text. Genom att göra M-y om
och om igen hämtas allt tidigare borttagen text tillbaka. När du har
nått den önskade texten behöver du inte göra något ytterligare för att
behålla den. Fortsätt bara med din redigeringen och lämna den
återtagna texten där den är.

Om du gör M-y tillräckligt många gånger kommer du att komma tillbaka
till startpunkten (texten som sist blev borttagen).

>> Ta bort en rad, flytta markören till en ny rad och ta bort även
   denna rad. Använd C-y för att hämta tillbaka den sista raden. Tryck
   M-y för att byta den mot den tidigare borttagna raden. Tryck flera
   M-y och se vad du får. Fortsätt med detta tills du får tillbaka den
   första raden igen och sedan några gånger till. Om du vill kan du
   prova med positiva och negativa argument till M-y.


* ÅNGRA
-------

Om du gör en förändring i texten och sedan ångrar dig, så kan du
upphäva ändringen med ångra-kommandot C-/.

Normalt kommer C-/ upphäva förändringen som gjordes av det sist
utförda kommandot. Om du repeterar C-/ flera gånger kommer varje
repetition upphäva ett kommando till.

Det finns två undantag. Kommandon som inte förändrar texten räknas
inte (detta inkluderar markörförflyttningar och bläddringskommandon),
och inskrivna enkelbokstäver blir vanligtvis grupperade i grupper om
upp till 20 tecken. Detta är för att reducera antalet C-/ som behövs
för att ångra inskriven text.

>> Ta bort den här raden med C-k, hämta sedan tillbaka den med C-/.

C-_ är ett alternativt ångra-kommandot. Den fungerar exakt på samma
sätt som C-/. På vissa textterminaler skickar C-/ faktiskt C-_ till
Emacs. Även C-x u fungerar precis som C-/, men är inte lika enkelt att
skriva.

Ett numeriskt argument till C-/, C-_ eller C-x u medför upprepning.

Du kan ångra radering av text precis på samma sätt som du kan ångra
att du tagit bort text. Skillnaden mellan att ta bort och att radera
någonting påverkar endast om du kan hämta tillbaka det med C-y. För
ångerfunktionen spelar det ingen roll hur texten försvunnit.


* FILER
-------

För att texten du har förändrat skall sparas permanent måste du lägga
den i en fil. Om du inte gör det kommer texten att försvinna när du
avslutar Emacs. Du lägger texten i en fil genom att först finna (find)
denna fil. Detta kallas också för att besöka filen (visit).

Att finna en fil innebär att du ser filens innehåll i Emacs. På många
sätt är det som om du förändrar själva filen men förändringen du gör
kommer inte bli permanent förrän filen sparas (save). Detta är för att
undvika att halvförändrade filer sparas när du inte vill det. Till och
med när du sparar filen kommer Emacs att behålla originalet under ett
nytt namn, som backup, ifall du senare ångrar alltihop.

Om du tittar nästan längst ner på skärmbilden så kommer du se en rad
som börjar med minustecken, och som startar med "--:-- TUTORIAL.sv"
eller något snarlikt. Denna del av skärmbilden visar normalt namnet på
filen du besöker. Just nu besöker du din personlig kopia av
vägledningen till Emacs, vilken heter "TUTORIAL.sv". Vilken fil du än
är inne i så kommer filnamnet stå där.

En annan sak med kommandot för att finna filer är att du måste ange
vilket filnamn du önskar. Vi säger att kommandot "läser ett
argument". I detta fall är argumentet namnet på filen. Efter att du
gett kommandot

        C-x C-f   Finn en fil

kommer Emacs fråga efter ett filnamn. Filnamnet du skriver syns på den
nedersta raden i skärmbilden. Denna sista rad kallas minibuffert när
den används på det här sättet. Du kan använda vanliga Emacs-kommandon
för att förändra filnamnet.

När du skriver in filnamnet, eller något annat i minibufferten, kan du
avbryta med kommandot C-g.

>> Skriv C-x C-f och så C-g. Detta avbryter minibufferten och
   avbryter också C-x C-f kommandot som använde minibufferten. Så att
   du inte finner någon fil.

När du är färdig med att skriva filnamnet trycker du <Return> för att
utföra kommandot. Minibufferten försvinner och C-x C-f kommandot börja
leta efter filen.

Filinnehållet visas nu upp på skärmen och du kan börja redigera
innehållet. När du vill spara filen kan du använda detta kommando

        C-x C-s   Spara fil

Detta sparar texten på skärmen till filen. Första gången detta görs
kommer Emacs att ge originalfilen ett nytt namn så att den inte går
förlorad. Det nya filnamnet bildas genom att lägga till ett "~" i
slutet av det ursprungliga filnamnet.

När lagringen är utförd kommer Emacs skriva ut namnet på filen som
blev sparad. Du bör spara ofta så att du inte förlorar så mycket om
systemet kraschar. (Se kapitlet om sparautomatik nedan.)

>> Skriv C-x C-s TUTORIAL.sv <Return>.
   Detta sparar den här handledningen i en fil med namnet TUTORIAL
   och "Wrote ...TUTORIAL.sv" skrivs ut nederst på skärmbilden.

Du kan finna en existerande fil, antingen för att förändra den eller
för att titta på den. Du kan också finna en fil som inte existerar.
Det är så man skapar nya filer med Emacs: finn filen, som är tom till
att börja med, och sätt igång med att skriva texten som skall in i
filen. Först när du sparar filen kommer Emacs att verkligen skapa
filen med den text du har skrivit. Från och med detta editerar du en
fil som existerar.


* BUFFERTAR
-----------

Om du finner en ny fil med C-x C-f kommer den första filen fortsätta
att vara öppen i Emacs. Du kan byta tillbaka till den genom att finna
den på nytt med C-x C-f. På så sätt kan du ha ett stort antal filer
öppna i Emacs.

Emacs sparar texten för varje fil i ett objekt kallat "buffert". När
du finner en ny fil skapas en ny buffert i Emacs. För att se en lista
över existerande buffertar i Emacs kan du skriva

        C-x C-b      Listning av buffertar.

>> Prova C-x C-b nu.

Se hur varje buffert har ett namn och att de också kan ha namnet på
den fil som innehållet kommer från. Vilken text du än ser i ett
Emacs-fönster så tillhör den alltid en buffert.

>> Skriv C-x 1 för att bli kvitt buffertlistan.

När du har flera buffertar så är bara en av dem "gällande" åt gången.
Det är den buffert du redigerar. Om du vill redigera en annan buffert
så måste du byta till den. Om du vill byta till en buffert som
motsvarar en fil kan du göra det genom att besöka den igen med C-x
C-f. Det finns dock ett enklare sätt: använd C-x b kommandot. I det
kommandot anger du buffertens namn.

>> Skapa en fil med namnet "foo" genom att trycka C-x C-f foo <Return>.
   Skriv sedan C-x b TUTORIAL.sv <Return> för att komma tillbaka till
   den här handledningen.

Mestadels är buffertens namn densamma som filens namn (utan
katalogdel.) Det är dock inte alltid så. Bufferlistan du skapar med
C-x C-b visar alltid namnen på varje buffert.

All text du ser i ett Emacsfönster är alltid del av någon buffert. En
del buffertar är inte knutna till någon fil, till exempel bufferten
"*Buffer List*". Det är den buffert som innehåller buffertlistan som
skapades med C-x C-b. Bufferten "*Messages*" motsvarar inte heller
någon fil. Den innehåller de meddelanden som visas på den nedersta
raden i Emacs sessionen.

>> Skriv C-x b *Messages* <Return> för att se meddelandebufferten.
   Skriv sedan C-x b TUTORIAL.sv <Return> för att återgå till den här
   handledningen.

Om du ändrar texten till en fil och sedan öppnar en ny fil, så kommer
inte den första filen sparas. Förändringen ligger kvar i
bufferten. Skapande och redigering av den nya filen påverkar inte den
första filens buffert. Detta kan vara bra men betyder också att du
behöver ett lämpligt sätt att spara den första filens buffert. Det är
omständligt att flytta tillbaka till den tidigare bufferten med C-x
C-f för att sedan spara filen med C-x C-s. Därför finns kommandot

        C-x s      Spara buffertar

C-x s frågar för varje buffert med ändringar, som inte sparats, om du
vill spara eller ej.

>> Sätt in en rad med text och spara med C-x s
   Du skall nu få frågan om du önskar spara bufferten
   TUTORIAL.sv. Svara ja på frågan genom att trycka "y" (yes).


* UTVIDGNING AV KOMMANDOMÄNGDEN
-------------------------------

Det finns mycket fler Emacs-kommandon än antalet KONTROLL- eller
META-tangenter. För att komma förbi denna begränsning har Emacs ett
"X"- (eXtend) kommando. Detta finns i två varianter:

        C-x     Tecken-utvidgning. Följs av ett tecken.
        M-x     Namngiven kommandoutvidgning. Följs av ett
                kommandonamn.

Detta är kommandon som är bra att ha men används mer sällan än de
kommandon du redan har lärt dig. Du har redan sett några av dem, C-x
C-f för finn, och C-x C-s för spara. Ett annat exempel är kommandot
för att avsluta Emacs som är C-x C-c. Var inte rädd för att förlora
förändringar du har gjort. C-x C-c erbjuder dig att spara förändringar
innan Emacs avslutas.

Om du använder ett fönstersystem behöver du inte något speciellt
kommando för att byta till ett annat program. Du kan göra det med
musen eller med ett kommando till fönsterhanteraren. Men om du
använder en textterminal, som bara kan visa ett program åt gången, så
måste du avbryta Emacs för att flytta till ett annat program.

C-z är kommandot för att avsluta Emacs *tillfälligt* så att du kan
återvända till samma Emacs senare. När Emacs körs från en textterminal
så avbryts Emacs med C-z, dvs du återgår till kommandoskalet utan att
Emacsprocessen förstörs. I de flesta vanliga kommandoskalen så kan man
återgå till Emacs med kommandot 'fg' eller med '%emacs'.

C-x C-c används när du skall avsluta Emacs. Det är klokt att avsluta
Emacs om den har startats av ett mail-program eller andra
applikationer.

Det finns många C-x kommandon. Här är en lista över de du har lärt dig
hittills:

	C-x C-f		Finn fil
	C-x C-s		Spara fil
	C-x s		Spara några buffertar
	C-x C-b		Lista buffertar
	C-x b		Byt buffert
	C-x C-c		Avsluta Emacs
	C-x 1		Ta bort alla utom ett fönster
	C-x u		Ångra

Namngivna utvidgade kommandon är kommandon som används mycket sällan
eller bara i vissa lägen. Ett exempel på ett sådant kommando är
replace-string, som globalt ersätter en teckensträng med en annan. När
du skriver M-x kommer Emacs visa en prompt nederst i skärmbilden med
M-x där du skall skriva in kommandot du önskar att köra, i det här
fallet "replace-string". Det är bara att skriva "repl s<TAB>" och
Emacs kommer då att fylla i kommandonamnet. (<TAB> är
tabulatortangenten, som vanligtvis finns över CapsLock- eller
skifttangenten nära den vänstra kanten på tangentbordet.) Kör
kommandot med <Return>.

Kommandot replace-string kräver två argument, teckensträngen som skall
ersättas och teckensträngen som den skall ersättas med. Du måste
avsluta bägge argumenten med <Return>.

>> Flytta markören till den blanka raden två rader under denna rad.
   Skriv M-x repl s<Return>förändrad<Return>ändrad<Return>.

   Lägg märke till hur den här raden har blivit förändrad. Du har
   ersatt ordet f-ö-r-ä-n-d-r-a-d med "ändrad" på alla platser där
   ordet förekom, från markören och nedåt.


* SPARAUTOMATIK
---------------

När du har gjort förändringar i en fil men inte sparat den, så kommer
ändringarna att gå förlorade om maskinen kraschar. Som ett skydd mot
detta sparar Emacs periodiskt ändringarna i en autosparfil för varje
fil du redigerar. Denna fil har ett # i början och slutet av
filnamnet. Om du till exempel har en fil med namnet "hej.c" så kommer
namnet på autosparfilen bli "#hej.c#". När du sparar filen på vanlig
sätt kommer Emacs radera autosparfilen.

Om maskinen kraschar kan du återfå dina automatiskt sparade ändringar
genom att finna filen på vanlig sätt (filen du redigerade, inte
autosparfilen) och skriva M-x recover-file<Return>. När Emacs vill ha
bekräftelse svarar du yes<Return> för att återställa filen.


* EKOOMRÅDE
-----------

Om Emacs ser att du skriver kommandon långsamt så kommer de att visas
på den nedersta raden i skärmbilden i ett område som kallas
"ekoområde" (echo area). Detta område innehåller den nedersta raden på
skärmbilden.


* LÄGESRADEN
------------

Raden precis över ekoområdet kallas "lägesrad" (mode line). Den ser
ungefär ut så här:

--:**  TUTORIAL       63% L749    (Fundamental)-----------------------

Raden innehåller information om Emacs och texten du redigerar.

Du vet redan vad filnamnet betyder, det är den fil du har funnit. NN%
visar den aktuella positionen i texten, dvs. NN procent av texten
befinner sig över toppen av skärmbilden. Om toppen av filen är i
skärmbilden kommer det stå "Top" istället för " 0%" och om slutet av
filen är i skärmbilden kommer det stå "Bot". Om du ser på en fil där
hela texten passar in på en sida kommer det stå "All".

Bokstaven L följd av siffror anger positionen på ett annat
sätt. Siffrorna visar vilken rad som markören befinner sig på.

Stjärnorna nära början av raden visar att det har skett förändringar i
filen sedan den sist blev sparad. När du precis har öppnat en fil
kommer det inte stå något här, bara minustecken.

Den del av lägesraden som står inom parentes visar vilket
redigeringsläge (mode) du använder. Standardläget är "Fundamental",
som du använder nu. Det är ett exempel på ett huvudläge (major mode).

Emacs har många olika huvudlägen. Några av dem är gjorda för
redigering av olika programmeringsspråk eller typer av text, till
exempel Lisp mode och Text mode. Det kan bara vara ett huvudläge åt
gången och lägesnamnet står alltid där det står Fundamental nu.

Varje huvudläge gör att en del kommandon uppför sig annorlunda. Det
finns till exempel kommandon för att sätta in kommentarer i programkod
och eftersom varje programmeringsspråk har sitt sätt att skriva
kommentarer på så måste de olika huvudlägena sätta in dessa
kommentarer på olika sätt. Varje huvudläge namnger ett utvidgat
kommando som används för att byte till det läget. Till exempel är M-x
fundamental-mode kommandot för att byta till huvudläget Fundamental.

Om du skall redigera text, såsom den här filen, bör du troligen
använda Text-läge.

>> Skriv M-x text-mode <Return>.

Inget av kommandona du har lärt dig hittills förändrar Emacs i någon
högre grad. Men lägg märke till att M-f och M-b nu behandlar
apostrofer som en del av ord. Tidigare, i Fundamental mode, behandlade
M-f och M-b apostrofer som ordavskiljare.

Varje huvudläge gör vanligtvis små förändringar som denna och de flesta
kommandon gör samma sak i varje huvudläge, de fungerar bara lite
annorlunda.

För att få fram dokumentationen för det läge du är i nu kan du skriva
C-h m.

>> Använd C-l C-l för att få denna rad överst på skärmbilden.
>> Skriv C-h m och se hur Text-läget skiljer sig från
   Fundamental-läget.
>> Tryck C-x 1 för att ta bort dokumentationen från skärmbilden.

Huvudläge kallas så för att det även finns sidolägen (minor mode).
Ett sidoläge ersätter inte ett huvudläge, utan modifierar det. Varje
sidoläge kan stängas av och på oberoende av andra sidolägen och
oberoende av huvudläget. Därför kan du använda ett sidoläge, en
kombination av flera sidolägen eller inget sidoläge alls.

Ett bra sidoläge, speciellt för redigering av text, är
radbrytningsläget (auto-fill-mode). När detta läge är på bryter Emacs
rader mellan ord automatisk när du skriver in text så att en rad blir
för lång.

Du kan slå på radbrytningsläget genom att skriva M-x auto-fill-mode
<Return>. När läget är påslaget kan du slå av det igen genom att
upprepa M-x auto-fill-mode <Return>. Om läget är avslaget slår
kommandot på det och vice versa. Vi säger att kommandot "växlar
läget".

>> Skriv M-x auto-fill-mode <Return> nu. Skriv så in en rad med
   "asdf " tills raden delar sig. Du måste sätta in blanktecken, för
   Auto Fill bryter bara raden mellan ord.

Marginalen är vanligtvis satt till 70 tecken men du kan ändra detta
genom att använda kommandot C-x f. Antalet tecken ges till kommandot
genom ett numeriskt argument.

>> Skriv C-x f med ett argument på 20. (C-u 2 0 C-x f). Skriv sedan in
   någon text och lägg märke till att Emacs bryter rader som är längre
   än 20 tecken. Sätt tillbaka marginalen till 70 tecken igen, genom
   att använda C-x f en gång till.

Om du gör förändringar mitt i en rad så kommer inte sidoläget Auto
Fill att kunna omformattera raderna för dig.
För att göra detta kan du trycka M-q med markören inne i det avsnittet
du önskar att omformatera.

>> Flytta markören in i föregående stycke och tryck M-q.


* SÖKNING
---------

Emacs kan söka efter textsträngar (en "sträng" är en grupp med
sammanhängande bokstäver) antingen framåt eller bakåt i texten. När du
söker efter text kommer markören att flytta sig till nästa plats där
teckensträngen uppträder.

Sökmetoden i Emacs är inkrementell. Detta betyder att sökandet fortgår
medan du skriver in teckensträngen du skall söka efter.

Kommandot för att inleda en sökning är C-s för att söka framåt och C-r
för att söka bakåt. MEN VÄNTA! Prova dem inte än.

När du skriver C-s kommer du lägga märke till att texten "I-search"
dyker upp i eko-området. Detta säger dig att Emacs är inne i sidoläget
inkrementell sökning och väntar på att du skall skriva in det du letar
efter. <Return> avslutar sökandet.

>> Skriv nu C-s för att starta en sökning. Skriv nu långsamt, en
   bokstav i taget, ordet 'markör', och gör en paus efter varje gång
   du skriver en bokstav så att du ser vad som sker med markören. Nu
   har du sökt efter ordet "markör" en gång.
>> Skriv C-s en gång till för att söka efter nästa förekomst av ordet
   "markör".
>> Tryck nu på <DEL> fyra gånger och se hur markören flyttar sig
>> Tryck <Return> för att avsluta sökandet.

Såg du vad som hände? Under inkrementell sökning försöker Emacs att gå
till den första förekomsten av texten som du har skrivit så långt, och
markerar träffen så att du ser var den är. För att gå till nästa
förekomst av ordet 'markör' är det bara att trycka C-s en gång till.
Om det inte finns flera förekomster kommer Emacs att pipa och meddela
att sökandet har misslyckats. C-g avbryter också sökandet.

Om du är inne i en inkrementell sökning och trycker <DEL> kommer den
sökningen att återgå till en tidigare plats. Om du skriver <DEL>
precis efter att du skrivit C-s för att gå till nästa förekomst av
söksträngen, kommer <DEL> att flytta markören tillbaka till en
tidigare förekomst. Om det inte finns några tidigare förekomster så
raderar <DEL> sista tecknet i söksträngen. Om du till exempel skriver
"m" för att söka efter den första förekomsten av "m", och sedan
trycker "a" så kommer markören flytta sig till första förekomsten av
"ma". Tryck nu <DEL>. Detta avlägsnar "a" från söksträngen, och
markören flyttar sig tillbaka till den första förekomsten av "m".

Om du är mitt i en sökning och trycker ett kontroll- eller meta-tecken
så avbryts sökandet. Undantag är tecken som används under sökningen,
så som C-s och C-r.

C-s startar en sökning som letar efter varje förekomst av söksträngen
EFTER markörspositionen. Om du skall söka efter en sträng tidigare i
texten måste du använda C-r. Allt vi har sagt om C-s gäller också för
C-r, bortsett från att riktningen på sökningen är den omvända.


* FLERA FÖNSTER
---------------

En av egenskaperna hos Emacs är att den kan visa mera än en buffert på
skärmen samtidig. (Notera att Emacs använder termen "ramar"
(frames), som beskrivs i nästa kapitel, för det som en del andra
program kallar för "fönster" (windows). Emacs-manualen innehåller en
ordlista över Emacs-termer.

>> Flytta markören till den här raden och tryck C-l C-l.

>> Skriv nu C-x 2, som leder till att skärmen delas i två
   fönster. Bägge fönstren visar den här vägledningen. 
   Redigeringsmarkören stannar i det övre fönstret.

>> Skriv C-M-v för att rulla det nedre fönstret.
   (Om du inte har META-tangenten trycker du <ESC> C-v.)

>> Skriv C-x o ("o" för "other") för att flytta markören till det
   nedre fönstret.
>> Använd C-v och M-v i det nedre fönstret för att flytta upp
   och ned i texten. Fortsätt att läsa den här texten i det övre
   fönstret.

>> Skriv C-x o igen för att flytta markören tillbaka till det övre
   fönstret. Markören i det övre fönstret står på samma plats som det
   gjorde när du lämnade det.

Du kan fortsätta att använda C-x o för att byta mellan de två
fönstren. Det valda fönstret, där de flesta redigeringarna äger rum, är
det med den tydligaste markören, som blinkar när du inte skriver. De
andra fönstren har sin egen markörposition. Om du kör Emacs under ett
fönstersystem, ritas dessa markörer som en tom ruta som inte blinkar..

Kommandot C-M-v är bra när du redigerar text i ett fönster och
använder det andra fönstret för referenser. Utan att lämna det valda
fönstret du kan använda C-M-v för att rulla det andra fönstret.

C-M-v är ett exempel på en KONTROLL-META-kombination. Om du har META-
eller Alt-tangenten håller du både KONTROLL och META nedtryckt
samtidigt som du trycker v. Det har ingen betydelse vilken av
tangenterna KONTROLL och META som trycks först, för bägge fungerar så
att de "modifierar" de andra tangenterna du trycker.

Om du inte har META-tangenten och använder <ESC> istället är
ordningsföljden viktig. Du måste trycka <ESC> följt av KONTROLL-v,
KONTROLL-<ESC> v fungerar inte. Det är för att <ESC> är ett tecken i
sig och inte en äkta "modifierare".

>> Skriv C-x 1 i det övre fönstret för att bli kvitt det nedre
   fönstret.

Om du hade skrivit C-x 1 i det nedre fönstret skulle det övre ha
försvunnit. Tänk på detta kommando som "Behåll bara ett fönster, det
som markören står i."

Du måste inte ha samma buffert i bägge fönstren. Du kan använda C-x
C-f för att finna en ny fil i ett av fönstren samtidigt som det andra
fönstret förblir oförändrat. Du kommer att märka att fönstren är helt
oberoende.

Här är ett annat sätt att använda två fönster till att visa två olika
filer:

>> Skriv C-x 4 C-f följt av ett filnamn. Avsluta med <Return>.
   Den nya filen kommer då att dyka upp i det nedre fönstret.
   Markören flyttats också dit.

>> Skriv C-x o för att gå tillbaka till det övre fönstret och C-x
   1 för att bli kvitt det nedre igen.

* MULTIPLA RAMAR
----------------

Emacs kan också skapa flera "ramar".  En ram är vad vi kallar en
samling av fönster tillsammans med menyer, rullningslister, ekoområde
etc. Det som Emacs kallar för ram kallar de flesta andra program för
fönster. Flera grafiska ramar kan visas på skärmen samtidigt. På en
textterminal kan bara en ram visas åt gången.

>> Skriv M-x make-frame <Return>.
   En ny ram visas på din skärm.

Du kan göra allt du gjorde i den första ramen i den här nya ramen. Det
finns inget speciellt med den första ramen.

>> Skriv M-x delete-frame <Return>.
   Ta bort den valda ramen.

Du kan också ta bort ramen genom den vanliga metod som tillhandahålls
av fönstersystemet (ofta klickar man på knappen med symbolen "X" i
något av de övre hörnen.) Om den sista ramen tas bort på det här
sättet så avlutas Emacs.

* REKURSIVA REDIGERINGSNIVÅER
-----------------------------

Ibland kan du hamna i något som kallas "rekursiv redigering"
(recursive editing level). Detta indikeras med hakparenteser runt
huvudläget i lägesraden. Till exempel kan det stå [(Fundamental)]
istället för (Fundamental).

För att komma ur rekursiv redigering trycker du <ESC> <ESC> <ESC>.
Detta är ett generellt brytkommando. Du kan också använda det för att
bli kvitt extra fönster och för att komma ut ur minibufferten.

>> Skriv M-x för att komma in i minibufferten. Skriv så <ESC> <ESC>
   <ESC> för att komma ut.

Du kan inte använda C-g för att komma ut ur rekursiv redigering.
Detta är för att C-g används för att avbryta kommandon och argument
under rekursiv redigering.


* MER HJÄLP
-----------

I denna vägledning har vi försökt inkludera precis så mycket
information att du kan börja använda Emacs. Det finns så många
möjligheter i Emacs att det skulle vara omöjligt att förklara alla
här. Men du VILL säkert lära dig mer om Emacs eftersom den har många
goda egenskaper. Emacs tillhandahåller kommandon för att läsa all
dokumentation. Dessa hjälpkommandon startas med teckenkombinationen
C-h.

För att använda hjälpen skriver du C-h följt av ett tecken för den
hjälp du behöver. Om du verkligen är helt villrådig kan du trycka C-h
? för att Emacs skall visa vilken hjälp som finns tillgänglig. Om du
har skrivit C-h och bestämmer dig för att du inte behöver ha någon
hjälp kan du trycka C-g för att avbryta.

(Om C-h inte visar ett hjälpmeddelande längst ner på skärmen, kan du i
stället försöka med funktionstangenten F1 eller M-x help <Return>.)

Den mest grundläggande hjälp-funktionen är C-h c. Skriv C-h, ett "c"
och en knappsekvens. Emacs ger då en beskrivning av kommandot.

>> Skriv C-h c C-p.

Meddelandet skall då bli något i stil med:

        C-p runs the command previous-line

Detta ger dig namnet på funktionen. Eftersom funktionerna har
beskrivande namn kan de också fungera som en enkel dokumentation,
tillräckligt för att påminna dig om kommandon du redan lärt dig.

Flerteckenskommandon, så som C-x C-s och (om du inte har META, EDIT
eller ALT tangenten) <ESC> v, är också tillåtna efter C-h c.

För att få mer information om ett kommando kan du använda C-h k
istället för C-h c.

>> Skriv C-h k C-p.

Detta kommer visa funktionens dokumentation och namn i ett eget
fönster. För att avsluta hjälpfönstret kan du trycka C-x 1. Du behöver
inte göra det omedelbart. Du kan editera med hjälptexten som stöd för
att först senare ta bort fönstret med C-x 1.

Här är fler varianter på C-h:

   C-h f        Beskriv en funktion. Du skriver in funktionsnamnet.

>> Prova att skriva C-h f previous-line<Return>.
   Detta ger den information Emacs har om funktionen
   som implementerar kommandot C-p.

Ett liknande kommando, C-h v, visar dokumentationen för de variabler
som du kan ändra värde på för att anpassa Emacs beteende. Du måste
ange namnet på variabeln när Emacs frågar efter den.

   C-h a        Kommando-apropå (Command Apropos). Skriv in ett
		nyckelord och Emacs listar all kommandon vars namn
		innehåller det nyckelordet. Alla dessa
                kommandon kan aktiveras med META-x. För några
                kommandon listas också de kombinationer bestående av
                en eller två tecken som kör det kommandot.

>> Skriv C-h a file<Return>.

Detta visar i ett annat fönster en lista över alla M-x kommandon där
"file" förekommer i kommandonamnet. Du kommer se teckenkommandon som
C-x C-f listade bredvid motsvarande kommandonamn, t.ex. find-file.

>> Tryck C-M-v för att rulla texten i hjälpfönstret. Gör det några
   gånger.

>> Type C-x 1 för att ta bort hjälpfönstret.

   C-h i        Läs den bifogade manualen (alias Info). Detta kommando
                placerar dig i en speciell buffer vid namn "*info*"
                där du kan läsa hjälpen för de paket som är
                installerade i ditt system. Slå m emacs <Return> för
                att läsa Emacs-manualen. Om du aldrig tidigare har
                använt dig av Info, skriv ? och Emacs tar dig på en
                guidad tur över Infolägets (Info-mode) möjligheter.
                När du väl har tagit dig igenom den här
                användarhandledningen så är det direktmanualen som är
                din huvudsakliga källa till dokumentation.


* MER FUNKTIONER
----------------

Du kan lära dig mer om Emacs genom att läsa dess manual, antingen i
bokform eller on-line i Info (använd Hjälp-menyn eller skriv F10 h r).
Två finesser som du kan komma att gilla speciellt är komplettering
(completion), som spar tangenttryckningar, och dired, som förenklar
filhantering.

Komplettering är ett sätt att undvika onödiga tangenttryckningar. Till
exempel, om du vill byta till *Messages* bufferten, kan du du skriva
C-x b *M<Tab> och Emacs kommer fylla i resten av buffertnamnet så
långt den kan räkna ut det från det du redan skrivit. Komplettering
finns beskrivet i Emacs-manualen i noden "Completion".

Dired gör det möjligt att lista filer i en katalog (och även dess
subkataloger), flytta runt i listan, besöka, byta namn, ta bort och
operera på olika sätt på filerna. Dired finns beskrivet i Info i
Emacs-manualen i noden "Dired".

Manualen beskriver även många andra Emacs funktioner.

* SLUTORD
---------

För att avsluta Emacs använd C-x C-c.

Den här handledningen är tänkt att vara förståelig för alla nya
Emacs-användare. Så om det är något som är oklart, klandra inte dig
själv, klaga!


* KOPIERING
-----------

Denna vägledning härstammar från en hel rad Emacs-vägledningar och den
första skrevs av Stuart Cracraft för den ursprungliga Emacs. Mats
Lidell översatte den till Svenska.

This version of the tutorial, like GNU Emacs, is copyrighted, and
comes with permission to distribute copies on certain conditions:

Copyright (C) 1985, 1996, 1998, 2001-2012  Free Software Foundation, Inc.

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
;;;   sentence-end-double-space: nil
;;; End:
