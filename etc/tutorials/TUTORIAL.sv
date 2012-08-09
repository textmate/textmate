Emacs anv�ndarhandledning. I slutet finns kopieringsvillkoren.

Emacs-kommandon inneb�r ofta anv�ndning av kontrolltangenten (vanligen
m�rkt CTRL eller CTL) eller META-tangenten (p� vissa tangentbord m�rkt
ALT eller EDIT). Vi anv�nder h�r f�ljande f�rkortningar:

 C-<chr> h�ll ner kontrolltangenten samtidigt som du skriver bokstaven
         <chr>. C-f betyder: h�ll ner kontrolltangenten och tryck f.
 M-<chr> h�ll ner META-tangenten samtidigt som du skriver <chr>. Om
         META-tangent saknas trycker du <ESC>, ESC-tangenten, sl�pper
         den och trycker sedan <chr>.

Viktigt: F�r att avsluta Emacs trycker du C-x C-c (tv� tecken).
F�r att avsluta kommandon som inte skrivits in fullt, tryck C-g.  
Tecknen ">>" i v�nstermarginalen anger att du kan prova ett
kommando. Till exempel:
<<Tomma rader s�tts in runt n�sta rad n�r help-with-tutorial aktiveras>>
[Tomma rader av pedagogiska sk�l. Texten forts�tter nedanf�r.]
>> Tryck C-v (View next screen) f�r att hoppa till n�sta sk�rmbild.
        Prova nu. H�ll ned kontrolltangenten och tryck v. G�r s� i
        forts�ttningen n�r du �r f�rdig med en sk�rmbild.

Notera att det �r ett �verlapp p� tv� rader n�r du byter fr�n
sk�rmbild till sk�rmbild. Detta �r f�r att beh�lla sammanhanget n�r du
bl�ddrar fram�t i filen.

Det f�rsta du beh�ver veta �r hur du man�vrerar fr�n plats till plats
i texten. Du har redan l�rt dig hur du flyttar en sk�rmbild fram�t,
med C-v. F�r att flytta dig en sk�rmbild bak�t trycker du M-v. (H�ll
ned META-tangenten och tryck v eller tryck <ESC>v om du inte har
META-, EDIT- eller ALT-tangent.)

>> Prova att trycka M-v och C-v n�gra g�nger.


* SAMMANFATTNING
----------------

F�ljande kommandon �r bra f�r att se hela sk�rmbilder:

        C-v     Flytta en sk�rmbild fram�t.
        M-v     Flytta en sk�rmbild bak�t.
        C-l     Rita om sk�rmen och placera texten d�r mark�ren st�r
                mitt p� sk�rmbilden. (Det �r KONTROLL-L, inte
                KONTROLL-1.)

>> Leta reda p� mark�ren och se vad som st�r d�r. Tryck sedan C-l.
   Hitta mark�ren igen och notera att det �r samma text som st�r kring
   mark�ren nu, men nu mitt p� sk�rmen. Om du trycker C-l igen s�
   flyttas texten h�gst upp p� sk�rmen. Tryck C-l igen och den flyttas
   ner till botten.

Du kan ocks� anv�nda PageUp och PageDn tangenterna, om din terminal
har dem, f�r att flytta en hel sk�rmbild �t g�ngen, men du redigerar
effektivare om du anv�nder C-v och M-v.


* GRUNDL�GGANDE MARK�RR�RELSER
------------------------------

Att flytta sig fr�n sk�rmbild till sk�rmbild kan vara bra, men hur
f�rflyttar man sig till en speciell plats p� sk�rmen?

Det finns flera s�tt att g�ra detta. Du kan anv�nda piltangenterna,
men det �r mer effektivt att ha h�nderna i standardl�get och anv�nda
kommandona C-p, C-b, C-f och C-n. Dessa tecken �r likv�rdiga med de
fyra piltangenterna. S� h�r:

			 F�reg�ende rad, C-p
                                  :
                                  :
   Bak�t, C-b .... Nuvarande mark�rposition .... Fram�t, C-f
                                  :
                                  :
                           N�sta rad, C-n

>> Flytta mark�ren till linjen mitt i diagrammet genom att anv�nda C-n
   och C-p. Anv�nd sedan C-l f�r att centrera diagrammet p�
   sk�rmbilden.

Detta �r enklare att komma ih�g om du t�nker p� dessa f�rkortningar: P
f�r f�reg�ende (previous), N f�r n�sta (next), B f�r bak�t (backward)
och F f�r fram�t (forward). Du kommer att anv�nda dessa grundl�ggande
kommandona hela tiden.

>> G�r n�gra C-n s� att du kommer ned till den h�r raden.

>> Flytta dig in i raden med hj�lp av n�gra C-f och sedan upp�t
   med n�gra C-p. L�gg m�rke till vad C-p g�r n�r mark�ren st�r mitt
   p� en rad.

Textrader �r �tskilda med radslutstecken. Den sista raden i filen
avslutas ocks� vanligtvis med ett radslut men Emacs kr�ver inte att
den g�r det.

>> Prova med C-b i b�rjan av en rad. Detta g�r att mark�ren
   flyttas till slutet av den tidigare raden. Detta �r f�r att den
   flyttar mark�ren �ver radslutstecknet.

C-f flyttar ocks� �ver radslut, precis som C-b.

>> G�r n�gra fler C-b s� att du f�r en k�nsla f�r var mark�ren
   �r. Tryck sedan n�gra C-f tills du kommer till slutet av
   raden. Tryck ytterligare en C-f s� att du flyttar mark�ren till
   n�sta rad.

N�r du flyttar mark�ren f�rbi toppen eller botten av sk�rmbilden
kommer texten utanf�r sk�rmen att komma fram. Detta kallas "rullning"
och g�r det m�jligt f�r Emacs att flytta mark�ren utan att den
f�rsvinner ut ur sk�rmbilden.

>> Prova att flytta mark�ren f�rbi sk�rmbildens nederkant med hj�lp av
   C-n och se vad som h�nder.

Om det g�r f�r sakta att flytta mark�ren ett tecken i taget kan du
flytta den ett ord. M-f flyttar mark�ren ett ord fram�t och M-b
flyttar den ett ord bak�t.

>> Prova n�gra M-f och M-b.

Om mark�ren st�r mitt i ett ord kommer M-f att flytta mark�ren till
slutet av ordet. Om du st�r mitt emellan tv� ord kommer M-f att flytta
mark�ren till slutet av n�sta ord. M-b fungerar p� samma s�tt men i
motsatt riktning.

>> Tryck M-f och M-b n�gra g�nger och skifta mark�rposition med n�gra
   C-f och C-b s� att du ser hur M-f och M-b uppf�r sig vid olika
   placeringar av mark�ren b�de i och mellan ord.

L�gg m�rke till likheten mellan C-f och C-b � ena sidan och M-f och
M-b � den andra. Ofta anv�nds META-kommandon till spr�krelaterade
operationer (ord, stycken, avsnitt), medan kontrollkommandon anv�nds
till grundl�ggande operationer som inte beror av vad man redigerar
(bokst�ver, rader, etc.).

Denna likhet finns ocks� mellan rader och stycken: C-a och C-e flyttar
mark�ren till b�rjan av en rad eller till slutet av en rad, medan M-a
och M-e flyttar den till b�rjan respektive slutet av ett stycke.

>> Prova n�gra C-a och sedan n�gra C-e.
   Prova ocks� n�gra M-a och sedan n�gra M-e.

Se hur efterf�ljande C-a efter varandra inte g�r n�got, medan flera
M-a forts�tter att flytta mark�ren till n�sta stycke. �ven om detta
inte verkar sj�lvklart �r det ganska naturligt.

Platsen d�r mark�ren �r i texten kallas ocks� f�r "arbetspunkt"
(point). Eller omskrivet: Mark�ren visar p� sk�rmen var arbetspunkten
�r i texten.

H�r �r en kort sammanfattning av de enklaste mark�rf�rflyttnings-
kommandona, inklusive ord- och styckesf�rflyttningskommandon:

        C-f     Flytta mark�ren ett steg fram�t
        C-b     Flytta mark�ren ett steg bak�t

        M-f     Flytta mark�ren ett ord fram�t
        M-b     Flytta mark�ren ett ord bak�t

        C-n     Flytta mark�ren till n�sta rad
        C-p     Flytta mark�ren till f�reg�ende rad

        C-a     Flytta mark�ren till b�rjan av raden
        C-e     Flytta mark�ren till slutet av raden

        M-a     Flytta mark�ren till b�rjan av meningen
        M-e     Flytta mark�ren till slutet av meningen

>> Prova alla dessa kommandon n�gra g�nger f�r tr�nings skull.
   Dessa �r de kommandon som anv�nds mest.

Tv� andra viktiga mark�rr�relsekommandon �r M-< (META mindre-�n), som
flyttar mark�ren till b�rjan av texten, och M-> (META st�rre-�n), som
flyttar den till slutet av texten.

P� en del tangentbord �r "<" placerad �ver komma, s� att man m�ste
anv�nda skift f�r att f� fram den. P� dessa tangentbord m�ste man
ocks� anv�nda skift f�r att skriva M-<. Utan skifttangenten skulle det
bli M-komma.

>> Prova M-< nu f�r att flytta mark�ren till b�rjan av v�gledningen.
   Anv�nd sedan C-v f�r att flytta mark�ren tillbaka hit igen.

>> Prova ocks� M-> f�r att flytta mark�ren till slutet av v�gledningen.
   Anv�nd sedan M-v f�r att flytta mark�ren tillbaka hit igen.

Du kan ocks� flytta mark�ren med hj�lp av piltangenterna, om
terminalen har piltangenter. Vi f�resl�r att du l�r dig C-b, C-f, C-n
och C-p av tre sk�l. F�r det f�rsta kommer de att fungera p� alla
slags terminaler. F�r det andra kommer du att finna, n�r du har f�tt
lite tr�ning i att anv�nda Emacs, att det g�r mycket snabbare att
anv�nda kontrollfunktionerna �n piltangenterna (f�r att du undviker
att �ndra fingers�ttningen). Den tredje anledningen �r att n�r man har
l�rt sig att anv�nda kontrolltangenten blir det l�ttare att l�ra sig
de mer avancerade kontrollfunktionerna.

De flesta kommandon i Emacs tar ett numeriskt argument och f�r de
flesta kommandon leder detta till att de repeteras. Ett numeriskt
argument anges genom att du skriver C-u och sedan talet, innan du
skriver kommandot. Om du har en META- (eller EDIT- eller ALT-) tangent
s� finns det ett annat alternativ f�r att ge numeriska argument: skriv
talet medan du h�ller ned META-tangenten. Vi f�resl�r att du anv�nder
C-u f�r det fungerar p� alla slags terminaler. Det numeriska
argumentet kallas ocks� f�r "prefixargument" eftersom det skrivs f�re
kommandot.

Till exempel: C-u 8 C-f flyttar mark�ren �tta steg fram�t.

>> Prova C-n eller C-p med ett numeriskt argument s� att du
   kommer s� n�ra den h�r raden som m�jligt med ett enda kommando.

De flesta kommandon anv�nder det numeriska argumentet f�r ett
repeterat utf�rande men det finns kommandon som anv�nder det
annorlunda. Flera kommandon, men inga av dem du l�rt dig hittills,
anv�nder det som en flagga. Med ett prefixargument, och oberoende av
dess v�rde, g�r kommandot n�got annat.

C-v och M-v finns med bland dessa undantag. Om man ger ett argument
till ett av dessa kommandon kommer sk�rmbilden flytta sig upp eller
ned s� m�nga rader som argumentet anger, ist�llet f�r s� m�nga
sk�rmbilder. Till exempel kommer C-u 8 C-v flytta sk�rmbilden 8 rader
upp�t.

>> Prova C-u 8 C-v nu.

Detta borde ha flyttat sk�rmbilden 8 rader upp�t. Om du �nskar flytta
tillbaka igen �r det bara att ge samma argument till M-v.

Om du anv�nder ett f�nstersystem, som X eller MS-Windows, finns det
troligen ett rektangul�rt omr�de p� sidan av Emacs-f�nstret, en s�
kallad rullningslist. Genom att klicka i den med musen kan du rulla
texten.

Om din mus har ett rullningshjul kan �ven den anv�ndas f�r att rulla
texten.

* OM EMACS SLUTAR SVARA
-----------------------

Om Emacs slutar att reagera p� kommandon kan du lugnt stoppa dem genom
att trycka C-g. Du kan ocks� anv�nda C-g f�r att stoppa ett kommando
som tar f�r l�ng tid att utf�ra.

Det �r ocks� m�jligt att anv�nda C-g f�r att avbryta ett numeriskt
argument eller b�rjan p� ett kommando som du inte �nskar att utf�ra.

>> Skriv C-u 100 f�r att ge ett numeriskt argument p� 100 och tryck
   C-g. Tryck nu C-f. Mark�ren skall nu flytta sig bara ett steg, f�r att
   du avbr�t argumentet med C-g.

Om du av misstag sl�r <ESC> blir du kvitt detta med ett C-g.


* SP�RRADE KOMMANDON
--------------------

En del Emacs-kommandon �r "sp�rrade" s� att nyb�rjare inte skall
anv�nda dem av misstag.

Om du provar ett av dessa sp�rrade kommandon kommer Emacs ge ett
meddelande som ber�ttar vilket kommando det �r och kommer att fr�ga om
du verkligen vill forts�tta och utf�ra detta kommando.

Om du verkligen �nskar att utf�ra kommandot skriver du <SPC>,
(mellanslagstangenten) som svar p� fr�gan. Normalt, om du inte �nskar
att utf�ra detta kommando, svarar du "n" p� fr�gan.

>> Skriv C-x C-l (som �r ett sp�rrat kommando).
   Skriv n som svar p� fr�gan.


* F�NSTER
---------

Emacs kan ha flera "f�nster" d�r varje kan visa sin egen text. Vi
kommer f�rklara senare hur man anv�nder flera f�nster. H�r skall vi
f�rklara hur man blir av med extra f�nster f�r att komma tillbaka till
det grundl�ggande l�get med endast ett f�nster. Det �r enkelt:

        C-x 1      Ett f�nster (dvs. ta bort alla andra f�nster).

Det �r KONTROLL-x f�ljt av siffran 1. C-x 1 utvidgar f�nstret d�r
mark�ren st�r s� att det fyller hela sk�rmbilden. Alla andra f�nster
tas bort.

>> Flytta mark�ren till den h�r raden och tryck C-u 0 C-l.
>> Tryck C-h k C-f.
   Se hur det h�r f�nstret krymper samtidigt som ett nytt upptr�der
   f�r att visa dokumentationen av C-f-kommandot.

>> Sl� C-x 1 och se hur dokumentationsf�nstret nu f�rsvinner.

Kommandot skiljer sig lite fr�n andra kommandon du har l�rt dig
eftersom det best�r av tv� tecken. Det startar med tecknet
KONTROLL-x. Det �r faktisk m�nga kommandon som startar med KONTROLL-x
och m�nga av dem har med filer, sk�rmbilder och liknande saker att
g�ra. Dessa kommandon �r tv�, tre eller fyra tecken l�nga.


* SKRIVA OCH TA BORT TEXT
-------------------------

Om du �nskar att s�tta in text �r det bara att skriva in texten. 
Vanliga tecken, som A, 7, *, etc., s�tts in direkt n�r du skriver dem. 
Tryck p� <Return> f�r att s�tta in en radbrytning. (Det �r den tangent
p� tangentbordet som ibland �r m�rkt med "Enter")

F�r att radera <DEL> tecknet omedelbart f�re aktuell mark�rposition,
skriv <DEL>. Det �r tangenten p� tangentbordet som vanligtvis �r
markerad med "Backspace" -- det �r samma tangent som du normal
anv�nder f�r att radera det sist inmatade tecknet utanf�r Emacs. 

Det kan finnas en annan tangent p� ditt tangentbordet som �r m�rkt med
"Delete", men det �r inte den vi menar med <DEL>.

>>  G�r detta nu: Skriv in n�gra tecken och ta bort dem genom att
    anv�nda <DEL>. Var inte r�dd f�r att skriva i den h�r filen, du
    kommer inte att kunna f�r�ndra originalet till v�gledningen. Detta
    �r bara en lokal kopia.

N�r en rad blir f�r l�ng f�r att rymmas p� en sk�rmbredd s� forts�tter
den p� raden under. Om du anv�nder ett f�nstersystem, visas sm� b�jda
pilar i det lilla utrymmet p� b�gge sidor om textmassan (i v�nster och
h�ger marginal) f�r att ange var en rad forts�tter, Om du anv�nder
en textterminal anges med ett bakstreck ("\") i kolumnen l�ngst till
h�ger att raden forts�tter.

>>  Skriv in lite text s� att du kommer till slutet av raden och
    forts�tt att skriva lite till. Du kommer d� att se hur
    forts�ttningstecknet ser ut.

>>  Anv�nd <DEL> f�r att radera texten tills raden ryms p� en
    sk�rmbredd igen. Forts�ttningstecknet kommer d� att f�rsvinna.

Du kan radera radbrytning precis som andra tecken. Genom att radera
radbrytningen mellan tv� rader sl�s dessa samman till en. Om
resultatet av denna sammanslagning blir f�r stor f�r att passa inom en
sk�rmbredd, s� kommer den att visas med ett forts�ttningstecken.

>> Flytta mark�ren till b�rjan av en rad och tryck <DEL>.
   Detta kommer att klistra ihop raden med raden �ver.

>> Tryck <Return> f�r att s�tta in radbrytningen du tog bort.

T�nk p� att de flesta Emacs-kommandon kan ta numeriska argument. Detta
g�ller ocks� texttecken. Genom att repetera ett texttecken kommer det
skrivas flera g�nger.

>> Prova det nu: Skriv C-u 8 * f�r att s�tta in ********.

Du har nu l�rt dig de mest grundl�ggande s�tten att skriva n�got i
Emacs och att r�tta fel. Du kan radera ord och rader ocks�. H�r �r en
�versikt �ver kommandon f�r radering:

        <DEL>        Raderar tecknet som st�r precis f�re mark�ren
        C-d          Raderar tecknet som st�r precis under mark�ren

        M-<DEL>      Raderar ordet precis f�re mark�ren
        M-d          Raderar ordet precis efter mark�ren

        C-k          Raderar fr�n mark�ren till slutet av raden
        M-k          Raderar till slutet av stycket

L�gg m�rke till att <DEL> och C-d kontra M-<DEL> och M-d f�ljer
m�nstret som b�rjade med C-f och M-f. (<DEL> �r inte precis ett
kontrolltecken men l�t oss inte bry oss om det.) C-k och M-k fungerar
p� liknande s�tt som C-e och M-e n�r det g�ller rader respektive
meningar.

Du kan ocks� ta bort en del av en texten med hj�lp av f�ljande
allm�nna metod. Flytta till ena �nden av det omr�de du vill ta bort
och tryck C-<SPC>. (<SPC> �r mellanslagstangenten.) Flytta sedan till
andra �nden av omr�det du vill ta bort. N�r du g�r det markerar Emacs
texten mellan mark�ren och den plats d�r du tryckte C-<SPC>. Slutligen,
tryck C-w. Detta tar bort texten mellan de tv� positionerna.

>> Flytta mark�ren till bokstaven D i f�reg�ende stycke.
>> Tryck C-<SPC>. Emacs skall nu visa meddelandet "Mark set"
   l�ngst ner p� sk�rmen.
>> Flytta mark�ren till bokstaven o i ordet metod p� andra raden i
   stycket.
>> Tryck C-w. Detta tar bort texten fr�n och med D fram till just f�re
   o.

Skillnaden mellan att "ta bort" (killing) och "radera" (deleting) text
�r att "borttagen" text kan s�ttas tillbaka (var som helst), medan
raderad text inte kan det p� det s�ttet. (Du kan dock �ngra en
radering--se nedan.) �terins�ttning av borttagen text kallas
"�terh�mtning" (yanking).  Generellt kan man s�ga att kommandon som
tar bort fler �n ett tecken sparar undan texten (s� att den kan
�terh�mtas) medan kommandon som bara raderar ett tecken, eller bara
raderar tomma rader och mellanrum inte sparar n�gonting (och den
texten kan allts� inte �terh�mtas). <DEL> och C-d raderar i det enkla
fallet utan argument. Med argument s� tar de bort i st�llet.

>> Flytta mark�ren till b�rjan av en rad som inte �r tom.
   Tryck C-k f�r att ta bort texten p� raden.
>> Tryck C-k en g�ng till. Du kommer nu se att den raderar den tomma
   raden som var kvar.

L�gg m�rke till att ett enstaka C-k bara raderar texten p� raden och
att det andra C-k raderar sj�lva raden och flyttar upp texten p� raden
under ett steg. C-k hanterar numeriska argument lite speciellt. Den
raderar s� m�nga rader OCH inneh�llet i dem. Detta �r allts� inte bara
en repetition av kommandot. C-u 2 C-k raderar tv� rader samt de tomma
raderna, medan C-k tv� g�nger inte kommer att g�ra det.

Att s�tta in borttagen text kallas att "�terh�mta" den (yanking).
(T�nk p� det som att du rycker, eller drar, tillbaka n�gon text som
tagits bort.) Du kan antingen h�mta tillbaka borttagen text till samma
plats som d�r den blev borttagen, eller s� kan du s�tta in den p� en
annan plats i texten du redigerar eller till och med i en helt annan
fil. Du kan ocks� h�mta tillbaka samma text flera g�nger s� att du f�r
flera kopior av den. N�gra andra textredigerare kallar "ta bort" och
"�terh�mta" att "klippa ut" respektive "klistra in" (Se ordlistan i
Emacs-manualen)

Kommandot f�r att h�mta tillbaka text �r C-y. Kommandot h�mtar
tillbaka den sist borttagna texten och placerar den d�r mark�ren �r.

>> Prova: G�r C-y f�r att f� tillbaka texten.

Om du g�r flera C-k i rad s� kommer all bortagen text att sparas
samlat s� att ett C-y �terh�mtar alla raderna p� en g�ng.

>> Prova detta. Tryck C-k ett par g�nger.

Och h�mta s� tillbaka igen:

>> Tryck C-y. Flytta mark�ren n�gra rader ned och tryck C-y igen.
   S� kopierar man text.

Men vad g�r du om du har en text du �nskar att h�mta tillbaka men du
har redan tagit bort n�got nytt? C-y skulle h�mta tillbaka den senaste
texten som blev borttagen men tidigare bortagen text �r inte
f�rlorad. Du kan f� tillbaka den med kommandot M-y. Efter att du har
anv�nt C-y f�r att h�mta tillbaka den sist borttagna texten kommer M-y
ers�tta denna text med tidigare borttagen text. Genom att g�ra M-y om
och om igen h�mtas allt tidigare borttagen text tillbaka. N�r du har
n�tt den �nskade texten beh�ver du inte g�ra n�got ytterligare f�r att
beh�lla den. Forts�tt bara med din redigeringen och l�mna den
�tertagna texten d�r den �r.

Om du g�r M-y tillr�ckligt m�nga g�nger kommer du att komma tillbaka
till startpunkten (texten som sist blev borttagen).

>> Ta bort en rad, flytta mark�ren till en ny rad och ta bort �ven
   denna rad. Anv�nd C-y f�r att h�mta tillbaka den sista raden. Tryck
   M-y f�r att byta den mot den tidigare borttagna raden. Tryck flera
   M-y och se vad du f�r. Forts�tt med detta tills du f�r tillbaka den
   f�rsta raden igen och sedan n�gra g�nger till. Om du vill kan du
   prova med positiva och negativa argument till M-y.


* �NGRA
-------

Om du g�r en f�r�ndring i texten och sedan �ngrar dig, s� kan du
upph�va �ndringen med �ngra-kommandot C-/.

Normalt kommer C-/ upph�va f�r�ndringen som gjordes av det sist
utf�rda kommandot. Om du repeterar C-/ flera g�nger kommer varje
repetition upph�va ett kommando till.

Det finns tv� undantag. Kommandon som inte f�r�ndrar texten r�knas
inte (detta inkluderar mark�rf�rflyttningar och bl�ddringskommandon),
och inskrivna enkelbokst�ver blir vanligtvis grupperade i grupper om
upp till 20 tecken. Detta �r f�r att reducera antalet C-/ som beh�vs
f�r att �ngra inskriven text.

>> Ta bort den h�r raden med C-k, h�mta sedan tillbaka den med C-/.

C-_ �r ett alternativt �ngra-kommandot. Den fungerar exakt p� samma
s�tt som C-/. P� vissa textterminaler skickar C-/ faktiskt C-_ till
Emacs. �ven C-x u fungerar precis som C-/, men �r inte lika enkelt att
skriva.

Ett numeriskt argument till C-/, C-_ eller C-x u medf�r upprepning.

Du kan �ngra radering av text precis p� samma s�tt som du kan �ngra
att du tagit bort text. Skillnaden mellan att ta bort och att radera
n�gonting p�verkar endast om du kan h�mta tillbaka det med C-y. F�r
�ngerfunktionen spelar det ingen roll hur texten f�rsvunnit.


* FILER
-------

F�r att texten du har f�r�ndrat skall sparas permanent m�ste du l�gga
den i en fil. Om du inte g�r det kommer texten att f�rsvinna n�r du
avslutar Emacs. Du l�gger texten i en fil genom att f�rst finna (find)
denna fil. Detta kallas ocks� f�r att bes�ka filen (visit).

Att finna en fil inneb�r att du ser filens inneh�ll i Emacs. P� m�nga
s�tt �r det som om du f�r�ndrar sj�lva filen men f�r�ndringen du g�r
kommer inte bli permanent f�rr�n filen sparas (save). Detta �r f�r att
undvika att halvf�r�ndrade filer sparas n�r du inte vill det. Till och
med n�r du sparar filen kommer Emacs att beh�lla originalet under ett
nytt namn, som backup, ifall du senare �ngrar alltihop.

Om du tittar n�stan l�ngst ner p� sk�rmbilden s� kommer du se en rad
som b�rjar med minustecken, och som startar med "--:-- TUTORIAL.sv"
eller n�got snarlikt. Denna del av sk�rmbilden visar normalt namnet p�
filen du bes�ker. Just nu bes�ker du din personlig kopia av
v�gledningen till Emacs, vilken heter "TUTORIAL.sv". Vilken fil du �n
�r inne i s� kommer filnamnet st� d�r.

En annan sak med kommandot f�r att finna filer �r att du m�ste ange
vilket filnamn du �nskar. Vi s�ger att kommandot "l�ser ett
argument". I detta fall �r argumentet namnet p� filen. Efter att du
gett kommandot

        C-x C-f   Finn en fil

kommer Emacs fr�ga efter ett filnamn. Filnamnet du skriver syns p� den
nedersta raden i sk�rmbilden. Denna sista rad kallas minibuffert n�r
den anv�nds p� det h�r s�ttet. Du kan anv�nda vanliga Emacs-kommandon
f�r att f�r�ndra filnamnet.

N�r du skriver in filnamnet, eller n�got annat i minibufferten, kan du
avbryta med kommandot C-g.

>> Skriv C-x C-f och s� C-g. Detta avbryter minibufferten och
   avbryter ocks� C-x C-f kommandot som anv�nde minibufferten. S� att
   du inte finner n�gon fil.

N�r du �r f�rdig med att skriva filnamnet trycker du <Return> f�r att
utf�ra kommandot. Minibufferten f�rsvinner och C-x C-f kommandot b�rja
leta efter filen.

Filinneh�llet visas nu upp p� sk�rmen och du kan b�rja redigera
inneh�llet. N�r du vill spara filen kan du anv�nda detta kommando

        C-x C-s   Spara fil

Detta sparar texten p� sk�rmen till filen. F�rsta g�ngen detta g�rs
kommer Emacs att ge originalfilen ett nytt namn s� att den inte g�r
f�rlorad. Det nya filnamnet bildas genom att l�gga till ett "~" i
slutet av det ursprungliga filnamnet.

N�r lagringen �r utf�rd kommer Emacs skriva ut namnet p� filen som
blev sparad. Du b�r spara ofta s� att du inte f�rlorar s� mycket om
systemet kraschar. (Se kapitlet om sparautomatik nedan.)

>> Skriv C-x C-s TUTORIAL.sv <Return>.
   Detta sparar den h�r handledningen i en fil med namnet TUTORIAL
   och "Wrote ...TUTORIAL.sv" skrivs ut nederst p� sk�rmbilden.

Du kan finna en existerande fil, antingen f�r att f�r�ndra den eller
f�r att titta p� den. Du kan ocks� finna en fil som inte existerar.
Det �r s� man skapar nya filer med Emacs: finn filen, som �r tom till
att b�rja med, och s�tt ig�ng med att skriva texten som skall in i
filen. F�rst n�r du sparar filen kommer Emacs att verkligen skapa
filen med den text du har skrivit. Fr�n och med detta editerar du en
fil som existerar.


* BUFFERTAR
-----------

Om du finner en ny fil med C-x C-f kommer den f�rsta filen forts�tta
att vara �ppen i Emacs. Du kan byta tillbaka till den genom att finna
den p� nytt med C-x C-f. P� s� s�tt kan du ha ett stort antal filer
�ppna i Emacs.

Emacs sparar texten f�r varje fil i ett objekt kallat "buffert". N�r
du finner en ny fil skapas en ny buffert i Emacs. F�r att se en lista
�ver existerande buffertar i Emacs kan du skriva

        C-x C-b      Listning av buffertar.

>> Prova C-x C-b nu.

Se hur varje buffert har ett namn och att de ocks� kan ha namnet p�
den fil som inneh�llet kommer fr�n. Vilken text du �n ser i ett
Emacs-f�nster s� tillh�r den alltid en buffert.

>> Skriv C-x 1 f�r att bli kvitt buffertlistan.

N�r du har flera buffertar s� �r bara en av dem "g�llande" �t g�ngen.
Det �r den buffert du redigerar. Om du vill redigera en annan buffert
s� m�ste du byta till den. Om du vill byta till en buffert som
motsvarar en fil kan du g�ra det genom att bes�ka den igen med C-x
C-f. Det finns dock ett enklare s�tt: anv�nd C-x b kommandot. I det
kommandot anger du buffertens namn.

>> Skapa en fil med namnet "foo" genom att trycka C-x C-f foo <Return>.
   Skriv sedan C-x b TUTORIAL.sv <Return> f�r att komma tillbaka till
   den h�r handledningen.

Mestadels �r buffertens namn densamma som filens namn (utan
katalogdel.) Det �r dock inte alltid s�. Bufferlistan du skapar med
C-x C-b visar alltid namnen p� varje buffert.

All text du ser i ett Emacsf�nster �r alltid del av n�gon buffert. En
del buffertar �r inte knutna till n�gon fil, till exempel bufferten
"*Buffer List*". Det �r den buffert som inneh�ller buffertlistan som
skapades med C-x C-b. Bufferten "*Messages*" motsvarar inte heller
n�gon fil. Den inneh�ller de meddelanden som visas p� den nedersta
raden i Emacs sessionen.

>> Skriv C-x b *Messages* <Return> f�r att se meddelandebufferten.
   Skriv sedan C-x b TUTORIAL.sv <Return> f�r att �terg� till den h�r
   handledningen.

Om du �ndrar texten till en fil och sedan �ppnar en ny fil, s� kommer
inte den f�rsta filen sparas. F�r�ndringen ligger kvar i
bufferten. Skapande och redigering av den nya filen p�verkar inte den
f�rsta filens buffert. Detta kan vara bra men betyder ocks� att du
beh�ver ett l�mpligt s�tt att spara den f�rsta filens buffert. Det �r
omst�ndligt att flytta tillbaka till den tidigare bufferten med C-x
C-f f�r att sedan spara filen med C-x C-s. D�rf�r finns kommandot

        C-x s      Spara buffertar

C-x s fr�gar f�r varje buffert med �ndringar, som inte sparats, om du
vill spara eller ej.

>> S�tt in en rad med text och spara med C-x s
   Du skall nu f� fr�gan om du �nskar spara bufferten
   TUTORIAL.sv. Svara ja p� fr�gan genom att trycka "y" (yes).


* UTVIDGNING AV KOMMANDOM�NGDEN
-------------------------------

Det finns mycket fler Emacs-kommandon �n antalet KONTROLL- eller
META-tangenter. F�r att komma f�rbi denna begr�nsning har Emacs ett
"X"- (eXtend) kommando. Detta finns i tv� varianter:

        C-x     Tecken-utvidgning. F�ljs av ett tecken.
        M-x     Namngiven kommandoutvidgning. F�ljs av ett
                kommandonamn.

Detta �r kommandon som �r bra att ha men anv�nds mer s�llan �n de
kommandon du redan har l�rt dig. Du har redan sett n�gra av dem, C-x
C-f f�r finn, och C-x C-s f�r spara. Ett annat exempel �r kommandot
f�r att avsluta Emacs som �r C-x C-c. Var inte r�dd f�r att f�rlora
f�r�ndringar du har gjort. C-x C-c erbjuder dig att spara f�r�ndringar
innan Emacs avslutas.

Om du anv�nder ett f�nstersystem beh�ver du inte n�got speciellt
kommando f�r att byta till ett annat program. Du kan g�ra det med
musen eller med ett kommando till f�nsterhanteraren. Men om du
anv�nder en textterminal, som bara kan visa ett program �t g�ngen, s�
m�ste du avbryta Emacs f�r att flytta till ett annat program.

C-z �r kommandot f�r att avsluta Emacs *tillf�lligt* s� att du kan
�terv�nda till samma Emacs senare. N�r Emacs k�rs fr�n en textterminal
s� avbryts Emacs med C-z, dvs du �terg�r till kommandoskalet utan att
Emacsprocessen f�rst�rs. I de flesta vanliga kommandoskalen s� kan man
�terg� till Emacs med kommandot 'fg' eller med '%emacs'.

C-x C-c anv�nds n�r du skall avsluta Emacs. Det �r klokt att avsluta
Emacs om den har startats av ett mail-program eller andra
applikationer.

Det finns m�nga C-x kommandon. H�r �r en lista �ver de du har l�rt dig
hittills:

	C-x C-f		Finn fil
	C-x C-s		Spara fil
	C-x s		Spara n�gra buffertar
	C-x C-b		Lista buffertar
	C-x b		Byt buffert
	C-x C-c		Avsluta Emacs
	C-x 1		Ta bort alla utom ett f�nster
	C-x u		�ngra

Namngivna utvidgade kommandon �r kommandon som anv�nds mycket s�llan
eller bara i vissa l�gen. Ett exempel p� ett s�dant kommando �r
replace-string, som globalt ers�tter en teckenstr�ng med en annan. N�r
du skriver M-x kommer Emacs visa en prompt nederst i sk�rmbilden med
M-x d�r du skall skriva in kommandot du �nskar att k�ra, i det h�r
fallet "replace-string". Det �r bara att skriva "repl s<TAB>" och
Emacs kommer d� att fylla i kommandonamnet. (<TAB> �r
tabulatortangenten, som vanligtvis finns �ver CapsLock- eller
skifttangenten n�ra den v�nstra kanten p� tangentbordet.) K�r
kommandot med <Return>.

Kommandot replace-string kr�ver tv� argument, teckenstr�ngen som skall
ers�ttas och teckenstr�ngen som den skall ers�ttas med. Du m�ste
avsluta b�gge argumenten med <Return>.

>> Flytta mark�ren till den blanka raden tv� rader under denna rad.
   Skriv M-x repl s<Return>f�r�ndrad<Return>�ndrad<Return>.

   L�gg m�rke till hur den h�r raden har blivit f�r�ndrad. Du har
   ersatt ordet f-�-r-�-n-d-r-a-d med "�ndrad" p� alla platser d�r
   ordet f�rekom, fr�n mark�ren och ned�t.


* SPARAUTOMATIK
---------------

N�r du har gjort f�r�ndringar i en fil men inte sparat den, s� kommer
�ndringarna att g� f�rlorade om maskinen kraschar. Som ett skydd mot
detta sparar Emacs periodiskt �ndringarna i en autosparfil f�r varje
fil du redigerar. Denna fil har ett # i b�rjan och slutet av
filnamnet. Om du till exempel har en fil med namnet "hej.c" s� kommer
namnet p� autosparfilen bli "#hej.c#". N�r du sparar filen p� vanlig
s�tt kommer Emacs radera autosparfilen.

Om maskinen kraschar kan du �terf� dina automatiskt sparade �ndringar
genom att finna filen p� vanlig s�tt (filen du redigerade, inte
autosparfilen) och skriva M-x recover-file<Return>. N�r Emacs vill ha
bekr�ftelse svarar du yes<Return> f�r att �terst�lla filen.


* EKOOMR�DE
-----------

Om Emacs ser att du skriver kommandon l�ngsamt s� kommer de att visas
p� den nedersta raden i sk�rmbilden i ett omr�de som kallas
"ekoomr�de" (echo area). Detta omr�de inneh�ller den nedersta raden p�
sk�rmbilden.


* L�GESRADEN
------------

Raden precis �ver ekoomr�det kallas "l�gesrad" (mode line). Den ser
ungef�r ut s� h�r:

--:**  TUTORIAL       63% L749    (Fundamental)-----------------------

Raden inneh�ller information om Emacs och texten du redigerar.

Du vet redan vad filnamnet betyder, det �r den fil du har funnit. NN%
visar den aktuella positionen i texten, dvs. NN procent av texten
befinner sig �ver toppen av sk�rmbilden. Om toppen av filen �r i
sk�rmbilden kommer det st� "Top" ist�llet f�r " 0%" och om slutet av
filen �r i sk�rmbilden kommer det st� "Bot". Om du ser p� en fil d�r
hela texten passar in p� en sida kommer det st� "All".

Bokstaven L f�ljd av siffror anger positionen p� ett annat
s�tt. Siffrorna visar vilken rad som mark�ren befinner sig p�.

Stj�rnorna n�ra b�rjan av raden visar att det har skett f�r�ndringar i
filen sedan den sist blev sparad. N�r du precis har �ppnat en fil
kommer det inte st� n�got h�r, bara minustecken.

Den del av l�gesraden som st�r inom parentes visar vilket
redigeringsl�ge (mode) du anv�nder. Standardl�get �r "Fundamental",
som du anv�nder nu. Det �r ett exempel p� ett huvudl�ge (major mode).

Emacs har m�nga olika huvudl�gen. N�gra av dem �r gjorda f�r
redigering av olika programmeringsspr�k eller typer av text, till
exempel Lisp mode och Text mode. Det kan bara vara ett huvudl�ge �t
g�ngen och l�gesnamnet st�r alltid d�r det st�r Fundamental nu.

Varje huvudl�ge g�r att en del kommandon uppf�r sig annorlunda. Det
finns till exempel kommandon f�r att s�tta in kommentarer i programkod
och eftersom varje programmeringsspr�k har sitt s�tt att skriva
kommentarer p� s� m�ste de olika huvudl�gena s�tta in dessa
kommentarer p� olika s�tt. Varje huvudl�ge namnger ett utvidgat
kommando som anv�nds f�r att byte till det l�get. Till exempel �r M-x
fundamental-mode kommandot f�r att byta till huvudl�get Fundamental.

Om du skall redigera text, s�som den h�r filen, b�r du troligen
anv�nda Text-l�ge.

>> Skriv M-x text-mode <Return>.

Inget av kommandona du har l�rt dig hittills f�r�ndrar Emacs i n�gon
h�gre grad. Men l�gg m�rke till att M-f och M-b nu behandlar
apostrofer som en del av ord. Tidigare, i Fundamental mode, behandlade
M-f och M-b apostrofer som ordavskiljare.

Varje huvudl�ge g�r vanligtvis sm� f�r�ndringar som denna och de flesta
kommandon g�r samma sak i varje huvudl�ge, de fungerar bara lite
annorlunda.

F�r att f� fram dokumentationen f�r det l�ge du �r i nu kan du skriva
C-h m.

>> Anv�nd C-l C-l f�r att f� denna rad �verst p� sk�rmbilden.
>> Skriv C-h m och se hur Text-l�get skiljer sig fr�n
   Fundamental-l�get.
>> Tryck C-x 1 f�r att ta bort dokumentationen fr�n sk�rmbilden.

Huvudl�ge kallas s� f�r att det �ven finns sidol�gen (minor mode).
Ett sidol�ge ers�tter inte ett huvudl�ge, utan modifierar det. Varje
sidol�ge kan st�ngas av och p� oberoende av andra sidol�gen och
oberoende av huvudl�get. D�rf�r kan du anv�nda ett sidol�ge, en
kombination av flera sidol�gen eller inget sidol�ge alls.

Ett bra sidol�ge, speciellt f�r redigering av text, �r
radbrytningsl�get (auto-fill-mode). N�r detta l�ge �r p� bryter Emacs
rader mellan ord automatisk n�r du skriver in text s� att en rad blir
f�r l�ng.

Du kan sl� p� radbrytningsl�get genom att skriva M-x auto-fill-mode
<Return>. N�r l�get �r p�slaget kan du sl� av det igen genom att
upprepa M-x auto-fill-mode <Return>. Om l�get �r avslaget sl�r
kommandot p� det och vice versa. Vi s�ger att kommandot "v�xlar
l�get".

>> Skriv M-x auto-fill-mode <Return> nu. Skriv s� in en rad med
   "asdf " tills raden delar sig. Du m�ste s�tta in blanktecken, f�r
   Auto Fill bryter bara raden mellan ord.

Marginalen �r vanligtvis satt till 70 tecken men du kan �ndra detta
genom att anv�nda kommandot C-x f. Antalet tecken ges till kommandot
genom ett numeriskt argument.

>> Skriv C-x f med ett argument p� 20. (C-u 2 0 C-x f). Skriv sedan in
   n�gon text och l�gg m�rke till att Emacs bryter rader som �r l�ngre
   �n 20 tecken. S�tt tillbaka marginalen till 70 tecken igen, genom
   att anv�nda C-x f en g�ng till.

Om du g�r f�r�ndringar mitt i en rad s� kommer inte sidol�get Auto
Fill att kunna omformattera raderna f�r dig.
F�r att g�ra detta kan du trycka M-q med mark�ren inne i det avsnittet
du �nskar att omformatera.

>> Flytta mark�ren in i f�reg�ende stycke och tryck M-q.


* S�KNING
---------

Emacs kan s�ka efter textstr�ngar (en "str�ng" �r en grupp med
sammanh�ngande bokst�ver) antingen fram�t eller bak�t i texten. N�r du
s�ker efter text kommer mark�ren att flytta sig till n�sta plats d�r
teckenstr�ngen upptr�der.

S�kmetoden i Emacs �r inkrementell. Detta betyder att s�kandet fortg�r
medan du skriver in teckenstr�ngen du skall s�ka efter.

Kommandot f�r att inleda en s�kning �r C-s f�r att s�ka fram�t och C-r
f�r att s�ka bak�t. MEN V�NTA! Prova dem inte �n.

N�r du skriver C-s kommer du l�gga m�rke till att texten "I-search"
dyker upp i eko-omr�det. Detta s�ger dig att Emacs �r inne i sidol�get
inkrementell s�kning och v�ntar p� att du skall skriva in det du letar
efter. <Return> avslutar s�kandet.

>> Skriv nu C-s f�r att starta en s�kning. Skriv nu l�ngsamt, en
   bokstav i taget, ordet 'mark�r', och g�r en paus efter varje g�ng
   du skriver en bokstav s� att du ser vad som sker med mark�ren. Nu
   har du s�kt efter ordet "mark�r" en g�ng.
>> Skriv C-s en g�ng till f�r att s�ka efter n�sta f�rekomst av ordet
   "mark�r".
>> Tryck nu p� <DEL> fyra g�nger och se hur mark�ren flyttar sig
>> Tryck <Return> f�r att avsluta s�kandet.

S�g du vad som h�nde? Under inkrementell s�kning f�rs�ker Emacs att g�
till den f�rsta f�rekomsten av texten som du har skrivit s� l�ngt, och
markerar tr�ffen s� att du ser var den �r. F�r att g� till n�sta
f�rekomst av ordet 'mark�r' �r det bara att trycka C-s en g�ng till.
Om det inte finns flera f�rekomster kommer Emacs att pipa och meddela
att s�kandet har misslyckats. C-g avbryter ocks� s�kandet.

Om du �r inne i en inkrementell s�kning och trycker <DEL> kommer den
s�kningen att �terg� till en tidigare plats. Om du skriver <DEL>
precis efter att du skrivit C-s f�r att g� till n�sta f�rekomst av
s�kstr�ngen, kommer <DEL> att flytta mark�ren tillbaka till en
tidigare f�rekomst. Om det inte finns n�gra tidigare f�rekomster s�
raderar <DEL> sista tecknet i s�kstr�ngen. Om du till exempel skriver
"m" f�r att s�ka efter den f�rsta f�rekomsten av "m", och sedan
trycker "a" s� kommer mark�ren flytta sig till f�rsta f�rekomsten av
"ma". Tryck nu <DEL>. Detta avl�gsnar "a" fr�n s�kstr�ngen, och
mark�ren flyttar sig tillbaka till den f�rsta f�rekomsten av "m".

Om du �r mitt i en s�kning och trycker ett kontroll- eller meta-tecken
s� avbryts s�kandet. Undantag �r tecken som anv�nds under s�kningen,
s� som C-s och C-r.

C-s startar en s�kning som letar efter varje f�rekomst av s�kstr�ngen
EFTER mark�rspositionen. Om du skall s�ka efter en str�ng tidigare i
texten m�ste du anv�nda C-r. Allt vi har sagt om C-s g�ller ocks� f�r
C-r, bortsett fr�n att riktningen p� s�kningen �r den omv�nda.


* FLERA F�NSTER
---------------

En av egenskaperna hos Emacs �r att den kan visa mera �n en buffert p�
sk�rmen samtidig. (Notera att Emacs anv�nder termen "ramar"
(frames), som beskrivs i n�sta kapitel, f�r det som en del andra
program kallar f�r "f�nster" (windows). Emacs-manualen inneh�ller en
ordlista �ver Emacs-termer.

>> Flytta mark�ren till den h�r raden och tryck C-l C-l.

>> Skriv nu C-x 2, som leder till att sk�rmen delas i tv�
   f�nster. B�gge f�nstren visar den h�r v�gledningen. 
   Redigeringsmark�ren stannar i det �vre f�nstret.

>> Skriv C-M-v f�r att rulla det nedre f�nstret.
   (Om du inte har META-tangenten trycker du <ESC> C-v.)

>> Skriv C-x o ("o" f�r "other") f�r att flytta mark�ren till det
   nedre f�nstret.
>> Anv�nd C-v och M-v i det nedre f�nstret f�r att flytta upp
   och ned i texten. Forts�tt att l�sa den h�r texten i det �vre
   f�nstret.

>> Skriv C-x o igen f�r att flytta mark�ren tillbaka till det �vre
   f�nstret. Mark�ren i det �vre f�nstret st�r p� samma plats som det
   gjorde n�r du l�mnade det.

Du kan forts�tta att anv�nda C-x o f�r att byta mellan de tv�
f�nstren. Det valda f�nstret, d�r de flesta redigeringarna �ger rum, �r
det med den tydligaste mark�ren, som blinkar n�r du inte skriver. De
andra f�nstren har sin egen mark�rposition. Om du k�r Emacs under ett
f�nstersystem, ritas dessa mark�rer som en tom ruta som inte blinkar..

Kommandot C-M-v �r bra n�r du redigerar text i ett f�nster och
anv�nder det andra f�nstret f�r referenser. Utan att l�mna det valda
f�nstret du kan anv�nda C-M-v f�r att rulla det andra f�nstret.

C-M-v �r ett exempel p� en KONTROLL-META-kombination. Om du har META-
eller Alt-tangenten h�ller du b�de KONTROLL och META nedtryckt
samtidigt som du trycker v. Det har ingen betydelse vilken av
tangenterna KONTROLL och META som trycks f�rst, f�r b�gge fungerar s�
att de "modifierar" de andra tangenterna du trycker.

Om du inte har META-tangenten och anv�nder <ESC> ist�llet �r
ordningsf�ljden viktig. Du m�ste trycka <ESC> f�ljt av KONTROLL-v,
KONTROLL-<ESC> v fungerar inte. Det �r f�r att <ESC> �r ett tecken i
sig och inte en �kta "modifierare".

>> Skriv C-x 1 i det �vre f�nstret f�r att bli kvitt det nedre
   f�nstret.

Om du hade skrivit C-x 1 i det nedre f�nstret skulle det �vre ha
f�rsvunnit. T�nk p� detta kommando som "Beh�ll bara ett f�nster, det
som mark�ren st�r i."

Du m�ste inte ha samma buffert i b�gge f�nstren. Du kan anv�nda C-x
C-f f�r att finna en ny fil i ett av f�nstren samtidigt som det andra
f�nstret f�rblir of�r�ndrat. Du kommer att m�rka att f�nstren �r helt
oberoende.

H�r �r ett annat s�tt att anv�nda tv� f�nster till att visa tv� olika
filer:

>> Skriv C-x 4 C-f f�ljt av ett filnamn. Avsluta med <Return>.
   Den nya filen kommer d� att dyka upp i det nedre f�nstret.
   Mark�ren flyttats ocks� dit.

>> Skriv C-x o f�r att g� tillbaka till det �vre f�nstret och C-x
   1 f�r att bli kvitt det nedre igen.

* MULTIPLA RAMAR
----------------

Emacs kan ocks� skapa flera "ramar".  En ram �r vad vi kallar en
samling av f�nster tillsammans med menyer, rullningslister, ekoomr�de
etc. Det som Emacs kallar f�r ram kallar de flesta andra program f�r
f�nster. Flera grafiska ramar kan visas p� sk�rmen samtidigt. P� en
textterminal kan bara en ram visas �t g�ngen.

>> Skriv M-x make-frame <Return>.
   En ny ram visas p� din sk�rm.

Du kan g�ra allt du gjorde i den f�rsta ramen i den h�r nya ramen. Det
finns inget speciellt med den f�rsta ramen.

>> Skriv M-x delete-frame <Return>.
   Ta bort den valda ramen.

Du kan ocks� ta bort ramen genom den vanliga metod som tillhandah�lls
av f�nstersystemet (ofta klickar man p� knappen med symbolen "X" i
n�got av de �vre h�rnen.) Om den sista ramen tas bort p� det h�r
s�ttet s� avlutas Emacs.

* REKURSIVA REDIGERINGSNIV�ER
-----------------------------

Ibland kan du hamna i n�got som kallas "rekursiv redigering"
(recursive editing level). Detta indikeras med hakparenteser runt
huvudl�get i l�gesraden. Till exempel kan det st� [(Fundamental)]
ist�llet f�r (Fundamental).

F�r att komma ur rekursiv redigering trycker du <ESC> <ESC> <ESC>.
Detta �r ett generellt brytkommando. Du kan ocks� anv�nda det f�r att
bli kvitt extra f�nster och f�r att komma ut ur minibufferten.

>> Skriv M-x f�r att komma in i minibufferten. Skriv s� <ESC> <ESC>
   <ESC> f�r att komma ut.

Du kan inte anv�nda C-g f�r att komma ut ur rekursiv redigering.
Detta �r f�r att C-g anv�nds f�r att avbryta kommandon och argument
under rekursiv redigering.


* MER HJ�LP
-----------

I denna v�gledning har vi f�rs�kt inkludera precis s� mycket
information att du kan b�rja anv�nda Emacs. Det finns s� m�nga
m�jligheter i Emacs att det skulle vara om�jligt att f�rklara alla
h�r. Men du VILL s�kert l�ra dig mer om Emacs eftersom den har m�nga
goda egenskaper. Emacs tillhandah�ller kommandon f�r att l�sa all
dokumentation. Dessa hj�lpkommandon startas med teckenkombinationen
C-h.

F�r att anv�nda hj�lpen skriver du C-h f�ljt av ett tecken f�r den
hj�lp du beh�ver. Om du verkligen �r helt villr�dig kan du trycka C-h
? f�r att Emacs skall visa vilken hj�lp som finns tillg�nglig. Om du
har skrivit C-h och best�mmer dig f�r att du inte beh�ver ha n�gon
hj�lp kan du trycka C-g f�r att avbryta.

(Om C-h inte visar ett hj�lpmeddelande l�ngst ner p� sk�rmen, kan du i
st�llet f�rs�ka med funktionstangenten F1 eller M-x help <Return>.)

Den mest grundl�ggande hj�lp-funktionen �r C-h c. Skriv C-h, ett "c"
och en knappsekvens. Emacs ger d� en beskrivning av kommandot.

>> Skriv C-h c C-p.

Meddelandet skall d� bli n�got i stil med:

        C-p runs the command previous-line

Detta ger dig namnet p� funktionen. Eftersom funktionerna har
beskrivande namn kan de ocks� fungera som en enkel dokumentation,
tillr�ckligt f�r att p�minna dig om kommandon du redan l�rt dig.

Flerteckenskommandon, s� som C-x C-s och (om du inte har META, EDIT
eller ALT tangenten) <ESC> v, �r ocks� till�tna efter C-h c.

F�r att f� mer information om ett kommando kan du anv�nda C-h k
ist�llet f�r C-h c.

>> Skriv C-h k C-p.

Detta kommer visa funktionens dokumentation och namn i ett eget
f�nster. F�r att avsluta hj�lpf�nstret kan du trycka C-x 1. Du beh�ver
inte g�ra det omedelbart. Du kan editera med hj�lptexten som st�d f�r
att f�rst senare ta bort f�nstret med C-x 1.

H�r �r fler varianter p� C-h:

   C-h f        Beskriv en funktion. Du skriver in funktionsnamnet.

>> Prova att skriva C-h f previous-line<Return>.
   Detta ger den information Emacs har om funktionen
   som implementerar kommandot C-p.

Ett liknande kommando, C-h v, visar dokumentationen f�r de variabler
som du kan �ndra v�rde p� f�r att anpassa Emacs beteende. Du m�ste
ange namnet p� variabeln n�r Emacs fr�gar efter den.

   C-h a        Kommando-aprop� (Command Apropos). Skriv in ett
		nyckelord och Emacs listar all kommandon vars namn
		inneh�ller det nyckelordet. Alla dessa
                kommandon kan aktiveras med META-x. F�r n�gra
                kommandon listas ocks� de kombinationer best�ende av
                en eller tv� tecken som k�r det kommandot.

>> Skriv C-h a file<Return>.

Detta visar i ett annat f�nster en lista �ver alla M-x kommandon d�r
"file" f�rekommer i kommandonamnet. Du kommer se teckenkommandon som
C-x C-f listade bredvid motsvarande kommandonamn, t.ex. find-file.

>> Tryck C-M-v f�r att rulla texten i hj�lpf�nstret. G�r det n�gra
   g�nger.

>> Type C-x 1 f�r att ta bort hj�lpf�nstret.

   C-h i        L�s den bifogade manualen (alias Info). Detta kommando
                placerar dig i en speciell buffer vid namn "*info*"
                d�r du kan l�sa hj�lpen f�r de paket som �r
                installerade i ditt system. Sl� m emacs <Return> f�r
                att l�sa Emacs-manualen. Om du aldrig tidigare har
                anv�nt dig av Info, skriv ? och Emacs tar dig p� en
                guidad tur �ver Infol�gets (Info-mode) m�jligheter.
                N�r du v�l har tagit dig igenom den h�r
                anv�ndarhandledningen s� �r det direktmanualen som �r
                din huvudsakliga k�lla till dokumentation.


* MER FUNKTIONER
----------------

Du kan l�ra dig mer om Emacs genom att l�sa dess manual, antingen i
bokform eller on-line i Info (anv�nd Hj�lp-menyn eller skriv F10 h r).
Tv� finesser som du kan komma att gilla speciellt �r komplettering
(completion), som spar tangenttryckningar, och dired, som f�renklar
filhantering.

Komplettering �r ett s�tt att undvika on�diga tangenttryckningar. Till
exempel, om du vill byta till *Messages* bufferten, kan du du skriva
C-x b *M<Tab> och Emacs kommer fylla i resten av buffertnamnet s�
l�ngt den kan r�kna ut det fr�n det du redan skrivit. Komplettering
finns beskrivet i Emacs-manualen i noden "Completion".

Dired g�r det m�jligt att lista filer i en katalog (och �ven dess
subkataloger), flytta runt i listan, bes�ka, byta namn, ta bort och
operera p� olika s�tt p� filerna. Dired finns beskrivet i Info i
Emacs-manualen i noden "Dired".

Manualen beskriver �ven m�nga andra Emacs funktioner.

* SLUTORD
---------

F�r att avsluta Emacs anv�nd C-x C-c.

Den h�r handledningen �r t�nkt att vara f�rst�elig f�r alla nya
Emacs-anv�ndare. S� om det �r n�got som �r oklart, klandra inte dig
sj�lv, klaga!


* KOPIERING
-----------

Denna v�gledning h�rstammar fr�n en hel rad Emacs-v�gledningar och den
f�rsta skrevs av Stuart Cracraft f�r den ursprungliga Emacs. Mats
Lidell �versatte den till Svenska.

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
