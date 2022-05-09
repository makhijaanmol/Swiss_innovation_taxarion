patent_inventors <- patent_inventors %>% 
  mutate(Canton = case_when(
    (city %in% c("Zurich", "Adliswil", "Bulach", "Monchaltdorf", "Adiswil", "Affoltern", "Affoltern a./A.", 
                 "Affoltern A/Albis", "Affoltern am Albis", "Albis", "Au", "Baretswil", "Baslerstrasse", 
                 "Bauma", "Benglen", "Birmensdorf", "Bisikon", "CH-8001 Zurich", "CH-8006 Zurich", 
                 "CH-8008 Zurich", "CH-8049 Zurich", "CH-8335 Hittnau", "CH-8413 Neftenbach", 
                 "CH-8442 Hettlingen", "CH-8610 Uster", "CH-8635 Oberdurnten", "CH-8802 Kilchberg",
                 "CH-8812, Horgen", "CH-8932 Mettmenstetten", "Dietikon", "Dietlikon", "Durnten", "Effretikon", "Egg",
                 "Embrach", "Erlenbach", "Flurlingen", "Gattikon", "Glattbrugg", "Glattfelden", "Gr ut", "Greifensee", 
                 "Grueningeu", "Gruningen", "Grut", "Herrliberg", "Hettlingeg", "Hettlingen", "Hinwil", "Hittnau", 
                 "Hittnau CH-8335", "Horgan", "Horgen", "Hori", "Huettikon", "Huttikon",  "K usnacht", "Kilchberg", 
                 "Klichberg", "Knonau", "Kuesnacht", "Kusnacht", "Langnau a. Albis", "Langnau a.A.", "Langnau am Albis", 
                 "Langnau Am Albis", "Langwiesen", "late of Horgen", "Leigruppenstrasse", "Maennedorf", "Marthalen",
                 "Mettmenhasli", "Mettmenstetten", "Neftenbach", "Niederdorf", "Niederhalsi", "Nussbaumen", 
                 "Ober-Durnten", "Oberduernten", "Oberdurnten", "Oberengstringen", "Oberweningen", "Opfikon", 
                 "Pfaeffikon", "Pfungen", "Raterschen", "Richterswil", "Rickenbach", "Riedikon", "Rueschilikon", 
                 "Rueschilkon", "Rueschliken", "Rueschlikon", "Ruschlikon", "Russchliken", "Russchlikon", "Russikon", 
                 "Ruti", "Saland", "Pfäffikon", "Schliern", "Schoenenberg", "Schutzengasse", "Stafa", "Steinmaur", 
                 "Thalwil", "Thawil", "Uitikon", "Uster", "Volken", "Volketswil", "Wadenswil", "Wadenswill", 
                 "Waedenswil", "Weisslingen", "Wernetshausen", "Wettswil", "Wettswil a. Albis", "Wetzikon", 
                 "Wiesendangen", "Wil", "Wil/ZH", "Wil/Zunich", "Winterhur", "Winterthur", "Wintherthur", "Wolfhausen",
                 "Zollikerberg", "Zollikon", "Zumiken", "Zumikon", "Zumilon", "Zuric", "Zurich", "Zurich CH-8046", 
                 "Zurich-Alstetten", "Sandgrubenstrasse", "Richterswill", "Niederhasli", "Richterswill", 
                 "8006 Zurich", "8049 Zurich", "8053 Zurich", "8057, Zurich", "8180 Bulach", "8617 Monchaltdorf", 
                 "Asliswil", "Adliswill")) ~ "Zurich",
    
    (city %in% c("Bern", "Berne", "Orvin", "Aarberg", "Belp", "Biel", "Biel Bienne", "Bienne", "Bolligen", 
                 "Bremgarten b. Bern", "Bueren", "Bulach", "Buren", "Burgdorf", "CH-3011 Bern", "Corcelles", 
                 "Fraubrunnen", "Grosshochstetten", "Grosshoechstetten", "Herzogenbuchsee", "Hinterkappelen", "Hunibach",
                 "Ins", "INS", "Jedenstorf", "Jegensdorf", "Kernenried", "Kirchdorf", "Koniz", "Konolfingen", "Langenthal",
                 "Langnan", "Langnau", "late of Berne", "Le Paradis 43", "Leimen", "Les Russilles", "Liebefeld", 
                 "Liebefeld-Bern", "Lotzwil", "Lutzelfluh", "Luzelfluh", "Lysse", "Mattstetten", "Muenchringen", 
                 "Muensingen", "Muensingen", "Muentschemier", "Munchringen", "Munsingen", "Muntschemier", 
                 "Neuenegg", "Niederscherli", "Orvin", "Saint-Imier", "Schliern bei Koniz", "Schonbuhl", 
                 "Schonbuhl-Urtenen", "Signau", "St-Imier", "St-Jodelweg", "Studen", "Thorishaus", "Thun", "Toffen",
                 "Tschugg", "Urtenen", "Utzenstorf", "Wabern", "Wattenwil", "Worb", "Zollikofen", "Schonbush-Urtenen", 
                 "2534 Orvin", "Kirchberg", "Jegenstorf", "Utzenstrof")) ~ "Bern",
    
    (city %in% c("Meggen", "Asch", "Baech", "Ballwil", "CH-6045 Meggen", "CH-6294 Ermensee", "Ebikon", "Horw", "Kriens",
                 "Lucerne", "Luzem", "Luzern", "Luzerne", "Meggen", "Vitznau", "6045 Meggen")) ~ "Luzern",
    
    (city %in% c("Buerglen", "Burglen", "Gross")) ~ "Uri",
    
    (city %in% c("CH-8832 Wollerau SZ", "Einsiedeln", "Feusisberg", "Huobstrasse", "Reichenburg", "Schindellegi", 
                 "Siebnen", "Wangen", "Wilen b. Wollerau", "Wolferau", "Wollerau")) ~ "Schwyz",
    
    (city %in% c("Alpnach Dorf", "Kerns")) ~ "Obwalden",
    
    (city %in% c("Stansstad", "Stanstaad")) ~ "Nidwalden",
    
    (city %in% c("Glarus", "CH-8750 Glarus")) ~ "Glarus",
    
    (city %in% c("Baar", "CH- 6315 Oberageri", "CH-6315 Oberageri", "CH-6330 Cham", "Cham", "Cham, CH-6330", 
                 "Guntenbuhl", "Hagendorn", "Huenenberg", "Hunenberg", "Menzingen", "Oberaegeri", "Oberageri", 
                 "Steinhausen", "Steinhausen-ZG", "Unteraegeri", "Zug")) ~ "Zug",
    
    (city %in% c("Fribourg", "Chatel-St-Denis", "Marly", "Praroman", "Bourguillon", "CH-1618 Chatel-St-Denis",
                 "CH-1756 Onnens", "CH-1756 Onnens FR", "Chatel St-Denis", "Chatonnaye", "Chavannes-sous-Orsonnens",
                 "Cheyres", "Cormerod", "Courgevaux", "Courtaman", "Courtepin", "Dudingen", "Farvagney-le-Petit", 
                 "Fribourg", "Giffers", "Kleinbosingen", "Kleinschonberg", "Les Paccots", "Les Rasalys", "Marly", 
                 "Marty", "Meyriez", "Neyruz", "Praromah", "Praroman", "Praroman-Le Mouret", "Saint-Aubin", 
                 "St. Antoni", "Uberstorf", "Ueberstorf", "Villar-sur-Glane", "Villars-sur-Glane", 
                 "Villarsel sur Marly", "Villarsel-Sur-Marly", "Wallenried", "Farvagny-le-Petit", 
                 "1425 Onnens", "1618 Chatel-St-Denis", "1700 Fribourg 4", "1723 Marly", "1724 Praroman", 
                 "Farvagny-le Petit")) ~ "Fribourg",
    
    (city %in% c("Bellach", "Bettlach", "Biezwil", "CH-4206 Seewen", "Dornach", "Fehren", "Flueh", "Grenchen", "Gretzenbach",
                 "Hochwald", "Hochwald SO", "Horriswil", "Kappel", "Kyburg-Buchegg", "Langendorf", "Lommiswil", 
                 "Luterbach", "Nennigkofen", "Nugiar", "Nuglar", "Nugler", "Oekingen", "Olten", "Rodersdorf", 
                 "Solothurn", "Wangen b. Olten", "Witterswil", "Witterswil 4108", "Zuchwil", "Zuchwill")) ~ "Solothurn",
    
    (city %in% c("Basel", "Bettingen", "Basle", "CH-4053 Basel", "CH-4058 Basel", "Richen", "Riechen", "Riehen", 
                 "W-4057 Basel", "4126 Bettingen", "Basel 4054")) ~ "Basel-Stadt",
    
    (city %in% c("Binningen", "Oberwil", "Munchenstein", "Aesch", "Allschwil", "Arlesheim", "Bennwil", 
                 "Biel-Benken", "Binningen", "Birsfelden", "Bottminger", "Bottmingen","CH-4105 Biel-Benken",
                 "CH-4106 Therwil", "CH-4434 Holstein", "Dietgen", "Ettingen", "Frenkendorf", "Fullinsdorf", "Giebenach", 
                 "Hegenheim", "Holstein", "Kaenerkinden", "Kanerkinden", "Lausen", "Liestal", "M unchenstein", 
                 "Maisprach", "Malsprach", "Muchenstein", "Muenchenstein", "Munchenstein", "Munchestein", "Muttenz", 
                 "Oberdorf", "Oberwil", "Oberwil BL", "Oberwils", "Overwil", "Pfeffingen", "Pratteln", "Reinach", 
                 "Reinach BL", "Schonenbuch", "Selitsberg", "Therwil", "Seltisberg", "Allschwill", "4102 Binningen",
                 "4104 Oberwil", "4142 Munchenstein")) ~ "Basel-Land",
    
    (city %in% c("Beringen", "CH-8234 Stetten", "Doerfingen", "Neuhausen", "Schaffhausen", "Stein am Rhein" 
    )) ~ "Schaffhausen",
    
    (city %in% c("Platz-Walzenhausen")) ~ "Appenzell A.Rh.",
    
    # (city %in% c()) ~ "Appenzell I.Rh.",
    
    (city %in% c("Alstatten", "Altstatten", "Azmoos", "Bad Ragaz", "Berneck", "Buchs", "Fontnas", "Gossau", "Heerbrugg",
                 "Jonschwil", "Mels", "Niederuzwil", "Plons", "Rieden", "St. Gallen", "Tufistrasse", "Ulisbach",
                 "Wattwil", "Widnau")) ~ "St.Gallen",
    
    (city %in% c("Alvaneu-Dorf", "CH-7203 Trimmis", "CH-7270 Davos", "CH-7412 Scharans", "Chur", "Davos", "Davos Dorf",
                 "Davos Frauenkirch", "Davos Paltz", "Davos-Platz", "Frauenkirch", "Scharans", "Silvaplana", 
                 "Trimmis", "Trin", "Davos Platz")) ~ "Graubünden",
    
    (city %in% c("Aarau", "Magden", "Ammerzwil", "Baden", "Baden-Rutihof", "Bellikon", "Berikon",
                 "Besenburen", "Besenbuven", "Bremgarten", "Brittnau", "Brugg", "Buttikon", 
                 "CH-4310 Rheinfelden", "CH-5070 Frick", "CH-5400 Baden", "Endingen", "Ennetbaden", "Erlinsbach", 
                 "Fislisbach", "Frick", "Gebenstorf", "Gipf-Oberfrick", "Gipf-Oerfrick", "Haegelerstrasse", 
                 "Hunzenschwil", "Klingnau", "Kolliken", "Kunten", "Lengnau", "Lenzburg", "Magden", "Mandach", 
                 "Moehlin", "Mohlin", "Mutschellen", "Neuenhof", "Niederlenz", "Niederrohrdorf", "Oberbozber", 
                 "Oberbozberg", "Oberlunkhofen", "Rementschwil", "Remetschwil", "Remwtschwil", "Rheinfelden", 
                 "Rieden-Nussbaumen", "Riniken", "Rothrist", "Ruetihof", "Rutihof", "Schmiedrued", "Staffelbach", 
                 "Stafferbach", "Stein", "Stetten", "Stilli", "Sulz", "Tegerfelden", "Unterbozberg", "Untererendingen",
                 "Untersiggenthal", "Villmergen", "Waltenschwil", "Wettingen", "Wohlenschwil", "Wuerenlingen", 
                 "Wurenlingen", "Zofingen", "4312 Magden", "Brewgarten")) ~ "Aargau",
    
    (city %in% c("Amriswil", "Arbon", "Arborn", "Balterswil", "Basadingen", "Berlingen", "Bottighofen", 
                 "CH-8572 Berg/TG", "CH-Arborn", "Frauenfeld", "Gerlikon", "Guttingen", "Hor", "Mattwil", "Ottoberg",
                 "Romansborn", "Schoenenberg an der Thur", "Stettfurt", "Taegerwilen", "Tagerwilen", "Tragerwilen",
                 "Romanshorn")) ~ "Thurgau",
    
    (city %in% c("Caslano", "Figino", "Lugano", "Ticino", "Viganello", "Villa Wellingtonia", "Romanshorn")) ~ "Ticino",
    
    (city %in% c("Lausanne", "Renens", "Saint-Sulpice", "Preverenges", "Morrens", "Chexbres", "Morges", 
                 "Onnens", "Aclens", "Assens", "Baulmes", "Begnins", "Belmont", "Belmont S/Lausanne",
                 "Belmont sur Lausanne", "Belmont Sur Lausanne", "Belmont-sur-Lausanne", "Berolle", 
                 "Blonay", "Boussens", "Bussigny", "Bussigny-pres-Lausanne", "Caudoz", "CH - 1066 Epalinges",
                 "CH 1025 Saint Sulpice", "CH-1801 Le Mont Pelerin VD", "CH-1801 Le Mont-Pelerin",
                 "CH-1806 St-Legier-La Chiesaz", "CH1000 Lausanne 9", "Chardonne", "Chavannes", "Chavannes de Bogis",
                 "Chavannes De Bogis", "Chavannes De Borgis", "Chavannes-Pres-Renens", "Chavornay", "Chemin de Plantaz",
                 "Chexbres", "Coppet", "Corbeyrier", "Corseaux", "Cossonay", "Cuarnens", "Denens", "Des Planches",
                 "Echallens", "Echandens", "Eclepens", "Ecublens", "Epalinges", "Faunex", "Ferreyres", "Founex",
                 "Froideville", "Gland", "Grande-Fin", "Jouxtens-Mezery", "La Conversion", "La Conversion s. Lutry",
                 "La Tour-de-Peilz", "Lausanne", "Lausanne CH-1015", "Le Mont Pelerin", "Le Mont Pelerin VD", 
                 "Le Mont sur Lausanne", "Le Mont-Pelerin", "Les Charbonnieres", "Les Muriers", "Lonay", "Lutry",
                 "Mex", "Morges", "Nyon", "Ollon", "Ollon/VD", "Onnens", "Oron la Ville", "Pallaigues", "Penthaz", 
                 "Pompaples", "Preverange", "Preverenges", "Preverenges VD", "Preverengrs", "Prilly", "Pully", 
                 "Renens", "Renens VD", "Reuschlikon", "Rolle", "Romanel", "Romanel-sur-Morges", "Ropraz", 
                 "Saint-Sulpice", "Savigny", "St Sulpice",  "St-Legier", "St-Legier-La Chiesa", "St-Legier-la Chiesaz", 
                 "St-Legier-La Chiesaz", "St-Legiera-la Chiesaz", "St-Sulpice", "St. Legier-la Chiesaz", "St. Sulpice", 
                 "St.-Legier-la-Chiesaz", "St.-Sulpice", "Sulpice", "Tannay", "Vand", "Vaud", "Vaux s/ Morges",
                 "Vaux sur Morges", "Vaux Sur Morges", "Vevey", "Villette", "Vuillerens", "Yverdon", 
                 "Yverdon les Pains", "Yverdon-les-bains", "Yvonand", "La Tour-De-Peilz", "Vullierens", 
                 "1005 Lausanne", "1006 Lausanne", "1010 Lausanne", "1020 Renens VD", "1025 Saint-Sulpice", 
                 "1028 Preverenges Vaud", "1054 Morrens", "1071 Chexbres", "1110 Morges", "Le Mont Sur Lausanne", 
                 "St-Legier-la-Chiesaz", "St-Legier-La-Chiesaz")) ~ "Vaud",
    
    (city %in% c("Baltschieder", "Brig-Glis", "Grimisuat", "Heremence", "Le Bouveret", "Saviese", "Sion")) ~ "Valais",
    
    (city %in% c("Neuchatel", "Auvernier", "Bevaix", "Bole",  "CH-2300 La Chaux-de-Fonds", "CH-2400 Le Locle",
                 "Chez-le-Bart", "Chezard", "Colombier", "Cornaux", "Enges", "Fontaines", "Fountainemelon",
                 "Haulerive", "La Chaux-de-Fonds", "Le Landeron", "Le Locle", "Lignieres", "Marin", "Marin-Epagnier", 
                 "Neucahtel", "Neuch&acirc ;tel", "Neuchatel", "Neuenburg", "Peseux", "Ruedes Jeanneret", 
                 "Saint-Blaise", "Vignes Perdues", "Wavre", "2000 Neuchatel", "2007 Neuchatel")) ~ "Neuchätel",
    
    (city %in% c("Geneva", "Chene-Bougeries", "Choulex", "Carouge", "CH-1202 Geneva", "CH-1215 Geneva 15",
                 "Chene-Bourge", "Collex", "Collex/Geneva", "Cologny", "Geneva", "Geneve", "Grand-Lancy", "Meyrin", 
                 "Meyrin-Geneva", "Onex", "Satigny", "Trionex", "Trolnex", "Versoix", "Vessy", "Troinex", 
                 "1028 Geneva", "1205 Geneva", "1207 Geneva", "1224 Chene-Bougeries", "1244 Choulex")) ~ "Genève",
    
    # (city %in% c()) ~ "Jura"
  ))

# Drop records that cannot be assigned
patent_inventors <- patent_inventors %>% 
  drop_na(Canton)


# Notes for assignment script:
# patent id 6304815, 6897222 marked as Switzerland country but in the German city of Aachen, Loerrach
# patent id 5929146 is in London but marked as Swiss
# patent id 6157643, 6770923, 6770923, 6442007, 6474723, 6641208, 6928736, have been filed in China but misclassified as Swiss patents
# location of patent id 6485492 unclear - the recorded location is not in Switzerland
# location of patent id 5738825 unclear - the recorded location is not in Switzerland
# location of patent id 5676525 unclear - the recorded location is not in Switzerland
# location of patent id 6979335 unclear - the recorded location is not in Switzerland
# location of patent id 6395558 unclear - the recorded location is not in Switzerland
# location of patent id 5953657unclear - the recorded location is not in Switzerland it is in Sweden
# location of patent id 5838142, 5738214 - the recorded location appears to be inventors last name
# location of patent id 6356068 unclear - lake bordering 3 cantons listed as city
# location of patent id 6405934, 6307814, 6405934 unclear- Assigned to Neuchätel since most likely 
# location of patent id 6724098 unclear - city listed as madrid country Switzerland (Assignee Zurich based co.)
# One city is recorded as Alstatten instead Altstatten 
# Arbon maybe mis-recorded as Arborn
# Adliswil maybe mis-recorded as Asliswil
# Basel misspelt as Basle
# Bern misspelt as Berne
# Chavannes De Bogis  misspelt as Chavannes De Borgis
# Bremgarten possibly misspelt as Brewgarten in patent id 7597707
# Baslerstrasse, Benglen appear to be a streets in Zurich mis-recorded as cities
