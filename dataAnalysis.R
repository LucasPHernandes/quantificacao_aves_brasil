
# PREPARAÇÃO DOS DADOS

install.packages("tidyverse")
install.packages("stringr")
library(tidyverse)
library(stringr)

# Exemplo de como criar coluna temporária e exclui-la: 
# ES_2005$Temporario[ES_2005$ESPÉCIE == "Tinamus solitarius (Vieillot, 1819)"] = "Tinamus solitarius"
# ES_2005$Temporario <- NULL
# FIM EXEMPLO


# Nomes CBRO_2021:
names_CBRO <- read.csv("Nomes Padronizados.tsv", sep = "\t", stringsAsFactors = FALSE)

# BLOCO MINAS GERAIS (MG)


MG <- read.csv("MG.csv", sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)

# Cria nova coluna "novo_nome" com dados da CBRO após comparação dos nomes "não oficiais"...
MG$CBRO_2021 <- names_CBRO$CBRO[match(MG$Espécie, names_CBRO$Nomes)]
#view(mg_birds)

MG$TAXON <- MG$Espécie
MG$CATEGORIA_AMEACA <- MG$Categoria.de.Ameaça
MG$NIVEL <- "Minas Gerais"
MG$ANO <- 2010
MG$FONTE <- "DELIBERAÇÃO NORMATIVA COPAM Nº 147, DE 30 DE ABRIL DE 2010"
MG$LINK <- "http://www.ief.mg.gov.br/images/stories/biodiversidade/deliberao_normativa_copam_n147.pdf"

# FIM BLOCO MINAS GERAIS


# BLOCO ESPIRITO SANTO (ES):
ES_2005 = read.csv("ES_2005.tsv", sep = "\t")
ES_2022 = read.csv("ES_2022.tsv", sep = "\t")


# Coluna TEMP. Recebe nomes com parentesis [ES_2022]
ES_2022$TEMP <- ES_2022$ESPÉCIE.OU.SUBESPÉCIE # Copia coluna original para a "TEMP"
ES_2022$TEMP[ES_2022$TEMP == "Anous minutus Boie, 1844"] = "Anous minutus (Boie, 1844)"
ES_2022$TEMP[ES_2022$TEMP == "Anthus hellmayri Hartert, 1909"] = "Anthus hellmayri (Hartert, 1909)"
ES_2022$TEMP[ES_2022$TEMP == "Anthus hellmayri Hartert, 1909"] = "Anthus hellmayri (Hartert, 1909)"
ES_2022$TEMP[ES_2022$TEMP == "Celeus flavus subflavus Sclater & Salvin, 1877"] = "Celeus flavus subflavus (Sclater & Salvin, 1877)"
ES_2022$TEMP[ES_2022$TEMP == "Chelidoptera tenebrosa brasiliensis Sclater, 1862"] = "Chelidoptera tenebrosa brasiliensis (Sclater, 1862)"
ES_2022$TEMP[ES_2022$TEMP == "Cichlopsis leucogenys Cabanis, 1851"] = "Cichlopsis leucogenys (Cabanis, 1851)"
ES_2022$TEMP[ES_2022$TEMP == "Crax blumenbachii Spix, 1825"] = "Crax blumenbachii (Spix, 1825)"
ES_2022$TEMP[ES_2022$TEMP == "Cyanoloxia brissonii sterea Oberholser, 1901"] = "Cyanoloxia brissonii sterea (Oberholser, 1901)"
ES_2022$TEMP[ES_2022$TEMP == "Falco deiroleucus Temminck, 1825"] = "Falco deiroleucus (Temminck, 1825)"
ES_2022$TEMP[ES_2022$TEMP == "Merulaxis ater Lesson, 1831"] = "Merulaxis ater (Lesson, 1831)"
ES_2022$TEMP[ES_2022$TEMP == "Micrastur mintoni Whittaker, 2003"] = "Micrastur mintoni (Whittaker, 2003)"
ES_2022$TEMP[ES_2022$TEMP == "Mimus gilvus antelius Oberholser, 1919"] = "Mimus gilvus antelius (Oberholser, 1919)"
ES_2022$TEMP[ES_2022$TEMP == "Myrmotherula minor Salvadori, 1864"] = "Myrmotherula minor (Salvadori, 1864)"
ES_2022$TEMP[ES_2022$TEMP == "Nemosia rourei Cabanis, 1870"] = "Nemosia rourei (Cabanis, 1870)"
ES_2022$TEMP[ES_2022$TEMP == "Neomorphus geoffroyi dulcis Snethlage, 1927"] = "Neomorphus geoffroyi dulcis (Snethlage, 1927)"
ES_2022$TEMP[ES_2022$TEMP == "Ornithion inerme Hartlaub, 1853"] = "Ornithion inerme (Hartlaub, 1853)"
ES_2022$TEMP[ES_2022$TEMP == "Oxyruncus cristatus cristatus Swainson, 1821"] = "Oxyruncus cristatus cristatus (Swainson, 1821)"
ES_2022$TEMP[ES_2022$TEMP == "Phaethornis margarettae Ruschi, 1972"] = "Phaethornis margarettae (Ruschi, 1972)"
ES_2022$TEMP[ES_2022$TEMP == "Phibalura flavirostrisVieillot, 1816"] = "Phibalura flavirostris (Vieillot, 1816)"
ES_2022$TEMP[ES_2022$TEMP == "Pionus reichenowi Heine, 1844"] = "Pionus reichenowi (Heine, 1844)"
ES_2022$TEMP[ES_2022$TEMP == "Platyrinchus leucoryphus Wied, 1831"] = "Platyrinchus leucoryphus (Wied, 1831)"
ES_2022$TEMP[ES_2022$TEMP == "Procellaria aequinoctialis Linnaeus, 1758"] = "Procellaria aequinoctialis (Linnaeus, 1758)"
ES_2022$TEMP[ES_2022$TEMP == "Puffinus lherminieri Lesson, 1839"] = "Puffinus lherminieri (Lesson, 1839)"
ES_2022$TEMP[ES_2022$TEMP == "Ramphocaenus melanurus Vieillot, 1819"] = "Ramphocaenus melanurus (Vieillot, 1819)"
ES_2022$TEMP[ES_2022$TEMP == "Saltator similis d'Orbigny & Lafresnaye, 1837"] = "Saltator similis (d'Orbigny & Lafresnaye, 1837)"
ES_2022$TEMP[ES_2022$TEMP == "Sarkidiornis sylvicola Ihering & Ihering, 1907"] = "Sarkidiornis sylvicola (Ihering & Ihering, 1907)"
ES_2022$TEMP[ES_2022$TEMP == "Sclerurus macconnelli Chubb, 1919"] = "Sclerurus macconnelli (Chubb, 1919)"
ES_2022$TEMP[ES_2022$TEMP == "Strix huhula Daudin, 1800"] = "Strix huhula (Daudin, 1800)"
ES_2022$TEMP[ES_2022$TEMP == "Sula dactylatra Lesson, 1831"] = "Sula dactylatra (Lesson, 1831)"
ES_2022$TEMP[ES_2022$TEMP == "Trogon collaris Vieillot, 1817"] = "Trogon collaris (Vieillot, 1817)"
ES_2022$TEMP[ES_2022$TEMP == "Turdus fumigatus fumigatus Lichtenstein, 1823"] = "Turdus fumigatus fumigatus (Lichtenstein, 1823)"



ES_2022$TAXON <- gsub("\\s*\\([^\\)]+\\)","",as.character(ES_2022$TEMP))

ES_2022$CBRO_2021 <- names_CBRO$CBRO[match(ES_2022$TAXON, names_CBRO$Nomes)]
ES_2022$TEMP <- NULL

ES_2022$CATEGORIA_AMEACA <- ES_2022$CATEGORIA.DE.AMEAÇA

ES_2022$NIVEL <- "Espírito Santo"
ES_2022$ANO <- 2022
ES_2022$FONTE <- "Instituto de Meio Ambiente e Recursos Hídricos"
ES_2022$LINK <- "https://iema.es.gov.br/especies-ameacadas/ameacadas"


ES_2005$TEMP <- ES_2005$ESPÉCIE
ES_2005$TEMP[ES_2005$TEMP == "Puffinus lherminieri Lesson, 1839"] = "Puffinus lherminieri (Lesson, 1839)"
ES_2005$TEMP[ES_2005$TEMP == "Fregata ariel Gray, 1845"] = "Fregata ariel (Gray, 1845)"
ES_2005$TEMP[ES_2005$TEMP == "Penelope obscura Temminck, 1815"] = "Penelope obscura (Temminck, 1815)"
ES_2005$TEMP[ES_2005$TEMP == "Crax blumembachii Spix, 1825"] = "Crax blumembachii (Spix, 1825)"
ES_2005$TEMP[ES_2005$TEMP == "Caprimulgus hirundinaceus Spix, 1825"] = "Caprimulgus hirundinaceus (Spix, 1825)"
ES_2005$TEMP[ES_2005$TEMP == "Trogon collaris Vieillot, 1817"] = "Trogon collaris (Vieillot, 1817)"
ES_2005$TEMP[ES_2005$TEMP == "Formicarius colma Boddaert, 1783"] = "Formicarius colma (Boddaert, 1783)"
ES_2005$TEMP[ES_2005$TEMP == "Sclerurus mexicanus Sclater, 1856"] = "Sclerurus mexicanus (Sclater, 1856)"
ES_2005$TEMP[ES_2005$TEMP == "Platyrinchus leucoryphus Wied, 1831"] = "Platyrinchus leucoryphus (Wied, 1831)"
ES_2005$TEMP[ES_2005$TEMP == "Cichlopsis leucogenys Cabanis, 1851"] = "Cichlopsis leucogenys (Cabanis, 1851)"
ES_2005$TEMP[ES_2005$TEMP == "Turdus fumigatus Lichtenstein, 1823"] = "Turdus fumigatus (Lichtenstein, 1823)"
ES_2005$TEMP[ES_2005$TEMP == "Nemosia rourei Cabanis, 1870"] = "Nemosia rourei (Cabanis, 1870)"

ES_2005$TAXON <- gsub("\\s*\\([^\\)]+\\)","",as.character(ES_2005$TEMP))
ES_2005$CBRO_2021 <- names_CBRO$CBRO[match(ES_2005$TAXON, names_CBRO$Nomes)]

ES_2005$TEMP <- NULL

ES_2005$CATEGORIA_AMEACA <- ES_2005$STATUS %>%
  recode("CRITICAMENTE EM PERIGO" = "CR", "EM PERIGO" = "EN", "VULNERÁVEL" = "VU")
  
ES_2005$NIVEL <- "Espírito Santo"
ES_2005$ANO <- 2005
ES_2005$FONTE <- "Instituto de Meio Ambiente e Recursos Hídricos"
ES_2005$LINK <- "https://iema.es.gov.br/especies-ameacadas/ameacadas"


# FIM BLOCO ESPIRITO SANTO

# BLOCO SANTA CATARINA (sc)

SC_2009 <- read.csv("SC_2009.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)
SC_2011 <- read.csv("SC_2011.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)


SC_2009$TEMP <- str_squish(SC_2009$Espécie)

SC_2009$CBRO_2021 <- names_CBRO$CBRO[match(SC_2009$TEMP, names_CBRO$Nomes)]
SC_2011$CBRO_2021 <- names_CBRO$CBRO[match(SC_2011$TEMP, names_CBRO$Nomes)]

SC_2009$TEMP <- NULL

SC_2011$TEMP <- SC_2011$Espécie

SC_2011$TEMP[SC_2011$TEMP == "Primolius  maracana"] = "Primolius maracana"
SC_2011$TEMP[SC_2011$TEMP == "Sporophila  falcirostris"] = "Sporophila falcirostris"
SC_2011$TEMP[SC_2011$TEMP == "nigropectus "] = "nigropectus"

SC_2011$TEMP <- NULL


SC_2009$CATEGORIA_AMEACA <- SC_2009$Categoria
SC_2009$TAXON <- SC_2009$Espécie
SC_2009$NIVEL <- "Santa Catarina"
SC_2009$ANO <- 2009
SC_2009$FONTE <- "Instituto do Meio Ambiente de Santa Catarina"
SC_2009$LINK <- "https://www.ima.sc.gov.br/index.php/downloads/biodiversidade/fauna"

SC_2011$CATEGORIA_AMEACA <- SC_2011$Categoria
SC_2011$TAXON <- SC_2011$Espécie
SC_2011$NIVEL <- "Santa Catarina"
SC_2011$ANO <- 2011
SC_2011$FONTE <- "Instituto do Meio Ambiente de Santa Catarina"
SC_2011$LINK <- "https://www.ima.sc.gov.br/index.php/downloads/biodiversidade/fauna"

# FIM BLOCO SANTA CATARINA (SC)

# BLOCO SÃO PAULO (SP)

SP <- read.csv("SP.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)

SP$TEMP <- SP$Táxon
SP$TEMP[SP$TEMP == "Sarkidiornis sylvicola Ihering & Ihering, 1907"] = "Sarkidiornis sylvicola (Ihering & Ihering, 1907)"
SP$TEMP[SP$TEMP == "Mergus octosetaceus Vieillot, 1817"] = "Mergus octosetaceus (Vieillot, 1817)"
SP$TEMP[SP$TEMP == "Ortalis guttata remota Pinto, 1964"] = "Ortalis guttata remota (Pinto, 1964)"
SP$TEMP[SP$TEMP == "Crax fasciolata fasciolata Spix, 1825"] = "Crax fasciolata fasciolata (Spix, 1825)"
SP$TEMP[SP$TEMP == "Diomedea exulans Linnaeus, 1758"] = "Diomedea exulans (Linnaeus, 1758)"
SP$TEMP[SP$TEMP == "Diomedea dabbenena Mathews, 1929"] = "Diomedea dabbenena (Mathews, 1929)"
SP$TEMP[SP$TEMP == "Rallus longirostris crassirostris Lawrence, 1871"] = "Rallus longirostris crassirostris (Lawrence, 1871)"
SP$TEMP[SP$TEMP == "Aramides cajaneus avicenniae Stotz, 1992"] = "Aramides cajaneus avicenniae (Stotz, 1992)"
SP$TEMP[SP$TEMP == "Laterallus xenopterus Conover, 1934"] = "Laterallus xenopterus (Conover, 1934)"
SP$TEMP[SP$TEMP == "Charadrius wilsonia wilsonia Ord, 1814"] = "Charadrius wilsonia wilsonia (Ord, 1814)"
SP$TEMP[SP$TEMP == "Haematopus palliatus palliatus Temminck, 1820"] = "Haematopus palliatus palliatus (Temminck, 1820)"
SP$TEMP[SP$TEMP == "Sterna hirundinacea Lesson, 1831"] = "Sterna hirundinacea (Lesson, 1831)"
SP$TEMP[SP$TEMP == "Antrostomus sericocaudatus sericocaudatus Cassin, 1849"] = "Antrostomus sericocaudatus sericocaudatus (Cassin, 1849)"
SP$TEMP[SP$TEMP == "Brachygalba lugubris melanosterna Sclater, 1855"] = "Brachygalba lugubris melanosterna (Sclater, 1855)"
SP$TEMP[SP$TEMP == "Chelidoptera tenebrosa brasiliensis Sclater, 1862"] = "Chelidoptera tenebrosa brasiliensis (Sclater, 1862)"
SP$TEMP[SP$TEMP == "Pteroglossus aracari wiedii Sturm & Sturm, 1847"] = "Pteroglossus aracari wiedii (Sturm & Sturm, 1847)"
SP$TEMP[SP$TEMP == "Pteroglossus castanotis australis Cassin, 1867"] = "Pteroglossus castanotis australis (Cassin, 1867)"
SP$TEMP[SP$TEMP == "Ara chloropterus Gray, 1859"] = "Ara chloropterus (Gray, 1859)"

SP$TAXON <- gsub("\\s*\\([^\\)]+\\)","",as.character(SP$Táxon))

SP$CBRO_2021 <- names_CBRO$CBRO[match(SP$TAXON, names_CBRO$Nomes)]

SP$TEMP <- NULL
SP$Binomial <- NULL

SP$CATEGORIA_AMEACA <- SP$Categoria
SP$NIVEL <- "São Paulo"
SP$ANO <- 2010
SP$FONTE <- "DECRETO Nº 56.031, DE 20 DE JULHO DE 2010"
SP$LINK <- "https://www.al.sp.gov.br/repositorio/legislacao/decreto/2010/decreto-56031-20.07.2010.html"


# FIM BLOCO SÃO PAULO (SP)


# BLOCO BAHIA (BA)

BA <- read.csv("BA.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)

BA$TEMP <- BA$Táxon
BA$TEMP[BA$TEMP == "Phibalura flavirostris Vieillot, 1816"] = "Phibalura flavirostris (Vieillot, 1816)"
BA$TEMP[BA$TEMP == "Anodorhynchus leari Bonaparte, 1856"] = "Anodorhynchus leari (Bonaparte, 1856)"
BA$TEMP[BA$TEMP == "Ara chloropterus Gray, 1859"] = "Ara chloropterus (Gray, 1859)"
BA$TEMP[BA$TEMP == "Cercomacra brasiliana Hellmayr, 1905"] = "Cercomacra brasiliana (Hellmayr, 1905)"
BA$TEMP[BA$TEMP == "Chamaeza meruloides Vigors, 1825"] = "Chamaeza meruloides (Vigors, 1825)"
BA$TEMP[BA$TEMP == "Charadrius wilsonia Ord, 1814"] = "Charadrius wilsonia (Ord, 1814)"
BA$TEMP[BA$TEMP == "Cichlopsis leucogenys Cabanis, 1851"] = "Cichlopsis leucogenys (Cabanis, 1851)"
BA$TEMP[BA$TEMP == "Crax blumenbachii Spix, 1825"] = "Crax blumenbachii (Spix, 1825)"
BA$TEMP[BA$TEMP == "Formicivora grantsaui Gonzaga, Carvalhaes & Buzzetti, 2007"] = "Formicivora grantsaui (Gonzaga, Carvalhaes & Buzzetti, 2007)"
BA$TEMP[BA$TEMP == "Haematopus palliatus Temminck, 1820"] = "Haematopus palliatus (Temminck, 1820)"
BA$TEMP[BA$TEMP == "Mergus octosetaceus Vieillot, 1817"] = "Mergus octosetaceus (Vieillot, 1817)"
BA$TEMP[BA$TEMP == "Merulaxis ater Lesson, 1830"] = "Merulaxis ater (Lesson, 1830)"
BA$TEMP[BA$TEMP == "Merulaxis stresemanni Sick, 1960"] = "Merulaxis stresemanni (Sick, 1960)"
BA$TEMP[BA$TEMP == "Micrastur mintoni Whittaker, 2002"] = "Micrastur mintoni (Whittaker, 2002)"
BA$TEMP[BA$TEMP == "Myrmotherula minor Salvadori, 1864"] = "Myrmotherula minor (Salvadori, 1864)"
BA$TEMP[BA$TEMP == "Penelope jacucaca Spix, 1825"] = "Penelope jacucaca (Spix, 1825)"
BA$TEMP[BA$TEMP == "Phaethon aethereus Linnaeus, 1758"] = "Phaethon aethereus (Linnaeus, 1758)"
BA$TEMP[BA$TEMP == "Phaethon lepturus Daudin, 1802"] = "Phaethon lepturus (Daudin, 1802)"
BA$TEMP[BA$TEMP == "Phaethornis margarettae Ruschi, 1972"] = "Phaethornis margarettae (Ruschi, 1972)"
BA$TEMP[BA$TEMP == "Phibalura flavirostris Vieillot, 1816"] = "Phibalura flavirostris (Vieillot, 1816)"
BA$TEMP[BA$TEMP == "Phyllomyias griseocapilla Sclater, 1862"] = "Phyllomyias griseocapilla (Sclater, 1862)"
BA$TEMP[BA$TEMP == "Phylloscartes beckeri Gonzaga & Pacheco, 1995"] = "Phylloscartes beckeri (Gonzaga & Pacheco, 1995)"
BA$TEMP[BA$TEMP == "Phylloscartes roquettei Snethlage, 1928"] = "Phylloscartes roquettei (Snethlage, 1928)"
BA$TEMP[BA$TEMP == "Pionus reichenowi Heine, 1844"] = "Pionus reichenowi (Heine, 1844)"
BA$TEMP[BA$TEMP == "Platyrinchus leucoryphus Wied, 1831"] = "Platyrinchus leucoryphus (Wied, 1831)"
BA$TEMP[BA$TEMP == "Sclerurus cearensis Snethlage, 1924"] = "Sclerurus cearensis (Snethlage, 1924)"
BA$TEMP[BA$TEMP == "Sclerurus macconnelli Chubb, 1919"] = "Sclerurus macconnelli (Chubb, 1919)"
BA$TEMP[BA$TEMP == "Scytalopus diamantinensis Bornschein, Maurício, Belmonte-Lopes, Mata & Bonato, 2007"] = "Scytalopus diamantinensis (Bornschein, Maurício, Belmonte-Lopes, Mata & Bonato, 2007)"
BA$TEMP[BA$TEMP == "Trogon collaris Vieillot, 1817"] = "Trogon collaris (Vieillot, 1817)"
BA$TEMP[BA$TEMP == "Acrobatornis fonsecai Pacheco, Whitney & Gonzaga, 1996"] = "Acrobatornis fonsecai (Pacheco, Whitney & Gonzaga, 1996)"

BA$TAXON <- gsub("\\s*\\([^\\)]+\\)","",as.character(BA$TEMP))

BA$CBRO_2021 <- names_CBRO$CBRO[match(BA$TAXON, names_CBRO$Nomes)]

BA$TEMP <- NULL
BA$Binomial <- NULL

BA$CATEGORIA_AMEACA <- BA$Categoria
BA$NIVEL <- "Bahia"
BA$ANO <- 2017
BA$FONTE <- "Secretaria Estadual do Meio Ambiente - SEMA"
BA$LINK <- "https://view.officeapps.live.com/op/view.aspx?src=http%3A%2F%2Fwww.meioambiente.ba.gov.br%2Farquivos%2FFile%2FEditais%2Fportaria37fauna.docx&wdOrigin=BROWSELINK"

# FIM BLOCO BAHIA (BA)

# BLOCO RIO GRANDE DO SUL (RS)

RS_2014 <- read.csv("RS_2014.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)
RS_2002_Origin <- read.csv("RS_2002.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)

RS_2014$CBRO_2021 <- names_CBRO$CBRO[match(RS_2014$NOME.CIENTÍFICO, names_CBRO$Nomes)]


RS_2014$TAXON <- RS_2014$NOME.CIENTÍFICO
RS_2014$CATEGORIA_AMEACA <- RS_2014$STATUS

RS_2014$NIVEL <- "Rio Grande do Sul"
RS_2014$ANO <- 2014
RS_2014$FONTE <- "DECRETO N.º 51.797, DE 8 DE SETEMBRO DE 2014"
RS_2014$LINK <- "http://www.al.rs.gov.br/filerepository/replegis/arquivos/dec%2051.797.pdf"

# Limpeza dos dados
RS_2002 <- slice(RS_2002_Origin,  -(1), -(4:5), -(9), -(13:14), -(16), -(18), -(33), -(37:38), -(41:42), -(45), -(47:48), -(50:51), -(54), -(56:57), -(61:62), -(68:69), -(73:74), -(77:78), -(81:82), -(86:87), -(89:90), -(95), -(100:101), -(104), -(118), -(126), -(129), -(143), -(147), -(151), -(153), -(155), -(157), -(173))
RS_2002 <- rename(RS_2002, Táxon = Tinamiformes, Nome.Popular = X, Categoria = X.1)
RS_2002$TEMP <- RS_2002$Táxon

RS_2002$TEMP[RS_2002$TEMP == "Diomedea dabbenena Mathews, 1929"] = "Diomedea dabbenena (Mathews, 1929)"
RS_2002$TEMP[RS_2002$TEMP == "Diomedea exulans Linnaeus, 1758"] = "Diomedea exulans (Linnaeus, 1758)"
RS_2002$TEMP[RS_2002$TEMP == "Procellaria aequinoctialis Linnaeus, 1758"] = "Procellaria aequinoctialis (Linnaeus, 1758)"
RS_2002$TEMP[RS_2002$TEMP == "Procellaria conspicillata Gould, 1844"] = "Procellaria conspicillata (Gould, 1844)"
RS_2002$TEMP[RS_2002$TEMP == "Circus cinereus Vieillot, 1816"] = "Circus cinereus (Vieillot, 1816)"
RS_2002$TEMP[RS_2002$TEMP == "Falco deiroleucus Temminck, 1825"] = "Falco deiroleucus (Temminck, 1825)"
RS_2002$TEMP[RS_2002$TEMP == "Falco refigularis Daudin, 1800"] = "Falco refigularis (Daudin, 1800)"
RS_2002$TEMP[RS_2002$TEMP == "Penelope superciliaris Temminck, 1815"] = "Penelope superciliaris (Temminck, 1815)"
RS_2002$TEMP[RS_2002$TEMP == "Porzana Spiloptera Durnfornd, 1877"] = "Porzana Spiloptera (Durnfornd, 1877)"
RS_2002$TEMP[RS_2002$TEMP == "Larus atlanticus Olrog, 1958"] = "Larus atlanticus (Olrog, 1958)"
RS_2002$TEMP[RS_2002$TEMP == "Columba cayennensis Bonnaterre, 1792"] = "Columba cayennensis (Bonnaterre, 1792)"
RS_2002$TEMP[RS_2002$TEMP == "Columba plumbea Vieillot, 1818"] = "Columba plumbea (Vieillot, 1818)"
RS_2002$TEMP[RS_2002$TEMP == "Crotophaga major Gmelin, 1788"] = "Crotophaga major (Gmelin, 1788)"
RS_2002$TEMP[RS_2002$TEMP == "Dromococcyx pavoninus Pelzeln, 1870"] = "Dromococcyx pavoninus (Pelzeln, 1870)"
RS_2002$TEMP[RS_2002$TEMP == "Pieroglossus castanotis Gould, 1834"] = "Pieroglossus castanotis (Gould, 1834)"
RS_2002$TEMP[RS_2002$TEMP == "Ramphastos toco Müller, 1776"] = "Ramphastos toco (Müller, 1776)"
RS_2002$TEMP[RS_2002$TEMP == "Coryphistera alaudina Burmeister, 1860"] = "Coryphistera alaudina (Burmeister, 1860)"
RS_2002$TEMP[RS_2002$TEMP == "Leptasthenura platensis Reichenbach, 1853"] = "Leptasthenura platensis (Reichenbach, 1853)"
RS_2002$TEMP[RS_2002$TEMP == "Philydor lichtensteini Cabanis & Heine, 1859"] = "Philydor lichtensteini (Cabanis & Heine, 1859)"
RS_2002$TEMP[RS_2002$TEMP == "Synollaxis albescens Temminck, 1823"] = "Synollaxis albescens (Temminck, 1823)"
RS_2002$TEMP[RS_2002$TEMP == "Formicarius colma Boddaert, 1783"] = "Formicarius colma (Boddaert, 1783)"
RS_2002$TEMP[RS_2002$TEMP == "Myrmeciza squamosa Pelzeln, 1868"] = "Myrmeciza squamosa (Pelzeln, 1868)"
RS_2002$TEMP[RS_2002$TEMP == "Phylloscartes kronei Willis & Oniki, 1992"] = "Phylloscartes kronei (Willis & Oniki, 1992)"
RS_2002$TEMP[RS_2002$TEMP == "Platyrinchus leucoryphus Wied-Neuwied, 1831"] = "Platyrinchus leucoryphus (Wied-Neuwied, 1831)"
RS_2002$TEMP[RS_2002$TEMP == "Anthus nattereri Sclater, 1878"] = "Anthus nattereri (Sclater, 1878)"
RS_2002$TEMP[RS_2002$TEMP == "Polioptila lactea Sharpe, 1885"] = "Polioptila lactea (Sharpe, 1885)"
RS_2002$TEMP[RS_2002$TEMP == "Sporophila hypoxantha Cabanis, 1851"] = "Sporophila hypoxantha (Cabanis, 1851)"
RS_2002$TEMP[RS_2002$TEMP == "Agelatius cyanopus Vieillot, 1819"] = "Agelatius cyanopus (Vieillot, 1819)"
RS_2002$TEMP[RS_2002$TEMP == "Phibalura flavirostris Vieillot, 1816"] = "Phibalura flavirostris (Vieillot, 1816)"
RS_2002$Binomial[RS_2002$Binomial == "Porzana Spiloptera"] = "Porzana Spiloptera"


RS_2002$TAXON <- gsub("\\s*\\([^\\)]+\\)","",as.character(RS_2002$TEMP))
RS_2002$TEMP <- NULL

RS_2002$CBRO_2021 <- names_CBRO$CBRO[match(RS_2002$TAXON, names_CBRO$Nomes)]

RS_2002$CATEGORIA_AMEACA <- RS_2002$Categoria %>%
  recode("em perigo" = "EN", "regionalmente extinta" = "RE", "vulnerável" = "VU", "criticamente em perigo" = "CR", "avulnerável" = "VU", "provavelmente extinta" = "CR PE")

RS_2002$NIVEL <- "Rio Grande do Sul"
RS_2002$ANO <- 2002
RS_2002$FONTE <- "DECRETO Nº 41.672, DE 11 DE JUNHO DE 2002"
RS_2002$LINK <- "http://www.al.rs.gov.br/legis/M010/M0100099.ASP?Hid_TodasNormas=840&hTexto=&Hid_IDNorma=840"


# FOM BLOCO RIO GRANDE DO SUL (RS)

# BLOCO PARÁ (PA)

PA <- read.csv("PA.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)
PA$TEMP <- PA$Nome.Científico

PA$TEMP[PA$TEMP == "Phlegopsis nigromaculata  paraensis"] = "Phlegopsis nigromaculata paraensis"
PA$TEMP[PA$TEMP == "Dendrexetastes rufigula  paraensis"] = "Dendrexetastes rufigula paraensis"
PA$TEMP[PA$TEMP == "Amazona ochrocephala  xantholaema"] = "Amazona ochrocephala xantholaema"
PA$TEMP[PA$TEMP == "Coryphaspiza melanotis  marajoara"] = "Coryphaspiza melanotis marajoara"
PA$TEMP[PA$TEMP == "Deconychura longicauda  zimmeri"] = "Deconychura longicauda zimmeri"
PA$TEMP[PA$TEMP == "Pteroglossus bitorquatus  bitorquatus"] = "Pteroglossus bitorquatus bitorquatus"
PA$TEMP[PA$TEMP == "Sakesphorus luctuosus  araguayae"] = "Sakesphorus luctuosus araguayae"

PA$TAXON <- PA$TEMP


PA$CBRO_2021 <- names_CBRO$CBRO[match(PA$TEMP, names_CBRO$Nomes)]

PA$TEMP <- NULL

PA$CATEGORIA_AMEACA <- PA$Categoria %>%
  recode("Em perigo" = "EN", "Regionalmente extinta" = "RE", "Vulnerável" = "VU", "Criticamente em perigo" = "CR", "avulnerável" = "VU", "Provavelmente extinta" = "CR PE")

PA$NIVEL <- "Pará"
PA$ANO <- 2006
PA$FONTE <- "RELATÓRIO TÉCNICO OFICINA DE TRABALHO \"DISCUSSÃO E ELABORAÇÃO DA LISTA DE ESPÉCIES AMEAÇADAS DE EXTINÇÃO DO ESTADO DO PARÁ\"" 
PA$LINK <- "https://www.semas.pa.gov.br/download/RELAT%C3%93RIO%20T%C3%89CNICO%20SOBRE%20AS%20ESP%C3%89CIES%20AMEA%C3%87ADAS%20DO%20PAR%C3%81.pdf"


 # FIM BLOCO PARÁ (PA)


# BLOCO PARANÁ (PR)

PR_2018 <- read.csv("PR_2018.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)
PR_2018$TEMP <- PR_2018$Nome.científico

PR_2018$TEMP[PR_2018$TEMP == "Sarkidiornis sylvicola Ihering & Ihering, 1907"] = "Sarkidiornis sylvicola (Ihering & Ihering, 1907)"
PR_2018$TEMP[PR_2018$TEMP == "Anas flavirostris Vieillot, 1816"] = "Anas flavirostris (Vieillot, 1816)"
PR_2018$TEMP[PR_2018$TEMP == "Mergus octosetaceus Vieillot, 1817"] = "Mergus octosetaceus (Vieillot, 1817)"
PR_2018$TEMP[PR_2018$TEMP == "Crax fasciolata Spix, 1825"] = "Crax fasciolata (Spix, 1825)"
PR_2018$TEMP[PR_2018$TEMP == "Procellaria aequinoctialis Linnaeus, 1758"] = "Procellaria aequinoctialis (Linnaeus, 1758)"
PR_2018$TEMP[PR_2018$TEMP == "Rallus longirostris Boddaert, 1783"] = "Rallus longirostris (Boddaert, 1783)"
PR_2018$TEMP[PR_2018$TEMP == "Haematopus palliatus Temminck, 1820"] = "Haematopus palliatus (Temminck, 1820)"
PR_2018$TEMP[PR_2018$TEMP == "Sterna hirundinacea Lesson, 1831"] = "Sterna hirundinacea (Lesson, 1831)"
PR_2018$TEMP[PR_2018$TEMP == "Strix huhula Daudin, 1800"] = "Strix huhula (Daudin, 1800)"
PR_2018$TEMP[PR_2018$TEMP == "Antrostomus sericocaudatus Cassin, 1849"] = "Antrostomus sericocaudatus (Cassin, 1849)"
PR_2018$TEMP[PR_2018$TEMP == "Ara chloropterus Gray, 1859"] = "Ara chloropterus (Gray, 1859)"
PR_2018$TEMP[PR_2018$TEMP == "Dysithamnus xanthopterus Burmeister, 1856"] = "Dysithamnus xanthopterus (Burmeister, 1856)"
PR_2018$TEMP[PR_2018$TEMP == "Herpsilochmus atricapillus Pelzeln, 1868"] = "Herpsilochmus atricapillus (Pelzeln, 1868)"
PR_2018$TEMP[PR_2018$TEMP == "Merulaxis ater Lesson, 1830"] = "Merulaxis ater (Lesson, 1830)"
PR_2018$TEMP[PR_2018$TEMP == "Scytalopus pachecoi Maurício, 2005"] = "Scytalopus pachecoi (Maurício, 2005)"
PR_2018$TEMP[PR_2018$TEMP == "Scytalopus iraiensis Bornschein, Reinert & Pichorim, 1998"] = "Scytalopus iraiensis (Bornschein, Reinert & Pichorim, 1998)"
PR_2018$TEMP[PR_2018$TEMP == "Chamaeza meruloides Vigors, 1825"] = "Chamaeza meruloides (Vigors, 1825)"
PR_2018$TEMP[PR_2018$TEMP == "Platyrinchus leucoryphus Wied, 1831"] = "Platyrinchus leucoryphus (Wied, 1831)"
PR_2018$TEMP[PR_2018$TEMP == "Phylloscartes paulista Ihering & Ihering, 1907"] = "Phylloscartes paulista (Ihering & Ihering, 1907)"
PR_2018$TEMP[PR_2018$TEMP == "Anthus nattereri Sclater, 1878"] = "Anthus nattereri (Sclater, 1878)"
PR_2018$TEMP[PR_2018$TEMP == "[Myiothlypis leucophrys Pelzeln, 1868]"] = "Myiothlypis leucophrys (Pelzeln, 1868)"
PR_2018$TEMP[PR_2018$TEMP == "Dacnis nigripes Pelzeln, 1856"] = "Dacnis nigripes (Pelzeln, 1856)"
PR_2018$TEMP[PR_2018$TEMP == "Sporophila beltoni Repenning & Fontana, 2013"] = "Sporophila beltoni (Repenning & Fontana, 2013)"
PR_2018$TEMP[PR_2018$TEMP == "Sporophila hypoxantha Cabanis, 1851"] = "Sporophila hypoxantha (Cabanis, 1851)"
PR_2018$TEMP[PR_2018$TEMP == "Anas georgica Gmelin, 1789"] = "Anas georgica (Gmelin, 1789)"
PR_2018$TEMP[PR_2018$TEMP == "Polioptila lactea Sharpe, 1885"] = "Polioptila lactea (Sharpe, 1885)"
PR_2018$TEMP[PR_2018$TEMP == "Podiceps occipitalis Garnot, 1826"] = "Podiceps occipitalis (Garnot, 1826)"
PR_2018$TEMP[PR_2018$TEMP == "Procellaria conspicillata Gould, 1844"] = "Procellaria conspicillata (Gould, 1844)"
PR_2018$TEMP[PR_2018$TEMP == "Buteo swainsoni Bonaparte, 1838"] = "Buteo swainsoni (Bonaparte, 1838)"
PR_2018$TEMP[PR_2018$TEMP == "Fulica rufifrons Philippi & Landbeck, 1861"] = "Fulica rufifrons (Philippi & Landbeck, 1861)"
PR_2018$TEMP[PR_2018$TEMP == "Numenius hudsonicusLatham, 1790"] = "Numenius hudsonicus (Latham, 1790)"
PR_2018$TEMP[PR_2018$TEMP == "Stercorarius chilensis Bonaparte, 1857"] = "Stercorarius chilensis (Bonaparte, 1857)"
PR_2018$TEMP[PR_2018$TEMP == "Stercorarius maccormicki Saunders, 1893"] = "Stercorarius maccormicki (Saunders, 1893)"
PR_2018$TEMP[PR_2018$TEMP == "Stercorarius longicaudus Vieillot, 1819"] = "Stercorarius longicaudus (Vieillot, 1819)"
PR_2018$TEMP[PR_2018$TEMP == "Synallaxis hypospodia Sclater, 1874"] = "Synallaxis hypospodia (Sclater, 1874)"
PR_2018$TEMP[PR_2018$TEMP == "Synallaxis albilora Pelzeln, 1856"] = "Synallaxis albilora (Pelzeln, 1856)"
PR_2018$TEMP[PR_2018$TEMP == "Phibalura flavirostris Vieillot, 1816"] = "Phibalura flavirostris (Vieillot, 1816)"
PR_2018$TEMP[PR_2018$TEMP == "Tyrannus albogularis Burmeister, 1856"] = "Tyrannus albogularis (Burmeister, 1856)"
PR_2018$TEMP[PR_2018$TEMP == "Progne elegans Baird, 1865)"] = "Progne elegans (Baird, 1865)"

PR_2018$TAXON <- gsub("\\s*\\([^\\)]+\\)","",as.character(PR_2018$TEMP))

PR_2018$CBRO_2021 <- names_CBRO$CBRO[match(PR_2018$TAXON, names_CBRO$Nomes)]

PR_2018$TEMP <- NULL

PR_2018$CATEGORIA_AMEACA <- PR_2018$Categoria
PR_2018$NIVEL <- "Paraná"
PR_2018$ANO <- 2018
PR_2018$FONTE <- "Decreto nº 11797 DE 22/11/2018"
PR_2018$LINK <- "https://www.normasbrasil.com.br/norma/decreto-11797-2018-pr_369613.html"


PR_2004 <- read.csv("PR_2004.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)

PR_2004$TEMP <- PR_2004$Nome.científico
PR_2004$TEMP[PR_2004$TEMP == "Ara chloropterus Gray, 1859"] = "Ara chloropterus (Gray, 1859)"
PR_2004$TEMP[PR_2004$TEMP == "Basileuterus leucophrys Pelzeln, 1868"] = "Basileuterus leucophrys (Pelzeln, 1868)"
PR_2004$TEMP[PR_2004$TEMP == "Crax fasciolata Spix, 1825"] = "Crax fasciolata (Spix, 1825)"
PR_2004$TEMP[PR_2004$TEMP == "Diomedea exulans Linnaeus, 1758"] = "Diomedea exulans (Linnaeus, 1758)"
PR_2004$TEMP[PR_2004$TEMP == "Mergus octosetaceus Vieillot, 1817"] = "Mergus octosetaceus (Vieillot, 1817)"
PR_2004$TEMP[PR_2004$TEMP == "Phylloscartes kronei Willis & Oniki, 1992"] = "Phylloscartes kronei (Willis & Oniki, 1992)"
PR_2004$TEMP[PR_2004$TEMP == "Platyrinchus leucoryphus Wied, 1831"] = "Platyrinchus leucoryphus (Wied, 1831)"
PR_2004$TEMP[PR_2004$TEMP == "Scytalopus iraiensis Bornschein, Reinert & Pichorim, 1998"] = "Scytalopus iraiensis (Bornschein, Reinert & Pichorim, 1998)"
PR_2004$TEMP[PR_2004$TEMP == "Stymphalornis acutirostris Bornschein, Reinert & Teixeira, 1995"] = "Stymphalornis acutirostris (Bornschein, Reinert & Teixeira, 1995)"
PR_2004$TEMP[PR_2004$TEMP == "Anthus nattereri Sclater, 1878"] = "Anthus nattereri (Sclater, 1878)"
PR_2004$TEMP[PR_2004$TEMP == "Buteo albonotatus Kaup, 1847"] = "Buteo albonotatus (Kaup, 1847)"
PR_2004$TEMP[PR_2004$TEMP == "Buteo swainsoni Bonaparte, 1838"] = "Buteo swainsoni (Bonaparte, 1838)"
PR_2004$TEMP[PR_2004$TEMP == "Caprimulgus longirostris Bonaparte, 1825"] = "Caprimulgus longirostris (Bonaparte, 1825)"
PR_2004$TEMP[PR_2004$TEMP == "Cariama cristata Linnaeus, 1766"] = "Cariama cristata (Linnaeus, 1766)"
PR_2004$TEMP[PR_2004$TEMP == "Chamaeza meruloides Vigors, 1825"] = "Chamaeza meruloides (Vigors, 1825)"
PR_2004$TEMP[PR_2004$TEMP == "Coccyzus euleri Cabanis, 1873"] = "Coccyzus euleri (Cabanis, 1873)"
PR_2004$TEMP[PR_2004$TEMP == "Dacnis nigripes Pelzeln, 1856"] = "Dacnis nigripes (Pelzeln, 1856)"
PR_2004$TEMP[PR_2004$TEMP == "Galbula ruficauda Cuvier, 1816"] = "Galbula ruficauda (Cuvier, 1816)"
PR_2004$TEMP[PR_2004$TEMP == "Merulaxis ater Lesson, 1830"] = "Merulaxis ater (Lesson, 1830)"
PR_2004$TEMP[PR_2004$TEMP == "Phibalura flavirostris Vieillot, 1816"] = "Phibalura flavirostris (Vieillot, 1816)"
PR_2004$TEMP[PR_2004$TEMP == "Phyllomyias griseocapilla Sclater, 1862"] = "Phyllomyias griseocapilla (Sclater, 1862)"
PR_2004$TEMP[PR_2004$TEMP == "Phylloscartes paulista Ihering & Ihering, 1907"] = "Phylloscartes paulista (Ihering & Ihering, 1907)"
PR_2004$TEMP[PR_2004$TEMP == "Ramphastos vitellinus Lichtenstein, 1823"] = "Ramphastos vitellinus (Lichtenstein, 1823)"
PR_2004$TEMP[PR_2004$TEMP == "Saltator atricollis Vieillot, 1817"] = "Saltator atricollis (Vieillot, 1817)"
PR_2004$TEMP[PR_2004$TEMP == "Sporophila hypoxantha Cabanis, 1851"] = "Sporophila hypoxantha (Cabanis, 1851)"
PR_2004$TEMP[PR_2004$TEMP == "Synallaxis hypospodia Sclater, 1874"] = "Synallaxis hypospodia (Sclater, 1874)"
PR_2004$TEMP[PR_2004$TEMP == "Thamnophilus pelzelni Hellmayr, 1924"] = "Thamnophilus pelzelni (Hellmayr, 1924)"

PR_2004$TAXON <- gsub("\\s*\\([^\\)]+\\)","",as.character(PR_2004$TEMP))

PR_2004$CBRO_2021 <- names_CBRO$CBRO[match(PR_2004$TAXON, names_CBRO$Nomes)]
PR_2004$TEMP <- NULL

PR_2004$CATEGORIA_AMEACA <- PR_2004$Categoria
PR_2004$NIVEL <- "Paraná"
PR_2004$ANO <- 2004
PR_2004$FONTE <- "DECRETO Nº 3148, DE 15 DE JUNHO DE 2004"
PR_2004$LINK <- "https://leisestaduais.com.br/pr/decreto-n-3148-2004-parana-estabelece-a-politica-estadual-de-protecao-a-fauna-nativa-seus-principios-alvos-objetivos-e-mecanismos-de-execucao-define-o-sistema-estadual-de-protecao-a-fauna-nativa-sisfauna-cria-o-conselho-estadual-de-protecao-a-fauna-confauna-implanta-a-rede-estadual-de-protecao-a-fauna-nativa-rede-pro-fauna-e-da-outras-providencias"

# FIM BLOCO PARANÁ (PR)

# BLOCO RIO DE JANEIRO (RJ)

RJ <- read.csv("RJ.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)

RJ$TAXON <- RJ$Especie
RJ$TAXON[RJ$TAXON == "Mimus gilvus (Vieillot, 1807)"] = "Mimus gilvus"

RJ$CBRO_2021 <- names_CBRO$CBRO[match(RJ$TAXON, names_CBRO$Nomes)]

RJ$CATEGORIA_AMEACA <- RJ$Categoria
RJ$NIVEL <- "Rio de Janeiro"
RJ$ANO <- 2018
RJ$FONTE <- "Diário Oficial do Estado do Rio de Janeiro - 5 DE JUNHO DE 2018"
RJ$LINK <- "http://www.ioerj.com.br/portal/modules/conteudoonline/mostra_edicao.php?session=VDFSQk1WRlVWVFJTVkZWMFVrUmFRazlETURCU1ZHUkhURlZHUTAxNmEzUk5WRTE2VDBSTmVVMXFXa2RTUlZFeg=="

# FIM BLOCO RIO DE JANEIRO (RJ)

lista_global <- read.csv("lista_global.csv", sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)
lista_global$NIVEL <- "Global"
lista_global$ANO <- 2023
lista_global$FONTE <- "BirdLife"
lista_global$LINK <- "http://datazone.birdlife.org/species/results?thrlev1=&thrlev2=&kw=&fam=0&gen=0&spc=&cmn=&reg=11&cty=30"
lista_global$CATEGORIA_AMEACA <- lista_global$Global.IUCN.Red.List.Category



lista_nacional <- read.csv("Lista_nacional.tsv", sep = "\t", encoding = "UTF-8", stringsAsFactors = FALSE)
lista_nacional$CATEGORIA_AMEACA <- lista_nacional$Categoria.de.ameaca
lista_nacional$TAXON <- lista_nacional$Nome.Cientifico
lista_nacional$CBRO_2021 <- names_CBRO$CBRO[match(lista_nacional$TAXON, names_CBRO$Nomes)]
lista_nacional$NIVEL <- "Brasil"
lista_nacional$ANO <- 2022
lista_nacional$FONTE <- "PORTARIA GM/MMA Nº 300, DE 13 DE DEZEMBRO DE 2022"
lista_nacional$LINK <- "https://www.in.gov.br/en/web/dou/-/portaria-gm/mma-n-300-de-13-de-dezembro-de-2022-450425464"


# SELECTS

lista_global <- lista_global %>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

lista_nacional <- lista_nacional %>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

RJ <- RJ%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

SP <- SP%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

RS_2002 <- RS_2002%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

RS_2014 <- RS_2014%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

SC_2009 <- SC_2009%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

SC_2011 <- SC_2011%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

PR_2018 <- PR_2018%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

PR_2004 <- PR_2004%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

PA <- PA%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

MG <- MG%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

ES_2022 <- ES_2022%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

ES_2005 <- ES_2005%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)

BA <- BA%>%
  select(TAXON, CBRO_2021, CATEGORIA_AMEACA, NIVEL, ANO, FONTE, LINK)






