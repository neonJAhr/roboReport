Bayesiaanse T-Toets voor Eén Gemiddelde
===

Met de one sample t-test kan de gebruiker de effectgrootte schatten en de nulhypothese testen dat het populatiegemiddelde gelijk is aan een specifieke constante, bijvoorbeeld de toetswaarde.

### Assumpties
- De afhankelijke variabele is continue.
- De data komen uit een aselecte steekproef uit de populatie.
- De afhankelijke variabele is normaal verdeeld in de populatie.

### Invoer
---

#### Invoerveld
- Variabelen: In deze box is de afhankelijke variabele geselecteerd.  

#### Toets
De toetswaarde gespecificeerd in de nulhypothese.

#### Hypothese
- &ne; Toetswaarde: Tweezijdige alternatieve hypothese dat het populatiegemiddelde niet gelijk is aan de toetswaarde.
- &gt; Toetswaarde: Eenzijdige alternatieve hypothese dat het populatiegemiddelde hoger is dan de toetswaarde.
- &lt; Toetswaarde: Eenzijdige alternatieve hypothese dat het populatiegemiddelde lager is dan de toetswaarde.

#### Bayes Factor
- BF10: Met deze optie geeft de Bayes factor bewijs voor de alternatieve hypothese ten opzichte van de nulhypothese.
- BF01: Met deze optie geeft de Bayes factor bewijs voor de nulhypothese ten opzichte van de alternatieve hypothese.
- Log(BF10): Natural logaritme of BF10.

#### Toetsen
- Student: De student t-toets. Dit is de standaardoptie.
- Wilcoxon rangtekentoets: Wilcoxon rangtekentoets.
  - Aantal steekproeven: Het aantal MCMC steekproeven.

#### Prior
- Gestandaardiseerde effectgrootte
  - Standaard
    - Cauchy: Schaal van de Cauchy prior verdeling van de effectgrootte onder de alternatieve hypothese; de standaardoptie is 0.707.
  - Geinformeerd
    - Cauchy: Schaal en locatie.
    - Normal: Gemiddelde en standaarddeviatie.
    - Student's t: Schaal, locatie en vrijheidsgraden (vg).

[comment]: # (- Pure effectgrootte (Dienes))
[comment]: # (  - Half-Normaal: Standaardafwijking)
[comment]: # (  - Normaal: gemiddelde en standaardafwijking)
[comment]: # (  - Uniform: Onder en bovengrens)

#### Aanvullende statistieken
- Beschrijvend: Steekproef grootte, steekproefgemiddelde, steekproef standaarddeviatie, standaardfout van het gemiddelde.

#### Reproduceerbaarheid
- Gebruik toevalsgenerator beginwaarde: Geeft de mogelijkheid een toevalsgenerator beginwaarde te gebruiken voor uw analyse. Een toevalsgenerator beginwaarde gebruiken, zorgt ervoor dat willekeurige processen geen invloed hebben op een analyse.

#### Grafieken
- Prior en posterior: Geeft de verdeling van de prior en posterior van de effectgrootte onder de alternatieve hypothese.
  - Aanvullende informatie: Voegt de Bayes Factor berekend met de door de gebruiker gedefinieerde prior toe; voegt een kanswiel toe die de kans van de data onder de nulhypothese vs. de alternatieve hypothese laat zien; voegt de mediaan en de 95% geloofwaardigheidsinterval van de verdeling van de posterior van de effectgrootte toe.
- Bayes factor robuustheidscheck: Geeft de Bayes factor weer als een functie van de breedte van de Cauchy prior voor effectgrootte. De schaal van de Cauchy prior varieert tussen 0 en 1.5, zodat de priors steeds minder informatief worden.
- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer terwijl de data binnenkomen, gebruik makende van de door de gebruiker gedefinieerde prior.
  - Robuustheidscheck: Voegt de resultaten van de sequentiële analyse toe, gebruik makende van de brede (scale=1) en ultrabrede prior (scale=sqrt (2)).
- Beschrijvende grafieken
  - Geloofwaardigheidsinterval: Standaardoptie is 95%.
- Staafdiagrammen: Geeft het steekproefgemiddelde weer als een balk en het geloofwaardigheidsinterval of de standaardfout als de foutenbalk.
  - Geloofwaardig interval: Geeft de centrale geloofwaardige intervallen weer. Standaard is het geloofwaardigheidsinterval ingesteld op 95%. Dit kan worden gewijzigd in het gewenste percentage.
  - Standaardfout: Door deze optie te selecteren, zullen de foutbalken standaardfouten van het gemiddelde weergeven.
  - Fix horizontal as to 0: Forceert de grafiek om de standaard x-as op y = 0 weer te geven.

#### Ontbrekende waarden
 - Het uitsluiten van waarnemingen, analyse bij analyse: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke test uitgevoerd met alle waarnemingen die valide data bevatten voor de afhankelijke variabele in de t-toets. De steekproefgroottes kunnen daardoor verschillen per toets.
 - Het uitsluiten van waarnemingen, lijstgewijs: Wanneer er meerdere t-toetsen in een analyse zitten, wordt elke t-toets uitgevoerd met enkel de waarnemingen die valide data voor alle afhankelijke variabelen bevatten. De steekproefgrootte is daardoor hetzelfde voor alle toetsen.

### Uitvoer
---

#### Bayesiaanse T-Toets voor één gemiddelde
- Bayes factor: Als een eenzijdige toets wordt gevraagd:
  - BF+0: De Bayes factor die bewijs geeft voor de eenzijdige hypothese dat het populatiegemiddelde hoger is dan de toetswaarde.
  - BF-0: De Bayes factor die bewijs geeft voor de eenzijdige hypothese dat het populatiegemiddelde lager is dan de toetswaarde.
  - BF0+: De Bayes factor die bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige hypothese dat het populatiegemiddelde hoger is dan de toetswaarde.
  - BF0-: De Bayes factor die bewijs geeft voor de nulhypothese ten opzichte van de eenzijdige hypothese dat het populatiegemiddelde lager is dan de toetswaarde.
- fout %: De fout van de Gaussiaanse kwadratuur intergratie methode die wordt gebruikt op de Bayes factor te berekenen.
- W: De toets statistiek van de rangtekentoets.
- R-dakje: Mate van convergentie voor de MCMC procedure van de rangtekentoets. Een ratio die de varianties vergelijkt binnen en tussen de MCMC ketens voor de delta parameter. Waarden minder dan, of gelijk aan, 1 duiden op convergentie.

#### Beschrijvende Statistiek
- N: De steekproefgrootte.
- Gemiddelde: Het steekproefgemiddelde.
- SD: Standaarddeviatie van het gemiddelde.
- Std. Fout: Standaardfout van het gemiddelde.

#### Grafieken
- Prior en posterior: Geeft de prior (stippellijn) en posterior (lijn) verdeling van de effectgrootte onder de alternatieve hypothese; de grijze cirkels geven de hoogte van de dichtheid van de prior en de posterior bij een effectgrootte delta = 0 weer. De horizontale lijn geeft de breedte van een 95% geloofwaardigheidsinterval van de posterior verdeling weer.
  - Aanvullende info: Geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior weer; geeft een kanswiel weer met de kans van de data onder de nulhypothese en alternatieve hypothese; geeft de mediaan en het 95% geloofwaardigheidsinterval van de posterior verdeling weer.
- Bayes factor robuustheidsgrafiek: Geeft de Bayes factor weer als een functie van de breedte van de Cauchy prior voor effectgrootte. De zwarte cirkel geeft de Bayes factor berekend met een brede prior weer; de witte cirkel geeft de Bayes factor berekend met een ultrabrede prior weer; de grijze cirkel geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior verdeling weer.
- Sequentiële analyse: Geeft de ontwikkeling van de Bayes factor weer, als een functie van het aantal datapunten (n), gebruik makenende van de door de gebruiker gedefinieerde prior; geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior weer; geeft een kanswiel weer die de kans van de data onder de nulhypothese vs. de alternatieve hypothese weergeeft; geeft de mediaan en de 95% geloofwaardigheidsinterval van de verdeling van de posterior weer; laat zien hoe besluitvaardig het bewijs is met Jeffreys' (1961) bewijscategorieën.
  - Robuustheidscheck: Geeft de ontwikkeling van de Bayes factor als een functie van het aantal datapunten (n), met de brede en ultrabrede verdeling van de prior. De zwarte cirkel geeft de Bayes factor berekend met een brede prior verdeling weer; de witte cirkel geeft de Bayes factor berekend met een ultrabrede prior verdeling weer; de grijze cirkel geeft de Bayes factor berekend met de door de gebruiker gedefinieerde prior verdeling weer.
- Beschrijvende grafieken:
  - Geloofwaardigheidsinterval: Standaardoptie is 95%.
- Staafdiagrammen: Geeft het steekproefgemiddelde (grijze balk), het x% geloofwaardigheidsinterval of de standaardfout (schijven), en de waarde van de teststatistiek (stippellijn) weer.

### Referenties
---
- Gronau, Q. F., Ly, A., & Wagenmakers, E.-J. (2020). Informed Bayesian t-tests. *The American Statistician, 74*, 137-143.
- Jeffreys, H. (1961).  *Theory of probability (3rd ed.)*. Oxford, UK: Oxford University Press.
- Morey, R. D., Rouder, J. N., Pratte, M. S., & Speckman, P. L. (2011). Using MCMC chain outputs to efficiently estimate Bayes factors.  *Journal of Mathematical Psychology, 55*, 368-378.
- Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t-tests for accepting and rejecting the null hypothesis.  *Psychonomic Bulletin & Review, 16*, 225-237.
- van Doorn, J., Ly, A., Marsman, M., & Wagenmakers, E.-J. (2020). Bayesian rank-based hypothesis testing for the rank sum test, the signed rank test, and Spearman’s ρ. *Journal of Applied Statistics, 47(16)*, 2984-3006.

### R-packages
---
- BayesFactor
- ggplot2
- logspline
- stats
