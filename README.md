Open Research Week 2021: Tools And Technology For Open Research - Research Analytical
================
Jacob L. Macdonald, University of Liverpool; <Jacob.Macdonald@liverpool.ac.uk>

Getting The Results Out There
-----------------------------

*Part 2 of Open Research Week 2021 Talk - Tools and Technology for Open Research*

Using some work on open data, this research analytical showcases an online presentation of outputs, visualization, code and analysis. We'll see a side by side comparison of the code and the output on github to give an example of how different research deliverables can be shared.

**Github** + **RMarkdown** are great tools to use together for making research and outputs accessible. This is one way in which we can combine the best of the statistical and analytical capabilities of *R* with the tracking, presentation and collaborative nature of Github.

Primarily, **Git** and **Github** are used to monitor and version control coding documents. This has developed itself into a hosting site for a wide range and types of outputs.

During this session, we'll overview the main ways and examples of how to incorporate: \* Text \* Code \* Visualization \* Methodologies \* Analyses

One of the main benefits of this format is the sharing of code and data outputs

``` r
set.seed(2021)
x <- data.frame(Type="Normal", Value=rnorm(1000, 2, 1))
set.seed(2021)
y <- data.frame(Type="Chi-Sq.", Value=rchisq(1000, 5))

distributions <- rbind(x, y)

p <- ggplot(distributions, aes(x=Value, fill=Type)) +
  geom_density(alpha=0.4) +
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")) +
  theme(legend.position="bottom", legend.title = element_blank())
p
```

![](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Rmd%20Code%20Example-1.png)

Section 1: Parametric Employment Subcentre Identification for Great Britain
---------------------------------------------------------------------------

Urban metropolitan areas are often seen to comprise clusters of employment over space; often with a primary nucleus and surrounding satellite *subcentres* where employment densities are strong.

Stylized facts across the globe have given rise to a large literature on the *Monocentric City Model* where a city's employment density decreases at some functional rate from a singular central business district.

Increasingly granular (census tract) and accessible data allow us to explore the validity of these theoretic models, and more generally apply their concepts to real world data to explore employment (or population) densities. Identifying patterns in the cluster and spatial configuration of the data.

**Can we build a programatic way to efficiently explore a wide variety of flexible models across a range of urban areas.** **Based only on openly accessible tools and software.**

Update to code for exisiting employment subcentre identification in LA and tailored to the local England context [Ban et al. 2017](https://www.mdpi.com/2073-445X/6/1/17/htm "Identifying Employment Subcenters: The Method of Exponentially Declining Cutoffs").

### 1.1. Monocentric Employment Density Decay

*D*<sub>*x*</sub> = *a* ⋅ *e*<sup>*f*(*x*)</sup>

-   *D*<sub>*x*</sub>: employment density of a given area (census tract) employment levels per hectare
-   *x*: distance of the area to the central business district (CBD); i.e. local region's densest census tract
-   *f*(*x*): some function of distance to the CBD (located at *x* = 0) where *f*′(*x*)&lt;0

Here *a* represents some baseline value of employment or population density at the central location,

An employment density gradient can be estimated on these data to define the shape of how local employment decays (as a function of distance to the CBD).

The functional form of *f*(*x*) defines the decaying pattern of how we would expect densities to decline moving away from the dense CBD.

In log form *l**n*(*D*<sub>*x*</sub>), this function represents the proportional decay of density as influenced by distance to the CBD.

ln(*D*<sub>*x*</sub>)=ln(*a*)+*f*(*x*)

In [Ban et al. 2017](https://www.mdpi.com/2073-445X/6/1/17/htm "Identifying Employment Subcenters: The Method of Exponentially Declining Cutoffs"), an algorithm is developed to find clusters of census tracts which have employment densities higher than what this monocentric decay would predict - i.e. peripheral subcentre employment zones.

### 1.2. Functional Form of Employment Density Gradients

The way in which we assume employment density patterns will decay from this central point is important, and can have important consequences. *i.e. the functional form of *f*(*x*)*

Efficient, open data and code allows us to build tools to quickly test and validate different models, assumptions, parameters and outputs.

Updating the existing open code to allow for more flexible parameter choice and more flexibility in choosing the assumptions behind what we expect employment density decay to look like.

The ability to be able to do this completely start to finish for many regions across a country, automatized, comparably, fast, and for free - shows the usefulness of how open research can be used for deep analysis and increasingly robust work.

We explore four common density gradients across different regions in England.

(a, b)
*f*(*x*)=*α* − *β*<sub>1</sub> ⋅ *x*

``` r
x <- 1:10

p1 <- ggplot(data.frame(x, y = 6*exp(-x)), aes(x,y)) +
  stat_function(fun=function(x) 6*exp(-x), colour="gray65") +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 6), expand = c(0, 0)) +
  labs(y = "Density", caption="",
    title = "Employment Density Gradients", subtitle = expression(paste("(a) ", D==a,e^-b[1],""^x))) +
  theme_classic() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),  axis.title.x=element_blank(),
    axis.text.y=element_blank(), axis.ticks.y=element_blank())
p2 <- ggplot(data.frame(x, y = 5.75-x), aes(x,y)) +
  stat_function(fun=function(x) 5.75-x, colour="gray65") +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 6), expand = c(0, 0)) +
  labs(y = "log Density", caption="Linear f(x) proportional decay",
    title = "", subtitle = expression(paste("(b) ", ln(D)==ln(a)-b[1],x))) +
  theme_classic() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank(),
    axis.text.y=element_blank(), axis.ticks.y=element_blank(), plot.caption = element_text(face = "italic"))
grid.arrange(p1, p2, ncol=2)
```

![](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Gradient%20Decay:%20Linear-1.png)

(c, d)
*f*(*x*)=*α* + *β*<sub>1</sub> ⋅ *x* − *β*<sub>2</sub> ⋅ *x*<sup>2</sup>

``` r
grid.arrange(p1, p2, ncol=2)
```

![](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Gradient%20Decay:%20Squared1%20Print-1.png)

(e, f)
*f*(*x*)=*α* − *β*<sub>1</sub> ⋅ *x* + *β*<sub>2</sub> ⋅ *x*<sup>2</sup>

``` r
grid.arrange(p1, p2, ncol=2)
```

![](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Gradient%20Decay:%20Squared2%20Print-1.png)

(g, h)
*f*(*x*)=*α* − *β*<sub>1</sub> ⋅ *x* + *β*<sub>2</sub> ⋅ *x*<sup>2</sup> − *β*<sub>3</sub> ⋅ *x*<sup>3</sup>

``` r
grid.arrange(p1, p2, ncol=2)
```

![](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Gradient%20Decay:%20Cubic%20Print-1.png)

(i, j)
*f*(*x*)=*α* − *β*<sub>4</sub> ⋅ ln(*x*)

``` r
grid.arrange(p1, p2, ncol=2)
```

![](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Gradient%20Decay:%20Log%20Print-1.png)

We're going to thus update the subcentre employment zone algorithm from [Ban et al. 2017](https://www.mdpi.com/2073-445X/6/1/17/htm "Identifying Employment Subcenters: The Method of Exponentially Declining Cutoffs") and then using the flexible update, apply this quickly and efficiently across multiple municipal regions across England.

Section 2: Data and Study Region
--------------------------------

We are interested in the small area spatial distribution of employment across England. We'll need three data sources for this work, all openly available and accessible.

-   Workplace Zone employment data for England (local area working day population) [ONS Nomis](https://www.nomisweb.co.uk/ "ONS Nomis Labour Market Statistics")
    -   Table WP102EW (Census 2011)
-   Workplace Zone spatial boundary file for England [ONS Open Geography Portal](https://geoportal.statistics.gov.uk/ "The Open Geography Portal")
    -   *From this we can derive area, distance and density measures for each tract*
-   Lookup tables between Workplace Zone, Local Authority, Regions, and Combined Authority

This analysis is tested across multiple distinct regions (urban areas) in England - taken as the Combined Authority geographies. Subcentre identification is applied region by region across the country with tailored parameters to best fit local magnitudes.

We can calculate and present the code to generate aggregate employment levels.

``` r
WZ.db$density <- ifelse(is.na(WZ.db$employment/WZ.db$area_ha), 0, WZ.db$employment/WZ.db$area_ha)

WZ.db$dRNK.nat <- cut(WZ.db$density, breaks = quantile(WZ.db$density, probs = 0:10/10, na.rm=T), labels = 1:10, right = FALSE, include.lowest = TRUE)
WZ.db <- do.call(rbind, lapply(split(WZ.db, WZ.db$Region), function(x) { x$dRNK.lcl <- cut(x$density, breaks = quantile(x$density, probs = 0:10/10, na.rm=T), labels = 1:10, right = FALSE, include.lowest = TRUE); return(x) })); rownames(WZ.db) <- NULL

WZ.stats <- as_tibble(WZ.db) %>%
  dplyr::group_by(Region) %>%
  dplyr::summarize(Total_Area=sum(area_ha), Avg_Area=mean(area_ha), Med_Area=median(area_ha), Min_Area=min(area_ha), Max_Area=max(area_ha), SD_Area=sd(area_ha),
    Total_Emp=sum(employment), Avg_Emp=mean(employment), Med_Emp=median(employment), Min_Emp=min(employment), Max_Emp=max(employment), SD_Emp=sd(employment),
    Avg_Dens=sum(density), Med_Dens=mean(density), Min_Dens=min(density), Max_Dens=max(density), SD_Dens=sd(density), WZ_count=length(WZ_CD)) %>%
  filter(!is.na(Region)) %>%
  arrange(desc(Total_Emp))

knitr::kable(WZ.stats[order(-WZ.stats$Total_Emp), grepl("Region|Emp", names(WZ.stats))])
```

| Region                          |  Total\_Emp|  Avg\_Emp|  Med\_Emp|  Min\_Emp|  Max\_Emp|   SD\_Emp|
|:--------------------------------|-----------:|---------:|---------:|---------:|---------:|---------:|
| London                          |     4500481|  551.9354|     401.0|       103|     11403|  613.3386|
| Greater Manchester              |     1243218|  490.6148|     375.0|       106|      9547|  486.4084|
| West Midlands                   |     1197976|  506.7580|     387.0|       113|      7939|  481.3485|
| West Yorkshire                  |     1036058|  494.3025|     374.0|       124|      7257|  455.1762|
| Liverpool City Region           |      630377|  488.2858|     375.0|       113|      5754|  416.8719|
| Sheffield City Region           |      579575|  509.2926|     386.0|       155|      7050|  457.7573|
| West of England                 |      467957|  498.3568|     374.0|       127|      8947|  501.5129|
| Cambridgeshire and Peterborough |      416373|  492.7491|     381.0|       101|      7337|  425.3815|
| North East                      |      373278|  486.0391|     362.5|       131|      6565|  462.5036|
| Tees Valley                     |      281298|  480.0307|     376.0|       156|      7837|  443.9992|
| North of Tyne                   |      253873|  562.9113|     405.0|       121|      6771|  600.3466|

Or similarly descriptive statistics on size or average small area density.

``` r
knitr::kable(WZ.stats[order(-WZ.stats$Total_Area), grepl("Region|Area", names(WZ.stats))])
```

| Region                          |  Total\_Area|  Avg\_Area|  Med\_Area|  Min\_Area|   Max\_Area|    SD\_Area|
|:--------------------------------|------------:|----------:|----------:|----------:|-----------:|-----------:|
| Cambridgeshire and Peterborough |    337255.43|  399.11885|  47.762026|  0.2060222|   8346.0595|   836.02686|
| North East                      |    243172.53|  316.63090|  44.726467|  0.1928011|  22527.5945|  1379.86731|
| West Yorkshire                  |    202883.62|   96.79562|  28.326656|  0.1189364|   6315.8853|   284.40668|
| London                          |    157287.02|   19.28955|   8.811943|  0.0403218|    922.0444|    42.53142|
| Sheffield City Region           |    154793.41|  136.02233|  37.745286|  0.1445606|  11662.4321|   492.68673|
| Greater Manchester              |    127596.90|   50.35395|  22.223517|  0.1049514|   2585.5374|   126.65268|
| West of England                 |     95762.41|  101.98339|  19.534042|  0.1325896|   3139.8320|   316.15281|
| West Midlands                   |     90160.97|   38.13916|  23.966201|  0.1823792|   1662.1176|    72.92285|
| Tees Valley                     |     79229.52|  135.20395|  32.023780|  0.4261533|   3263.6117|   393.95844|
| Liverpool City Region           |     72042.84|   55.80390|  26.300237|  0.0956581|   1335.4281|   112.21359|
| North of Tyne                   |     19525.07|   43.29284|  20.116010|  0.2169694|   1528.1743|    99.29295|

``` r
knitr::kable(WZ.stats[order(-WZ.stats$Avg_Dens), grepl("Region|Dens|count", names(WZ.stats))])
```

| Region                          |   Avg\_Dens|  Med\_Dens|  Min\_Dens|   Max\_Dens|   SD\_Dens|  WZ\_count|
|:--------------------------------|-----------:|----------:|----------:|-----------:|----------:|----------:|
| London                          |  2637852.51|  323.50411|  0.3474238|  88094.2273|  1252.9912|       8154|
| Greater Manchester              |   244493.27|   96.48511|  0.1423814|  12155.1947|   387.6941|       2534|
| West Midlands                   |   206545.95|   87.37139|  0.3218531|   7440.5423|   326.9675|       2364|
| West Yorkshire                  |   173156.91|   82.61303|  0.0519325|   7785.6713|   287.5536|       2096|
| Liverpool City Region           |   102759.44|   79.59678|  0.2140405|   7637.3918|   306.7437|       1291|
| West of England                 |   101675.19|  108.28029|  0.1051173|   4502.6148|   292.4186|        939|
| Sheffield City Region           |    69949.42|   61.46698|  0.0319830|   5091.2920|   211.6335|       1138|
| Cambridgeshire and Peterborough |    50669.82|   59.96428|  0.0297146|   7406.9701|   286.6337|        845|
| North of Tyne                   |    48033.52|  106.50447|  0.4220723|   2489.5545|   237.0000|        451|
| Tees Valley                     |    30746.70|   52.46878|  0.0800669|    915.1636|   101.5189|        586|
| North East                      |    27721.54|   36.09575|  0.0150038|   2022.8099|   108.9077|        768|

### 2.2. Employment Density Plots

Plotting density decile ranks for each of the small area (WZ) areas across the city reveal hotspot patterns and spatial distributions of employment.

It is against these density plots that we want to evaluate the validity of the monocentric model - and to which we apply the subcentre identification algorithm.

``` r
p <- density.plots[["Liverpool City Region"]]
p
```

![Figure 2: Liverpool Employment Densities](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%202a-1.png)

``` r
p <- density.plots[["Greater Manchester"]]
p
```

![Figure 2: Manchester Employment Densities](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%202b-1.png)

``` r
p <- density.plots[["London"]]
p
```

![Figure 2: London Employment Densities](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%202c-1.png)

Section 3: Urban Monocentricity Validation
------------------------------------------

For a robust research methodology, we want to evaluate whether the assumed model is appropriate across different urban areas. The *Monocentric* city model is contingent on single directional distance from the central business district.

With many urban areas constrained by natural features such as bodies of water or topography, this uniform decay from the centre may be an unrealistic assumption upon which to identify employment subcentres.

-   Urban areas are much more complex, so we want to explore whether these stylized patterns exist in our actual data.

The applicability of this concern can be visuliazed by taking cross sections of the urban area centred on the CBD. We take bands of latitude and longitude points respectively around the presumed CBD and plot densities and employment across the univariate directions East-West and North-South respectively.

And for Liverpool

``` r
p <- ray.plots[["Liverpool City Region"]]
p
```

![Figure 3: Liverpool Bands](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%203-2-1.png)

With corresponding patterns in observed employment density:

``` r
p <- cross.sections[["Liverpool City Region"]][[1]]
p
```

    ## TableGrob (2 x 1) "arrange": 2 grobs
    ##   z     cells    name           grob
    ## 1 1 (1-1,1-1) arrange gtable[layout]
    ## 2 2 (2-2,1-1) arrange gtable[layout]

``` r
p <- cross.sections[["Liverpool City Region"]][[4]]
p
```

    ## TableGrob (2 x 1) "arrange": 2 grobs
    ##   z     cells    name           grob
    ## 1 1 (1-1,1-1) arrange gtable[layout]
    ## 2 2 (2-2,1-1) arrange gtable[layout]

Section 4: England Regional Employment Subcentre Identification
---------------------------------------------------------------

### 4.1. Estimated Employment Density Gradients by Region (Urban Area)

Included in this repository is the subcentre identification algorithm tool itself located in *Subcentre\_Identification.R*

This updated version of [Ban et al. 2017](https://www.mdpi.com/2073-445X/6/1/17/htm "Identifying Employment Subcenters: The Method of Exponentially Declining Cutoffs") can be applied to the different regions across England to systematically identify where we have **employment hotspots**

In first plotting the estimated density gradients of regions across England, we can select a more refined baseline employment levels and density values from which we evaluate subcentre deviations. The *distance* variable generated (within each region group) measures the distance of each Workzone tract from the respective tract within the region which has the highest employment density. While the choice of 'where' constitutes the 'central' area of an urban region, using the maximum density tract evaluates ......

For each of the estimated log (proportiate) gradient values across the different regions, we can extract the beta parameters from which we can build the different functional forms and generate a plot for each region. This allows us to view how different estimated functional forms can potentially impact

``` r
p <- estimated.density[["Liverpool City Region"]]
p
```

![Figure 4: Liverpool Density Gradients](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%204a-1.png)

``` r
p <- estimated.density[["Greater Manchester"]]
p
```

![Figure 4: Manchester Density Gradients](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%204b-1.png)

``` r
p <- estimated.density[["London"]]
p
```

![Figure 4: London Density Gradients](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%204c-1.png)

We can also plot the estimated model results from fitting the gradient along the different types of functional form, which helps us evaluate which model we should be choosing.

``` r
p <- ggplot() +
  geom_point(data=t[t$max==1,], aes(x = Region, y = Rsq, fill=Model, group = Model, color = Model)) +
  scale_color_manual(values = c("#d54062", "#ffa36c", "#ebdc87", "#799351")) +
  geom_point(data=t[t$max==0,], aes(x = Region, y = Rsq, fill=Model, group = Model, color = Model), alpha=0.3) +
  scale_color_manual(values = c("#d54062", "#ffa36c", "#ebdc87", "#799351")) +
  labs(y = "R Squared", x = "Region", title = "Estimated Density Gradient Fits") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom", legend.title = element_blank())
p
```

![](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Density%20Gradient%20Model%20Fits%20Plot-1.png)

### 4.2. Subcentre Identification

We get log(a) from the estimation directly, so we can use that as one of the options for our density cutoff can't we? That; average within some distance or some specified value per ha. already determined

``` r
p <- SC.plot[[1]]
p
```

![Figure 4: Liverpool Density Gradients](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%205a-1.png)

``` r
p <- SC.plot[[2]]
p
```

![Figure 4: London Density Gradients](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%205b-1.png)

References
----------

Office for National Statistics (ONS). 2014. "Workplace Zones: A new geography for workplace statistics" May - Bruce Mitchell <https://data.gov.uk/dataset/6620567e-f237-4c6b-b561-64a2bc218783/workplace-zones-a-new-geography-for-workplace-statistics>

Bin et al. (2017)

[Nomis](https://www.nomisweb.co.uk/ "ONS Nomis Labour Market Statistics") Table WP102EW

Links From The Slides
---------------------
