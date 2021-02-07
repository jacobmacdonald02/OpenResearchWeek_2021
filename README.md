ORW2021: Tools And Technology For Open Research - Research Analytical
================
Jacob L. Macdonald, University of Liverpool; <Jacob.Macdonald@liverpool.ac.uk>

Getting The Results Out There
-----------------------------

*Part 2 of Open Research Week 2021 Talk - Tools and Technology for Open Research*

Using some work on open data, this research analytical showcases an online presentation of outputs, visualization, code and analysis. We'll see a side by side comparison of the code and the output on github to give an example of how different research deliverables can be shared.

**Github** + **RMarkdown** are great tools to use together for making research and outputs accessible. This is one way in which we can combine the best of the statistical and analytical capabilities of R with the tracking, presentation and collaborative nature of Github.

Primarily, **Git** and **Github** are used to monitor and version control coding documents. This has developed itself into a hosting site for a wide range and types of outputs.

During this talk, we'll overview the main ways and examples of how to incorporate: \* Text \* Code \* Visualization \* Methodologies \* Analyses

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

![](https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/README_files/figure-markdown_github/Rmd Code Example-1.png?raw=true)



Section 1: Parametric Employment Subcentre Identification for Great Britain
---------------------------------------------------------------------------

Urban metropolitan areas are often seen to comprise clusters of employment areas over space; often with a primary nucleus and surrounding satellite *subcentres* where employment densities are strong.

Stylized facts across the globe have given rise to a large literature on the *Monocentric City Model* where a city's employment density decreases at some functional rate from a singular central business district.

Increasingly granular (census tract) and accessible data allow us to explore the validity of these theoretic models, and more generally apply their concepts to real world data to explore employment (or population) densities. Identifying patterns in the cluster and spatial configuration of the data.

*Can we build a programatic way to efficiently explore a wide variety of flexible models across a range of urban areas.* *Bbased only on openly accessible tools and software.*

Update to code for exisiting employment subcentre identification in LA and tailored to the local GB context [Ban et al. 2017](https://www.mdpi.com/2073-445X/6/1/17/htm "Identifying Employment Subcenters: The Method of Exponentially Declining Cutoffs").

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

Different assumptions on the functional form of *f*(*x*) can have a difference on the assumed pattern of density decay away from the CBD and

updating the original algorithm to adapt of these differences is useful in applying the model in different urban contexts.

-   the updated code allows for more flexible parameter choice (original model scaled to the US) and more flexibility in choosing the assumptions behind what we expect employment density decay to look like.

The ability to be able to do this completely start to finish for many regions across a country, automatized, comparably, fast, and for free - shows the usefulness of how open research can be used for deep analysis and increasingly robust.

We explore four common density gradients across different regions in England.

The following figure, adapted from figure 1 of Warnes (1975) represents the commonly used employment density gradient patterns. These plots highlight how density levels, and the log density levels - representing the proportional density .... - vary as we move further away from the CBD.

Using a different functional form as opposed to the linear version allows for the estimated density gradient, from which deviations are used to identify subcentres, to adapt relative to distance to the CBD. A constant gradient parameter from the Linear version would discount the proportional rate of decline by a constant rate.

(a, b)
*f*(*x*)=*α* − *β*<sub>1</sub> ⋅ *x*

(c, d)
*f*(*x*)=*α* + *β*<sub>1</sub> ⋅ *x* − *β*<sub>2</sub> ⋅ *x*<sup>2</sup>

(e, f)
*f*(*x*)=*α* − *β*<sub>1</sub> ⋅ *x* + *β*<sub>2</sub> ⋅ *x*<sup>2</sup>

(g, h)
*f*(*x*)=*α* − *β*<sub>1</sub> ⋅ *x* + *β*<sub>2</sub> ⋅ *x*<sup>2</sup> − *β*<sub>3</sub> ⋅ *x*<sup>3</sup>

(i, j)
*f*(*x*)=*α* − *β*<sub>4</sub> ⋅ ln(*x*)

``` r
knitr::include_graphics("https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/Outputs/ExDensGradients.jpeg")
```

![Figure 1: Theoretic Employment Density Gradients](https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/Outputs/ExDensGradients.jpeg)

We're going to thus update the subcentre employment zone algorithm from Ban et al. (2017) and then using the flexible update, apply this quickly and efficiently across multiple municipal regions across GB.

Section 2: Data and Study Region
--------------------------------

We'll need three data sources for this work, all openly available and accessible.

-   Workplace Zone employment data for England (local area working day population) [ONS Nomis](https://www.nomisweb.co.uk/ "ONS Nomis Labour Market Statistics")
-   WZ geography (Table WP102EW)
-   Workplace Zone spatial boundary file for England [ONS Open Geography Portal](https://geoportal.statistics.gov.uk/ "The Open Geography Portal") -- from this we can derive area and distance measures for each tract
-   -   densities to be calculated using areas derived from spatial file

-   Lookup tables between Workplace Zone, Local Authority, Regions, and Combined Authority

This analysis is conducted across multiple distinct regions (representing urban areas) in England.

Subcentre identification is applied region by region across the country with tailored parameters to best fit local magnitudes.

The collection of Combined Authorities in England is used in addition to London, to represent metropolitan areas. There are a total of 11 of these regions.

For each region we want to assess the validity of the model, look at the patterns of employment density over space, and apply the model with the best parameter fits to identify subcentres using open granular census employment data.

The identification of employment subcentres is conditional only on total employment levels and densities across the small area geographies.

Data and lookup tables are provided within this repo and are all open source and obtained from the ONS and NOMIS catalogues:

``` r
WZ.stats[order(-WZ.stats$Total_Emp), grepl("Region|Emp", names(WZ.stats))]
```

    ## # A tibble: 11 x 7
    ##    Region                       Total_Emp Avg_Emp Med_Emp Min_Emp Max_Emp SD_Emp
    ##    <fct>                            <int>   <dbl>   <dbl>   <int>   <int>  <dbl>
    ##  1 London                         4500481    552.    401      103   11403   613.
    ##  2 Greater Manchester             1243218    491.    375      106    9547   486.
    ##  3 West Midlands                  1197976    507.    387      113    7939   481.
    ##  4 West Yorkshire                 1036058    494.    374      124    7257   455.
    ##  5 Liverpool City Region           630377    488.    375      113    5754   417.
    ##  6 Sheffield City Region           579575    509.    386      155    7050   458.
    ##  7 West of England                 467957    498.    374      127    8947   502.
    ##  8 Cambridgeshire and Peterbor…    416373    493.    381      101    7337   425.
    ##  9 North East                      373278    486.    362.     131    6565   463.
    ## 10 Tees Valley                     281298    480.    376      156    7837   444.
    ## 11 North of Tyne                   253873    563.    405      121    6771   600.

``` r
WZ.stats[order(-WZ.stats$Total_Area), grepl("Region|Area", names(WZ.stats))]
```

    ## # A tibble: 11 x 7
    ##    Region                 Total_Area Avg_Area Med_Area Min_Area Max_Area SD_Area
    ##    <fct>                       <dbl>    <dbl>    <dbl>    <dbl>    <dbl>   <dbl>
    ##  1 Cambridgeshire and Pe…    337255.    399.     47.8    0.206     8346.   836. 
    ##  2 North East                243173.    317.     44.7    0.193    22528.  1380. 
    ##  3 West Yorkshire            202884.     96.8    28.3    0.119     6316.   284. 
    ##  4 London                    157287.     19.3     8.81   0.0403     922.    42.5
    ##  5 Sheffield City Region     154793.    136.     37.7    0.145    11662.   493. 
    ##  6 Greater Manchester        127597.     50.4    22.2    0.105     2586.   127. 
    ##  7 West of England            95762.    102.     19.5    0.133     3140.   316. 
    ##  8 West Midlands              90161.     38.1    24.0    0.182     1662.    72.9
    ##  9 Tees Valley                79230.    135.     32.0    0.426     3264.   394. 
    ## 10 Liverpool City Region      72043.     55.8    26.3    0.0957    1335.   112. 
    ## 11 North of Tyne              19525.     43.3    20.1    0.217     1528.    99.3

``` r
WZ.stats[order(-WZ.stats$Avg_Dens), grepl("Region|Dens|count", names(WZ.stats))]
```

    ## # A tibble: 11 x 7
    ##    Region                   Avg_Dens Med_Dens Min_Dens Max_Dens SD_Dens WZ_count
    ##    <fct>                       <dbl>    <dbl>    <dbl>    <dbl>   <dbl>    <int>
    ##  1 London                   2637853.    324.    0.347    88094.   1253.     8154
    ##  2 Greater Manchester        244493.     96.5   0.142    12155.    388.     2534
    ##  3 West Midlands             206546.     87.4   0.322     7441.    327.     2364
    ##  4 West Yorkshire            173157.     82.6   0.0519    7786.    288.     2096
    ##  5 Liverpool City Region     102759.     79.6   0.214     7637.    307.     1291
    ##  6 West of England           101675.    108.    0.105     4503.    292.      939
    ##  7 Sheffield City Region      69949.     61.5   0.0320    5091.    212.     1138
    ##  8 Cambridgeshire and Pete…   50670.     60.0   0.0297    7407.    287.      845
    ##  9 North of Tyne              48034.    107.    0.422     2490.    237.      451
    ## 10 Tees Valley                30747.     52.5   0.0801     915.    102.      586
    ## 11 North East                 27722.     36.1   0.0150    2023.    109.      768

### 2.2. Employment Density Plots

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

``` r
p <- density.plots[["West Midlands"]]
p
```

![Figure 2: West Midlands Employment Densities](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%202d-1.png)

Section 3: Urban Monocentricity Validation
------------------------------------------

The above subcentre identification strategy is contigent upon the single directional *distance from the CBD* and does not take into account the directionality of the employment density with respect to the central area. With many urban areas constrained by natural features such as bodies of water or topography, this uniform decay from the centre may be an unrealistic assumption upon which to identify employment subcentres.

The applicability of this concern can be visuliazed by taking cross sections of the urban area centred on the CBD. We take bands of latitude and longitude points respectively around the presumed CBD and plot densities and employment across the univariate directions East-West and North-South respectively.

-- if we find this looks ok, then great. Hopefully for London it does because it's so concentric. Be nice to find an example that works and one that doesn't. London vs. Cambridge

``` r
knitr::include_graphics("https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/Outputs/LIV/DensDeciles_LIV.jpeg")
```

![Figure 3: Liverpool Employment Densities](https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/Outputs/LIV/DensDeciles_LIV.jpeg)

``` r
knitr::include_graphics("https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/Outputs/MAN/DensDeciles_MAN.jpeg")
```

![Figure 3: Manchester Employment Densities](https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/Outputs/MAN/DensDeciles_MAN.jpeg)

``` r
knitr::include_graphics("https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/Outputs/LND/DensDeciles_LND.jpeg")
```

![Figure 3: London Employment Densities](https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/Outputs/LND/DensDeciles_LND.jpeg)

``` r
knitr::include_graphics("https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/Outputs/WsM/DensDeciles_WsM.jpeg")
```

![Figure 3: West Midlands Employment Densities](https://github.com/jacobmacdonald02/OpenResearchWeek_2021/tree/master/Outputs/WsM/DensDeciles_WsM.jpeg)

Section 4: England Regional Employment Subcentre Identification
---------------------------------------------------------------

### 4.1. Estimated Employment Density Gradients by Region (Urban Area)

Although the *Subcentre\_Identification.R* function will calculate the density and distance variables for each region directly in itself, we generate these variables here for the purpose of visualizing the estimated employment densities of different areas across England. Visualizing these dynamics prior to attempting to identify subcentres is key to accounting for differences in the absolute employment levels and the density gradients of different regions. While the original subcentre identification employment and density parameters used in Bin et al. (2017) were appropriate in Los Angeles, they stem from seminal work on employment subcentres by Guilliano and Small (199x) for the region. Not only does (for e.g.) London have significantly different sprawl and density patterns than Los Angeles - and many US urban areas - employment levels which form the basis of cutoff thresholds need to also be adjusted for the relative increase in population and employment centre vitality over time and across continents.

In first plotting the estimated density gradients of regions across England, we can select a more refined baseline employment levels and density values from which we evaluate subcentre deviations. The *distance* variable generated (within each region group) measures the distance of each Workzone tract from the respective tract within the region which has the highest employment density. While the choice of 'where' constitutes the 'central' area of an urban region, using the maximum density tract evaluates ......

``` r
source(paste0(wd, "/Code/Subcentre_Identification.R"))

# We source the Subcentre_Identification.R function located in the Code folder. This function reads in a cleaned shapefile to identify employment subcentres based on our choice of functional form and input parameters. 

WZ.db <- st_transform(WZ.db, crs=4326)
WZ.db2 <- lapply(split(WZ.db, WZ.db$Region), function(x) {x$dist_CBD <- as.numeric(st_distance(x, x[which.max(x$density),], which ="Great Circle"))*0.001; return(x)})
WZ.db <- do.call(rbind, WZ.db2)
rownames(WZ.db) <- NULL
rm(WZ.db2)

WZ.db$dist_CBD.sq <- WZ.db$dist_CBD*WZ.db$dist_CBD
WZ.db$dist_CBD.cb <- WZ.db$dist_CBD.sq*WZ.db$dist_CBD
```

For each of the estimated log (proportiate) gradient values across the different regions, we can extract the beta parameters from which we can build the different functional forms and generate a plot for each region. This allows us to view how different estimated functional forms can potentially impact

The employment density gradient is obtained by estimating the rate at which the proportional rate of employment density falls with respect to distance from the CDB. This can be estimated by regressing the log of Workzone employment density (ln(*D*<sub>*z*</sub>)) on various function forms for the distance from the CBD (*f*(*x*<sub>*z*</sub>)).

The slope of the ln(*D*<sub>*z*</sub>)=*f*(*x*<sub>*z*</sub>) function represents the rate at which the proportional employment density falls with respect to distance from the CBD. This gradient value is used in the Bin et al. (2017) to trace out the employment density patterns expected at each tract level conditional on distance to the densest point. With the linear version estimated in this paper, we assumed a constant rate with which proportional employment density was discounted in addition to their distance.

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

``` r
p <- estimated.density[["West Midlands"]]
p
```

![Figure 4: West Midlands Density Gradients](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Figure%204d-1.png)

We can also plot the estimated model results from fitting the gradient along the different types of functional form, which helps us evaluate which model we should be choosing.

``` r
ggplot() +
  geom_point(data=t[t$max==1,], aes(x = Region, y = Rsq, fill=Model, group = Model, color = Model)) +
  scale_color_manual(values = c("#d54062", "#ffa36c", "#ebdc87", "#799351")) +
  geom_point(data=t[t$max==0,], aes(x = Region, y = Rsq, fill=Model, group = Model, color = Model), alpha=0.3) +
  scale_color_manual(values = c("#d54062", "#ffa36c", "#ebdc87", "#799351")) +
  labs(y = "R Squared", x = "Region", title = "Estimated Density Gradient Fits") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="bottom", legend.title = element_blank())
```

![](/Users/jake_mac02/Dropbox/Research/OpenResearchWeek_2021/README_files/figure-markdown_github/Density%20Gradient%20Model%20Fits%20Plot-1.png)

### 4.2. Subcentre Identification

We get log(a) from the estimation directly, so we can use that as one of the options for our density cutoff can't we? That; average within some distance or some specified value per ha. already determined

References
----------

Office for National Statistics (ONS). 2014. "Workplace Zones: A new geography for workplace statistics" May - Bruce Mitchell <https://data.gov.uk/dataset/6620567e-f237-4c6b-b561-64a2bc218783/workplace-zones-a-new-geography-for-workplace-statistics>

Bin et al. (2017)

[Nomis](https://www.nomisweb.co.uk/ "ONS Nomis Labour Market Statistics") Table WP102EW

Warnes 1975

Giuliano and Small (1991)

Links From The Slides
---------------------
