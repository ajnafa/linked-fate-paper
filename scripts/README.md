Reproducibility Information
================

## System and Computational Information

The sub-folders of this directory contain each of the scripts necessary
to reproduce the analysis in the main text and are sequentially numbered
in the order they must be executed. All analyses presented in the
manuscript were performed under R version 4.2.1 “Funny-Looking Kid” on a
Windows 10 desktop computer with a 12-core Ryzen 9 5900X CPU, 128GB of
DDR4 Memory, and an Nvidia RTX 3080TI GPU using cmdstan 2.30.1 as a
backend via `cmdstanr` version 0.5.3 and `brms` version 2.17.5 ([Bürkner
2017](#ref-Buerkner2017), [2018](#ref-Buerkner2018); [Gabry and Češnovar
2022](#ref-Gabry2022); [R Core Team 2022](#ref-RLang2022)).

## Data Preparation

- The survey data we rely on is currently only available to those listed
  as [collaborators on the 2020
  CMPS](https://ucla.box.com/shared/static/kj0b769nu55r443sb113jo9egyzs6jiw.pdf)
  and their co-authors but can be obtained from the ICPSR repository
  sometime in 2023. In the meantime, detailed information on the survey
  design can be found at [the webpage for the 2020
  CMPS](https://cmpsurvey.org/2020-survey/) ([Frasure et al.
  2021](#ref-Frasure2021)). All pre-processing and harmonization
  necessary for compatability with the Census data we use for
  post-stratification is performed by and documented in the scripts
  [`01_Latino_CMPS_2020.R`](01-data-prep/01_Latino_CMPS_2020.R) and
  [`02_Asian_CMPS_2020.R`](01-data-prep/02_Asian_CMPS_2020.R) located
  under the `01-data-prep` directory of this folder.

- Data for the post-stratification tables is based on the U.S. Census
  Bureau’s 5-year estimates from the 2019 American Community Survey and
  can be obtained from the [IPUMS USA data
  portal](https://usa.ipums.org/usa/) ([Ruggles et al.
  2022](#ref-Ruggles2022)). All recoding and aggregation necessary to
  construct the post-stratification tables for each group is performed
  by and documented in the scripts
  [`03_Latino_Post-Stratification_Table.R`](01-data-prep/03_Latino_Post-Stratification_Table.R)
  and
  [`04_AAPI_Post-Stratification_Table.R`](01-data-prep/04_AAPI_Post-Stratification_Table.R)
  located under the `01-data-prep` directory of this folder.

- This step requires the `tidyverse` ([Wickham et al.
  2019](#ref-Wickham2019)), `sjlabelled` ([Lüdecke
  2022](#ref-Ludecke2022)), `arrow` ([Richardson et al.
  2022](#ref-Richardson2022)), `ipumsr` ([Freedman Ellis and Burk
  2022](#ref-GFE2022)), `datawizard` ([Patil et al.
  2022](#ref-Patil2022)), and `haven` packages ([Wickham, Miller, and
  Smith 2022](#ref-Wickham2022))

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Buerkner2017" class="csl-entry">

Bürkner, Paul-Christian. 2017. “[<span class="nocase">brms</span>: An R
Package for Bayesian Multilevel Models Using
Stan](https://doi.org/10.18637/jss.v080.i01).” *Journal of Statistical
Software* 80: 1–28.

</div>

<div id="ref-Buerkner2018" class="csl-entry">

———. 2018. “[Advanced Bayesian Multilevel Modeling with the R Package
Brms](https://doi.org/10.32614/RJ-2018-017).” *The R Journal* 10:
395–411.

</div>

<div id="ref-Frasure2021" class="csl-entry">

Frasure, Lorrie, Janelle Wong, Matt A. Barreto, and Edward D. Vargas.
2021. “The 2020 Collaborative Multiracial Post-Election Survey (CMPS).”

</div>

<div id="ref-GFE2022" class="csl-entry">

Freedman Ellis, Greg, and Derek Burk. 2022. *<span
class="nocase">ipumsr</span>: Read ’IPUMS’ Extract Files*.
<https://CRAN.R-project.org/package=ipumsr>.

</div>

<div id="ref-Gabry2022" class="csl-entry">

Gabry, Jonah, and Rok Češnovar. 2022. *<span
class="nocase">cmdstanr</span>: R Interface to ’CmdStan’*.

</div>

<div id="ref-Ludecke2022" class="csl-entry">

Lüdecke, Daniel. 2022. *<span class="nocase">sjlabelled</span>: Labelled
Data Utility Functions (Version 1.2.0)*.
<https://CRAN.R-project.org/package=sjlabelled>.

</div>

<div id="ref-Patil2022" class="csl-entry">

Patil, Indrajeet et al. 2022. “<span class="nocase">datawizard</span>:
An R Package for Easy Data Preparation and Statistical Transformations.”
*CRAN*. <https://easystats.github.io/datawizard/>.

</div>

<div id="ref-RLang2022" class="csl-entry">

R Core Team. 2022. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-Richardson2022" class="csl-entry">

Richardson, Neal et al. 2022. *<span class="nocase">arrow</span>:
Integration to ’Apache’ ’Arrow’*.
<https://CRAN.R-project.org/package=arrow>.

</div>

<div id="ref-Ruggles2022" class="csl-entry">

Ruggles, Steven et al. 2022. “[IPUMS USA: Version
12.0](https://doi.org/10.18128/D010.V12.0).”

</div>

<div id="ref-Wickham2019" class="csl-entry">

Wickham, Hadley et al. 2019. “[Welcome to the <span
class="nocase">tidyverse</span>](https://doi.org/10.21105/joss.01686).”
*Journal of Open Source Software* 4(43): 1686.

</div>

<div id="ref-Wickham2022" class="csl-entry">

Wickham, Hadley, Evan Miller, and Danny Smith. 2022. *<span
class="nocase">haven</span>: Import and Export ’SPSS’, ’Stata’ and ’SAS’
Files*. <https://CRAN.R-project.org/package=haven>.

</div>

</div>
