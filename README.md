<h3>Datavisualization Metagenomics </h3>

R scripts to generate bar plots with metagenomics data.

Install R and R studio
    Install libraries: 
        tidyverse
        readxl
        openxlsx
        ggplot2
        tidyr
        wesanderson
        cowplot

The count data representing taaxonomic and functional output from taxonomic and functional profiling of metagenomics data is used to generate the visual comparison between samples.

figure1.R generates a single stacked bar plot and a legend, applicable for observations at phylum level or at class level when the number of observed categories are few. 

figure2.R generates a stacked bar plot and a seperate legend, applicble when you want to look at large number of categories mostly seen at a genus or phylum level.

figure4.R generates a bar plot of catetgories as percentage, applicable to visualize functional profile at a certain level and to visually compare various categories within one or more systems.

figure6.R generates a bar plot using observed count and not as a percentage to visualize actual differences between the two samples