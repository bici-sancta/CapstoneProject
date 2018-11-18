---
title: "to_do_home_stretch"
author: ""
date: "18 novembre 2018"
output: html_document
---

0. Table that shows (by address/intersection) which locations are most severe, both from cost factor and also number of events, associate to grid cell number. At results / analysis section, show same list in comparison to list of Top20 PSI grid cells. Need address/intersection for grid cells that are Top20 PSI.

1. All plots - big size labels & symbols so everything is easily distinguished in published paper format. Color choices ? I use a lot of colors from http://colorbrewer2.org/

2. side=by-side figures can be done like this. (I think the Fiugre number reference needs some tweaking)

```{r eval = FALSE}
\usepackage{subcaption}		% enables side-by-side figures
\captionsetup{compatibility = false}	% needed for subcaption package

\begin{document}

Figure ~\ref{figure : dataFeatureDevelopment}. \newline
\begin{figure}
 \begin{subfigure}[b]{0.5\textwidth}
    \includegraphics[width = \textwidth, height = \textheight, keepaspectratio]{trafficAccidents.png}
    \caption{Walk Score - Max values}
    \label{fig:1}
  \end{subfigure}
%
  \begin{subfigure}[b]{0.5\textwidth}
    \includegraphics[width = \textwidth, height = \textheight, keepaspectratio]{busStopDistance.png}
    \caption{Public Transportaton accessibility}
    \label{fig:2}
  \end{subfigure}
\end{figure}

\end{document}
```

3. we can put plots in a subdirectory :

```{r eval = FALSE}

% ... -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
% ...	 directory paths for images
% ... -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

\graphicspath{
    {.} % document root dir
    {images/}
}
```

4. Empirical Bayesian
- there is a fair amount of literature from highway safety studies that describe both standard and locally developed weighting factors for empirical bayesian. We can pick one of the pre-defined weighting factors to complete the bayesian estimate for regression model. We will need some paragraphs in text to describe / defend the choice of weighting factor. Our weighting factor will be ~0.5.

```{r eval = FALSE}
    http://onlinepubs.trb.org/onlinepubs/nchrp/docs/nchrp17-50_userguide.pdf
    https://safety.fhwa.dot.gov/rsdp/downloads/spf_development_guide_final.pdf
    https://safety.fhwa.dot.gov/hsip/resources/fhwasa09029/sec6.cfm
```

(the last one has tables of standardized weighting factors, based on vehicle, road types, intersection types, etc)
 

5. Supervised Learning model structure
- should consider the model is something like this :
P(event_i) = p(binary model) * p(regression model)
this provides first level estimation of event/non-event and then estimation of severity based on regression model
- we shold probably do a BLR to compare with the random forest binary predictor

6. Data description - update section to clearly describe how each data set was manipulated to produce its feature set .. (__Pat__) Include table that shows range of values or characteristics of each data set

7. Cross correlation heat map / analysis ... which features are cross-correlated ?

8. We never did develop the whole complex set of features using combinations of features (+/-/*//). Do we want to do anything like this ... too late ? or try something anyway ?

9. Do we want to combine Results & Analysis into one section ?

10. Update Conclusions 




