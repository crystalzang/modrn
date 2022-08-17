# Online Random Effect Meta-Analysis Calculator

You can access the app here: 

https://statshero.shinyapps.io/modrn/

# Objective
This R Shiny online calculator performs a random effects meta-analysis using 
the Hartung-Knapp-Sidik-Jonkman method and provide convenient visualization and 
downloadable output summaries. Estimates from individual sites are combined to 
produce pooled estimates. The required inputs are the site-specific estimates 
for a single or multiple parameters, their standard errors, and columns that 
identifies the site and the parameter. A sample input is provided under the 
data tab. Important outputs include the pooled estimates along with confidence
intervals (CI). In addition, it generates prediction intervals (PI) around the 
pooled estimates, which represent the interval we would expect to contain the 
estimates if the analysis had been conducted with a different sample of states. 
The confidence level of the CI and PI can be separately specified and modified 
via slider controls.
