# Change of support

## Simple features (OGC + R::sf)

    - point
    - linestring
    - polygon
    - multilinestring
    - multipolygon
    - geometry collection

## Types

    - Categorical (point -> line)
    - Qualitative (e.g. polygon changes shape)
    - Clusters (point -> polygon)

## Test data

    - OBIA
    - Tweet clusters
 
## Input 

    - Time seris of sets of multipolygons
    -> One rectangular object

## Result

    - Where is object A (t + dt)?
    - What was the change of support A (t) -> A (t + dt)?

## Output 
    
    -labelled multipolygons
    - extra CoS column
    - Hierarichal CoS?
    - Encoding CoS
   
## Process
    
    -Machine Learning 

## Meta-Task

- Find examples of Vector input to ML

# To do

    ~~- Install Rstudio~~
    ~~- Set up package in github/zgis~~
    - Machine learning for this problem?
