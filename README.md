# Case study on input sensitivity and transfer learning of performance prediction of x264 configurations 


`x264-analysis-final.ipynb` is the main Python notebook. 
There is one important parameter, specified as a global variable: `predDimension`. 
It can equal to `size` or `elapsedtime`, since we're conduction the study for the two performance properties (encoding time and size of the output videos). 

Call `jupyter-notebook` and an execution of this notebook should work.  
It should compute many things, save some figures in PDF or PNG, etc. 

All data is here: 
```
dataTimeFolder = './datacalda2/'
dataSizeFolder = './datay4m2/'
```

Paper (in progress)
https://www.overleaf.com/10673417tkmtdmhyvmrs

## Requirements 

pandas, scikit-learn, dot, seaborn, numpy, matplotlib, scipy (to complete) 

## Other files

 * RawDataAnalysis.ipynb: a very specific script that loads raw data/raw logs and properly organize them. The raw data includes measurements' repetitions, as well as traces of x264 executions. The script eliminates some measurements since they have been made on different machines and different conditions. It's a few gigabytes and stored in an hard disk. This script is also useful for generating LaTeX tables about videos and about measurements' repetitions. This script is useful for a raw and preliminary analysis -- you're certainly interested by the organized data 
 * other notebooks are some past/exploratory attempts with some interesting ideas 
 * there are also R scripts that are no longer maintained 
 * many PDFs, PNGs, etc. 

 





