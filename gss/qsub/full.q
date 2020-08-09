#!/bin/bash
#$ -N full_parafac
#$ -q LT
#$ -l mf=64G        
#$ -l h_rt=320:00:00
#$ -l s_rt=320:00:00
#$ -wd /Users/ssrivastva/gss/code/
#$ -m a
#$ -M sanvesh-srivastava@uiowa.edu
#$ -t 1-10
#$ -V
#$ -e /Users/ssrivastva/err/
#$ -o /Users/ssrivastva/out/

module load matlab/R2015a

matlab -nojvm -nodisplay -singleCompThread -r "submit_parafac_full($SGE_TASK_ID, 20, 10000, 5000, 5)"

