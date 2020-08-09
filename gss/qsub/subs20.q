#!/bin/bash
#$ -N subs20_parafac
#$ -q LT
#$ -l mf=16G        
#$ -l h_rt=320:00:00
#$ -l s_rt=320:00:00
#$ -wd /Users/ssrivastva/gss/code/
#$ -m a
#$ -M sanvesh-srivastava@uiowa.edu
#$ -t 1-200
#$ -V
#$ -e /Users/ssrivastva/err/
#$ -o /Users/ssrivastva/out/

module load matlab/R2015b

matlab -nojvm -nodisplay -singleCompThread -r "submit_parafac_sub20($SGE_TASK_ID, 20, 20, 10000, 5000, 5)"
