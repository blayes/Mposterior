#!/bin/bash
#$ -N marg10
#$ -q LT
#$ -l mf=8G        
#$ -l h_rt=320:00:00
#$ -l s_rt=320:00:00
#$ -wd /Users/ssrivastva/gss/code
#$ -m a
#$ -M sanvesh-srivastava@uiowa.edu
#$ -t 1-90
#$ -V
#$ -e /Users/ssrivastva/err/
#$ -o /Users/ssrivastva/out/

module load R/3.2.1

R CMD BATCH --no-save --no-restore "--args 1 $SGE_TASK_ID" submit_mpost.R marg/marg10_$SGE_TASK_ID.rout


