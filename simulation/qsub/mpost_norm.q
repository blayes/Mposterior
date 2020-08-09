#!/bin/bash
#$ -N subs_norm
#$ -q LT
#$ -l h_rt=320:00:00
#$ -l s_rt=320:00:00
#$ -wd /Users/ssrivastva/mposterior/code/
#$ -m a
#$ -M sanvesh-srivastava@uiowa.edu
#$ -t 1-1250
#$ -V
#$ -e /Users/ssrivastva/err/
#$ -o /Users/ssrivastva/out/
#$ -j y

module load R/3.3.2_gcc-5.4.0

R CMD BATCH --no-save --no-restore "--args 5 $SGE_TASK_ID" submit.R subs/norm_subs_$SGE_TASK_ID.rout
