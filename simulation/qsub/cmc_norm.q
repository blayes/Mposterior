#!/bin/bash
#$ -N cmc_norm
#$ -q INFORMATICS
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

module load R

R CMD BATCH --no-save --no-restore "--args 6 $SGE_TASK_ID" submit.R cmc/norm_cmc_$SGE_TASK_ID.rout
