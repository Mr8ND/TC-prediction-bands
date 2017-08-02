#!/bin/bash

# This script should be run in the project folder location
# it creates folders for each validation TC in the desired folders

cd data/generate/

cd Val_Sims_Auto_DeathRegs

ls ../../training/validate/*.txt | xargs  basename | tr -d .txt | xargs -L 1 mkdir

cd ../Val_Sims_Auto_NoDeathRegs

ls ../../training/validate/*.txt | xargs  basename | tr -d .txt | xargs -L 1 mkdir

cd ../Val_Sims_NoAuto_DeathRegs

ls ../../training/validate/*.txt | xargs  basename | tr -d .txt | xargs -L 1 mkdir

cd ../Val_Sims_NoAuto_NoDeathRegs

ls ../../training/validate/*.txt | xargs  basename | tr -d .txt | xargs -L 1 mkdir
