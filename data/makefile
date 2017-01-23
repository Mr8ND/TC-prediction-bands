all:
	make get_data
	make structure_data

get_data: 
	wget http://www.aoml.noaa.gov/hrd/hurdat/hurdat2-1851-2015-070616.txt

structure_data:
	mkdir generate
	mkdir test
	mkdir training

	cd training && mkdir {train,train_50,validate,validate_50}

	cd generate && mkdir {Val_Sims_Auto_DeathRegs,Val_Sims_Auto_NoDeathRegs,Val_Sims_NoAuto_DeathRegs,Val_Sims_NoAuto_NoDeathRegs}
