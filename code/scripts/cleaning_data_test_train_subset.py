# initial data cleaning and processing

#################
# Data Read in: #
#################

import numpy as np
import pickle
import re
from collections import Counter
import matplotlib.pyplot as plt

with open("../../data/hurdat2-1851-2015-070616.txt","r") as ftext:
	read_data =ftext.read()



data_line = re.split("\n",read_data)
if data_line[-1] == "":
	data_line = data_line[:-1]

data_dict = dict()
for line in data_line:
	line = 	re.sub(" ","",line)

	line_parts = re.split(",", line)

	clean_line = np.array(line_parts)[np.array(line_parts)!=""]
	
	if len(clean_line)==3:
		assert(clean_line[0] not in data_dict.keys())
		hurricane_name = clean_line[0]
		data_dict[hurricane_name] =dict()
		data_dict[hurricane_name]["name"] = clean_line[1]
		data_dict[hurricane_name]["n_points"] = clean_line[2]
		print(hurricane_name)
	else:
		data_dict[hurricane_name][str(clean_line[0])+":"+str(clean_line[1])]= clean_line[1:]

#############
# Data Save #
#############


pickle.dump(data_dict,open("../../data/"+"hurricane_dict_full.pkl","wb"))
# load that file:
#with open("../../data/"+"hurricane_dict_full.pkl","rb") as pfile:
#	hurricanes = pickle.load(pfile) 


################################
# Subsetting for TC after 1950 #
################################

data_dict_new = dict()
for key in data_dict.keys():
	if int(key[-4:]) > 1950:
		data_dict_new[key] = data_dict[key]


data_dict_old = data_dict
data_dict = data_dict_new

pickle.dump(data_dict,open("../../data/"+"hurricane_dict.pkl","wb"))
# load that file:
# with open("../../data/"+"hurricane_dict.pkl","rb") as pfile:
# 	hurricanes = pickle.load(pfile) 



########################################
# Splitting Data (Test/Training 2/1)
########################################

####
# Thought, we'll do a randomly selection per year with 2/1 ratio for test/train


#####
# Only run on Ben's Computer so as not to allow python to mess with setting seed 
# 	(line 97)
run_initial_time =False #only done on Ben's computer

if run_initial_time ==True:
	count_dict=dict()

	for key in data_dict.keys():
		a = key[-4:]
		if not a in count_dict.keys():
			count_dict[a] = 1
		else:
			count_dict[a] += 1

	xx = np.array(sorted(count_dict.keys()))
	count_vec=np.array([count_dict[name] for name in sorted(count_dict.keys())])

	np.random.seed(2)

	storage = np.zeros(xx.shape[0],dtype = np.int)
	for i in np.arange(storage.shape[0]):
		n = count_vec[i]
		storage[i] = n//3
		if n%3 != 0:
			storage[i] +=np.random.binomial(n=1,p=(n%3)/3)

	test_selection = dict()
	for i in np.arange(storage.shape[0]):
		amount = storage[i] 
		n      = count_vec[i]
		test_selection[xx[i]] = np.random.choice(a = np.arange(n),
								 size = amount,replace=False)

	names_dict_sorted = dict()
	for key in data_dict.keys():
		a = key[-4:]
		if not a in names_dict_sorted.keys():
			names_dict_sorted[a] = [key]
		else:
			names_dict_sorted[a] += [key]
	names_dict = names_dict_sorted.copy()

	for key in names_dict.keys():
		names_dict_sorted[key] = np.array(sorted(names_dict[key]))



	######
	# now we need to use the selected numbers and the ordered names to get the 
	# test/train hurricanes

	test_list = list()
	for year in names_dict_sorted.keys():
		selection = test_selection[year]
		test_list += list(names_dict_sorted[year][selection])


	test_list = np.array(test_list)

	np.save("../../data/test/"+"test_names.npy",test_list)
	# to load in: np.load("../../data/test/training_names.npy")

	training_names = list()

	for name in data_dict:
		if name not in test_list:
			training_names += [name]


if run_initial_time == False:
	training_list = np.load("../../data/training/"+"training_names.npy")
	test_list = np.load("../../data/test/"+"test_names.npy")



data_dict_training = dict()
data_dict_test = dict()


for name in data_dict:
	if name in test_list:
		data_dict_test[name] = data_dict[name] 
	else:
		data_dict_training[name] = data_dict[name] 


	#training_names



##############################################################
# making compadable with R data frame approach (out of dict) #
##############################################################

data_r_training = dict()
for hurr in data_dict_training.keys():
	interested_names = np.array(sorted(data_dict_training[hurr]))[:-2]
	storage_mat = np.ones((interested_names.shape[0],19+1),dtype = np.object)*"NA" 
	# 19 is the size of the information per point, need also to capture date
	for i,step in enumerate(interested_names):
		storage_mat[i,0] = step
		if data_dict_training[hurr][step].shape[0] == 19:
			storage_mat[i,1:] = data_dict_training[hurr][step]
		else:
			storage_mat[i,1] = data_dict_training[hurr][step][0]
			storage_mat[i,3:] = data_dict_training[hurr][step][1:]

	data_r_training[hurr] = storage_mat
	np.savetxt("../../data/training/"+ hurr + ".txt",storage_mat, 
		fmt="%s")


data_r_test = dict()
for hurr in data_dict_test.keys():
	interested_names = np.array(sorted(data_dict_test[hurr]))[:-2]
	storage_mat = np.ones((interested_names.shape[0],19+1),dtype = np.object)*"NA" 
	# 19 is the size of the information per point, need also to capture date
	for i,step in enumerate(interested_names):
		storage_mat[i,0] = step
		if data_dict_test[hurr][step].shape[0] == 19:
			storage_mat[i,1:] = data_dict_test[hurr][step]
		else:
			storage_mat[i,1] = data_dict_test[hurr][step][0]
			storage_mat[i,3:] = data_dict_test[hurr][step][1:]

	data_r_test[hurr] = storage_mat
	np.savetxt("../../data/test/"+ hurr + ".txt",storage_mat, 
		fmt="%s")


