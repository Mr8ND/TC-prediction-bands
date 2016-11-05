# initial data cleaning and processing

# run from code/scripts/

#################
# Data Read in: #
#################

import numpy as np
import pickle
import re
from collections import Counter
import matplotlib.pyplot as plt
import sys

project_location = "../../"
function_location =project_location + "code/functions/"
sys.path.append(function_location)

from convert_latlon import convert_latlon, inner_convert

with open(project_location+"data/hurdat2-1851-2015-070616.txt","r") as ftext:
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
		#print(hurricane_name)
	else:
		data_dict[hurricane_name][str(clean_line[0])+":"+str(clean_line[1])]= clean_line[1:]


# small corrections to some errors in the code:
data_dict["AL051952"]["19520914:0600"][3] = str(360-359.1)+"E"
data_dict["AL051952"]["19520914:1200"][3] = str(360-358.4)+"E"


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
			storage[i] +=np.random.binomial(n=n%3,p=1/3)


	######### ## ## 
	# needed code for training validation split (50/50 split)
	training_count = count_vec-storage
	validation_count = np.zeros(xx.shape[0],dtype = np.int)
	for i in np.arange(validation_count.shape[0]):
		n = training_count[i]
		validation_count[i] = n//2
		if n%2 != 0:
			validation_count[i] +=np.random.binomial(n=n%2,p=1/2)
	######### ## ## 

	test_selection = dict()
	for i in np.arange(storage.shape[0]):
		amount = storage[i] 
		n      = count_vec[i]
		test_selection[xx[i]] = np.random.choice(a = np.arange(n),
								 size = amount,replace=False)

	######### ## ## 
	# needed code for training validation split (50/50 split)
	training_selection = dict()
	for i,year in enumerate(xx):
		training_selection[year] = np.array([x for x in np.arange(count_vec[i]) if x not in test_selection[year]])

	validation_selection = dict()
	for i in np.arange(validation_count.shape[0]):
		amount = validation_count[i] 
		n      = training_count[i]
		validation_selection[xx[i]] = training_selection[xx[i]][np.random.choice(a = np.arange(n),
								 size = amount,replace=False)]
	######### ## ## 

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
	train_first_list = list()
	valid_list = list()
	for year in names_dict_sorted.keys():
		selection_test = test_selection[year]
		selection_valid = validation_selection[year]
		selection_train_first = np.array([x for x in training_selection[year] if x not in selection_valid])
		test_list += list(names_dict_sorted[year][selection_test])
		train_first_list += list(names_dict_sorted[year][selection_train_first])
		valid_list += list(names_dict_sorted[year][selection_valid])



	test_list = np.array(test_list)
	train_first_list = np.array(train_first_list)
	valid_list = np.array(valid_list)


	np.save("../../data/test/"+"test_names.npy",test_list)
	np.save("../../data/training/validate/"+"validate_names.npy",valid_list)
	np.save("../../data/training/train/"+"training_names.npy",train_first_list)


	# to load in: np.load("../../data/test/training_names.npy")

	# training_names = list()

	# for name in data_dict:
	# 	if name not in test_list:
	# 		training_names += [name]


if run_initial_time == False:
	valid_list = np.load("../../data/training/validate/"+"validate_names.npy")
	train_first_list = np.load("../../data/training/train/"+"training_names.npy")
	test_list = np.load("../../data/test/"+"test_names.npy")



data_dict_train_first = dict()
data_dict_validate = dict()
data_dict_test = dict()


for name in data_dict:
	if name in test_list:
		data_dict_test[name] = data_dict[name] 
	elif name in train_first_list:
		data_dict_train_first[name] = data_dict[name] 
	else:
		data_dict_validate[name] = data_dict[name]


	#training_names



##############################################################
# making compadable with R data frame approach (out of dict) #
##############################################################

data_r_train_first = dict()
for hurr in data_dict_train_first.keys():
	interested_names = np.array(sorted(data_dict_train_first[hurr]))[:-2]
	storage_mat = np.ones((interested_names.shape[0],19+1),dtype = np.object)*"NA" 
	# 19 is the size of the information per point, need also to capture date
	for i,step in enumerate(interested_names):
		storage_mat[i,0] = step
		if data_dict_train_first[hurr][step].shape[0] == 19:
			storage_mat[i,1:] = data_dict_train_first[hurr][step]
		else:
			storage_mat[i,1] = data_dict_train_first[hurr][step][0]
			storage_mat[i,3:] = data_dict_train_first[hurr][step][1:]

	storage_mat[:,4:6] = convert_latlon(storage_mat[:,4:6]) 
	data_r_train_first[hurr] = storage_mat
	np.savetxt("../../data/training/train/"+ hurr + ".txt",storage_mat, 
		fmt="%s")

data_r_validate = dict()
for hurr in data_dict_validate.keys():
	interested_names = np.array(sorted(data_dict_validate[hurr]))[:-2]
	storage_mat = np.ones((interested_names.shape[0],19+1),dtype = np.object)*"NA" 
	# 19 is the size of the information per point, need also to capture date
	for i,step in enumerate(interested_names):
		storage_mat[i,0] = step
		if data_dict_validate[hurr][step].shape[0] == 19:
			storage_mat[i,1:] = data_dict_validate[hurr][step]
		else:
			storage_mat[i,1] = data_dict_validate[hurr][step][0]
			storage_mat[i,3:] = data_dict_validate[hurr][step][1:]

	storage_mat[:,4:6] = convert_latlon(storage_mat[:,4:6]) 
	data_r_validate[hurr] = storage_mat
	np.savetxt("../../data/training/validate/"+ hurr + ".txt",storage_mat, 
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

	storage_mat[:,4:6] = convert_latlon(storage_mat[:,4:6]) 
	data_r_test[hurr] = storage_mat
	np.savetxt("../../data/test/"+ hurr + ".txt",storage_mat, 
		fmt="%s")



