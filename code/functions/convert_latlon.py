import numpy as np

def convert_latlon(col2_array):
	mat = col2_array

	mat_out = np.array([inner_convert(mat[i,:]) for i in np.arange(mat.shape[0])])
	return mat_out


def inner_convert(loc):
	# tests 
	# loc = np.array(["29.2N","73.2W"])
	# inner_convert(loc)
	# array([ 29.2,  73.2])
	# loc2 = np.array(["29.2S","73.2E"])
	# inner_convert(loc)
	# array([-29.2, -73.2])

	north = loc[0][-1]
	west = loc[1][-1]

	if north != "N":
		first = -np.float(loc[0][:-1])
	else:
		first = np.float(loc[0][:-1])

	if west == "W":
		second = -np.float(loc[1][:-1])
	else:
		second = np.float(loc[1][:-1])

	loc_out = np.array([first,second])
	return loc_out

