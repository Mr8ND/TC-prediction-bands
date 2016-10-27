# dealing_with_curvature.py 
###
# This file holds functions:
##
# 0) d_to_r (degrees to radius transform)
# 1) r_to_d (radius to degrees transform)
# 2) calc_distance (calc distance between 2 points)
# 3) calc_distance_between_outer (calc distances along a line)
# 4) calc_distance_between_inter (calc distances between different lists of 
#	points)
# 5) bearing (calculates bearing between points)

def d_to_r(d):
	"""
	converts angles degrees to radius, can deal with lat/lon structure "N,E,S,W"

	Input:
	------
	d = (2,) np.array with 2 values (either floats or strings for strings, last
		character is N,E,S, or W)
	
	Output:
	-------
	r = (2,) np.array with 2 values (both floats)
	"""
	if isinstance(d[0], str):
		north = d[0][-1]
		west = d[1][-1]

		if north != "N":
			first = -np.float(d[0][:-1])
		else:
			first = np.float(d[0][:-1])

		if west != "W":
			second = -np.float(d[1][:-1])
		else:
			second = np.float(d[1][:-1])

		d = np.array([first,second])
		r = d*np.pi/180
		# degrees to radians
	return r

def r_to_d(r,nw=True):
	"""
	converts angles radius to degrees, can output with lat/lon structure "N,E,S,W"

	Input:
	------
	r  = (2,) np.array with 2 values (both floats)
	nw = logical value, tells if return should be strings (with North/West 
		structure) or not

	Output:
	-------
	d = (2,) np.array with 2 values (either floats or strings for strings, last
		character is N,E,S, or W)
	"""
	# going backwards
	a = r*180/np.pi
	if nw==False:
		return a
	out = np.zeros(2,dtype = np.object)
	### lat
	if a[0] > 0:
		out[0] = str(a[0])+"N"
	else:
		out[0] = str(np.abs(a[0]))+"S"
	### lon
	if a[1] > 0:
		out[1] = str(a[1])+"W"
	else:
		out[1] = str(np.abs(a[1]))+"E"

	d = out

	return d

def calc_distance(loc_1,loc_2):
	"""
	Calculates distance between 2 points on the earth's surface - nuatical miles

	Inputs:
	-------
	loc_1 = (2,) np.array with degree related lat,lon coordinates 
		(string or float)
	loc_2 = (2,) np.array with degree related lat,lon coordinates 
		(string or float)

	Ouput
	-----
	d = float value that is the distance between the two points

	Comments:
	---------
	Took equations from: http://www.movable-type.co.uk/scripts/latlong.html
	"""

	R = 3440 # radius of earth in nautical miles- do we want this?
			
	loc_1 = d_to_r(loc_1)
	loc_2 = d_to_r(loc_2)

	delta_lat  = loc_1[0]-loc_2[0]
	delta_long = loc_1[1]-loc_2[1]
	a = np.sin(delta_lat/2)**2 +\
			np.cos(loc_1[0])*np.cos(loc_2[0]) *\
			np.sin(delta_long/2)**2
	c = 2*np.arctan2(np.sqrt(a),np.sqrt(1-a))
	d = R*c

	return d

def calc_distance_between_outer(locs_1,locs_2):
	"""
	Calculates inner distance between points on the earth's surface

	Inputs:
	-------
	locs_1 = (n,2) np.array with lat long coordinates pairs  
		(floats or strings with NS)
	locs_2 = (n,2) np.array with lat long coordinates pairs  
		(floats or strings with NS)

	Output:
	-------
	d = (n,) np.array of inner distance between points

	Example:
	--------
	locs_1 = test[:,4:6]
	locs_2 = test2[:,4:6]
	d_vect = calc_distance_between_outer(locs_1,locs_2)
	"""


	assert(locs_1.shape == locs_2.shape) # need to be the same shape

	d = np.zeros(locs_1.shape[0])

	for i,locs in enumerate(zip(locs_1,locs_2)):
		d[i] = calc_distance(locs[0],locs[1])

	return d

def calc_distance_between_inter(lats,lons):
	"""
	Calculates inner distance between points on the earth's surface

	Inputs:
	-------
	lats = (n,) np.array with latitude coordinates (floats or strings with NS)
	lons = (n,) np.array with longitude coordinates (floats or strings with EW)

	Output:
	-------
	d = (n-1,) np.array of inner distance between points

	Example:
	--------
	lats = storage_mat[:,4]
	lons = storage_mat[:,5]
	d_vect=calc_distance_between_inter(lats,lons)
	"""
	assert(lats.shape == lons.shape) # need to be the same shape

	locs_1 = np.array(list(zip(lats[:-1],lons[:-1]))) 
	locs_2 = np.array(list(zip(lats[1:],lons[1:])))  
	d = calc_distance_between_outer(locs_1,locs_2)

	return d

def bearing(loc_1,loc_2,nw =False): 
	"""
	Calculates angle between locations

	Inputs:
	-------
	locs_1 = (2,) np.array with lat long coordinates (floats or strings with NS)
	locs_2 = (2,) np.array with lat long coordinates (floats or strings with NS)
	nw     = logical to decide if the output is string with N,E,S,W, structure 
		or float values 
		
	Output:
	-------
	bearing = float or string of bearing between point 1 to point 2

	"""
	R = 3440 # radius of earth in nautical miles- do we want this?
			
	loc_1 = d_to_r(loc_1)
	loc_2 = d_to_r(loc_2)

	delta_lat  = loc_1[0]-loc_2[0]
	delta_long = loc_1[1]-loc_2[1]


	delta_psi = np.log(np.tan(np.pi/4 + loc_1[0]/2)/\
						np.tan(np.pi/4 + loc_2[0]/2))

	# not quiet working as described, will need to adjust.
	if (np.abs(delta_long) > np.pi):
		if delta_long > 0:
			delta_long = 2*np.pi-delta_long
		else:
			delta_long = -2*np.pi+delta_long

	bearing = np.arctan2(delta_long,delta_lat) # in radians
	bearing = r_to_d(bearing,nw)
	return bearing

