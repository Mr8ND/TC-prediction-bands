def d_to_r(d):
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
		# degrees to radians
	
	return d*np.pi/180



def r_to_d(r,nw=True):
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

	return out
		


def calc_distance(loc_1,loc_2):
	# locations are: lat,lon
	#http://www.movable-type.co.uk/scripts/latlong.html
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


def calc_distance_between_inter(lats,lons):
	#lats = storage_mat[:,4]
	#lons = storage_mat[:,5]

	assert(lats.shape == lons.shape) # need to be the same shape

	d = np.zeros(lats.shape[0]-1)

	loc_1 =np.array([lats[0],lons[0]])
	for i,loc in enumerate(zip(lats[1:],lons[1:])):
		loc_2 = np.array(loc)

		d[i] = calc_distance(loc_1,loc_2)



		loc_1 = loc_2

	return d


def calc_distance_between_outer(locs_1,locs_2):
	# locs_1 = test[:,4:6]
	# locs_2 = test2[:,4:6]
	assert(locs_1.shape == locs_2.shape) # need to be the same shape

	d = np.zeros(locs_1.shape[0])

	for i,locs in enumerate(zip(locs_1,locs_2)):
		d[i] = calc_distance(locs[0],locs[1])

	return d


def bearing(loc_1,loc_2,nw =False): # nw means cover to north/west strings
	# locations are: lat,lon

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

