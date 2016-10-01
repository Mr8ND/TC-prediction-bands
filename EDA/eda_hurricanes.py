# you'll probably have to download mpl_toolkits.basemap since it's not on pip

import numpy as np
import pickle
import re
from collections import Counter
import matplotlib.pyplot as plt

with open("hurdat2-1851-2015-070616.txt","r") as ftext:
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



count_2004 = 0
data_dict_new = dict()
for key in data_dict.keys():
	if int(key[-4:]) > 1950:
		data_dict_new[key] = data_dict[key]
	if int(key[-4:]) > 2004:
		count_2004+=1


data_dict_old = data_dict
data_dict = data_dict_new


# len(data_dict)  # 992



count_named = 0
vector_length = np.zeros(len(data_dict))
for i,key in enumerate(data_dict.keys()):
	if data_dict[key]["name"] !="UNNAMED":
		count_named +=1
	vector_length[i] = data_dict[key]["n_points"]

table  = dict(Counter(vector_length))
amount = np.array(list(table.keys()))
count  = np.array([table[x] for x in amount])

fig,ax_bar = plt.subplots()
ax_bar.bar(amount,count)
ax_bar.set_xlabel("Length of data collection")
ax_bar.set_ylabel("Frequency")
ax_bar.set_title("Counts plot")
fig.savefig("counts_plot.png")
plt.close()

count_cumsum= np.cumsum(count[::-1])[::-1]
fig,ax_barcs = plt.subplots()
ax_barcs.plot(amount,count_cumsum/np.max(count_cumsum))
ax_barcs.set_xlabel("Length of data collection")
ax_barcs.set_ylabel("Proportion larger than that value")
ax_barcs.set_title("1- Empirical CDF of number of records per element")
fig.savefig("empirical_cdf_counts.png")
plt.close()


dict_hurricanes = dict()
for i,key in enumerate(data_dict.keys()):
	answer = 0
	for key_day in data_dict[key].keys():
		if key_day in ["n_points",'name']:
			pass
		elif data_dict[key][key_day][1] == "HU":
			answer =1
	dict_hurricanes[key] =answer

num_hurricanes = np.sum(np.array([dict_hurricanes[key] for key in dict_hurricanes.keys()]))
# 833



# only hurricanes
count_named_h = 0
vector_length_h = np.zeros(num_hurricanes)
i = 0
for key in data_dict.keys():
	if dict_hurricanes[key]==1:
		vector_length_h[i] = data_dict[key]["n_points"]
		i+=1

table_h  = dict(Counter(vector_length_h))
amount_h = np.array(list(table_h.keys()))
count_h  = np.array([table_h[x] for x in amount_h])


fig,ax_barH = plt.subplots()
ax_barH.bar(amount_h,count_h)
ax_barH.set_xlabel("Length of data collection")
ax_barH.set_ylabel("Frequency")
ax_barH.set_title("Counts plot, Storms that were Hurricanes \n"+\
								"(at least for a little)")
fig.savefig("counts_plot_H.png")
plt.close()



count_cumsumH= np.cumsum(count_h[::-1])[::-1]
fig,ax_barcsH = plt.subplots()
ax_barcsH.plot(amount_h,count_cumsumH/np.max(count_cumsumH))
ax_barcsH.set_xlabel("Length of data collection")
ax_barcsH.set_ylabel("Proportion larger than that value")
ax_barcsH.set_title("1- Empirical CDF of number of records per Storm \n that were a Hurricane for a least a little")
fig.savefig("empirical_cdf_counts_H.png")
plt.close()






####
from mpl_toolkits.basemap import Basemap
# Lambert Conformal Conic map.
m = Basemap(llcrnrlon=-100.,llcrnrlat=0.,urcrnrlon=-20.,urcrnrlat=57.,
            projection='lcc',lat_1=20.,lat_2=40.,lon_0=-60.,
            resolution ='l',area_thresh=1000.)





for i,key in enumerate(data_dict.keys()):
	if dict_hurricanes[key]==1:
		xx = list()
		yy = list()
		for j,key_day in enumerate(np.array(sorted(list(data_dict[key].keys())))[:-2]):
			if key_day in ["n_points",'name']:
				pass
			elif len(data_dict[key][key_day][2])==2:
				pass
			else:
				xx.append(data_dict[key][key_day][2][:-1])
				yy.append(data_dict[key][key_day][3][:-1])
		xx = np.array(xx,dtype=np.float)
		yy = np.array(yy,dtype=np.float)

		#m.plot(y=xx,x=-yy,latlon=True,linewidth=1,color="black")
		if i % 25 ==0:
			m.plot(y=xx,x=-yy,latlon=True,linewidth=1,color="black")
			if i % 125 ==0:
				m.plot(y=xx,x=-yy,latlon=True,linewidth=1,color="red")

m.drawcoastlines()
m.drawcountries()
m.drawmapboundary(fill_color='#99ffff')
m.fillcontinents(color='#cc9966',lake_color='#99ffff')
m.drawparallels(np.arange(10,70,20),labels=[1,1,0,0])
m.drawmeridians(np.arange(-100,0,20),labels=[0,0,0,1])
plt.title("Examples of Hurricane paths")
plt.savefig("example_of_hurricane_paths.png")
plt.close()

