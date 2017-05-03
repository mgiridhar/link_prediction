import csv
from igraph import *

user_dict = {}
i=1
with open("/Users/giridhar.manoharan/Documents/ms_cs/elements_of_network_science/project/data/so-rdata.csv", "rb") as f:
    reader = csv.DictReader(f)
    for row in reader:
        tagList = row['tags'].split(',')
        if row['i'] in user_dict:
            user_dict[row['i']].extend(tagList[:])
        else:
            user_dict[row['i']] = tagList[:]
        if row['j'] in user_dict:
            user_dict[row['j']].extend(tagList[:])
        else:
            user_dict[row['j']] = tagList[:]
        #i += 1
        #if(i > 5):
        #    break
#print(user_dict)

#g = Graph.Read_Ncol("/Users/giridhar.manoharan/Documents/ms_cs/elements_of_network_science/project/data/NAWeek1.ncol",
#                names=True, weights="auto", directed=False)
#g = Graph.Read_Ncol("/Users/giridhar.manoharan/Documents/ms_cs/elements_of_network_science/project/data/NA_FirstMonth.ncol",
#                names=True, weights="auto", directed=False)
g = Graph.Read_Ncol("/Users/giridhar.manoharan/Documents/ms_cs/elements_of_network_science/project/data/NAFullNetwork.ncol",
                names=True, weights="auto", directed=False)

with open('/Users/giridhar.manoharan/Documents/ms_cs/elements_of_network_science/project/data/year-tags-dict.csv', 'wb') as f:
    #writer = csv.writer(f)
    #for key, value in user_dict.items():
    for v in g.vs:
        key = v['name']
        if key in user_dict:
            #print(user_dict[str(key)])
            value = ",".join(user_dict[str(key)])
            #writer.writerow([key, value])
            f.write(str(key)+";"+value+"\n")
        else:
            print(key)
            f.write(str(key)+";\n")