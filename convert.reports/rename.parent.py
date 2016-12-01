import os
import csv
import json
import glob
import codecs
import sys  

reload(sys)  
sys.setdefaultencoding('utf8')

file1 = open('./data/npo_reports_change_cashitems.csv')
reader = csv.reader(file1)
mapping = list(reader)

npos = []
maplen = xrange(1, len(mapping))
for i in maplen:
	if mapping[i][4] not in npos:
		npos.append(mapping[i][4])

for npo in npos:
	reports = glob.glob('./npodatas/' + npo + '/reports/*.json')
	# print npo

	for report in reports:
		print report
		# npo = "npo2566"
		# report = "/home/helen/donation/npodatas/npo2566/reports/7486c1d96729b511a4b25c304df38543.json"
		if len(report.split('.'))==3:
			jsonpath = report
			print jsonpath

			with open(jsonpath, 'rb') as file2:
				json_data = json.loads(file2.read().decode('utf8'))

				for i in maplen:
					if mapping[i][4] == npo:
						key = mapping[i][3]
						newp = mapping[i][2]
						print key

						if key in json_data['fields'].keys():
							print key
							json_data['fields'][key.decode('utf8')]['parent'] = newp

							f = json.dumps(json_data, ensure_ascii=False)
							outfile = codecs.open(jsonpath, "w", "utf8")
							outfile.write(f)
							outfile.close()

# npo = "npo0414"
# report = "/home/helen/donation/npodatas/npo0414/reports/fba1ef9a1658bdb861a09ce7bba99374.json"
# if len(report.split('.'))==2:
# 	jsonpath = report
# 	with open(jsonpath, 'rb') as file2:
# 		json_data = json.loads(file2.read().decode('utf8'))

# 		print json_data['fields'].keys().decode('utf8')
# 		for i in maplen:
# 			if mapping[i][4] == npo:
# 				key = mapping[i][3]
# 				newp = mapping[i][2]						
				
# 				json_data['fields'][key.decode('utf8')]['parent'] = newp
				
# 				f = json.dumps(json_data, ensure_ascii=False)
# 				outfile = codecs.open(jsonpath, "w", "utf8")
# 				outfile.write(f)
# 				outfile.close()
