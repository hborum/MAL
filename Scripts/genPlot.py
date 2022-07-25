import matplotlib.pyplot as plt
import csv

def convMethod(method):
	if method == "Edlund":
		return "C#"
	elif method == "Default":
		return "MAL"
	elif method == "Deforest":
		return "MAL_df"
	else:
		return method

def convDeps(dep):
	if method == "RealisticDep":
		return "reaDep"
	elif method == "MinimalDep":
		return "minDep"
	else:
		return dep

timeUnit = "?"
allocatedUnit = "?"

with open('../BenchmarkDotNet.Artifacts/results/Benchmarks.GeneralBench-report.csv') as csvfile:
	rows = csv.reader(csvfile, delimiter=',', quotechar='|')
	first = True

	collectors = {
	  "Mean": -1,
	  "MeanUnit": -1,
	  "StdDev": -1,
	  "Allocated": -1,
	  "AllocatedUnit": -1,
	  "policies": -1,
	  "depKind": -1,
	  "states": -1,
	  "Method": -1,
	  "Ratio": -1
	}

	measures = []


	for row in rows:
		if first:
			first = False
			i = 0
			for c in row:
				for key in collectors:
					if c == key:
						collectors[key] = i
				i += 1
		else:
			entry = {}
			for key in collectors:
				if key == "Mean":
					val = row[collectors[key]].split(" ")
					entry[key] = float(val[0])
					timeUnit = val[1]
				elif key == "Allocated":
					val = row[collectors[key]].split(" ")
					entry[key] = float(val[0])
					allocatedUnit = val[1]
				elif key == "policies":
					entry[key] = int(row[collectors[key]])
				else:
					entry[key] = row[collectors[key]]
			measures.append(entry)

series = {}
policies = []

for ms in measures:
	nm = f'{convMethod(ms["Method"])} {convMethod(ms["depKind"])} {ms["states"]}'
	if not (nm in series):
		series[nm] = []
	series[nm].append(ms)
	if not (ms["policies"] in policies):
		policies.append(ms["policies"])

def projPolicies(ms):
  return ms["policies"]

for key in series:
	series[key].sort(key=projPolicies)

policies.sort()
legends = []
for key in series:
	plotSeries = []
	for entry in series[key]:
		plotSeries.append(entry["Mean"])
	plt.plot(policies, plotSeries,label="asdasd")
	legends.append(key)

plt.legend(legends)
plt.yscale('log')
plt.xscale('log')
plt.xlabel('Policies')
plt.ylabel(f'Time ({timeUnit})')
plt.savefig("Time")
plt.clf()


for key in series:
	plotSeries = []
	for entry in series[key]:
		plotSeries.append((1 / float(entry["Ratio"]))*100)
	plt.plot(policies, plotSeries)
plt.legend(legends)
plt.xlabel('Policies')
plt.xscale('log')
plt.ylabel('Speedup (%)')
plt.savefig("Speedup")
plt.clf()

for key in series:
	plotSeries = []
	for entry in series[key]:
		plotSeries.append(float(entry["Allocated"]))
	plt.plot(policies, plotSeries)
plt.legend(legends)
plt.xlabel('Policies')
plt.xscale('log')
plt.ylabel(f'Allocated ({allocatedUnit})')
plt.savefig("Allocated")
