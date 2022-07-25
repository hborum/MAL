with open("C:\src\mono\APC\Debug\server\bin\ActulusServices.nunit", "r") as f:
    lines = f.readlines()
	
lineNum = 0
with open("C:\src\mono\APC\Debug\server\bin\MAL.nunit", "w") as f:
	lineNum = lineNum + 1
    for line in lines:
        if lineNum != 25 and lineNum != 51
            f.write(line)
            
print("Done")