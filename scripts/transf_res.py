import sys

fichier = sys.argv[1]

with open(fichier,"r") as fle0:
	fle = fle0.read()
	fle = fle.split("\n")
	log_prof = fle[0]
	log_prof = log_prof.split(",")
	logic,prof = log_prof[0] , log_prof[1]
	z3 = ["TO","UNKNOWN"]
	mSAT = ["TO","UNKNOWN"]
	mini = ["TO","UNKNOWN"]
	direct = ["TO","UNKNOWN"]
	i = 1

	while i < len(fle):
		if i > len(fle) -3 or "oracle" in fle[i+1]:
		# Cas de timeout sur un oracle
			i += 1
		
		elif fle[i] == "oracle direct":
			if "Calculs" in fle[i+1]:
				tps , res = i+1 , i+2
			else:
				tps , res = i+2 , i+1
			direct[0] = fle[tps][21:-2:]
			if fle[res][:5:] == "\033[92m":
				direct[1] = "SAT"
			else:
				direct[1] = "UNSAT"
			i += 3
		elif fle[i] == "oracle minisat":
			if "Calculs" in fle[i+1]:
				tps , res = i+1 , i+2
			else:
				tps , res = i+2 , i+1
			mini[0] = fle[i+1][21:-2:]
			if fle[i+2][:5:] == "\033[92m":
				mini[1] = "SAT"
			else:
				mini[1] = "UNSAT"
			i += 3
		elif fle[i] == "oracle mSAT":
			if "Calculs" in fle[i+1]:
				tps , res = i+1 , i+2
			else:
				tps , res = i+2 , i+1
			mSAT[0] = fle[i+1][21:-2:]
			if fle[i+2][:5:] == "\033[92m":
				mSAT[1] = "SAT"
			else:
				mSAT[1] = "UNSAT"
			i += 3
		elif fle[i] == "oracle z3":
			if "Calculs" in fle[i+1]:
				tps , res = i+1 , i+2
			else:
				tps , res = i+2 , i+1
			z3[0] = fle[i+1][21:-2:]
			if fle[i+2][:5:] == "\033[92m":
				z3[1] = "SAT"
			else:
				z3[1] = "UNSAT"
			i += 3

	with open("resultatsz3.csv","a") as res : 
		res.write(logic+","+prof)
		for oracle in [z3,mSAT,mini,direct]:
			res.write(","+oracle[0]+","+oracle[1])
		res.write("\n")



