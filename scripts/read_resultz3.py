def getlign(a): 
	res = a[0]
	for i in range(1,len(a)):
		res += ","+a[i]
	return res+"\n"

def check(ligne):
	def aux(res1,res2):
		if res1 == "UNKNOWN":
			return True
		elif res2 == "UNKNOWN":
			return True
		else:
			return res1 == res2
	m1 = aux(ligne[3],ligne[7])
	m2 = aux(ligne[3],ligne[9])
	m3 = aux(ligne[7],ligne[9])
	return (m1 and m2 and m3)

def statut(ligne):
	if ligne[3]== "SAT":
		return True
	elif ligne[7]== "SAT":
		return True
	elif ligne[9]== "SAT":
		return True
	else:
		return False

with open("resultatsz3.csv","r") as fle:
	text = fle.read()
	text = text.split('\n')
	cor = []
	k = []
	t = []
	b = []
	l4= []
	l5 = []
	s4 = []
	s5 = []
	cd = []
	fail = 0
	for ligne in text:
		if ligne == "FAIL ":
			fail += 1
		ligne = ligne.split(',')
		if len(ligne) == 10:
			if not(ligne[2]=="TO" and ligne[6]=="TO" and ligne[8]=="TO"):
				if not check(ligne):
					fail += 1
					print(getlign(ligne))
					ligne += ["FAIL!"]

				if ligne[0] == "-K":
					k += [ligne]
				elif ligne[0] == "-T":
					t += [ligne]
				elif ligne[0] == "-B":
					b += [ligne]
				elif ligne[0] == "-4":
					l4 += [ligne]
				elif ligne[0] == "-5":
					l5 += [ligne]
				elif ligne[0] == "-S4":
					s4 += [ligne]
				elif ligne[0] == "-S5":
					s5 += [ligne]
				elif ligne[0] == "-CD":
					cd += [ligne]

	print("on a fait "+str(fail)+" Fails !!")

	with open("lisibles.csv","a") as fle2:
		l0 = "logique,prof"
		for oracle in ["z3","mSAT","minisat","direct"]:
			l0 += ",tps {},res {}".format(oracle,oracle)
		fle2.write(l0+"\n\n")




	for a in sorted(k):
		with open("lisibles.csv","a") as fle2:
			fle2.write(getlign(a))

	with open("lisibles.csv","a") as fle2:
		fle2.write("\n\n")
	
	for a in sorted(t):
		with open("lisibles.csv","a") as fle2:
			fle2.write(getlign(a))
	

	with open("lisibles.csv","a") as fle2:
		fle2.write("\n\n")
	
	for a in sorted(b):
		with open("lisibles.csv","a") as fle2:
			fle2.write(getlign(a))


	with open("lisibles.csv","a") as fle2:
		fle2.write("\n\n")
	
	for a in sorted(l4):
		with open("lisibles.csv","a") as fle2:
			fle2.write(getlign(a))

	with open("lisibles.csv","a") as fle2:
		fle2.write("\n\n")
	
	for a in sorted(s4):
		with open("lisibles.csv","a") as fle2:
			fle2.write(getlign(a))

	with open("lisibles.csv","a") as fle2:
		fle2.write("\n\n")
	
	for a in sorted(l5):
		with open("lisibles.csv","a") as fle2:
			fle2.write(getlign(a))

	with open("lisibles.csv","a") as fle2:
		fle2.write("\n\n")
	
	for a in sorted(s5):
		with open("lisibles.csv","a") as fle2:
			fle2.write(getlign(a))

	with open("lisibles.csv","a") as fle2:
		fle2.write("\n\n")

	for a in sorted(cd):
		with open("lisibles.csv","a") as fle2:
			fle2.write(getlign(a))
