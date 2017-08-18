t_out = "timeout 405s "


def get_line(logic,j,prof):
	fle = "core{}.{}".format(logic,j)
	l1 = "./printformula.native {} {} {}\n".format(fle,prof,logic)
	lmid = ""
	for oracle in ["--z3","--mSAT","--mini","--direct"]:
		lmid += "{0} ./moloss {1}.bml {2} --time >> {1}.out\n".format(t_out,fle,oracle) 
	lf = "python transf_res.py {}.out".format(fle)
	return l1+lmid+lf

logics = ["-CD","-K","-T","-B","-4","-S4","-5","-S5"]


with open("script.sh","w") as script:
	script.write("\n")

for logic in logics:
	for j in range (5): # On a 5 coeur par logique .... 
		with open("script.sh","a") as script:
			script.write("(")
		for i in range(10,24):
			with open("script.sh","a") as script:
				script.write("for i in {1..10}\ndo\n")
				script.write(get_line(logic,j,i)+" \n")
				script.write("done \n")
		with open("script.sh","a") as script:
			script.write(") &\n")

