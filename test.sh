for filename in ./data/InToHyLo/test-basic*; do
	echo " "
	echo "$filename"
	./moloss.native "$filename" --time $*
	if [ $? != 0 ]
		then exit 1
	fi
	./moloss.native "$filename" --time --mSAT $*
	if [ $? != 0 ]
		then exit 1
	fi
	# ./moloss.native "$filename" --time --z3
	# if [ $? != 0 ]
	# 	then exit 1
	# fi
	# ./moloss.native "$filename" --time --direct
	# if [ $? != 0 ]
	# 	then exit 1
	# fi
done
