for filename in ./data/InToHyLo/*; do

	echo "\n\n====================================================="
	echo "$filename"
	#cat $ilename
	echo "\n"
	./moloss.native "$filename"  #--get-simplify
	#./moloss.native "$filename" -S  #--get-simplify
done
