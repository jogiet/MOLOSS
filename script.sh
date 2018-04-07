for filename in ./data/InToHyLo/test*; do
	echo " "
	echo "$filename"
	./moloss.native "$filename" $*
	if [ $? != 0 ]
		then exit 1
	fi
done
