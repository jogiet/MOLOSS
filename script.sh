for filename in ./data/InToHyLo/test*; do
	echo " "
	echo "$filename"
	./moloss.native "$filename" $* #--get-simplify
done
