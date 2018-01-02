for filename in ../data/InToHyLo/*; do
	echo "$filename"
	../moloss.native "$filename" --get-simplify
	../moloss.native "$filename" -S --get-simplify
done
