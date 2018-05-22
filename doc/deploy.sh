git checkout master

make doc_html

git checkout gh-pages
git pull

mv moloss.docdir/*.html ./
git add *.html

mv moloss.docdir/*.css ./
git add *.css

git commit -m "Updating doc"
git push

git checkout master
