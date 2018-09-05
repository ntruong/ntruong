.PHONY : all site deploy clean

all :
	stack build

site : all
	stack exec site build

deploy : clean site
	git push origin :gh-pages && \
	git subtree push --prefix _site origin gh-pages

clean : all
	stack exec site clean
