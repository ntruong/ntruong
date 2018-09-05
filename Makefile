.PHONY : all site deploy clean

all :
	stack build

site : all
	stack exec site build

deploy : clean site
	cd _site && \
	git add --all && \
	git commit -m "deploy" && \
	git push origin gh-pages

clean : all
	stack exec site clean
