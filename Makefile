NAME = $(shell grep 'Package:' DESCRIPTION | cut -d ' ' -f2)
VER = $(shell grep 'Version:' DESCRIPTION | cut -d ' ' -f2)

install:
	make document
	R CMD INSTALL .

build:
	make document
	cd ..; R CMD build $(NAME)

check:
	make document
	Rscript -e "devtools::check()"

document:
	Rscript -e "devtools::document()"
