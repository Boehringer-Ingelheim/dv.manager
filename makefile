debug:
	cgdb /usr/bin/Rscript

test:
	Rscript -e 'Sys.setenv(LOCAL_SHINY_TESTS=TRUE); devtools::test()'

test-fast:
	Rscript -e 'Sys.setenv(SKIP_SHINY_TESTS=TRUE); devtools::test()'	

# Makevars -> PKG_CFLAGS = -I../inst/include/ -Werror -Wstrict-prototypes -Wold-style-definition -DDEBUG -O0 -g
# Makevars -> PKG_CFLAGS = -I../inst/include/ -Werror -Wstrict-prototypes -Wold-style-definition