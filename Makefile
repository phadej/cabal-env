all : 
	cabal v2-build

install :
	cabal v2-install --overwrite-policy=always
