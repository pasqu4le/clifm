stack-all:
	stack --resolver nightly build $(FLAGS)
	@echo
	stack --resolver lts build $(FLAGS)
	@echo
	stack --resolver lts-15 build $(FLAGS)
	@echo
	stack --resolver lts-14 build $(FLAGS)
	@echo
	stack --resolver lts-13 --stack-yaml stack-lts13.yaml build $(FLAGS)
	@echo
	stack --resolver lts-12 build $(FLAGS)
	@echo
	stack --resolver lts-11 build $(FLAGS)
# lts-10 conduit-1.2 does not export Conduit
