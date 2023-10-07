.PHONY: test bench

build: clean format test lint

format:
	for path in $$(ls -d *.cabal); do cabal-fmt --no-tabular -c $$path 2> /dev/null || cabal-fmt --no-tabular -i $$path; done
	ormolu -ci $$(find . -name "*.hs" -not -path "./*.stack-work/*" -not -path "./dist/*" -not -path "./dist-newstyle/*" -not -path "./.git/*")

test: 
	cabal test --builddir dist/test --disable-optimisation --run-tests --test-show-details always -j +RTS -A128m -n2m -N -RTS

lint:
	cabal build --builddir dist/lint --enable-tests --enable-benchmarks -j +RTS -A128m -n2m -N -RTS --ghc-options="-Werror -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wunused-packages -Wno-name-shadowing -Wno-unused-matches -Wno-unused-do-bind -Wno-type-defaults"

docs:
	cabal haddock --enable-documentation --builddir dist/docs

clean:
	rm -rf dist
