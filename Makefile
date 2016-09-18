spec:
	busted

spec-ucd:
	busted --run=ucd

clean:
	rm -rf build *.so

lint:
	luacheck src spec examples --ignore 542

docs:
	ldoc -d docs  .

.PHONY: clean lint spec spec-ucd docs
