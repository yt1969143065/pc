top:
	mill Core
simTop:
	mill Core.test.runMain top.Main
	sed -i 's/\\\\033/\\033/g' build/*.v
	cp src/test/vsrc/tb.v ./build
clean:
	rm -rf *.v *.fir *.json out build
.PHONY: run clean
