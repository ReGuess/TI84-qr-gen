TOKENIZE = ./tools/nolib8xp

ASM = $(shell which spasm fasmg z80asm | head -n1)



.PHONY: all
all: assembly basic

.PHONY: assembly
assembly: ./bin/GF256PLY.8xp ./bin/ECCTBL.8xp ./bin/BIN2HEXZ.8xp

./bin/GF256PLY.8xp: ./src/asm/GF256PLY.z80
	$(ASM) -I./include ./src/asm/GF256PLY.z80 ./bin/GF256PLY.8xp

./bin/ECCTBL.8xp: ./src/asm/ECCTBL.z80
	$(ASM) -I./include ./src/asm/ECCTBL.z80 ./bin/ECCTBL.8xp

./bin/BIN2HEXZ.8xp: ./src/asm/BIN2HEXZ.z80
	$(ASM) -I./include ./src/asm/BIN2HEXZ.z80 ./bin/BIN2HEXZ.8xp



.PHONY: basic
basic: ./bin/REAL2BIN.8xp ./bin/QRZMAIN.8xp
#	mv ./bin/REAL2BIN.8xp ./bin/QRZMAIN.8xp ./build


./tmp/REAL2BIN.8xp.1.txt: ./src/basic/REAL2BIN.8xp.txt
	cat ./src/basic/REAL2BIN.8xp.txt | tr -d '\t' > ./tmp/REAL2BIN.8xp.1.txt
./bin/REAL2BIN.8xp.bin: ./tmp/REAL2BIN.8xp.1.txt
	$(TOKENIZE) ./tmp/REAL2BIN.8xp.1.txt ./bin/REAL2BIN.8xp.bin
./bin/REAL2BIN.8xp: ./bin/REAL2BIN.8xp.bin
	cd ./bin && wabbit REAL2BIN.8xp.bin REAL2BIN.8xp && cd ..


#./bin/QRZMAIN.8xp: ./src/basic/QRZMAIN.8xp.txt
#	touch ./bin/QRZMAIN.8xp

./tmp/QRZMAIN.8xp.1.txt: ./src/basic/QRZMAIN.8xp.txt
	cat ./src/basic/QRZMAIN.8xp.txt | tr -d '\t' > ./tmp/QRZMAIN.8xp.1.txt
./bin/QRZMAIN.8xp.bin: ./tmp/QRZMAIN.8xp.1.txt
	$(TOKENIZE) ./tmp/QRZMAIN.8xp.1.txt ./bin/QRZMAIN.8xp.bin
./bin/QRZMAIN.8xp: ./bin/QRZMAIN.8xp.bin
	cd ./bin && wabbit QRZMAIN.8xp.bin QRZMAIN.8xp && cd ..





.PHONY: clean
clean:
	rm -f ./bin/* ./tmp/*  #.8xp


#	sed 's/\t//g;s/(?<=prgm)([A-Z0-9]|theta)+' ./src/basic/REAL2BIN.8xp.txt > ./tmp/REAL2BIN.8xp.txt
