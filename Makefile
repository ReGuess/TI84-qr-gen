#TITOKENS_PATH ?= ../TITokens
#TOKENIZE = $(TITOKENS_PATH)/build.sh
TOKENIZE = ./nolib8xp

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

./bin/REAL2BIN.8xp: ./src/basic/REAL2BIN.8xp.txt
#	sed 's/\t//g;s/(?<=prgm)([A-Z0-9]|theta)+' ./src/basic/REAL2BIN.8xp.txt > ./tmp/REAL2BIN.8xp.txt
	$(TOKENIZE) <(cat ./src/basic/REAL2BIN.8xp.txt | tr -d '\t' ) ./bin/REAL2BIN.8xp.bin
#	$(ASM) 
#	touch ./bin/REAL2BIN.8xp

./bin/QRZMAIN.8xp: ./src/basic/QRZMAIN.8xp.txt
	touch ./bin/QRZMAIN.8xp



.PHONY: clean
clean:
	rm -f ./bin/*.8xp
