
ASM = $(shell which spasm fasmg z80asm | head -n1)

all: ./bin/GF256PLY.8xp ./bin/ECCTBL.8xp ./bin/BIN2HEXZ.8xp ./bin/REAL2BIN.8xp ./bin/QRZMAIN.8xp

./bin/GF256PLY.8xp: ./src/asm/GF256PLY.asm
	$(ASM) -I./include ./src/asm/GF256PLY.asm ./bin/GF256PLY.8xp

./bin/ECCTBL.8xp: ./src/asm/ECCTBL.asm
	$(ASM) -I./include ./src/asm/ECCTBL.asm ./bin/ECCTBL.8xp

./bin/BIN2HEXZ.8xp: ./src/asm/BIN2HEXZ.asm
	$(ASM) -I./include ./src/asm/BIN2HEXZ.asm ./bin/BIN2HEXZ.8xp

./bin/REAL2BIN.8xp: ./src/basic/REAL2BIN.8xp.txt
	touch ./bin/REAL2BIN.8xp

./bin/QRZMAIN.8xp: ./src/basic/QRZMAIN.8xp.txt
	touch ./bin/QRZMAIN.8xp


.PHONY: clean

clean:
	rm -f ./bin/*.8xp
