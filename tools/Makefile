
nolib8xp: nolib8xp.yy.c
	gcc "nolib8xp.yy.c" -o "nolib8xp"


nolib8xp.yy.c: nolib8xp.l
	lex -I -o "nolib8xp.yy.c" "nolib8xp.l"

nolib8xp.l: titablegen.py NoLibReGuess.xml
	python2 ./titablegen.py NoLibReGuess.xml > nolib8xp.l

.PHONY: clean
clean:
	rm -f nolib8xp nolib8xp.yy.c nolib8xp.l *~
