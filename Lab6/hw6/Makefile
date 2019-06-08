default: check

dist:
	tar -czvf hw6.tar.gz main.kt Makefile
check:
	kotlinc main.kt -include-runtime -d hello.jar
	java -jar hello.jar
