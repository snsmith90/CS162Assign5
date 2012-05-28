all: checker

checker: syntax.scala levels.scala checker.scala 
	scalac -d build -unchecked syntax.scala levels.scala checker.scala

clean:
	rm -rf build/*
