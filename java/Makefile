# the main class name
MAIN=main

# Location of trees.
SOURCE_DIR=src
OUTPUT_DIR=class

# Java tools
JAVA=java
JFLAGS=-classpath $(CLASSPATH)

JAVAC=javac
JCFLAGS=-sourcepath $(SOURCE_DIR) -d $(OUTPUT_DIR) -classpath $(CLASSPATH)

JAVADOC=javadoc
JDFLAGS=-sourcepath $(SOURCE_DIR) -d $(DOC_DIR)

# jMusic Jars
JM_JAR=jMusic/jMusic1.6.4.jar
JM_INSTR=jMusic/inst/

# Set the CLASSPATH
CLASSPATH=$(OUTPUT_DIR):$(JM_JAR):$(JM_INSTR):.

# List the sourcefiles
FILES=$(SOURCE_DIR)/$(MAIN).java	
	
# compile and run default
default: compile run
	
# Compile the source
.PHONY: compile
compile: 
	mkdir -p $(OUTPUT_DIR)
	$(JAVAC) $(JCFLAGS) $(FILES)

# Run the java main
.PHONY: run
run: 
	$(JAVA) $(JFLAGS) $(MAIN)

.PHONY: clean
clean: 
	rm -rf $(OUTPUT_DIR)/* $(DOC_DIR)/*
	
# all - Perform all tasks for a complete build
.PHONY: all
all: default javadoc

.PHONY: cp
cp: 
	@echo CLASSPATH='$(CLASSPATH)'