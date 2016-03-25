BSDS_LIB=src/image/bsds
BSDS_OBJECTS=$(BSDS_LIB)/bsds.o $(BSDS_LIB)/match.o $(BSDS_LIB)/Timer.o \
						 $(BSDS_LIB)/Matrix.o $(BSDS_LIB)/kofn.o $(BSDS_LIB)/csa.o \
						 $(BSDS_LIB)/String.o $(BSDS_LIB)/Exception.o $(BSDS_LIB)/Random.o

all: src/tags tests/mllib_tests 

src/tags: src/*.sml src/image/*.sml src/ml/*.sml src/math/*.sml
	ctags-exuberant -f src/tags --tag-relative=yes -R src/*

tests/mllib_tests: tests/mllib_tests.mlb tests/ml/*.sml tests/math/*.sml tests/*.sml src/mllib.mlb src/*.sml src/image/mllib_image.mlb src/image/*.sml src/image/io/mllib_image_io.mlb src/image/io/*.sml src/ml/mllib_ml.mlb src/ml/*.sml $(BSDS_OBJECTS)
	mlton -link-opt '-lstdc++' tests/mllib_tests.mlb src/image/f_measure.c $(BSDS_OBJECTS) 

$(BSDS_OBJECTS): %.o: %.cc 
	g++ -Wall -c -DNOBLAS -fPIC $< -o $@

.PHONY: clean

clean:
	rm src/tags tests/mllib_tests $(BSDS_LIB)/*.o 
