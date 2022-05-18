CC = gcc
SRC = examples
ASSEMBLY_FLAGS = -S -O3 -fno-asynchronous-unwind-tables

SOURCE = $(wildcard $(SRC)/*.c)

GENERATED = $(patsubst $(SRC)/%.c, $(SRC)/%.s, $(SOURCE))
TARGET = $(patsubst $(SRC)/%.c, $(SRC)/%, $(SOURCE))

GCC_GENERATED = $(patsubst $(SRC)/%.c, $(SRC)/gcc_%.s, $(SOURCE))
GCC_TARGET = $(patsubst $(SRC)/%.c, $(SRC)/gcc_%, $(SOURCE))

$(TARGET): $(SOURCE)
	stack run $^
	$(CC) $(GENERATED) -o $(TARGET)

.PHONY : gcc
gcc:
	$(CC) $(ASSEMBLY_FLAGS) $(SOURCE) -o $(GCC_GENERATED)
	$(CC) $(GENERATED) -o $(GCC_TARGET)

.PHONY : clean
clean:
	rm -f $(TARGET) $(GENERATED) $(GCC_GENERATED) $(GCC_TARGET)