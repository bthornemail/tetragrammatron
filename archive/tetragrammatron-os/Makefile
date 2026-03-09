# Seed build (portable C99)
CC ?= cc
CFLAGS ?= -O2 -std=c99 -Wall -Wextra -Werror

BIN_DIR := bin
SEED_DIR := src/canasm/seed

all: $(BIN_DIR)/canasm0 $(BIN_DIR)/canasm0-disasm

$(BIN_DIR):
	mkdir -p $(BIN_DIR)

$(BIN_DIR)/canasm0: $(BIN_DIR) $(SEED_DIR)/canasm0.c
	$(CC) $(CFLAGS) -o $@ $(SEED_DIR)/canasm0.c

$(BIN_DIR)/canasm0-disasm: $(BIN_DIR) $(SEED_DIR)/canasm0_disasm.c
	$(CC) $(CFLAGS) -o $@ $(SEED_DIR)/canasm0_disasm.c

clean:
	rm -rf $(BIN_DIR)

.PHONY: all clean
