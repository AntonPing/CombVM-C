.DEFAULT_GOAL := $(TARGET)
TARGET = $(DIR_BIN)/Norem
CFLAGS := -Wall -Wextra -Werror -ledit
CC := gcc

DIR_INC = ./include
DIR_SRC = ./src
DIR_OBJ = ./obj
DIR_BIN = ./bin

SRCS = $(wildcard $(DIR_SRC)/*.c)
OBJS = $(patsubst %.c,$(DIR_OBJ)/%.o,$(notdir $(SRCS)))

$(TARGET) : $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $@

$(DIR_OBJ)/%.o : ${DIR_SRC}/%.c
	$(CC) $(CFLAGS) -c $< -o $@

debug: CFLAGS += -D DEBUG
debug: $(TARGET) 

build: CFLAGS += -O3
build: $(TARGET) 

run: $(TARGET)
	@$(TARGET)

.PHONY: clean
clean:
	@rm -rf $(DIR_OBJ)/*
	@touch $(DIR_OBJ)/.gitkeep
	@rm -rf $(DIR_BIN)/*
	@touch $(DIR_BIN)/.gitkeep