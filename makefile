.DEFAULT_GOAL := $(TARGET)
TARGET = $(DIR_BIN)/Norem
LIBS = -ledit -lpthread
CFLAGS := -Wall -Wextra -Werror
CC := gcc

DIR_INC = ./include
DIR_SRC = ./src
DIR_OBJ = ./obj
DIR_BIN = ./bin

SRCS = $(wildcard $(DIR_SRC)/*.c)
OBJS = $(patsubst %.c,$(DIR_OBJ)/%.o,$(notdir $(SRCS)))

$(TARGET) : $(OBJS)
	$(CC) $(CFLAGS) $(LIBS) $(OBJS) -o $@

$(DIR_OBJ)/%.o : ${DIR_SRC}/%.c
	$(CC) $(CFLAGS) $(LIBS) -c $< -o $@

debug: CFLAGS += -D DEBUG
debug: $(TARGET) 

build: CFLAGS += -O3
build: $(TARGET) 

run: $(TARGET)
	@$(TARGET)

.PHONY: clean
clean:

	@rm -r $(DIR_OBJ)/*.o
	@touch $(DIR_OBJ)/.gitkeep
	@rm -r $(DIR_BIN)/*
	@touch $(DIR_BIN)/.gitkeep