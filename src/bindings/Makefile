.PHONY: all clean
all: libdigest_bindings.a

CFLAGS=-ccopt "-I ./include/ -O2 -std=c99 -Wall "
OBJS=digest.o

dlldigest_bindings.a: $(OBJS)
	$(AR) r $@ $^

clean:
	$(RM) $(OBJS) libdigest_bindings.a
