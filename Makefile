
ERL=erl
APP=crypt

CC=gcc

#Mac OS X: use "-m64" for a 64-bit erlang
ARCH=-m32

# By default, use the system crypt(3), which is DES only
FLAGS=$(ARCH) -O3 -fPIC -bundle -flat_namespace -undefined suppress -fno-common 

# Linux
#FLAGS=-fPIC -shared -lcrypt

ERL_ROOT=/usr/local/lib/erlang
CFLAGS=-g -Wall


all: dir erl nif 

dir:
	-@mkdir -p priv ebin

erl:
	@$(ERL) -noinput +B \
		-eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

nif:
	(cd c_src && \
	$(CC) -g -Wall $(FLAGS) -o ../priv/$(APP).so \
		$(APP).c -I $(ERL_ROOT)/usr/include/ )

clean:  
	@rm -fv ebin/*.beam priv/$(APP).so c_src/*.a c_src/*.o


