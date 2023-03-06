# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")

C_SRC_DIR = $(CURDIR)
C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT).so

# System type and C compiler/flags.
CC ?= cc

UNAME_SYS := $(shell uname -s)
CFLAGS_CRYPT = -finline-functions -std=c99 -fPIC -I $(ERTS_INCLUDE_DIR)
ifeq ($(UNAME_SYS), Darwin)
	CFLAGS ?= -O3 -Wall -Wmissing-prototypes
	LIBS = -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), Linux)
	UNAME_OP := $(shell uname -o)
  ifeq ($(UNAME_OP), Android)
		CFLAGS ?= -O3 -Wall -Wmissing-prototypes
		LIBS = -lcrypt
  else
		CFLAGS_CRYPT += -DHAVE_CRYPT_R -fwrapv -fno-strict-aliasing
		CFLAGS ?= -O3 -Wall -Wmissing-prototypes \
			-pedantic -D_FORTIFY_SOURCE=2 \
			-fstack-protector-strong -Wformat -Werror=format-security \
			-Wconversion -Wshadow -Wpointer-arith -Wcast-qual
		LIBS = -lpthread -lcrypt
		LDFLAGS ?= -Wl,-z,relro,-z,now -Wl,-z,noexecstack
  endif
else ifneq (,$(wildcard /usr/lib/libcrypt.*))
	CFLAGS ?= -O3 -Wall -Wmissing-prototypes
	LIBS = -lcrypt
endif

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CPPFLAGS) $(CFLAGS_CRYPT) $(CFLAGS) -c
LINK_C = $(link_verbose) $(CC) $(CFLAGS) $(LDFLAGS) -shared

$(C_SRC_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(LINK_C) $(OBJECTS) $(LIBS) -o $(C_SRC_OUTPUT)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS)
