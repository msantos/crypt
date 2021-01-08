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
ifeq ($(UNAME_SYS), Darwin)
	CFLAGS ?= -O3 -std=c99 -arch x86_64 -finline-functions -Wall -Wmissing-prototypes
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(UNAME_SYS), Linux)
	UNAME_OP := $(shell uname -o)
  ifeq ($(UNAME_OP), Android)
		CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
		LDFLAGS ?= -lcrypt
  else
		CFLAGS ?= -DHAVE_CRYPT_R -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes \
			-pedantic -fwrapv -D_FORTIFY_SOURCE=2 \
			-fstack-protector-strong -Wformat -Werror=format-security \
			-fno-strict-aliasing -Wconversion -Wshadow -Wpointer-arith \
			-Wcast-qual
		LDFLAGS ?= -lpthread -lcrypt -Wl,-z,relro,-z,now -Wl,-z,noexecstack
  endif
else ifneq (,$(wildcard /usr/lib/libcrypt.*))
	CFLAGS ?= -O3 -std=c99 -finline-functions -Wall -Wmissing-prototypes
	LDFLAGS ?= -lcrypt
endif

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR)

LDFLAGS += -shared

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.c" -o -name "*.C" -o -name "*.cc" -o -name "*.cpp" \))
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) -c

$(C_SRC_OUTPUT): $(OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CC) $(OBJECTS) $(LDFLAGS) -o $(C_SRC_OUTPUT)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

clean:
	@rm -f $(C_SRC_OUTPUT) $(OBJECTS)
