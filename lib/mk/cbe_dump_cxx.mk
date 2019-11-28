LIBS += spark aes_cbc_4k

INC_DIR += $(REP_DIR)/src/lib/cbe_dump
INC_DIR += $(REP_DIR)/src/lib/cbe_common
INC_DIR += $(REP_DIR)/src/lib/cbe_dump_cxx
INC_DIR += $(REP_DIR)/src/lib/cbe_cxx_common

SRC_ADB += cbe-cxx-cxx_dump_library.adb

vpath % $(REP_DIR)/src/lib/cbe_dump_cxx
