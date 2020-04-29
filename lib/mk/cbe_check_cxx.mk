LIBS += spark aes_cbc_4k cbe_cxx_common

INC_DIR += $(REP_DIR)/src/lib/cbe_check
INC_DIR += $(REP_DIR)/src/lib/cbe_common
INC_DIR += $(REP_DIR)/src/lib/cbe_check_cxx
INC_DIR += $(REP_DIR)/src/lib/cbe_cxx_common

SRC_ADB += cbe-cxx-cxx_check_library.adb

vpath % $(REP_DIR)/src/lib/cbe_check_cxx
