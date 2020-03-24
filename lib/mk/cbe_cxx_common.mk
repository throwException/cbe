LIBS += spark cbe_common

INC_DIR += $(REP_DIR)/src/lib/cbe_common
INC_DIR += $(REP_DIR)/src/lib/cbe_cxx_common

SRC_ADB += cbe-cxx.adb

vpath % $(REP_DIR)/src/lib/cbe_cxx_common
