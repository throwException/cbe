REQUIRES := x86_64

TARGET  := cbe_tester
SRC_CC  := main.cc

INC_DIR := $(PRG_DIR)
LIBS    += base cbe_cxx external_crypto_cxx
LIBS    += cbe_init_cxx cbe_check_cxx cbe_dump_cxx
LIBS    += external_trust_anchor_cxx
