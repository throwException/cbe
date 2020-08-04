REQUIRES += x86_64

TARGET  := cbe_init

SRC_CC  += main.cc
INC_DIR += $(PRG_DIR)
LIBS    += base cbe_init_cxx external_trust_anchor_cxx

CONFIG_XSD = config.xsd
