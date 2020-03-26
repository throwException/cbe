REQUIRES += x86_64

TARGET  := cbe_dump

# force binder for adainit
SRC_ADS += dummy.ads
LIBS    += spark

SRC_CC  += main.cc
INC_DIR += $(PRG_DIR)
LIBS    += base cbe_dump cbe_dump_cxx

CONFIG_XSD = config.xsd
