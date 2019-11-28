REQUIRES += x86_64

TARGET  := cbe_check

# force binder for adainit
SRC_ADS += dummy.ads
LIBS    += spark

SRC_CC  += main.cc
INC_DIR += $(PRG_DIR)
LIBS    += base cbe_check cbe_check_cxx

CONFIG_XSD = config.xsd
