REQUIRES += x86_64

TARGET  := cbe_init

# force binder for adainit
SRC_ADS += dummy.ads
LIBS    += spark

SRC_CC  += main.cc
INC_DIR += $(PRG_DIR)
LIBS    += base cbe_init cbe_init_cxx

CONFIG_XSD = config.xsd
