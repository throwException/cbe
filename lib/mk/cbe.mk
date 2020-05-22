LIBS    += spark aes_cbc_4k sha256_4k cbe_common

INC_DIR += $(REP_DIR)/src/lib/cbe

SRC_ADB += cbe-library.adb
SRC_ADB += cbe-pool.adb
SRC_ADB += cbe-crypto.adb
SRC_ADB += cbe-tree_helper.adb
SRC_ADB += cbe-translation.adb
SRC_ADB += cbe-sync_superblock.adb
SRC_ADB += cbe-virtual_block_device.adb
SRC_ADB += cbe-write_back.adb
SRC_ADB += cbe-cache.adb
SRC_ADB += cbe-new_free_tree.adb
SRC_ADB += cbe-ft_resizing.adb
SRC_ADB += cbe-meta_tree.adb
SRC_ADB += cbe-generic_index_queue.adb
SRC_ADB += cbe-superblock_control.adb
SRC_ADB += cbe-vbd_rekeying.adb

vpath % $(REP_DIR)/src/lib/cbe

CC_ADA_OPT += -gnatec=$(REP_DIR)/src/lib/cbe/pragmas.adc
