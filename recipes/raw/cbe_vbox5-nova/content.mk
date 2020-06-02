content: init.config usb_devices

init.config:
	cp $(REP_DIR)/recipes/raw/cbe_vbox5-nova/$@ $@

usb_devices:
	cp $(REP_DIR)/recipes/raw/cbe_vbox5-nova/$@ $@
