#!/bin/bash

echo "--- Automated CBE testing ---"

test_create_snapshot() {
	local cbe_dir="$1"

	echo "Create snapshot"
	echo true > $cbe_dir/control/create_snapshot
}

test_rw() {
	local data_file="$1"
	[ "$data_file" = "" ] && exit 1

	# read complete current snapshot
	echo "Read '$data_file'"
	dd if=$data_file of=/dev/null bs=4096

	# write complete current snapshot
	echo "Write '$data_file'"
	dd if=$data_file of=/dev/null bs=4096
}

produce_pattern() {
	local pattern="$1"
	local size="$2"
	[ "$pattern" = "" ] && exit 1

	local N=16384
	# prints numbers until N and uses pattern as delimiter and
	# generates about 85 KiB of data with a 1 byte pattern
	seq -s "$pattern" $N | dd count=1 bs=$size 2>/dev/null
}

test_write_1() {
	local data_file="$1"
	local offset=$2
	local pattern="$3"

	local pattern_file="/tmp/pattern.$pattern"
	produce_pattern "$pattern" "4096" > $pattern_file
	cat $pattern_file
	dd bs=4096 count=1 if=$pattern_file of=$data_file seek=$offset || exit 1
	cat $pattern_file
}

test_read_compare_1() {
	local data_file="$1"
	local offset=$2
	local pattern="$3"

	local pattern_file="/tmp/pattern.$pattern"
	produce_pattern "$pattern" "4096" > $pattern_file

	dd bs=4096 count=1 if=$data_file of=$pattern_file.out seek=$offset || exit 1
	sha1sum $pattern_file
	sha1sum $pattern_file.out
}

test_list_snapshots() {
	local cbe_dir="$1"

	echo "List content of '$cbe_dir'"
	ls -l $cbe_dir/snapshots
}

test_discard_snapshot() {
	local cbe_dir="$1"
	local snap_id=$2

	echo "Discard snapshot with id: $snap_id"
	echo $snap_id > $cbe_dir/control/discard_snapshot
}

test_rekey() {
	local cbe_dir="$1"

	echo "Start rekeying"
	echo on > $cbe_dir/control/rekey
	echo "Reykeying started"
}

main() {
	local cbe_dir="/dev/cbe"
	local data_file="$cbe_dir/current/data"

	test_write_1 "$data_file" "0" "1"
	test_create_snapshot "$cbe_dir"

	test_read_compare_1 "$data_file" "0" "1"

	test_rekey "$cbe_dir"

	test_read_compare_1 "$data_file" "0" "1"

	echo "--- Automated CBE testing finished, shell is yours ---"
}

main "$@"

# just drop into shell
# exit 0
