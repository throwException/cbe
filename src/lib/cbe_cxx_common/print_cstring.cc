/*
 * Copyright (C) 2020 Genode Labs GmbH, Componolit GmbH, secunet AG
 *
 * This file is part of the Consistent Block Encrypter project, which is
 * distributed under the terms of the GNU Affero General Public License
 * version 3.
 */

/* Genode includes */
#include <util/string.h>
#include <base/log.h>

using namespace Genode;

/**
 * print_cstring
 */
extern "C" void print_cstring(char const *s, Genode::size_t len)
{
	log(Cstring(s, len));
}

/**
 * print_cstring_buffered
 */
extern "C" void print_cstring_buffered(char const     *input,
                                       Genode::size_t  input_len)
{
	enum { BUF_SIZE = 256 };

	static char   buf[BUF_SIZE];
	static size_t buf_left { BUF_SIZE };

	char const *input_curr = input;
	for (size_t input_left = input_len; input_left; input_left--) {
		if (*input_curr == '\n' || buf_left == 0) {
			log(Cstring(buf, BUF_SIZE - buf_left));
			buf_left = BUF_SIZE;
		} else {
			buf[BUF_SIZE - buf_left] = *input_curr;
			buf_left--;
		}
		input_curr++;
	}

}
