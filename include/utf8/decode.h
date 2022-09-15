#ifndef UTF8_DECODE_H
#define UTF8_DECODE_H

#include <stdint.h>

extern inline uint8_t utf8_size (uint8_t);

extern uint32_t utf8_decode_block (uint8_t*, uint32_t*, uint32_t);

#endif // UTF8_DECODE_H