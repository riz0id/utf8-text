
#include <stdint.h>

/* TODO
 *
 */
static const uint8_t utf8_size_table[] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 3, 3, 4, 0 };

/* TODO
 *
 */
extern inline uint8_t utf8_size (uint8_t x) {
  return utf8_size_table[x >> 3];
}

/* TODO
 *
 */
static inline uint32_t utf8_validate (uint8_t* src, uint32_t* dst) {
  static const uint32_t mins[] = { 4194304, 0, 128, 2048, 65536 };
  static const uint8_t shift[] = { 0, 6, 4, 2, 0 };

  uint8_t len = utf8_size (src[0]);

  uint32_t err;

  err = (*dst < mins[len]) << 6;      // non-canonical encoding
  err |= ((*dst >> 11) == 0x1b) << 7; // surrogate half?
  err |= (*dst > 0x10FFFF) << 8;      // out of range?
  err |= (src[1] & 0xc0) >> 2;
  err |= (src[2] & 0xc0) >> 4;
  err |= (src[3]) >> 6;
  err ^= 0x2a; // top two bits of each tail byte correct?
  err >>= shift[len];

  return err;
}

/* TODO
 *
 */
static inline uint8_t* utf8_decode (uint8_t* src, uint32_t* dst, uint32_t* err) {
  static const uint8_t masks[] = { 0x00, 0x7f, 0x1f, 0x0f, 0x07 };
  static const uint8_t shift[] = { 0, 18, 12, 6, 0 };

  uint8_t len = utf8_size (src[0]);

  *dst = (uint32_t)(src[0] & masks[len]) << 18;
  *dst |= (uint32_t)(src[1] & 0x3f) << 12;
  *dst |= (uint32_t)(src[2] & 0x3f) << 6;
  *dst |= (uint32_t)(src[3] & 0x3f) << 0;
  *dst >>= shift[len];

  // Accumulate the various error conditions.
  *err = utf8_validate (src, dst);

  return src + len + !len;
}

/* TODO
 *
 */
extern uint32_t utf8_decode_block (uint8_t* src, uint32_t* dst, uint32_t block_size) {
  uint8_t* end = src + block_size;
  uint32_t len = 0;
  uint32_t err = 0;

  for (uint8_t* chr = src; chr < end;) {
    uint32_t e;
    chr = utf8_decode (chr, dst + len++, &e);
    err |= e;
  }

  return len;
}
