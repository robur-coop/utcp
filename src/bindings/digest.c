// This file is largely inspired by:
// https://blogs.igalia.com/dpino/2018/06/14/fast-checksum-computation/
// https://github.com/snabbco/snabb/pull/899
#include <stdint.h>
#include <stdlib.h>

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

CAMLprim value
digest_32_le (value buf, value size)
{
    CAMLparam2(buf, size);

    uint64_t sum = 0;
    uint64_t sum1 = 0;
    const uint64_t *u64 = (const uint64_t *)Caml_ba_data_val(buf);
    uint16_t len = Int_val(size);

    while (len >= 8) {
        uint64_t d = *u64;
        sum  += d&0xffffffff;
        sum1 += d>>32;
        u64 += 1;
        len -= 8;
    }
    sum += sum1;

    // Collect remaining 16b data
    const uint16_t *u16 = (const uint16_t *)u64;
    while (len >= 2) {
        sum += *u16;
        u16 += 1;
        len -= 2;
    }

    // Last one byte?
    if (len == 1) {
        sum += *((const uint8_t *)u16);
    }

    // Fold sum into 16-bit word.
    while (sum>>16) {
        sum = (sum & 0xffff) + (sum>>16);
    }
    CAMLreturn(Val_int((uint16_t)~sum));
}
