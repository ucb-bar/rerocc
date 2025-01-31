#include "rerocc.h"
#include <stdio.h>

static inline void accum_write(int idx, unsigned long data) {
  ROCC_INSTRUCTION_SS(1, data, idx, 0);
}

static inline unsigned long accum_read(int idx) {
  unsigned long value;
  ROCC_INSTRUCTION_DSS(1, value, 0, idx, 1);
  return value;
}

static inline void accum_load(int idx, void *ptr) {
  asm volatile ("fence");
  ROCC_INSTRUCTION_SS(1, (uintptr_t) ptr, idx, 2);
}

static inline void accum_add(int idx, unsigned long addend) {
  ROCC_INSTRUCTION_SS(1, addend, idx, 3);
}

char test_string[64] __attribute__ ((aligned (64))) = "The quick brown fox jumped over the lazy dog";

static inline unsigned long count_chars(char *start, char needle) {
  unsigned long count;
  ROCC_INSTRUCTION_DSS(2, count, start, needle, 0);
  return count;
}

int accum_test() {
  unsigned long data = 0x3421;
  unsigned long result;

  accum_load(0, &data);
  accum_add(0, 2);
  result = accum_read(0);

  if (result != data + 2)
    return 1;

  accum_write(0, 3);
  accum_add(0, 1);
  result = accum_read(0);

  if (result != 4)
    return 2;

  return 0;
}

int charcount_test() {
  unsigned long count = count_chars(test_string + 14, 'o');
  if (count != 3) return count + 1;
  return 0;
}

int main(void) {
  printf("ReRoCC test\n");

  static uint64_t accum_accels[] = {0, 1, 2, 3};
  static uint64_t charcount_accels[] = {4};

  for (int i = 0; i < 4; i++)
    if (!rr_acquire_multi(i, accum_accels, 4))
      printf("Failed to acquire to cfg %d\n", i);
    else
      printf("Acquired accelerator %ld to cfg %d\n", read_rr_csr(CSR_RRCFG0 + i) & RR_CFG_MGR_MASK, i);

  for (int i = 0; i < 4; i++) rr_release(i);

  for (int i = 0; i < 4; i++)
    if (!rr_acquire_multi(i, accum_accels, 4))
      printf("Failed to acquire to cfg %d\n", i);
    else
      printf("Acquired accelerator %ld to cfg %d\n", read_rr_csr(CSR_RRCFG0 + i) & RR_CFG_MGR_MASK, i);

  int r = 0;

  // For each tracker, assign opcode 1 to it, then perform the operation
  // The tracker will automatically forward instructions from the assigned opcode
  // the the accelerator allocated to that tracker
  for (int i = 0; i < 4; i++) {
    rr_set_opc(0x1, i);
    r += accum_test();
    rr_fence(i);
  }

  rr_acquire_multi(4, charcount_accels, 1);
  rr_set_opc(0x2, 4);
  r += charcount_test();
  rr_fence(4);

  for (int i = 0; i < 16; i++) rr_release(i);

  return 0;
}
