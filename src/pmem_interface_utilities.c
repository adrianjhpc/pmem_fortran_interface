#if defined(__aarch64__)
#include <sys/syscall.h>
#endif

#include <sys/sysinfo.h>

#if defined(__aarch64__)
// TODO: This might be general enough to provide the functionality for any system
// regardless of processor type given we aren't worried about thread/process migration.
// Test on Intel systems and see if we can get rid of the architecture specificity
// of the code.
unsigned long get_processor_and_core(int *chip, int *core){
  return syscall(SYS_getcpu, core, chip, NULL);
}
// TODO: Add in AMD function
#else
// If we're not on an ARM processor assume we're on an intel processor and use the
// rdtscp instruction.
unsigned long get_processor_and_core(int *chip, int *core){
  unsigned long a,d,c;
  __asm__ volatile("rdtscp" : "=a" (a), "=d" (d), "=c" (c));
  *chip = (c & 0xFFF000)>>12;
  *core = c & 0xFFF;
  return ((unsigned long)a) | (((unsigned long)d) << 32);;
}
#endif
