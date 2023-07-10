module pmem
  use iso_c_binding
  use iso_fortran_env
  implicit none

  private

  ! C types. Might be platform dependent
  integer, parameter, public :: mode_t=int32
  
  public pmem_has_hw_drain
  public pmem_drain
  public pmem_has_auto_flush
  public pmem_deep_flush
  public pmem_flush
  public pmem_persist
  public pmem_msync
  public pmem_is_pmem
  public pmem_map_fileU
  public pmem_map_File
  public pmem_map_fileW
  public pmem_unmap
  public pmem_memmove
  public pmem_memcpy
  public pmem_memset
  public pmem_memmove_nodrain
  public pmem_memcpy_nodrain
  public pmem_memset_nodrain
  public pmem_memmove_persist
  public pmem_memcpy_persist
  public pmem_memset_persist
  public pmem_deep_persist
  public pmem_deep_drain
  public get_processor_and_core

  integer, public :: PMEM_FILE_CREATE = ishft(1,0)
  integer, public :: PMEM_FILE_EXCL = ishft(1,1)
  integer, public :: PMEM_FILE_SPARSE = ishft(1,2)
  integer, public :: PMEM_FILE_TMPFILE = ishft(1,3)

  interface

     !
     ! pmem_has_hw_drain -- return whether or not HW drain was found
     ! 
     ! Always false for x86: HW drain is done by HW with no SW involvement.
     ! 
     !int pmem_has_hw_drain(void)
     type(integer) function pmem_has_hw_drain() bind(c,name="pmem_has_hw_drain")
     end function pmem_has_hw_drain

     !
     ! pmem_drain -- wait for any PM stores to drain from HW buffers
     !
     !void pmem_drain(void)
     subroutine pmem_drain() bind(c,name="pmem_drain")
     end subroutine pmem_drain

     !
     ! pmem_has_auto_flush -- check if platform supports eADR
     !
     !int pmem_has_auto_flush()
     type(integer) function pmem_has_auto_flush() bind(c,name="pmem_has_auto_flush")
     end function pmem_has_auto_flush

     !
     ! pmem_deep_flush -- flush processor cache for the given range
     ! regardless of eADR support on platform
     !
     !void pmem_deep_flush(const void *addr, size_t len)     
     subroutine pmem_deep_flush(addr, len) bind(C,name="pmem_deep_flush")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: addr
       integer(kind=c_size_t), value, intent(in)  :: len
     end subroutine pmem_deep_flush

     !
     ! pmem_flush -- flush processor cache for the given range
     !
     !void pmem_flush(const void *addr, size_t len)
     subroutine pmem_flush(addr, len) bind(C,name="pmem_flush")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: addr
       integer(kind=c_size_t), value, intent(in)  :: len
     end subroutine pmem_flush

     !
     ! pmem_persist -- make any cached changes to a range of pmem persistent
     !
     !void pmem_persist(const void *addr, size_t len)
     subroutine pmem_persist(addr, len) bind(C,name="pmem_persist")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: addr
       integer(kind=c_size_t), value, intent(in)  :: len
     end subroutine pmem_persist

     !
     ! pmem_msync -- flush to persistence via msync
     !
     ! Using msync() means this routine is less optimal for pmem (but it
     ! still works) but it also works for any memory mapped file, unlike
     ! pmem_persist() which is only safe where pmem_is_pmem() returns true.
     !
     !int pmem_msync(const void *addr, size_t len)
     type(integer) function pmem_msync(addr, len) bind(c,name="pmem_msync")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: addr
       integer(kind=c_size_t), value, intent(in)  :: len
     end function pmem_msync

     !
     ! pmem_is_pmem -- return true if entire range is persistent memory
     !
     !int pmem_is_pmem(const void *addr, size_t len)
     type(integer) function pmem_is_pmem(addr, len) bind(c,name="pmem_is_pmem")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: addr
       integer(kind=c_size_t), value, intent(in)  :: len
     end function pmem_is_pmem

     !
     ! pmem_map_fileU -- create or open the file and map it to memory
     !
     !void * pmem_map_fileU(const char *path, size_t len, int flags,
     !mode_t mode, size_t *mapped_lenp, int *is_pmemp)
     type(c_ptr) function pmem_map_fileU(path, len, flags, mode, mapped_lenp, is_pmemp) bind(C,name="pmem_map_fileU")
       import :: mode_t
       import :: c_ptr
       import :: c_size_t
       import :: c_int
       import :: c_char
       character(kind=C_CHAR), dimension(*), intent(in) :: path
       integer(kind=c_size_t), value, intent(in)  :: len
       integer(kind=c_int), value, intent(in) :: flags
       integer(kind=mode_t), value, intent(in)  :: mode
       integer(kind=c_size_t), intent(out)  :: mapped_lenp
       integer(kind=c_int), intent(out) :: is_pmemp
     end function pmem_map_fileU
     
     !
     ! pmem_map_file -- create or open the file and map it to memory
     !
     !void * pmem_map_file(const char *path, size_t len, int flags,
     !	mode_t mode, size_t *mapped_lenp, int *is_pmemp)
     type(c_ptr) function pmem_map_file(path, len, flags, mode, mapped_lenp, is_pmemp) bind(C,name="pmem_map_file")
       import mode_t
       import :: c_ptr
       import :: c_size_t
       import :: c_int
       import :: c_char
       character(kind=C_CHAR), dimension(*), intent(in) :: path
       integer(kind=c_size_t), value, intent(in)  :: len
       integer(kind=c_int), value, intent(in) :: flags
       integer(kind=mode_t), value, intent(in)  :: mode
       integer(kind=c_size_t), intent(out)  :: mapped_lenp
       integer(kind=c_int), intent(out) :: is_pmemp
     end function pmem_map_file

     
     !
     ! pmem_map_fileW -- create or open the file and map it to memory
     !
     !void * pmem_map_fileW(const wchar_t *path, size_t len, int flags, mode_t mode,
     !		size_t *mapped_lenp, int *is_pmemp)
     type(c_ptr) function pmem_map_fileW(path, len, flags, mode, mapped_lenp, is_pmemp) bind(C,name="pmem_map_fileW")
       import :: mode_t
       import :: c_ptr
       import :: c_size_t
       import :: c_int
       import :: c_char
       character(kind=C_CHAR), dimension(*), intent(in) :: path
       integer(kind=c_size_t), value, intent(in)  :: len
       integer(kind=c_int), value, intent(in) :: flags
       integer(kind=mode_t), value, intent(in)  :: mode
       integer(kind=c_size_t), intent(out)  :: mapped_lenp
       integer(kind=c_int), intent(out) :: is_pmemp
     end function pmem_map_fileW

     !
     ! pmem_unmap -- unmap the specified region
     !
     !int pmem_unmap(void *addr, size_t len)
     type(integer) function pmem_unmap(addr, len) bind(c,name="pmem_unmap")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: addr
       integer(kind=c_size_t), value, intent(in)  :: len
     end function pmem_unmap
     
     !
     ! pmem_memmove --  memmove to pmem
     !
     !void * pmem_memmove(void *pmemdest, const void *src, size_t len, unsigned flags)
     type(c_ptr) function pmem_memmove(pmemdest, src, len, flags) bind(C,name="pmem_memmove")
       import :: c_ptr
       import :: c_size_t
       import :: c_int
       type(c_ptr), value,  intent(in) :: pmemdest, src
       integer(kind=c_size_t), value, intent(in)  :: len
       integer(kind=c_int), value, intent(in)  :: flags      
     end function pmem_memmove
     
     !
     ! pmem_memcpy --  memcpy to pmem
     !
     !void * pmem_memcpy(void *pmemdest, const void *src, size_t len, unsigned flags)
     type(c_ptr) function pmem_memcpy(pmemdest, src, len, flags) bind(C,name="pmem_memcpy")
       import :: c_ptr
       import :: c_size_t
       import :: c_int
       type(c_ptr), value,  intent(in) :: pmemdest, src
       integer(kind=c_size_t), value, intent(in)  :: len
       integer(kind=c_int), value, intent(in)  :: flags      
     end function pmem_memcpy
     
     !
     ! pmem_memset -- memset to pmem
     !
     !void * pmem_memset(void *pmemdest, int c, size_t len, unsigned flags)
     type(c_ptr) function pmem_memset(pmemdest, c, len, flags) bind(C,name="pmem_memset")
       import :: c_ptr
       import :: c_size_t
       import :: c_int
       type(c_ptr), value,  intent(in) :: pmemdest
       integer(kind=c_size_t), value, intent(in)  :: len
       integer(kind=c_int), value, intent(in)  :: c, flags      
     end function pmem_memset
     
     !
     ! pmem_memmove_nodrain -- memmove to pmem without hw drain
     !
     !void * pmem_memmove_nodrain(void *pmemdest, const void *src, size_t len)
     type(c_ptr) function pmem_memmove_nodrain(pmemdest, src, len) bind(C,name="pmem_memmove_nodrain")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: pmemdest, src
       integer(kind=c_size_t), value, intent(in)  :: len
     end function pmem_memmove_nodrain
     
     !
     ! pmem_memcpy_nodrain -- memcpy to pmem without hw drain
     !
     !void * pmem_memcpy_nodrain(void *pmemdest, const void *src, size_t len)
     type(c_ptr) function pmem_memcpy_nodrain(pmemdest, src, len) bind(C,name="pmem_memcpy_nodrain")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: pmemdest, src
       integer(kind=c_size_t), value, intent(in)  :: len
     end function pmem_memcpy_nodrain
     
     !
     ! pmem_memmove_persist -- memmove to pmem
     !
     !void * pmem_memmove_persist(void *pmemdest, const void *src, size_t len)
     type(c_ptr) function pmem_memmove_persist(pmemdest, src, len) bind(C,name="pmem_memmove_persist")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: pmemdest, src
       integer(kind=c_size_t), value, intent(in)  :: len
     end function pmem_memmove_persist
     
     !
     ! pmem_memcpy_persist -- memcpy to pmem
     !
     !void * pmem_memcpy_persist(void *pmemdest, const void *src, size_t len)
     type(c_ptr) function pmem_memcpy_persist(pmemdest, src, len) bind(C,name="pmem_memcpy_persist")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: pmemdest, src
       integer(kind=c_size_t), value, intent(in)  :: len
     end function pmem_memcpy_persist
     
     !
     ! pmem_memset_nodrain -- memset to pmem without hw drain
     !
     !void * pmem_memset_nodrain(void *pmemdest, int c, size_t len)
     type(c_ptr) function pmem_memset_nodrain(pmemdest, c, len) bind(C,name="pmem_memset_nodrain")
       import :: c_ptr
       import :: c_size_t
       import :: c_int
       type(c_ptr), value,  intent(in) :: pmemdest
       integer(kind=c_size_t), value, intent(in)  :: len
       integer(kind=c_int), value, intent(in)  :: c
     end function pmem_memset_nodrain
     
     !
     ! pmem_memset_persist -- memset to pmem
     !
     !void * pmem_memset_persist(void *pmemdest, int c, size_t len)
     type(c_ptr) function pmem_memset_persist(pmemdest, c, len) bind(C,name="pmem_memset_persist")
       import :: c_ptr
       import :: c_size_t
       import :: c_int
       type(c_ptr), value,  intent(in) :: pmemdest
       integer(kind=c_size_t), value, intent(in)  :: len
       integer(kind=c_int), value, intent(in)  :: c
     end function pmem_memset_persist
     
     !
     ! pmem_init -- load-time initialization for pmem.c
     !
     !void pmem_init(void)
     ! NOT VISIBLE
     subroutine pmem_init() bind(c,name="pmem_init")
     end subroutine pmem_init

     !
     ! pmem_deep_persist -- perform deep persist on a memory range
     !
     ! It merely acts as wrapper around an msync call in most cases, the only
     ! exception is the case of an mmap'ed DAX device on Linux.
     !
     !int pmem_deep_persist(const void *addr, size_t len)
     type(integer) function pmem_deep_persist(addr, len) bind(c,name="pmem_deep_persist")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: addr
       integer(kind=c_size_t), value, intent(in)  :: len
     end function pmem_deep_persist

     !
     ! pmem_deep_drain -- perform deep drain on a memory range
     !
     !int pmem_deep_drain(const void *addr, size_t len)
     type(integer) function pmem_deep_drain(addr, len) bind(c,name="pmem_deep_drain")
       import :: c_ptr
       import :: c_size_t
       type(c_ptr), value,  intent(in) :: addr
       integer(kind=c_size_t), value, intent(in)  :: len
     end function pmem_deep_drain
     
     !
     ! get_processor_and_core -- get the processor and core this process
     !                           is running on
     !
     !void get_processor_and_core(int *chip, int *core)
     subroutine get_processor_and_core(chip, core) bind(c,name="get_processor_and_core")
       import :: c_int
       integer(kind=c_int) :: chip, core
     end subroutine get_processor_and_core

  end interface


end module pmem
