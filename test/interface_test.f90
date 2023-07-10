program interface_test

  use pmem, only : pmem_has_hw_drain, pmem_map_file, pmem_unmap, pmem_persist, get_processor_and_core, mode_t
  use pmem, only : PMEM_FILE_CREATE, PMEM_FILE_EXCL
  use iso_c_binding
  use iso_fortran_env 
!  use ifcore

  implicit none

  integer :: loop, length
  integer :: has_drain, is_pmem
  integer :: error_code
  integer :: processor, core
  integer(kind=c_size_t) :: array_length, mapped_length
  integer(kind=mode_t) :: mode
  type(c_ptr) :: memory_address
  real(kind=real64), pointer :: fortran_data(:)
  character(len=1, kind=C_CHAR) :: c_pathname(200)
  character(len=200) :: pathname
  character(range(processor)) :: processor_name

  call get_processor_and_core(processor, core)

  write(processor_name,'(i0)') processor

  write(*,*) 'Processor (socket)',processor,', core ',core

  pathname = '/mnt/pmem_fsdax'
  pathname = trim(pathname)//processor_name
  pathname = trim(pathname)//'/datafile.dat'

  write(*,*) 'Path: ', pathname

  ! Converting Fortran string to C string
  length = len_trim(pathname)
  do loop = 1, length
     c_pathname(loop) = pathname(loop:loop)
  end do
  c_pathname(length + 1) = C_NULL_CHAR


  array_length = 100


  memory_address = pmem_map_file(c_pathname, array_length, ior(PMEM_FILE_CREATE, PMEM_FILE_EXCL), 0666, mapped_length, is_pmem)

  if(.not. c_associated(memory_address)) then
     write(*,*) 'Error undertaking pmem_map_file'
     call perror("pmem_map_file")
     stop
  endif

  write(*,*) trim(pathname), ' pmem status ',is_pmem, ' mapped length ', mapped_length
  call c_f_pointer(memory_address, fortran_data, (/ array_length /))

  do loop = 1, length
     fortran_data(loop) = loop
  end do

  error_code = pmem_unmap(memory_address, mapped_length)

  open(unit=5, file=pathname, status="old")
  close(unit=5, status="delete")

end program interface_test
