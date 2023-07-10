program streams_memory
  
  use pmem, only : pmem_has_hw_drain, pmem_map_file, pmem_unmap, pmem_persist, mode_t
  use pmem, only : get_processor_and_core, PMEM_FILE_CREATE, PMEM_FILE_EXCL
  use iso_c_binding
  use iso_fortran_env 
  use omp_lib
  use ifcore

  implicit none
  
  !     .. Parameters ..
  integer n,offset,ndim,ntimes
  parameter (n=20000000,offset=1000000,ndim=n+offset,ntimes=10)
  !     ..
  !     .. Local Scalars ..
  double precision scalar,t
  integer j,k,nbpw,quantum
  !     ..
  !     .. Local Arrays ..
  double precision maxtime(4),mintime(4),avgtime(4),times(4,ntimes)
  integer bytes(4)
  character label(4)*11
  !     ..
  !     .. External Functions ..
  double precision mysecond
  integer checktick,realsize
  external mysecond,checktick,realsize
  !     ..
  !     .. Arrays in Common ..
  double precision, pointer :: a(:),b(:),c(:)
  !     ..
  !     .. Common blocks ..
  !     common a,b,c
  !     ..
  !     .. Data statements ..
  data avgtime/4*0.0D0/,mintime/4*1.0D+36/,maxtime/4*0.0D0/
  data label/'Copy:      ','Scale:     ','Add:       ', &
       &     'Triad:     '/
  data bytes/2,2,3,3/
  !     ..
  
  !       --- SETUP --- determine precision and check timing ---

  integer :: has_drain, is_pmem
  integer :: error_code, loop, length
  integer :: processor, core, dim
  integer(kind=c_size_t) :: array_length, mapped_length
  integer(kind=mode_t) :: mode
  double precision :: test_db
  double precision, pointer :: fortran_data(:,:)
  type(c_ptr) :: memory_address, save_memory_address
  character(len=1, kind=C_CHAR) :: c_pathname(200)
  character(len=200) :: pathname
  character(range(processor)) :: processor_name

  call get_processor_and_core(processor, core)

  write(processor_name,'(i0)') processor

  write(*,*) 'Processor (socket)',processor,', core ',core

  pathname = '/mnt/pmem_fsdax'
  pathname = trim(pathname)//processor_name
  pathname = trim(pathname)//'/stream_data.dat'

  ! Converting Fortran string to C string
  length = len_trim(pathname)
  do loop = 1, length
     c_pathname(loop) = pathname(loop:loop)
  end do
  c_pathname(length + 1) = C_NULL_CHAR

  array_length = ndim * 3

  memory_address = pmem_map_file(c_pathname, array_length*sizeof(test_db), ior(PMEM_FILE_CREATE, PMEM_FILE_EXCL), 0666, & 
       & mapped_length, is_pmem)

  if(.not. c_associated(memory_address)) then
     write(*,*) 'Error undertaking pmem_map_file'
     call perror("pmem_map_file")
     stop
  endif

  write(*,*) trim(pathname), ' pmem status ',is_pmem, ' mapped length ', mapped_length

  save_memory_address = memory_address
  call c_f_pointer(save_memory_address, fortran_data, (/ ndim, 3 /))

  do loop = 1, ndim
     do dim = 1, 3
        fortran_data(loop, dim) = loop
     end do
  end do

  a => fortran_data(:,1)
  b => fortran_data(:,2)
  c => fortran_data(:,3)

  do loop = 1, ndim
     a(loop) = loop
     b(loop) = loop
     c(loop) = loop
  end do

  call pmem_persist(memory_address, mapped_length)
  
  do loop = 1, ndim
     b(loop) = a(loop)
     c(loop) = a(loop)
  end do  

  call pmem_persist(memory_address, mapped_length)

  nbpw = realsize()
  
  print *,'----------------------------------------------'
  print *,'STREAM Version $Revision: 5.6 $'
  print *,'----------------------------------------------'
  write (*,FMT=9010) 'Array size = ',n
  write (*,FMT=9010) 'Offset     = ',offset
  write (*,FMT=9020) 'The total memory requirement is ', &
       & 3*nbpw*n/ (1024*1024),' MB'
  write (*,FMT=9030) 'You are running each test ',ntimes,' times'
  write (*,FMT=9030) '--'
  write (*,FMT=9030) 'The *best* time for each test is used'
  write (*,FMT=9030) '*EXCLUDING* the first and last iterations'
  
  !$OMP PARALLEL
  !$OMP MASTER
  print *,'----------------------------------------------'
  !$    print *,'Number of Threads = ',OMP_GET_NUM_THREADS()
  !$OMP END MASTER
  !$OMP END PARALLEL
  
  print *,'----------------------------------------------'
  !$OMP PARALLEL
  print *,'Printing one line per active thread....'
  !$OMP END PARALLEL
  
  ! !$OMP PARALLEL DO
  do j = 1,n
     a(j) = 2.0d0
     b(j) = 0.5D0
     c(j) = 0.0D0
  end do
  t = mysecond()
  ! !$OMP PARALLEL DO
  do j = 1,n
     a(j) = 0.5d0*a(j)
  end do
  t = mysecond() - t
  print *,'----------------------------------------------------'
  quantum = checktick()
  write (*,FMT=9000) &
       &  'Your clock granularity/precision appears to be ',quantum, &
       &  ' microseconds'
  print *,'----------------------------------------------------'
  
  !       --- MAIN LOOP --- repeat test cases NTIMES times ---
  scalar = 0.5d0*a(1)
  do k = 1,ntimes
     
     t = mysecond()
     a(1) = a(1) + t
   !  !$OMP PARALLEL DO
     do j = 1,n
        c(j) = a(j)
     end do
     t = mysecond() - t
     c(n) = c(n) + t
     times(1,k) = t
     
     t = mysecond()
     c(1) = c(1) + t
   !  !$OMP PARALLEL DO
     do j = 1,n
        b(j) = scalar*c(j)
     end do
     t = mysecond() - t
     b(n) = b(n) + t
     times(2,k) = t
     
     t = mysecond()
     a(1) = a(1) + t
   !  !$OMP PARALLEL DO
     do  j = 1,n
        c(j) = a(j) + b(j)
     end do
     t = mysecond() - t
     c(n) = c(n) + t
     times(3,k) = t
     
     t = mysecond()
     b(1) = b(1) + t
   !  !$OMP PARALLEL DO
     do j = 1,n
        a(j) = b(j) + scalar*c(j)
     end do
     t = mysecond() - t
     a(n) = a(n) + t
     times(4,k) = t
  end do

  call pmem_persist(memory_address, mapped_length)

  !       --- SUMMARY ---
  do k = 2,ntimes
     do j = 1,4
        avgtime(j) = avgtime(j) + times(j,k)
        mintime(j) = min(mintime(j),times(j,k))
        maxtime(j) = max(maxtime(j),times(j,k))
     end do
  end do
  write (*,FMT=9040)
  do j = 1,4
     avgtime(j) = avgtime(j)/dble(ntimes-1)
     write (*,FMT=9050) label(j),n*bytes(j)*nbpw/mintime(j)/1.0D6, &
          &      avgtime(j),mintime(j),maxtime(j)
  end do
  print *,'----------------------------------------------------'
  call checksums (a,b,c,n,ntimes)
  print *,'----------------------------------------------------'

  error_code = pmem_unmap(memory_address, mapped_length)

  open(unit=5, file=pathname, status="old")
  close(unit=5, status="delete")

9000 FORMAT (1x,a,i6,a)
9010 FORMAT (1x,a,i10)
9020 FORMAT (1x,a,i4,a)
9030 FORMAT (1x,a,i3,a,a)
9040 FORMAT ('Function',5x,'Rate (MB/s)  Avg time   Min time  Max time')
9050 FORMAT (a,4 (f12.4,2x))

end program streams_memory

!-------------------------------------
! integer function dblesize()
!
! A semi-portable way to determine the precision of double precision
! in Fortran.
! Here used to guess how many bytes of storage a double precision
! number occupies.
!
integer function realsize()
  implicit none
  
  !     .. Local Scalars ..
  double precision result,test
  integer j,ndigits
  !  ..
  !     .. Local Arrays ..
  double precision ref(30)
  !     ..
  !     .. External Subroutines ..
  external confuse
  !     ..
  
  !       Test #1 - compare single(1.0d0+delta) to 1.0d0
  
  do j = 1,30
     ref(j) = 1.0d0 + 10.0d0** (-j)
  end do
  
  do j = 1,30
     test = ref(j)
     ndigits = j
     call confuse(test,result)
     if (test.EQ.1.0D0) then
        GO TO 40
     end if
  end do
  GO TO 50

40 write (*,FMT='(a)') &
        &  '----------------------------------------------'
  write (*,FMT='(1x,a,i2,a)') 'Double precision appears to have ', &
       &  ndigits,' digits of accuracy'
  if (ndigits.LE.8) then
     realsize = 4
  else
     realsize = 8
  end if
  write (*,FMT='(1x,a,i1,a)') 'Assuming ',realsize, &
       &  ' bytes per double precision word'
  write (*,FMT='(a)') &
       &  '----------------------------------------------'
  return
  
50 print *,'Hmmmm.  I am unable to determine the size.'
  print *,'Please enter the number of Bytes per double precision', &
  &  ' number : '
  read (*,FMT=*) realsize
  if (realsize.NE.4 .AND. realsize.NE.8) then
     print *,'Your answer ',realsize,' does not make sense.'
     print *,'Try again.'
     print *,'Please enter the number of Bytes per ', &
          &      'double precision number : '
     read (*,FMT=*) realsize
  end if
  print *,'You have manually entered a size of ',realsize, &
       &  ' bytes per double precision number'
  write (*,FMT='(a)') &
       &  '----------------------------------------------'
end function realsize

subroutine confuse(q,r)
  implicit none
  !     .. Scalar Arguments ..
  double precision q,r
  !     ..
  r = cos(q)
  return
end subroutine confuse

! A semi-portable way to determine the clock granularity
! Adapted from a code by John Henning of Digital Equipment Corporation
!
integer function checktick()
  implicit none

  !     .. Parameters ..
  integer n
  parameter (n=20)
  !     ..
  !     .. Local Scalars ..
  double precision t1,t2
  integer i,j,jmin
  !     ..
  !     .. Local Arrays ..
  double precision timesfound(n)
  !     ..
  !     .. External Functions ..
  double precision mysecond
  external mysecond
  !     ..
  i = 0
  
10 t2 = mysecond()
  if (t2.EQ.t1) GO TO 10
  
  t1 = t2
  i = i + 1
  timesfound(i) = t1
  if (i.LT.n) GO TO 10
  
  jmin = 1000000
  do i = 2,n
     j = nint((timesfound(i)-timesfound(i-1))*1d6)
     jmin = min(jmin,max(j,0))
  end do
  
  if (jmin.gt.0) then
     checktick = jmin
  else
     print *,'Your clock granularity appears to be less ', &
          &     'than one microsecond'
     checktick = 1
  end if
  return
  
end function checktick

subroutine checksums(a,b,c,n,ntimes)
  implicit none
  !     ..
  !     .. Arguments ..
  double precision a(*),b(*),c(*)
  integer n,ntimes
  !     ..
  !     .. Local Scalars ..
  double precision aa,bb,cc,scalar,suma,sumb,sumc,epsilon
  integer j,k
  !     ..
  
  !     Repeat the main loop, but with scalars only.
  !     This is done to check the sum & make sure all
  !     iterations have been executed correctly.
  
  aa = 2.0D0
  bb = 0.5D0
  cc = 0.0D0
  aa = 0.5D0*aa
  scalar = 0.5d0*aa
  do k = 1,ntimes
     cc = aa
     bb = scalar*cc
     cc = aa + bb
     aa = bb + scalar*cc
  end do
  aa = aa*DBLE(n-2)
  bb = bb*DBLE(n-2)
  cc = cc*DBLE(n-2)
  
  !     Now sum up the arrays, excluding the first and last
  !     elements, which are modified using the timing results
  !     to confuse aggressive optimizers.
  
  suma = 0.0d0
  sumb = 0.0d0
  sumc = 0.0d0
  !$OMP PARALLEL DO REDUCTION(+:suma,sumb,sumc)
  do j = 2,n-1
     suma = suma + a(j)
     sumb = sumb + b(j)
     sumc = sumc + c(j)
  end do
  
  epsilon = 1.D-6
  
  if (abs(suma-aa)/suma .gt. epsilon) then
     print *,'Failed Validation on array a()'
     print *,'Target   Sum of a is = ',aa
     print *,'Computed Sum of a is = ',suma
  elseif (abs(sumb-bb)/sumb .gt. epsilon) then
     print *,'Failed Validation on array b()'
     print *,'Target   Sum of b is = ',bb
     print *,'Computed Sum of b is = ',sumb
  elseif (abs(sumc-cc)/sumc .gt. epsilon) then
     print *,'Failed Validation on array c()'
     print *,'Target   Sum of c is = ',cc
     print *,'Computed Sum of c is = ',sumc
  else
     print *,'Solution Validates!'
  endif
  
end subroutine checksums

