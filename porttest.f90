program porttest
   implicit none

   call ch2ch('XYZZY')
   call ch2ch('PLUGH')
   call ch2ch('KEYS ')
   call ch2ch('     ')
   call ch2ch('')

contains

   subroutine ch2ch(before)
      use pdp10
      use advn2
      character(len=*), intent(in) :: before
      character(len=20) :: after
      integer(kind=A5) :: word36, dummy
      integer :: i, n

      print '(3A)', 'Text before: "', before, '"'
      word36 = IA5(before)
      print '(A,O12.12,A)', '36-bit integer: O"', word36, '"'
      dummy = 0
      call A5TOA1(word36, dummy, dummy, after, n)
      print '(A,20A)', 'Text after: "', after(:n), '"'
   end subroutine

end program
