module pdp10
   implicit none

   integer, parameter :: A5 = selected_int_kind(11)  ! 2**35 ~= 3.4e+10

contains

!  UTILITY ROUTINES TO BRIDGE THE GAP BETWEEN PDP-10 AND MODERN SYSTEMS

   integer(kind=A5) function IA5(CHARS)

!  PACKS THE 7-BIT CHARACTERS IN CHARS INTO A 36-BIT INTEGER.

      character(len=*), intent(in) :: CHARS
      integer(kind=A5) :: CH
      integer :: I

      IA5 = 0
      do I = 1, 5
         CH = iachar(CHARS(I:I))
         call mvbits(CH, 0, 7, IA5, 36 - 7 * I)
      end do
      return
   end function IA5


   character(len=5) function A5I(BITS)

!  CONVERTS A 36-BIT INTEGER TO 7-BIT CHARACTERS.

      integer(kind=A5), intent(in) :: BITS
      integer(kind=A5) :: CH
      integer :: I

      do I = 1, 5
         CH = ibits(BITS, 36 - 7 * I, 7)
         A5I(I:I) = achar(CH)
      end do
      return
   end function A5I

end module pdp10
