module locations
   implicit none

   public
   integer, parameter :: LOCSIZ = 150
   integer, dimension(LOCSIZ) :: ATLOC = 0, ABB = 0, LTEXT = 0, STEXT = 0,  &
         KEY = 0, COND = 0

contains

   subroutine set_ltext(LOC, LINDEX)
      integer, intent(in) :: LOC, LINDEX

      LTEXT(LOC) = LINDEX
      return
   end subroutine set_ltext


   subroutine set_stext(LOC, LINDEX)
      integer, intent(in) :: LOC, LINDEX

      STEXT(LOC) = LINDEX
      return
   end subroutine set_stext

end module locations
