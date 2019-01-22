module text
   use pdp10
   implicit none

   integer, parameter :: LINSIZ = 9650, RTXSIZ = 205
   integer(kind=A5) :: LINES(LINSIZ) = 0
   integer :: RTEXT(RTXSIZ) = 0
   public :: SPEAK, RSPEAK

contains

   subroutine SPEAK(N)

!  PRINT THE MESSAGE WHICH STARTS AT LINES(N).  PRECEDE IT WITH A BLANK LINE
!  UNLESS BLKLIN IS FALSE.

      integer, intent(in) :: N
      integer :: I, K, L, M
      character(len=80) :: BUF
      LOGICAL BLKLIN
      COMMON /BLKCOM/ BLKLIN

      if (N == 0) return
      if (LINES(N+1) == IA5('>$<  ')) return
      if (BLKLIN) print 2
      K=N
      do
         L = abs(LINES(K)) - 1
         K = K + 1
         BUF = ''
         do I = K, L
            M = 5 * (I - K)
            BUF(M+1:M+5) = A5I(LINES(I))
         end do
         print 2, BUF
2        format (' ', A)
         K=L+1
         if (LINES(K) < 0) exit
      end do
      return
   end subroutine SPEAK


   subroutine RSPEAK(I)

!  PRINT THE I-TH "RANDOM" MESSAGE (SECTION 6 OF DATABASE).

      integer, intent(in) :: I

      if (I /= 0) call SPEAK(RTEXT(I))
      return
   end subroutine RSPEAK


   logical function YES(X,Y,Z)

!  CALL YESX (BELOW) WITH MESSAGES FROM SECTION 6.

      integer, intent(in) :: X, Y, Z

      YES = YESX(X, Y, Z, RSPEAK)
      return
   end function YES


   logical function YESX(X, Y, Z, SPK)

!  PRINT MESSAGE X, WAIT FOR YES/NO ANSWER.  IF YES, PRINT Y AND LEAVE YEA
!  TRUE; IF NO, PRINT Z AND LEAVE YEA FALSE.  SPK IS EITHER RSPEAK OR MSPEAK.

      integer, intent(in) :: X, Y, Z
      integer(kind=A5) :: REPLY, JUNK1, JUNK2, JUNK3
      external :: SPK

      do
         if (X /= 0) call SPK(X)
         call GETIN(REPLY, JUNK1, JUNK2, JUNK3)
         if (REPLY == IA5('YES  ') .or. REPLY == IA5('Y    ')) then
            YESX = .true.
            if (Y /= 0) call SPK(Y)
            return
         else if (REPLY == IA5('NO   ') .or. REPLY == IA5('N    ')) then
            YESX = .false.
            if (Z /= 0) call SPK(Z)
            return
         else
            print 9
9           format (/' PLEASE ANSWER THE QUESTION.')
         end if
      end do
   end function YESX


   subroutine GETIN(WORD1, WORD1X, WORD2, WORD2X)

!  GET A COMMAND FROM THE ADVENTURER.  SNARF OUT THE FIRST WORD, PAD IT WITH
!  BLANKS, AND RETURN IT IN WORD1.  CHARS 6 THRU 10 ARE RETURNED IN WORD1X, IN
!  CASE WE NEED TO PRINT OUT THE WHOLE WORD IN AN ERROR MESSAGE.  ANY NUMBER OF
!  BLANKS MAY FOLLOW THE WORD.  IF A SECOND WORD APPEARS, IT IS RETURNED IN
!  WORD2 (CHARS 6 THRU 10 IN WORD2X), ELSE WORD2 IS SET TO ZERO.

      integer(kind=A5), intent(out) :: WORD1, WORD1X, WORD2, WORD2X
      integer :: I, J, K, SECOND
      integer(kind=A5) :: MSK, MSK2
      LOGICAL BLKLIN
      COMMON /BLKCOM/ BLKLIN
      character(len=20) :: ITXT
      character(len=5) :: IWORD
      integer(kind=A5) :: A(5), MASKS(6), BLANKS
      DATA MASKS/O"4000000000",O"20000000",O"100000",O"400",O"2",0/  &
           ,BLANKS/O"201004020100"/

      if (BLKLIN) print 1
1     format ()
      retry: do
         read 3, ITXT
3        format (A20)
         do I = 1, 4
            IWORD = ITXT(5*I-4:5*I)
            A(I) = IA5(IWORD)
         end do
         J = 0
         do I = 1, 4
            if (A(I) /= BLANKS) J = 1
            ! convert lowercase to uppercase
            A(I) = iand(A(I), ieor(ishft(iand(A(I), IA5('@@@@@')), -1), -1))
         end do
         if (BLKLIN .and. J == 0) cycle retry

         SECOND = 0
         WORD1 = A(1)
         WORD1X = A(2)
         WORD2 = 0

         do J = 1, 4
            do K = 1, 5
               ! is char K a blank?
               if (ibits(A(J), 36 - 7 * K, 7) /= iachar(' ')) then
                  if (SECOND == 3) then
                     print 21
21                   format (/' PLEASE STICK TO 1- AND 2-WORD COMMANDS.'/)
                     cycle retry
                  end if
                  if (SECOND /= 1) cycle
                  MSK = -MASKS(6-K)
                  MSK2 = -2 - MSK
                  MSK = iand(MSK, O"777777777776")
                  WORD2 = iand(ishft(A(J), 7*(K-1)), MSK)  &
                        + iand(ishft(A(J+1), 7*(K-6)), MSK2)
                  WORD2X = iand(ishft(A(J+1), 7*(K-1)), MSK)  &
                        + iand(ishft(A(J+2), 7*(K-6)), MSK2)
                  SECOND = 2
                  cycle
               end if
               if (SECOND == 2) SECOND = 3
               if (SECOND /= 0) cycle
               SECOND = 1
               ! overwrite WORD1(K+1:) with blanks
               if (J == 1) call mvbits(BLANKS, 0, 36 - 7 * K, WORD1, 0)
            end do
         end do
         return
      end do retry
   end subroutine GETIN


   subroutine set_rtext(ID, LINDEX)
      use advn2
      integer, intent(in) :: ID, LINDEX

      if (ID <= 0 .or. ID > RTXSIZ) call BUG(6)
      RTEXT(ID) = LINDEX
      return
   end subroutine set_rtext

end module text
