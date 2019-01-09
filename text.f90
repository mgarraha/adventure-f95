module text
   use pdp10
   implicit none

   integer, parameter :: LINSIZ = 9650, RTXSIZ = 205, MAGSIZ = 35
   integer(kind=A5) :: LINES(LINSIZ)
   integer :: RTEXT(RTXSIZ), PTEXT(100), MTEXT(MAGSIZ)
   public :: SPEAK, PSPEAK, RSPEAK, MSPEAK

contains

   subroutine SPEAK(N)

!  PRINT THE MESSAGE WHICH STARTS AT LINES(N).  PRECEDE IT WITH A BLANK LINE
!  UNLESS BLKLIN IS FALSE.

      INTEGER N
      INTEGER I,K,L,M
      character(len=80) :: BUF
      LOGICAL BLKLIN
      COMMON /BLKCOM/ BLKLIN

      if (N == 0) RETURN
      if (LINES(N+1) == IA5('>$<  ')) RETURN
      if (BLKLIN) PRINT 2
      K=N
1     L=ABS(LINES(K))-1
      K=K+1
      BUF=''
      do I=K,L
         M=5*(I-K)
         BUF(M+1:M+5)=A5I(LINES(I))
      end do
      PRINT 2,BUF
2     FORMAT(' ',A)
      K=L+1
      if (LINES(K) >= 0) GOTO 1
      RETURN
   end subroutine SPEAK


   subroutine PSPEAK(MSG,SKIP)

!  FIND THE SKIP+1ST MESSAGE FROM MSG AND PRINT IT.  MSG SHOULD BE THE INDEX OF
!  THE INVENTORY MESSAGE FOR OBJECT.  (INVEN+N+1 MESSAGE IS PROP=N MESSAGE).

      INTEGER MSG,SKIP
      INTEGER I,M

      M=PTEXT(MSG)
      do I=0,SKIP
1        M=ABS(LINES(M))
         if (LINES(M) >= 0) GOTO 1
      end do
      CALL SPEAK(M)
      RETURN
   end subroutine PSPEAK


   subroutine RSPEAK(I)

!  PRINT THE I-TH "RANDOM" MESSAGE (SECTION 6 OF DATABASE).

      INTEGER I

      if (I /= 0) CALL SPEAK(RTEXT(I))
      RETURN
   end subroutine RSPEAK


   subroutine MSPEAK(I)

!  PRINT THE I-TH "MAGIC" MESSAGE (SECTION 12 OF DATABASE).

      INTEGER I

      if (I /= 0) CALL SPEAK(MTEXT(I))
      RETURN
   end subroutine MSPEAK


   logical function YES(X,Y,Z)

!  CALL YESX (BELOW) WITH MESSAGES FROM SECTION 6.

      INTEGER X,Y,Z

      YES=YESX(X,Y,Z,RSPEAK)
      RETURN
   end function YES


   logical function YESM(X,Y,Z)

!  CALL YESX (BELOW) WITH MESSAGES FROM SECTION 12.

      INTEGER X,Y,Z

      YESM=YESX(X,Y,Z,MSPEAK)
      RETURN
   end function YESM


   logical function YESX(X,Y,Z,SPK)

!  PRINT MESSAGE X, WAIT FOR YES/NO ANSWER.  IF YES, PRINT Y AND LEAVE YEA
!  TRUE; IF NO, PRINT Z AND LEAVE YEA FALSE.  SPK IS EITHER RSPEAK OR MSPEAK.

      INTEGER X,Y,Z
      integer(kind=A5) :: REPLY, JUNK1, JUNK2, JUNK3
      EXTERNAL SPK

1     if (X /= 0) CALL SPK(X)
      CALL GETIN(REPLY,JUNK1,JUNK2,JUNK3)
      if (REPLY == IA5('YES  ') .or. REPLY == IA5('Y    ')) GOTO 10
      if (REPLY == IA5('NO   ') .or. REPLY == IA5('N    ')) GOTO 20
      PRINT 9
9     FORMAT(/' PLEASE ANSWER THE QUESTION.')
      GOTO 1
10    YESX=.true.
      if (Y /= 0) CALL SPK(Y)
      RETURN
20    YESX=.false.
      if (Z /= 0) CALL SPK(Z)
      RETURN
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

      if (BLKLIN) PRINT 1
1     FORMAT()
2     READ 3,ITXT
3     FORMAT(A20)
      do I=1,4
         IWORD=ITXT(5*I-4:5*I)
         A(I)=IA5(IWORD)
      end do
      J=0
      do I=1,4
         if (A(I) /= BLANKS)J=1
         ! convert lowercase to uppercase
         A(I)=IAND(A(I),IEOR(ISHFT(IAND(A(I),IA5('@@@@@')),-1),-1))
      end do
      if (BLKLIN .and. J == 0) GOTO 2

      SECOND=0
      WORD1=A(1)
      WORD1X=A(2)
      WORD2=0

      do J=1,4
         do K=1,5
            MSK=O"774000000000"
            if (K /= 1) MSK=O"177"*MASKS(K)
            ! is char K a blank?
            if (IAND(IEOR(A(J),BLANKS),MSK) /= 0) then
               if (SECOND == 3) GOTO 20
               if (SECOND /= 1) cycle
               MSK=-MASKS(6-K)
               MSK2=-2-MSK
               MSK=IAND(MSK,O"777777777776")
               WORD2=IAND(ISHFT(A(J),7*(K-1)),MSK)  &
                     +IAND(ISHFT(A(J+1),7*(K-6)),(MSK2))
               WORD2X=IAND(ISHFT(A(J+1),7*(K-1)),MSK)  &
                     +IAND(ISHFT(A(J+2),7*(K-6)),(MSK2))
               SECOND=2
               cycle
            end if
            if (SECOND == 2) SECOND=3
            if (SECOND /= 0) cycle
            SECOND=1
            ! overwrite WORD1(K+1:) with blanks
            if (J == 1) WORD1=IOR(IAND(WORD1,-MASKS(K)),  &
                  IAND(BLANKS,IEOR(-MASKS(K),-1)))
         end do
      end do
      RETURN

20    PRINT 21
21    FORMAT(/' PLEASE STICK TO 1- AND 2-WORD COMMANDS.'/)
      GOTO 2
   end subroutine GETIN

end module text
