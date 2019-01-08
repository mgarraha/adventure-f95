!  I/O ROUTINES (SPEAK, PSPEAK, RSPEAK, GETIN, YES, A5TOA1)


      SUBROUTINE SPEAK(N)

!  PRINT THE MESSAGE WHICH STARTS AT LINES(N).  PRECEDE IT WITH A BLANK LINE
!  UNLESS BLKLIN IS FALSE.

      IMPLICIT NONE
      INTEGER N
      INTEGER I,K,L,M
      CHARACTER*80 BUF
      CHARACTER*5 A5I
      LOGICAL BLKLIN
      COMMON /TXTCOM/ RTEXT,LINES
      COMMON /BLKCOM/ BLKLIN
      INTEGER RTEXT,LINES
      DIMENSION RTEXT(205),LINES(9650)
      EXTERNAL IA5
      INTEGER IA5

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
      END



      SUBROUTINE PSPEAK(MSG,SKIP)

!  FIND THE SKIP+1ST MESSAGE FROM MSG AND PRINT IT.  MSG SHOULD BE THE INDEX OF
!  THE INVENTORY MESSAGE FOR OBJECT.  (INVEN+N+1 MESSAGE IS PROP=N MESSAGE).

      IMPLICIT NONE
      INTEGER MSG,SKIP
      INTEGER I,M
      COMMON /TXTCOM/ RTEXT,LINES
      COMMON /PTXCOM/ PTEXT
      INTEGER RTEXT,LINES,PTEXT
      DIMENSION RTEXT(205),LINES(9650),PTEXT(100)

      M=PTEXT(MSG)
      do I=0,SKIP
1        M=ABS(LINES(M))
         if (LINES(M) >= 0) GOTO 1
      end do
      CALL SPEAK(M)
      RETURN
      END



      SUBROUTINE RSPEAK(I)

!  PRINT THE I-TH "RANDOM" MESSAGE (SECTION 6 OF DATABASE).

      IMPLICIT NONE
      INTEGER I
      COMMON /TXTCOM/ RTEXT,LINES
      INTEGER RTEXT,LINES
      DIMENSION RTEXT(205),LINES(9650)

      if (I /= 0) CALL SPEAK(RTEXT(I))
      RETURN
      END



      SUBROUTINE MSPEAK(I)

!  PRINT THE I-TH "MAGIC" MESSAGE (SECTION 12 OF DATABASE).

      IMPLICIT NONE
      INTEGER I
      COMMON /MTXCOM/ MTEXT
      INTEGER MTEXT
      DIMENSION MTEXT(35)

      if (I /= 0) CALL SPEAK(MTEXT(I))
      RETURN
      END



      SUBROUTINE GETIN(WORD1,WORD1X,WORD2,WORD2X)

!  GET A COMMAND FROM THE ADVENTURER.  SNARF OUT THE FIRST WORD, PAD IT WITH
!  BLANKS, AND RETURN IT IN WORD1.  CHARS 6 THRU 10 ARE RETURNED IN WORD1X, IN
!  CASE WE NEED TO PRINT OUT THE WHOLE WORD IN AN ERROR MESSAGE.  ANY NUMBER OF
!  BLANKS MAY FOLLOW THE WORD.  IF A SECOND WORD APPEARS, IT IS RETURNED IN
!  WORD2 (CHARS 6 THRU 10 IN WORD2X), ELSE WORD2 IS SET TO ZERO.

      IMPLICIT NONE
      INTEGER WORD1,WORD1X,WORD2,WORD2X
      INTEGER I,J,K,MSK,MSK2,SECOND
      LOGICAL BLKLIN
      COMMON /BLKCOM/ BLKLIN
      CHARACTER*20 ITXT
      CHARACTER*5 IWORD
      INTEGER A,MASKS,BLANKS
      DIMENSION A(5),MASKS(6)
      DATA MASKS/O"4000000000",O"20000000",O"100000",O"400",O"2",0/  &
           ,BLANKS/O"201004020100"/
      EXTERNAL IA5
      INTEGER IA5

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
!  convert lowercase to uppercase
         A(I)=IAND(A(I),IEOR(ISHFT(IAND(A(I),IA5('@@@@@')),-1),-1))
      end do
      if (BLKLIN .and. J == 0) GOTO 2

      SECOND=0
      WORD1=A(1)
      WORD1X=A(2)
      WORD2=0

      DO J=1,4
         DO K=1,5
            MSK=O"774000000000"
            if (K /= 1) MSK=O"177"*MASKS(K)
!  is char K a blank?
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
!  overwrite WORD1(K+1:) with blanks
            if (J == 1) WORD1=IOR(IAND(WORD1,-MASKS(K)),  &
                  IAND(BLANKS,IEOR(-MASKS(K),-1)))
         end do
      end do
      RETURN

20    PRINT 21
21    FORMAT(/' PLEASE STICK TO 1- AND 2-WORD COMMANDS.'/)
      GOTO 2
      END



      LOGICAL FUNCTION YES(X,Y,Z)

!  CALL YESX (BELOW) WITH MESSAGES FROM SECTION 6.

      IMPLICIT NONE
      INTEGER X,Y,Z
      EXTERNAL RSPEAK
      LOGICAL YESX

      YES=YESX(X,Y,Z,RSPEAK)
      RETURN
      END



      LOGICAL FUNCTION YESM(X,Y,Z)

!  CALL YESX (BELOW) WITH MESSAGES FROM SECTION 12.

      IMPLICIT NONE
      INTEGER X,Y,Z
      EXTERNAL MSPEAK
      LOGICAL YESX

      YESM=YESX(X,Y,Z,MSPEAK)
      RETURN
      END



      LOGICAL FUNCTION YESX(X,Y,Z,SPK)

!  PRINT MESSAGE X, WAIT FOR YES/NO ANSWER.  IF YES, PRINT Y AND LEAVE YEA
!  TRUE; IF NO, PRINT Z AND LEAVE YEA FALSE.  SPK IS EITHER RSPEAK OR MSPEAK.

      IMPLICIT NONE
      INTEGER X,Y,Z
      INTEGER REPLY,JUNK1,JUNK2,JUNK3
      EXTERNAL SPK
      EXTERNAL IA5
      INTEGER IA5

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
      END



      SUBROUTINE A5TOA1(A,B,C,CHARS,LENG)

!  A AND B CONTAIN A 1- TO 9-CHARACTER WORD IN A5 FORMAT, C CONTAINS ANOTHER
!  WORD AND/OR PUNCTUATION.  THEY ARE UNPACKED TO ONE CHARACTER PER WORD IN THE
!  ARRAY "CHARS", WITH EXACTLY ONE BLANK BETWEEN B AND C (OR NONE, IF C >= 0).
!  THE INDEX OF THE LAST NON-BLANK CHAR IN CHARS IS RETURNED IN LENG.

      IMPLICIT NONE
      INTEGER A,B,C,LENG
      CHARACTER*1 CHARS
      INTEGER WORDS
      DIMENSION CHARS(20),WORDS(3)
      INTEGER POSN,WORD,CH
      INTEGER MASK,BLANK
      DATA MASK,BLANK/O"774000000000",O"200000000000"/

      WORDS(1)=A
      WORDS(2)=B
      WORDS(3)=C
      POSN=1
      do WORD=1,3
         if (WORD == 2 .and. POSN /= 6) cycle
         if (WORD == 3 .and. C < 0)POSN=POSN+1
         do CH=1,5
            CHARS(POSN)=ACHAR(ISHFT(IAND(WORDS(WORD),MASK),-29))
            if (CHARS(POSN) == ' ') exit
            LENG=POSN
            WORDS(WORD)=ISHFT(WORDS(WORD),7)
            POSN=POSN+1
         end do
      end do
      RETURN
      END
!  DATA STRUCTURE ROUTINES (VOCAB, DSTROY, JUGGLE, MOVE, PUT, CARRY, DROP)


      INTEGER FUNCTION VOCAB(ID,INIT)

!  LOOK UP ID IN THE VOCABULARY (ATAB) AND RETURN ITS "DEFINITION" (KTAB), OR
!  -1 IF NOT FOUND.  IF INIT IS POSITIVE, THIS IS AN INITIALISATION CALL SETTING
!  UP A KEYWORD VARIABLE, AND NOT FINDING IT CONSTITUTES A BUG.  IT ALSO MEANS
!  THAT ONLY KTAB VALUES WHICH TAKEN OVER 1000 EQUAL INIT MAY BE CONSIDERED.
!  (THUS "STEPS", WHICH IS A MOTION VERB AS WELL AS AN OBJECT, MAY BE LOCATED
!  AS AN OBJECT.)  AND IT ALSO MEANS THE KTAB VALUE IS TAKEN MOD 1000.

      IMPLICIT NONE
      INTEGER ID,INIT
      INTEGER HASH,I
      COMMON /VOCCOM/ KTAB,ATAB,TABSIZ
      INTEGER KTAB,ATAB,TABSIZ
      DIMENSION KTAB(300),ATAB(300)
      EXTERNAL IA5
      INTEGER IA5

      HASH=IEOR(ID,IA5('PHROG'))
      do I=1,TABSIZ
         if (KTAB(I) == -1) GOTO 2
         if (INIT >= 0 .and. KTAB(I)/1000 /= INIT) cycle
         if (ATAB(I) == HASH) GOTO 3
      end do
      CALL BUG(21)

2     VOCAB=-1
      if (INIT < 0)RETURN
      CALL BUG(5)

3     VOCAB=KTAB(I)
      if (INIT >= 0)VOCAB=MOD(VOCAB,1000)
      RETURN
      END



!  UTILITY ROUTINES (RANI, DATIME, CIAO, BUG)


      integer function RANI(rrange)

!  Returns a value uniformly distributed between 0 and rrange-1.
!  Crowther and Woods implemented an LCG (m=2**20, a=1021, c=0)
!  as an alternative to RAN (the infamous RANDU?) in DEC LIB40.
!  Most F95 random_number implementations are better.

      implicit none
      integer, intent(in) :: rrange
      integer, allocatable :: seed(:)
      integer :: D, T, ticks, nseed
      real :: R
      logical, save :: seeded = .false.

      D = 1
      if (.not. seeded) then
         ! F18 random_init was not widespread in 2019.
         ! Some F95 random_seed implementations have predictable results
         ! when called with no argument.
         call random_seed(size=nseed)
         allocate(seed(nseed))
         ! not ideal but should be OK for this program
         call system_clock(count=ticks)
         if (ticks == 0) ticks = 1
         seed(:) = ticks
         call random_seed(put=seed)
         deallocate(seed)
         seeded = .true.
         call DATIME(D, T)
         D = 10 + mod(D, 10)
      end if
      do T = 1, D
         call random_number(R)
      end do
      RANI = int(rrange * R)
      return
      end function RANI



      SUBROUTINE DATIME(D,T)

!  RETURN THE DATE AND TIME IN D AND T.  D IS NUMBER OF DAYS SINCE 01-JAN-77,
!  T IS MINUTES PAST MIDNIGHT.  THIS IS HARDER THAN IT SOUNDS, BECAUSE THE
!  FINAGLED DEC FUNCTIONS RETURN THE VALUES ONLY AS ASCII STRINGS!

      IMPLICIT NONE
      INTEGER D,T
      INTEGER X,MON,YEAR
      INTEGER DTTM,HATH
      DIMENSION DTTM(8),HATH(12)
      DATA HATH/31,28,31,30,31,30,31,31,30,31,30,31/

      CALL DATE_AND_TIME(VALUES=DTTM)

      YEAR=DTTM(1)-1977
      D=DTTM(3)-1
      X=DTTM(2)
      do MON=1,12
         if (X == MON) GOTO 2
         D=D+HATH(MON)
      end do
      CALL BUG(28)

2     D=D+YEAR*365+YEAR/4
      if (MOD(YEAR,4) == 3 .and. MON > 2) D=D+1
      T=DTTM(5)*60+DTTM(6)
      RETURN
      END



      SUBROUTINE CIAO

!  EXITS, AFTER ISSUING REMINDER TO SAVE NEW CORE IMAGE.  USED WHEN SUSPENDING
!  AND WHEN CREATING NEW VERSION VIA MAGIC MODE.  ON SOME SYSTEMS, THE CORE
!  IMAGE IS LOST ONCE THE PROGRAM EXITS.  IF SO, SET K=31 INSTEAD OF 32.

      IMPLICIT NONE
      INTEGER K,A,B,C,D
      DATA K/32/

      CALL MSPEAK(K)
      if (K == 31) CALL GETIN(A,B,C,D)
      STOP
      END



      SUBROUTINE BUG(NUM)
      IMPLICIT NONE
      INTEGER NUM

!  THE FOLLOWING CONDITIONS ARE CURRENTLY CONSIDERED FATAL BUGS.  NUMBERS < 20
!  ARE DETECTED WHILE READING THE DATABASE; THE OTHERS OCCUR AT "RUN TIME".
!     0     MESSAGE LINE > 70 CHARACTERS
!     1     NULL LINE IN MESSAGE
!     2     TOO MANY WORDS OF MESSAGES
!     3     TOO MANY TRAVEL OPTIONS
!     4     TOO MANY VOCABULARY WORDS
!     5     REQUIRED VOCABULARY WORD NOT FOUND
!     6     TOO MANY RTEXT OR MTEXT MESSAGES
!     7     TOO MANY HINTS
!     8     LOCATION HAS COND BIT BEING SET TWICE
!     9     INVALID SECTION NUMBER IN DATABASE
!     20    SPECIAL TRAVEL (500>L>300) EXCEEDS GOTO LIST
!     21    RAN OFF END OF VOCABULARY TABLE
!     22    VOCABULARY TYPE (N/1000) NOT BETWEEN 0 AND 3
!     23    INTRANSITIVE ACTION VERB EXCEEDS GOTO LIST
!     24    TRANSITIVE ACTION VERB EXCEEDS GOTO LIST
!     25    CONDITIONAL TRAVEL ENTRY WITH NO ALTERNATIVE
!     26    LOCATION HAS NO TRAVEL ENTRIES
!     27    HINT NUMBER EXCEEDS GOTO LIST
!     28    INVALID MONTH RETURNED BY DATE FUNCTION

      PRINT 1, NUM
1     FORMAT (' FATAL ERROR, SEE SOURCE CODE FOR INTERPRETATION.'/  &
              ' PROBABLE CAUSE: ERRONEOUS INFO IN DATABASE.'/  &
              ' ERROR CODE =',I2/)
      STOP
      END

