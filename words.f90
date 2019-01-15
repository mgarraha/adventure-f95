module words
   use pdp10
   implicit none

   integer, parameter, public :: TABSIZ = 300
   integer(kind=A5), private :: ATAB(TABSIZ)
   integer, public :: KTAB(TABSIZ)

   integer, protected :: AXE, BACK, BATTER, BEAR, BIRD, BOTTLE,  &
         CAGE, CAVE, CHAIN, CHASM, CHEST, CLAM, COINS, DOOR, DPRSSN, DRAGON,  &
         DWARF, EGGS, EMRALD, ENTRNC, FIND, FISSUR, FOOD, GRATE, INVENT,  &
         KEYS, KNIFE, LAMP, LOCK, LOOK, MAGZIN, MESSAG, MIRROR, NOOP, NUGGET,  &
         OIL, OYSTER, PEARL, PILLOW, PLANT, PLANT2, PYRAM, ROD, ROD2, RUG,  &
         SAY, SNAKE, SPICES, STEPS, TABLET, THROW, TRIDNT, TROLL, TROLL2,  &
         VASE, VEND, WATER

contains

   integer function VOCAB(ID, INIT)

!  LOOK UP ID IN THE VOCABULARY (ATAB) AND RETURN ITS "DEFINITION" (KTAB), OR
!  -1 IF NOT FOUND.  IF INIT IS POSITIVE, THIS IS AN INITIALISATION CALL SETTING
!  UP A KEYWORD VARIABLE, AND NOT FINDING IT CONSTITUTES A BUG.  IT ALSO MEANS
!  THAT ONLY KTAB VALUES WHICH TAKEN OVER 1000 EQUAL INIT MAY BE CONSIDERED.
!  (THUS "STEPS", WHICH IS A MOTION VERB AS WELL AS AN OBJECT, MAY BE LOCATED
!  AS AN OBJECT.)  AND IT ALSO MEANS THE KTAB VALUE IS TAKEN MOD 1000.

      use advn2
      integer(kind=A5), intent(in) :: ID
      integer, intent(in) :: INIT
      integer(kind=A5) :: HASH
      integer :: I

      HASH = ieor(ID, IA5('PHROG'))
      do I = 1, TABSIZ
         if (KTAB(I) == -1) GOTO 2
         if (INIT >= 0 .and. KTAB(I)/1000 /= INIT) cycle
         if (ATAB(I) == HASH) GOTO 3
      end do
      call BUG(21)

2     VOCAB = -1
      if (INIT < 0) return
      call BUG(5)

3     VOCAB = KTAB(I)
      if (INIT >= 0) VOCAB = mod(VOCAB, 1000)
      return
   end function VOCAB


   integer function read_vocab(fileno)

!  HERE WE READ IN THE VOCABULARY.  KTAB(N) IS THE WORD NUMBER, ATAB(N) IS
!  THE CORRESPONDING WORD.  THE -1 AT THE END OF SECTION 4 IS LEFT IN KTAB
!  AS AN END-MARKER.  THE WORDS ARE GIVEN A MINIMAL HASH TO MAKE READING THE
!  CORE-IMAGE HARDER.  NOTE THAT '/7-08' HAD BETTER NOT BE IN THE LIST, SINCE
!  IT COULD HASH TO -1.

      use advn2
      integer, intent(in) :: fileno
      integer :: TABNDX, IOE
      character(len=80) :: TXT
      character(len=5) :: WORD

      do TABNDX = 1, TABSIZ
1043     read (fileno,'(A80)') TXT
         read (TXT,*,IOSTAT=IOE) KTAB(TABNDX), WORD
         ATAB(TABNDX) = IA5(WORD)
         if (KTAB(TABNDX) == 0) GOTO 1043  ! AVOID F40 EOL BUG
         if (KTAB(TABNDX) == -1) then
            read_vocab = TABNDX
            return
         end if
         ATAB(TABNDX) = ieor(ATAB(TABNDX), IA5('PHROG'))
      end do
      CALL BUG(4)
   end function read_vocab


   subroutine SETWORDS()

!  DEFINE SOME HANDY MNEMONICS.  THESE CORRESPOND TO OBJECT NUMBERS.

      KEYS=VOCAB(IA5('KEYS '),1)
      LAMP=VOCAB(IA5('LAMP '),1)
      GRATE=VOCAB(IA5('GRATE'),1)
      CAGE=VOCAB(IA5('CAGE '),1)
      ROD=VOCAB(IA5('ROD  '),1)
      ROD2=ROD+1
      STEPS=VOCAB(IA5('STEPS'),1)
      BIRD=VOCAB(IA5('BIRD '),1)
      DOOR=VOCAB(IA5('DOOR '),1)
      PILLOW=VOCAB(IA5('PILLO'),1)
      SNAKE=VOCAB(IA5('SNAKE'),1)
      FISSUR=VOCAB(IA5('FISSU'),1)
      TABLET=VOCAB(IA5('TABLE'),1)
      CLAM=VOCAB(IA5('CLAM '),1)
      OYSTER=VOCAB(IA5('OYSTE'),1)
      MAGZIN=VOCAB(IA5('MAGAZ'),1)
      DWARF=VOCAB(IA5('DWARF'),1)
      KNIFE=VOCAB(IA5('KNIFE'),1)
      FOOD=VOCAB(IA5('FOOD '),1)
      BOTTLE=VOCAB(IA5('BOTTL'),1)
      WATER=VOCAB(IA5('WATER'),1)
      OIL=VOCAB(IA5('OIL  '),1)
      PLANT=VOCAB(IA5('PLANT'),1)
      PLANT2=PLANT+1
      AXE=VOCAB(IA5('AXE  '),1)
      MIRROR=VOCAB(IA5('MIRRO'),1)
      DRAGON=VOCAB(IA5('DRAGO'),1)
      CHASM=VOCAB(IA5('CHASM'),1)
      TROLL=VOCAB(IA5('TROLL'),1)
      TROLL2=TROLL+1
      BEAR=VOCAB(IA5('BEAR '),1)
      MESSAG=VOCAB(IA5('MESSA'),1)
      VEND=VOCAB(IA5('VENDI'),1)
      BATTER=VOCAB(IA5('BATTE'),1)

!  OBJECTS FROM 50 THROUGH WHATEVER ARE TREASURES.  HERE ARE A FEW.

      NUGGET=VOCAB(IA5('GOLD '),1)
      COINS=VOCAB(IA5('COINS'),1)
      CHEST=VOCAB(IA5('CHEST'),1)
      EGGS=VOCAB(IA5('EGGS '),1)
      TRIDNT=VOCAB(IA5('TRIDE'),1)
      VASE=VOCAB(IA5('VASE '),1)
      EMRALD=VOCAB(IA5('EMERA'),1)
      PYRAM=VOCAB(IA5('PYRAM'),1)
      PEARL=VOCAB(IA5('PEARL'),1)
      RUG=VOCAB(IA5('RUG  '),1)
      CHAIN=VOCAB(IA5('CHAIN'),1)
      SPICES=VOCAB(IA5('SPICE'),1)

!  THESE ARE MOTION-VERB NUMBERS.

      BACK=VOCAB(IA5('BACK '),0)
      LOOK=VOCAB(IA5('LOOK '),0)
      CAVE=VOCAB(IA5('CAVE '),0)
      NOOP=VOCAB(IA5('NULL '),0)
      ENTRNC=VOCAB(IA5('ENTRA'),0)
      DPRSSN=VOCAB(IA5('DEPRE'),0)

!  AND SOME ACTION VERBS.

      SAY=VOCAB(IA5('SAY  '),2)
      LOCK=VOCAB(IA5('LOCK '),2)
      THROW=VOCAB(IA5('THROW'),2)
      FIND=VOCAB(IA5('FIND '),2)
      INVENT=VOCAB(IA5('INVEN'),2)

   end subroutine SETWORDS

end module words
