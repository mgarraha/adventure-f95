      program porttest
         implicit none
         character*10 buf

         call date(buf)
         print '(A9)', buf
         call time(buf)
         print '(A8)', buf
         call ch2ch('XYZZY')
         call ch2ch('PLUGH')
         call ch2ch('     ')
         call ch2ch(' ')
         call ch2ch('')

      contains

         subroutine ch2ch(before)
            implicit none
            character*5 before
            integer word36, dummy
            integer i, n
            integer after(20)
            integer IA5
            external IA5

            print '(3A)', 'Text before: "', before, '"'
            word36 = IA5(before)
            print '(A,O12.12,A)', '36-bit integer: O"', word36, '"'
            dummy = 0
            call A5TOA1(word36, dummy, dummy, after, n)
            print '(A,20A)', 'Text after: ', (char(after(i)), i = 1, n)
         end subroutine

      end program
