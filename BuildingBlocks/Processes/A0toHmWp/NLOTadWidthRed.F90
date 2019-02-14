double complex function A0toHmWpTad()
 use constants
 implicit none
#include "looptools.h"
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(31)

 amplitudes(1) = 0.D0

 amplitudes(2) = 0.D0

 amplitudes(3) = 0.D0

 amplitudes(4) = 0.D0

 amplitudes(5) = 0.D0

 amplitudes(6) = 0.D0

 amplitudes(7) = 0.D0

 amplitudes(8) = 0.D0

 amplitudes(9) = 0.D0

 amplitudes(10) = 0.D0

 amplitudes(11) = 0.D0

 amplitudes(12) = 0.D0

 amplitudes(13) = 0.D0

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

 amplitudes(16) = 0.D0

 amplitudes(17) = 0.D0

 amplitudes(18) = 0.D0

 amplitudes(19) = 0.D0

 amplitudes(20) = 0.D0

 amplitudes(21) = 0.D0

 amplitudes(22) = 0.D0

 amplitudes(23) = 0.D0

 amplitudes(24) = 0.D0

 amplitudes(25) = 0.D0

 amplitudes(26) = 0.D0

 amplitudes(27) = 0.D0

 amplitudes(28) = 0.D0

 amplitudes(29) = 0.D0

 amplitudes(30) = 0.D0

 amplitudes(31) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,31
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do

 A0toHmWpTad = totalAmplitude
end function A0toHmWpTad