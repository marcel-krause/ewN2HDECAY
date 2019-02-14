double precision function H2toH1H1Tree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = DBLE(CS1S1S1f211**INT(2.D0))

 H2toH1H1Tree = totalAmplitude
end function H2toH1H1Tree