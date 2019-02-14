double precision function H3toWmWpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (EL2*MW2*(3.D0 - (1.D0*MH32)/MW2 + 0.25D0*DBLE(MH3**INT(4.D0))*DBLE(MW**INT(-4.D0)))*DBLE((CB*RR31 + RR32*SB)**&
  &INT(2.D0)))/SW2

 H3toWmWpTree = totalAmplitude
end function H3toWmWpTree