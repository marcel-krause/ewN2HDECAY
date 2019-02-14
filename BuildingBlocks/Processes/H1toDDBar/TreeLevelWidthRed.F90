double precision function H1toDDBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MD2*(-4.D0*MD2 + MH12)*DBLE(YukS1Quark1**INT(2.D0)))/(MW2*SW2)

 H1toDDBarTree = totalAmplitude
end function H1toDDBarTree