double precision function H2toDDBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MD2*(-4.D0*MD2 + MH22)*DBLE(YukS1Quark2**INT(2.D0)))/(MW2*SW2)

 H2toDDBarTree = totalAmplitude
end function H2toDDBarTree