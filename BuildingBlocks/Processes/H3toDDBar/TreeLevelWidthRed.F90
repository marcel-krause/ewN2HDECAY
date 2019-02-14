double precision function H3toDDBarTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = (1.5D0*EL2*MD2*(-4.D0*MD2 + MH32)*DBLE(YukS1Quark3**INT(2.D0)))/(MW2*SW2)

 H3toDDBarTree = totalAmplitude
end function H3toDDBarTree