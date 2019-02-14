double precision function H3toHmHpTree()
 use constants
 implicit none
#include "looptools.h"
 double precision :: totalAmplitude

 totalAmplitude = DBLE(CS1S3S3f322**INT(2.D0))

 H3toHmHpTree = totalAmplitude
end function H3toHmHpTree