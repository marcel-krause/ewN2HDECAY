double complex function SelfDBLeftUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (0.03125D0*CKM11*CKMC13*((-0.5D0*EL2*MU2*SB2*A0(MU2))/(MW2*SW2) + (0.5D0*EL2*MU2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0&
  &*EL2*MU2*SB2*B0(x, MU2, MW2))/SW2 + (0.5D0*EL2*MU2*SB2*x*B0(x, MU2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MU2, MW2)*DBLE(MU**&
  &INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(2) = (0.03125D0*CKM21*CKMC23*((-0.5D0*EL2*MC2*SB2*A0(MC2))/(MW2*SW2) + (0.5D0*EL2*MC2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0&
  &*EL2*MC2*SB2*B0(x, MC2, MW2))/SW2 + (0.5D0*EL2*MC2*SB2*x*B0(x, MC2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MC2, MW2)*DBLE(MC**&
  &INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(3) = (0.03125D0*CKM31*CKMC33*((-0.5D0*EL2*MT2*SB2*A0(MT2))/(MW2*SW2) + (0.5D0*EL2*MT2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0&
  &*EL2*MT2*SB2*B0(x, MT2, MW2))/SW2 + (0.5D0*EL2*MT2*SB2*x*B0(x, MT2, MW2))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MT2, MW2)*DBLE(MT**&
  &INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(4) = (0.03125D0*CKM11*CKMC13*((0.5D0*CB2*EL2*MU2*A0(MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MU2*A0(MU2))/(MW2*SW2) - (0.5D0&
  &*CB2*EL2*MHp2*MU2*B0(x, MHp2, MU2))/(MW2*SW2) + (0.5D0*CB2*EL2*MU2*x*B0(x, MHp2, MU2))/(MW2*SW2) + (0.5D0*CB2*EL2*B0(x, MHp2, &
  &MU2)*DBLE(MU**INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(5) = (0.03125D0*CKM21*CKMC23*((-0.5D0*CB2*EL2*MC2*A0(MC2))/(MW2*SW2) + (0.5D0*CB2*EL2*MC2*A0(MHp2))/(MW2*SW2) - (0.5D&
  &0*CB2*EL2*MC2*MHp2*B0(x, MC2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*MC2*x*B0(x, MC2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*B0(x, MC2, &
  &MHp2)*DBLE(MC**INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(6) = (0.03125D0*CKM31*CKMC33*((0.5D0*CB2*EL2*MT2*A0(MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MT2*A0(MT2))/(MW2*SW2) - (0.5D0&
  &*CB2*EL2*MHp2*MT2*B0(x, MHp2, MT2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT2*x*B0(x, MHp2, MT2))/(MW2*SW2) + (0.5D0*CB2*EL2*B0(x, MHp2, &
  &MT2)*DBLE(MT**INT(4.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(7) = (0.03125D0*CKM11*CKMC13*EL2*(-1.D0*x - 1.D0*A0(MU2) + A0(MW2) + MU2*B0(x, MU2, MW2) - 1.D0*MW2*B0(x, MU2, MW2) +&
  & x*B0(x, MU2, MW2)))/(PI2*SW2*x)

 amplitudes(8) = (0.03125D0*CKM21*CKMC23*EL2*(-1.D0*x - 1.D0*A0(MC2) + A0(MW2) + MC2*B0(x, MC2, MW2) - 1.D0*MW2*B0(x, MC2, MW2) +&
  & x*B0(x, MC2, MW2)))/(PI2*SW2*x)

 amplitudes(9) = (0.03125D0*CKM31*CKMC33*EL2*(-1.D0*x - 1.D0*A0(MT2) + A0(MW2) + MT2*B0(x, MT2, MW2) - 1.D0*MW2*B0(x, MT2, MW2) +&
  & x*B0(x, MT2, MW2)))/(PI2*SW2*x)

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfDBLeftUsual = totalAmplitude
end function SelfDBLeftUsual

