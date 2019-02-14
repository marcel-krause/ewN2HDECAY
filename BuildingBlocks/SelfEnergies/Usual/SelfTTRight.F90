double complex function SelfTTRightUsual(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(16)

 amplitudes(1) = (0.0078125D0*CA22*EL2*MT2*SA12*(A0(MH12) - 1.D0*A0(MT2) - 1.D0*MH12*B0(x, MH12, MT2) + MT2*B0(x, MH12, MT2) + x*&
  &B0(x, MH12, MT2)))/ (MW2*PI2*SB2*SW2*x)

 amplitudes(2) = (0.0078125D0*EL2*MT2*(A0(MH22) - 1.D0*A0(MT2) - 1.D0*MH22*B0(x, MH22, MT2) + MT2*B0(x, MH22, MT2) + x*B0(x, MH22&
  &, MT2))* DBLE((CA1*CA3 - 1.D0*SA1*SA2*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2*x)

 amplitudes(3) = (0.0078125D0*EL2*MT2*(A0(MH32) - 1.D0*A0(MT2) - 1.D0*MH32*B0(x, MH32, MT2) + MT2*B0(x, MH32, MT2) + x*B0(x, MH32&
  &, MT2))* DBLE((-1.D0*CA3*SA1*SA2 - 1.D0*CA1*SA3)**INT(2.D0)))/(MW2*PI2*SB2*SW2*x)

 amplitudes(4) = (0.0078125D0*EL2*MT2*(-1.D0*A0(MT2) + A0(MZ2) + MT2*B0(x, MT2, MZ2) - 1.D0*MZ2*B0(x, MT2, MZ2) + x*B0(x, MT2, MZ&
  &2)))/(MW2*PI2*SW2*x)

 amplitudes(5) = (0.0078125D0*CB2*EL2*MT2*(A0(MA02) - 1.D0*A0(MT2) - 1.D0*MA02*B0(x, MA02, MT2) + MT2*B0(x, MA02, MT2) + x*B0(x, &
  &MA02, MT2)))/ (MW2*PI2*SB2*SW2*x)

 amplitudes(6) = (0.03125D0*CKM31*CKMC31*((-0.5D0*EL2*MT2*SB2*A0(MD2))/(MW2*SW2) + (0.5D0*EL2*MT2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0&
  &*EL2*MT2*SB2*B0(x, MD2, MW2))/SW2 + (0.5D0*EL2*MD2*MT2*SB2*B0(x, MD2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT2*SB2*x*B0(x, MD2, MW2))/&
  &(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(7) = (0.03125D0*CKM32*CKMC32*((-0.5D0*EL2*MT2*SB2*A0(MS2))/(MW2*SW2) + (0.5D0*EL2*MT2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0&
  &*EL2*MT2*SB2*B0(x, MS2, MW2))/SW2 + (0.5D0*EL2*MS2*MT2*SB2*B0(x, MS2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT2*SB2*x*B0(x, MS2, MW2))/&
  &(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(8) = (0.03125D0*CKM33*CKMC33*((-0.5D0*EL2*MT2*SB2*A0(MB2))/(MW2*SW2) + (0.5D0*EL2*MT2*SB2*A0(MW2))/(MW2*SW2) - (0.5D0&
  &*EL2*MT2*SB2*B0(x, MB2, MW2))/SW2 + (0.5D0*EL2*MB2*MT2*SB2*B0(x, MB2, MW2))/(MW2*SW2) + (0.5D0*EL2*MT2*SB2*x*B0(x, MB2, MW2))/&
  &(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(9) = (0.03125D0*CKM31*CKMC31*((-0.5D0*CB2*EL2*MT2*A0(MD2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT2*A0(MHp2))/(MW2*SW2) + (0.5D&
  &0*CB2*EL2*MD2*MT2*B0(x, MD2, MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MT2*B0(x, MD2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT2*x*B0(&
  &x, MD2, MHp2))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(10) = (0.03125D0*CKM32*CKMC32*((0.5D0*CB2*EL2*MT2*A0(MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MT2*A0(MS2))/(MW2*SW2) - (0.5D&
  &0*CB2*EL2*MHp2*MT2*B0(x, MHp2, MS2))/(MW2*SW2) + (0.5D0*CB2*EL2*MS2*MT2*B0(x, MHp2, MS2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT2*x*B0(&
  &x, MHp2, MS2))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(11) = (0.03125D0*CKM33*CKMC33*((-0.5D0*CB2*EL2*MT2*A0(MB2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT2*A0(MHp2))/(MW2*SW2) + (0.5&
  &D0*CB2*EL2*MB2*MT2*B0(x, MB2, MHp2))/(MW2*SW2) - (0.5D0*CB2*EL2*MHp2*MT2*B0(x, MB2, MHp2))/(MW2*SW2) + (0.5D0*CB2*EL2*MT2*x*B0&
  &(x, MB2, MHp2))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(12) = (0.027777777777777776D0*EL2*(-1.D0*x - 1.D0*A0(MT2) + MT2*B0(x, 0.D0, MT2) + x*B0(x, 0.D0, MT2)))/(PI2*x)

 amplitudes(13) = (0.001736111111111111D0*EL2*(-16.D0*x*DBLE(SW**INT(4.D0)) - 16.D0*A0(MT2)*DBLE(SW**INT(4.D0)) + 16.D0*A0(MZ2)*D&
  &BLE(SW**INT(4.D0)) + 16.D0*MT2*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) - 16.D0*MZ2*B0(x, MT2, MZ2)*DBLE(SW**INT(4.D0)) + 16.D0*x*B&
  &0(x, MT2, MZ2)*DBLE(SW**INT(4.D0))))/ (CW2*PI2*SW2*x)

 amplitudes(14) = 0.D0

 amplitudes(15) = 0.D0

 amplitudes(16) = 0.D0

  totalAmplitude = (0D0,0D0)
 do j=1,16
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 SelfTTRightUsual = totalAmplitude
end function SelfTTRightUsual

