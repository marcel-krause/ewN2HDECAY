double complex function DSelfUTLeft(x)
 use constants
 implicit none
#include "looptools.h"
 double precision, intent(in) :: x
 integer :: j
 double complex :: totalAmplitude
 double complex :: amplitudes(9)

 amplitudes(1) = (-0.03125D0*CKM31*CKMC11*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MD2*SB2*A0(MD2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2&
  &) + (0.5D0*EL2*MD2*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*B0(x, MD2, MW2)*DBLE(YukS3Quark1**&
  &INT(2.D0)))/SW2 + (0.5D0*EL2*MD2*SB2*x*B0(x, MD2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MD2, MW2&
  &)*DBLE(MD**INT(4.D0))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM31*CKMC11*((0.5D0*EL2*MD2*SB2*B0(x, &
  &MD2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*SB2*DB0(x, MD2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0&
  &.5D0*EL2*MD2*SB2*x*DB0(x, MD2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/ (MW2*SW2) + (0.5D0*EL2*SB2*DB0(x, MD2, MW2)*DBLE(MD**INT(4.&
  &D0))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(2) = (-0.03125D0*CKM32*CKMC12*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MS2*SB2*A0(MS2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2&
  &) + (0.5D0*EL2*MS2*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MS2*SB2*B0(x, MS2, MW2)*DBLE(YukS3Quark1**&
  &INT(2.D0)))/SW2 + (0.5D0*EL2*MS2*SB2*x*B0(x, MS2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MS2, MW2&
  &)*DBLE(MS**INT(4.D0))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM32*CKMC12*((0.5D0*EL2*MS2*SB2*B0(x, &
  &MS2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MS2*SB2*DB0(x, MS2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0&
  &.5D0*EL2*MS2*SB2*x*DB0(x, MS2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/ (MW2*SW2) + (0.5D0*EL2*SB2*DB0(x, MS2, MW2)*DBLE(MS**INT(4.&
  &D0))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(3) = (-0.03125D0*CKM33*CKMC13*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MB2*SB2*A0(MB2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2&
  &) + (0.5D0*EL2*MB2*SB2*A0(MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*SB2*B0(x, MB2, MW2)*DBLE(YukS3Quark1**&
  &INT(2.D0)))/SW2 + (0.5D0*EL2*MB2*SB2*x*B0(x, MB2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*B0(x, MB2, MW2&
  &)*DBLE(MB**INT(4.D0))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM33*CKMC13*((0.5D0*EL2*MB2*SB2*B0(x, &
  &MB2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*SB2*DB0(x, MB2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/SW2 + (0&
  &.5D0*EL2*MB2*SB2*x*DB0(x, MB2, MW2)*DBLE(YukS3Quark1**INT(2.D0)))/ (MW2*SW2) + (0.5D0*EL2*SB2*DB0(x, MB2, MW2)*DBLE(MB**INT(4.&
  &D0))*DBLE(YukS3Quark1**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(4) = (-0.03125D0*CKM31*CKMC11*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MD2*SB2*A0(MD2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2&
  &) + (0.5D0*EL2*MD2*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*MHp2*SB2*B0(x, MD2, MHp2)*DBLE(YukS3Q&
  &uark2**INT(2.D0)))/ (MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*B0(x, MD2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2&
  &*B0(x, MD2, MHp2)*DBLE(MD**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM31*CKMC11*((0.5D0*EL&
  &2*MD2*SB2*B0(x, MD2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MD2*MHp2*SB2*DB0(x, MD2, MHp2)*DBLE(YukS3Quark&
  &2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MD2*SB2*x*DB0(x, MD2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*DB0&
  &(x, MD2, MHp2)*DBLE(MD**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(5) = (-0.03125D0*CKM32*CKMC12*DBLE(x**INT(-2.D0))*((0.5D0*EL2*MS2*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2&
  &) - (0.5D0*EL2*MS2*SB2*A0(MS2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MHp2*MS2*SB2*B0(x, MHp2, MS2)*DBLE(YukS3Qu&
  &ark2**INT(2.D0)))/ (MW2*SW2) + (0.5D0*EL2*MS2*SB2*x*B0(x, MHp2, MS2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*&
  &B0(x, MHp2, MS2)*DBLE(MS**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM32*CKMC12*((0.5D0*EL2&
  &*MS2*SB2*B0(x, MHp2, MS2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MHp2*MS2*SB2*DB0(x, MHp2, MS2)*DBLE(YukS3Quark2&
  &**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MS2*SB2*x*DB0(x, MHp2, MS2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*DB0(&
  &x, MHp2, MS2)*DBLE(MS**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(6) = (-0.03125D0*CKM33*CKMC13*DBLE(x**INT(-2.D0))*((-0.5D0*EL2*MB2*SB2*A0(MB2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2&
  &) + (0.5D0*EL2*MB2*SB2*A0(MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*MHp2*SB2*B0(x, MB2, MHp2)*DBLE(YukS3Q&
  &uark2**INT(2.D0)))/ (MW2*SW2) + (0.5D0*EL2*MB2*SB2*x*B0(x, MB2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2&
  &*B0(x, MB2, MHp2)*DBLE(MB**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2) + (0.03125D0*CKM33*CKMC13*((0.5D0*EL&
  &2*MB2*SB2*B0(x, MB2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) - (0.5D0*EL2*MB2*MHp2*SB2*DB0(x, MB2, MHp2)*DBLE(YukS3Quark&
  &2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*MB2*SB2*x*DB0(x, MB2, MHp2)*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2) + (0.5D0*EL2*SB2*DB0&
  &(x, MB2, MHp2)*DBLE(MB**INT(4.D0))*DBLE(YukS3Quark2**INT(2.D0)))/(MW2*SW2)))/(PI2*SB2*x)

 amplitudes(7) = (0.03125D0*CKM31*CKMC11*EL2*(-1.D0 + B0(x, MD2, MW2) + MD2*DB0(x, MD2, MW2) - 1.D0*MW2*DB0(x, MD2, MW2) + x*DB0(&
  &x, MD2, MW2)))/(PI2*SW2*x) - (0.03125D0*CKM31*CKMC11*EL2*(-1.D0*x - 1.D0*A0(MD2) + A0(MW2) + MD2*B0(x, MD2, MW2) - 1.D0*MW2*B0&
  &(x, MD2, MW2) + x*B0(x, MD2, MW2))* DBLE(x**INT(-2.D0)))/(PI2*SW2)

 amplitudes(8) = (0.03125D0*CKM32*CKMC12*EL2*(-1.D0 + B0(x, MS2, MW2) + MS2*DB0(x, MS2, MW2) - 1.D0*MW2*DB0(x, MS2, MW2) + x*DB0(&
  &x, MS2, MW2)))/(PI2*SW2*x) - (0.03125D0*CKM32*CKMC12*EL2*(-1.D0*x - 1.D0*A0(MS2) + A0(MW2) + MS2*B0(x, MS2, MW2) - 1.D0*MW2*B0&
  &(x, MS2, MW2) + x*B0(x, MS2, MW2))* DBLE(x**INT(-2.D0)))/(PI2*SW2)

 amplitudes(9) = (0.03125D0*CKM33*CKMC13*EL2*(-1.D0 + B0(x, MB2, MW2) + MB2*DB0(x, MB2, MW2) - 1.D0*MW2*DB0(x, MB2, MW2) + x*DB0(&
  &x, MB2, MW2)))/(PI2*SW2*x) - (0.03125D0*CKM33*CKMC13*EL2*(-1.D0*x - 1.D0*A0(MB2) + A0(MW2) + MB2*B0(x, MB2, MW2) - 1.D0*MW2*B0&
  &(x, MB2, MW2) + x*B0(x, MB2, MW2))* DBLE(x**INT(-2.D0)))/(PI2*SW2)

  totalAmplitude = (0D0,0D0)
 do j=1,9
  totalAmplitude = totalAmplitude + amplitudes(j)
 end do
 DSelfUTLeft = totalAmplitude
end function DSelfUTLeft

