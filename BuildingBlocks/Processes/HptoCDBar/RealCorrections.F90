double precision function HptoCDBarReal()
 use constants
 use counterterms
 implicit none
#include "looptools.h"

 double precision :: totalAmplitude
 double precision :: p2, p3, E2, E3, m1, m2, m3, kappa, beta1, beta2, beta3
 double precision :: I11, I22, I33, I12, I13, I23
 double precision :: IFin, Id1Fin, Id2Fin, Id3Fin, Iu2d1Fin, Iu3d1Fin, Iu1d2Fin, Iu1d3Fin, Iu2d3Fin, Iu3d2Fin, Iu22d33Fin
 double precision :: Iu11d22Fin, Osvv11, Osvv12, Osvv22
 double precision :: OLL00, OLL01, OLL02, OLL11, OLL12, OLL22, OLR00, OLR01, OLR02, OLR11, OLR12, OLR22, OLLFull, OLRFull
 double precision :: Ossv00, Ossv01, Ossv11, Ossv02, Ossv12, Ossv22, Ossv03, Ossv13, Ossv23, Ossv33, OssvFull
 double precision :: Qssv0, Qssv1, Qssv2, Qsvv1, Qsvv2, QsvvFull, Osss11, Osss12, Osss22, OsssFull, Qsss1, Qsss2

 m1 = MHp
 m2 = MD
 m3 = MC

 kappa = DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 - 2D0*m2**2*m3**2)
 beta1 = (m1**2 - m2**2 - m3**2 + kappa)/(2D0*m2*m3)
 beta2 = (m1**2 - m2**2 + m3**2 - kappa)/(2D0*m1*m3)
 beta3 = (m1**2 + m2**2 - m3**2 - kappa)/(2D0*m1*m2)

 p2 = kappa/(2D0*m1)
 p3 = kappa/(2D0*m1)
 E2 = DSQRT( m2**2 + p2**2 )
 E3 = DSQRT( m3**2 + p3**2 )

 I11 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m2**2-m3**2)*DLOG(beta2/beta3) - m1**2*DLOG(beta1) )&
     &/(4D0*m1**4)
 I22 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m1**2-m3**2)*DLOG(beta1/beta3) - m2**2*DLOG(beta2) )&
     &/(4D0*m1**2*m2**2)
 I33 = ( kappa*DLOG(kappa**2/(DSQRT(IRLambda)*m1*m2*m3)) - kappa - (m1**2-m2**2)*DLOG(beta1/beta2) - m3**2*DLOG(beta3) )&
     &/(4D0*m1**2*m3**2)
 I12 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta3) + 2D0*DLOG(beta3)**2 - DLOG(beta1)**2 - DLOG(beta2)**2 + &
     & 2D0*Li2(1D0 - beta3**2) - Li2(1D0 - beta1**2) - Li2(1D0 - beta2**2) )/(4D0*m1**2)
 I13 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta2) + 2D0*DLOG(beta2)**2 - DLOG(beta1)**2 - DLOG(beta3)**2 + &
     & 2D0*Li2(1D0 - beta2**2) - Li2(1D0 - beta1**2) - Li2(1D0 - beta3**2) )/(4D0*m1**2)
 I23 = ( - 2D0*DLOG(DSQRT(IRLambda)*m1*m2*m3/kappa**2)*DLOG(beta1) + 2D0*DLOG(beta1)**2 - DLOG(beta2)**2 - DLOG(beta3)**2 + &
     & 2D0*Li2(1D0 - beta1**2) - Li2(1D0 - beta2**2) - Li2(1D0 - beta3**2) )/(4D0*m1**2)
 IFin = ( kappa/2D0*(m1**2 + m2**2 + m3**2) + 2D0*m1**2*m2**2*DLOG(beta3) + 2D0*m1**2*m3**2*DLOG(beta2) + &
     & 2D0*m2**2*m3**2*DLOG(beta1) )/(4D0*m1**2)
 Id1Fin = ( - 2D0*m2**2*DLOG(beta3) - 2D0*m3**2*DLOG(beta2) - kappa )/(4D0*m1**2)
 Id2Fin = ( - 2D0*m1**2*DLOG(beta3) - 2D0*m3**2*DLOG(beta1) - kappa )/(4D0*m1**2)
 Id3Fin = ( - 2D0*m1**2*DLOG(beta2) - 2D0*m2**2*DLOG(beta1) - kappa )/(4D0*m1**2)
 Iu2d1Fin = ( m2**4*DLOG(beta3) - m3**2*(2D0*m1**2 - 2D0*m2**2 + m3**2)*DLOG(beta2) &
     & - kappa/4D0*(m1**2 - 3D0*m2**2 + 5D0*m3**2) )/(4D0*m1**2)
 Iu3d1Fin = ( m3**4*DLOG(beta2) - m2**2*(2D0*m1**2 - 2D0*m3**2 + m2**2)*DLOG(beta3) &
     & - kappa/4D0*(m1**2 - 3D0*m3**2 + 5D0*m2**2) )/(4D0*m1**2)
 Iu1d2Fin = ( m1**4*DLOG(beta3) - m3**2*(2D0*m2**2 - 2D0*m1**2 + m3**2)*DLOG(beta1) &
     & - kappa/4D0*(m2**2 - 3D0*m1**2 + 5D0*m3**2) )/(4D0*m1**2)
 Iu1d3Fin = ( m1**4*DLOG(beta2) - m2**2*(2D0*m3**2 - 2D0*m1**2 + m2**2)*DLOG(beta1) &
     & - kappa/4D0*(m3**2 - 3D0*m1**2 + 5D0*m2**2) )/(4D0*m1**2)
 Iu2d3Fin = ( m2**4*DLOG(beta1) - m1**2*(2D0*m3**2 - 2D0*m2**2 + m1**2)*DLOG(beta2) &
     & - kappa/4D0*(m3**2 - 3D0*m2**2 + 5D0*m1**2) )/(4D0*m1**2)
 Iu3d2Fin = ( m3**4*DLOG(beta1) - m1**2*(2D0*m2**2 - 2D0*m3**2 + m1**2)*DLOG(beta3) &
     & - kappa/4D0*(m2**2 - 3D0*m3**2 + 5D0*m1**2) )/(4D0*m1**2)
 Iu11d22Fin = ( 2D0*m3**2*(m2**2 + m3**2 - m1**2)*DLOG(beta1) &
     & + kappa**3/(6D0*m2**2) + 2D0*kappa*m3**2 )/(4D0*m1**2)
 Iu22d33Fin = ( 2D0*m1**2*(m1**2 + m3**2 - m2**2)*DLOG(beta2) &
     & + kappa**3/(6D0*m3**2) + 2D0*kappa*m1**2 )/(4D0*m1**2)

OLR00 = 16D0*I11*m1**2*m2*m3
OLR01 = -16D0*Id1Fin*m2*m3 - 16D0*Id2Fin*m2*m3 + I12*(-16D0*m1**2*m2*m3 - 16D0*m2**3*m3 + 16D0*m2*m3**3)
OLR02 = 16D0*Id1Fin*m2*m3 + I13*(16D0*m1**2*m2*m3 - 16D0*m2**3*m3 + 16D0*m2*m3**3)
OLR11 = 16D0*I22*m2**3*m3
OLR12 = 16D0*Id2Fin*m2*m3 + I23*(-16D0*m1**2*m2*m3 + 16D0*m2**3*m3 + 16D0*m2*m3**3)
OLR22 = 16D0*Id3Fin*m2*m3 + 16D0*I33*m2*m3**3
OLL00 = -8D0*Id1Fin*m1**2 + I11*(-8D0*m1**4 + 8D0*m1**2*m2**2 + 8D0*m1**2*m3**2)
OLL01 = 4D0*IFin + 4D0*Iu1d2Fin + Id2Fin*(12D0*m1**2 - 4D0*m2**2 - 12D0*m3**2) + Id1Fin*(-8D0*m2**2 - 8D0*m3**2) + &
    & I12*(8D0*m1**4 - 8D0*m2**4 - 16D0*m1**2*m3**2 + 8D0*m3**4)
OLL02 = -4D0*IFin - 4D0*Iu1d3Fin + Id3Fin*(-12D0*m1**2 + 12D0*m2**2 + 4D0*m3**2) + Id1Fin*(8D0*m2**2 + 8D0*m3**2) + &
    & I13*(-8D0*m1**4 + 16D0*m1**2*m2**2 - 8D0*m2**4 + 8D0*m3**4)
OLL11 = Id2Fin*(4D0*m1**2 + 4D0*m2**2 - 4D0*m3**2) + I22*(-8D0*m1**2*m2**2 + 8D0*m2**4 + 8D0*m2**2*m3**2)
OLL12 = 8D0*IFin + 4D0*Iu3d2Fin + 4D0*Iu2d3Fin + Id3Fin*(-12D0*m1**2 + 12D0*m2**2 + 4D0*m3**2) + Id2Fin*(-12D0*m1**2 + &
    & 4D0*m2**2 + 12D0*m3**2 ) + I23*(8D0*m1**4 - 16D0*m1**2*m2**2 + 8D0*m2**4 - 16D0*m1**2*m3**2 + 16D0*m2**2*m3**2&
    & + 8D0*m3**4)
OLL22 = Id3Fin*(4D0*m1**2 - 4D0*m2**2 + 4D0*m3**2) + I33*(-8D0*m1**2*m3**2 + 8D0*m2**2*m3**2 + 8D0*m3**4)

! Ossv00 = -4D0*IFin*m1**2 + Id1Fin*(-8D0*m1**4 + 8D0*m1**2*m2**2 + 8D0*m1**2*m3**2) + I11*(-4D0*m1**6 + 8D0*m1**4*m2**2 - &
!     & 4D0*m1**2*m2**4 + 8D0*m1**4*m3**2 + 8D0*m1**2*m2**2*m3**2 - 4D0*m1**2*m3**4 )
! Ossv01 = Iu2d1Fin*(4D0*m1**2 - 4D0*m2**2 + 4D0*m3**2) + Iu1d2Fin*(-4D0*m1**2 + 4D0*m2**2 + 4D0*m3**2) + IFin*(4D0*m1**2 +&
!     & 4D0*m2**2 + 4D0*m3**2 ) + Id2Fin*(-8D0*m1**4 + 8D0*m1**2*m2**2 + 16D0*m1**2*m3**2 + 8D0*m2**2*m3**2 - 8D0*m3**4) + &
!     & Id1Fin*(8D0*m1**2*m2**2 - 8D0*m2**4 + 8D0*m1**2*m3**2 + 16D0*m2**2*m3**2 - 8D0*m3**4) + I12*(-4D0*m1**6 + &
!     & 4D0*m1**4*m2**2 + 4D0*m1**2*m2**4 - 4D0*m2**6 + 12D0*m1**4*m3**2 + 8D0*m1**2*m2**2*m3**2 + 12D0*m2**4*m3**2 - &
!     & 12D0*m1**2*m3**4 - 12D0*m2**2*m3**4 + 4D0*m3**6)
! Ossv02 = Iu3d1Fin*(-2D0*m1**2 + 2D0*m2**2 - 2D0*m3**2) + IFin*(2D0*m1**2 + 6D0*m2**2 + 2D0*m3**2) + Id1Fin*(2D0*m1**4 + &
!     & 4D0*m1**2*m2**2 - 6D0*m2**4 + 4D0*m1**2*m3**2 + 12D0*m2**2*m3**2 - 6D0*m3**4 ) + Id3Fin*(-4D0*m1**4 + &
!     & 8D0*m1**2*m2**2 - 4D0*m2**4 + 4D0*m3**4 ) + I13*(-4D0*m1**6 + 12D0*m1**4*m2**2 - 12D0*m1**2*m2**4 + 4D0*m2**6 + &
!     & 4D0*m1**4*m3**2 + 8D0*m1**2*m2**2*m3**2 - 12D0*m2**4*m3**2 + 4D0*m1**2*m3**4 + 12D0*m2**2*m3**4 - 4D0*m3**6 )
! Ossv03 = IFin*(2D0*m1**2 - 2D0*m2**2 + 2D0*m3**2) + Iu3d1Fin*(2D0*m1**2 - 2D0*m2**2 + 2D0*m3**2) + Id1Fin*(2D0*m1**4 - &
!     & 4D0*m1**2*m2**2 + 2D0*m2**4 - 4D0*m1**2*m3**2 - 4D0*m2**2*m3**2 + 2D0*m3**4 )
! Ossv11 = -4D0*IFin*m2**2 + Id2Fin*(8D0*m1**2*m2**2 - 8D0*m2**4 + 8D0*m2**2*m3**2) + I22*(-4D0*m1**4*m2**2 + &
!     & 8D0*m1**2*m2**4 - 4D0*m2**6 + 8D0*m1**2*m2**2*m3**2 + 8D0*m2**4*m3**2 - 4D0*m2**2*m3**4 )
! Ossv12 = IFin*(-6D0*m1**2 - 2D0*m2**2 - 2D0*m3**2) + Iu3d2Fin*(-2D0*m1**2 + 2D0*m2**2 + 2D0*m3**2) + Id3Fin*(4D0*m1**4 - &
!     & 8D0*m1**2*m2**2 + 4D0*m2**4 - 4D0*m3**4 ) + Id2Fin*(6D0*m1**4 - 4D0*m1**2*m2**2 - 2D0*m2**4 - 12D0*m1**2*m3**2 - &
!     & 4D0*m2**2*m3**2 + 6D0*m3**4 ) + I23*(-4D0*m1**6 + 12D0*m1**4*m2**2 - 12D0*m1**2*m2**4 + 4D0*m2**6 + 12D0*m1**4*m3**2 &
!     & - 8D0*m1**2*m2**2*m3**2 - 4D0*m2**4*m3**2 - 12D0*m1**2*m3**4 - 4D0*m2**2*m3**4 + 4D0*m3**6 )
! Ossv13 = IFin*(-2D0*m1**2 + 2D0*m2**2 + 2D0*m3**2) + Iu3d2Fin*(-2D0*m1**2 + 2D0*m2**2 + 2D0*m3**2) + Id2Fin*(2D0*m1**4 - &
!     & 4D0*m1**2*m2**2 + 2D0*m2**4 - 4D0*m1**2*m3**2 - 4D0*m2**2*m3**2 + 2D0*m3**4 )
! Ossv22 = IFin*(-2D0*m1**2 - 2D0*m2**2 - m3**2) + 8D0*Iu2d3Fin*m3**2 + 8D0*Iu22d33Fin*m3**2 + Id3Fin*(8D0*m1**2*m3**2 + &
!     & 8D0*m2**2*m3**2 - 8D0*m3**4 ) + I33*(-4D0*m1**4*m3**2 + 8D0*m1**2*m2**2*m3**2 - 4D0*m2**4*m3**2 + 8D0*m1**2*m3**4 + &
!     & 8D0*m2**2*m3**4 - 4D0*m3**6 )
! Ossv23 = IFin*(2D0*m1**2 - 2D0*m2**2 - 4D0*m3**2) - 8D0*Iu2d3Fin*m3**2
! Ossv33 = IFin*m3**2

! Osvv11 = -2D0*IFin*m1**2 + 2D0*Iu1d2Fin*m1**2 + 2D0*Id2Fin*m1**4 + 4D0*IFin*m2**2 + 6D0*Iu1d2Fin*m2**2 + &
!     & 4D0*Iu11d22Fin*m2**2 - 2D0*I22*m1**4*m2**2 - 2D0*Id2Fin*m2**4 + 4D0*I22*m1**2*m2**4 - 2D0*I22*m2**6 + 4D0*m3**2 - &
!     & 2D0*Iu1d2Fin*m3**2 - 4D0*Id2Fin*m1**2*m3**2 + 4D0*I22*m1**2*m2**2*m3**2 - 20D0*I22*m2**4*m3**2 + 2D0*Id2Fin*m3**4 - &
!     & 2D0*I22*m2**2*m3**4
! Osvv12 = -8D0*IFin*m1**2 - 2D0*Iu3d2Fin*m1**2 - 2D0*Iu2d3Fin*m1**2 + 4D0*Id2Fin*m1**4 + 4D0*Id3Fin*m1**4 - 2D0*I23*m1**6 &
!     & + 4D0*IFin*m2**2 - 6D0*Iu3d2Fin*m2**2 + 2D0*Iu2d3Fin*m2**2 - 4D0*Id2Fin*m1**2*m2**2 - 8D0*Id3Fin*m1**2*m2**2 + &
!     & 6D0*I23*m1**4*m2**2 + 4D0*Id3Fin*m2**4 - 6D0*I23*m1**2*m2**4 + 2D0*I23*m2**6 + 4D0*IFin*m3**2 + 2D0*Iu3d2Fin*m2**2 - &
!     & 6D0*Iu2d3Fin*m3**2 - 8D0*Id2Fin*m1**2*m3**2 - 4D0*Id3Fin*m1**2*m3**2 + 6D0*I23*m1**4*m3**2 + 20D0*Id2Fin*m2**2*m3**2 &
!     & + 20D0*Id3Fin*m2**2*m3**2 - 28D0*I23*m1**2*m2**2*m3**2 + 22D0*I23*m2**4*m3**2 + 4D0*Id2Fin*m3**4 - &
!     & 6D0*I23*m1**2*m3**4 + 22D0*I23*m2**2*m3**4 + 2D0*I23*m3**6
! Osvv22 = -4D0*IFin*m1**2 - 2D0*Iu2d3Fin*m1**2 + 2D0*Id3Fin*m1**4 + 6D0*IFin*m2**2 + 2D0*Iu2d3Fin*m2**2 - &
!     & 4D0*Id3Fin*m1**2*m2**2 + 2D0*Id3Fin*m2**4 + 2D0*IFin*m3**2 + 2D0*Iu2d3Fin*m2**2 + 4D0*Iu22d33Fin*m3**2 - &
!     & 2D0*I33*m1**4*m3**2 + 4D0*I33*m1**2*m2**2*m3**2 - 2D0*I33*m2**4*m3**2 - 2D0*Id3Fin*m3**4 + 4D0*I33*m1**2*m3**4 - &
!     & 20D0*I33*m2**2*m3**4 - 2D0*I33*m3**6

! Osss11 = -4D0*Id2Fin - 8D0*I22*m2**2
! Osss12 = 4D0*Id2Fin + 4D0*Id3Fin + I23*(-8D0*m1**2 + 8D0*m2**2 + 8D0*m3**2)
! Osss22 = -4D0*Id3Fin - 8D0*I33*m3**2

 OLLFull = OLL00 - 1D0/3D0*OLL01 + 1D0/9D0*OLL11 + 2D0/3D0*OLL02 - 2D0/9D0*OLL12 + 4D0/9D0*OLL22
 OLRFull = OLR00 - 1D0/3D0*OLR01 + 1D0/9D0*OLR11 + 2D0/3D0*OLR02 - 2D0/9D0*OLR12 + 4D0/9D0*OLR22

! Qssv0 = 0D0
! Qssv1 = 0D0
! Qssv2 = 0D0

! OssvFull = Qssv0*Qssv0*Ossv00 + Qssv0*Qssv1*Ossv01 + Qssv1*Qssv1*Ossv11 + Qssv0*Qssv2*Ossv02 + Qssv1*Qssv2*Ossv12 + &
!    & Qssv2*Qssv2*Ossv22 + Qssv0*(Qssv0 + Qssv1)*Ossv03 + Qssv1*(Qssv0 + Qssv1)*Ossv13 + Qssv2*(Qssv0 + Qssv1)*Ossv23 + &
!    & (Qssv0 + Qssv1)**2*Ossv33

! Qsvv1 = 0D0
! Qsvv2 = 0D0

! QsvvFull = Qsvv1*Qsvv1*Osvv11 + Qsvv1*Qsvv2*Osvv12 + Qsvv2*Qsvv2*Osvv22

! Qsss1 = 0D0
! Qsss2 = 0D0

! OsssFull = Qsss1*Qsss1*Osss11 + Qsss1*Qsss2*Osss12 + Qsss2*Qsss2*Osss22

 totalAmplitude = 3D0*EL2*CKM21**2/(2D0*MW2*SW2)*(EL2/(2D0*(4D0*PI)**3*m1))*&
                &(16D0*PI*m1**3)/DSQRT(m1**4 + m2**4 + m3**4 - 2D0*m1**2*m2**2 - 2D0*m1**2*m3**2 - 2D0*m2**2*m3**2 )*&
                &( (m3**2/TB2 + m2**2*YukS3Quark2**2)*( OLLFull ) - 2D0*m2*m3*YukS3Quark2/TB*( OLRFull ) )

 HptoCDBarReal = totalAmplitude
end function HptoCDBarReal
