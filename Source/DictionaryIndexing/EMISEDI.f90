! ###################################################################
! Copyright (c) 2016-2025, Marc De Graef Research Group/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are 
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list 
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this 
!        list of conditions and the following disclaimer in the documentation and/or 
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
!        of its contributors may be used to endorse or promote products derived from 
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################

program EMISEDI
  !! author: MDG
  !! version: 1.0 
  !! date: 02/26/23
  !!
  !! Indexing of ISE tilt series image intensities
  !!

use mod_kinds
use mod_global
use mod_EMsoft
use mod_ISEDI

IMPLICIT NONE

character(fnlen)        :: progname
character(fnlen)        :: progdesc = 'Indexing based on ISE tilt series intensities'

type(EMsoft_T)          :: EMsoft
type(ISEDI_T)           :: ISEDI

progname = 'EMISEDI.f90'

! print the EMsoft header and handle any command line arguments  
EMsoft = EMsoft_T( progname, progdesc, tpl = (/ 89 /) )

! deal with the namelist stuff
ISEDI = ISEDI_T(EMsoft%nmldeffile)

! perform the computations
call ISEDI%ISEDI(EMsoft, progname)

end program EMISEDI
