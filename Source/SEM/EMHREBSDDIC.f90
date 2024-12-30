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

program EMHREBSDDIC
  !! author: MDG
  !! version: 1.0 
  !! date: 06/28/23
  !!
  !! f90 version of the DIC implementation by Ernould et al.

use mod_kinds
use mod_global
use mod_EMsoft
use mod_HREBSDDIC
use mod_HDFnames
use stringconstants

IMPLICIT NONE

character(fnlen)                :: progname = 'EMHREBSDDIC.f90'
character(fnlen)                :: progdesc = 'Implementation of the HR-EBSD-DIC algorithm'

type(EMsoft_T)                  :: EMsoft
type(HREBSD_DIC_T)              :: HREBSDDIC 
type(HDFnames_T)                :: HDFnames

! print the EMsoft header and handle any command line arguments  
EMsoft = EMsoft_T( progname, progdesc, tpl = (/ 289 /) )

! deal with the namelist stuff
HREBSDDIC = HREBSD_DIC_T(EMsoft%nmldeffile)

HDFnames = HDFnames_T() 
call HDFnames%set_ProgramData(SC_HREBSDDIC) 
call HDFnames%set_NMLlist(SC_HREBSDDICNameList) 
call HDFnames%set_NMLfilename(SC_HREBSDDICNML) 
call HDFnames%set_Variable(SC_HREBSDDIC) 

! perform the computations
call HREBSDDIC%HREBSD_DIC(EMsoft, progname, HDFnames)

end program EMHREBSDDIC
