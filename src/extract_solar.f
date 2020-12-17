C     path:      $Source$
C     author:    $Author: miacono $
C     revision:  $Revision: 32419 $
C     created:   $Date: 2018-07-12 15:43:03 -0400 (Thu, 12 Jul 2018) $
C
      program extract_solar
C
C  --------------------------------------------------------------------------
C |                                                                          |
C |  Copyright 2002, 2003, Atmospheric & Environmental Research, Inc. (AER). |
C |  This software may be used, copied, or redistributed as long as it is    |
C |  not sold and this copyright notice is reproduced on each copy made.     |
C |  This model is provided as is without any express or implied warranties. |
C |                       (http://www.rtweb.aer.com/)                        |
C |                                                                          |
C  --------------------------------------------------------------------------
C
c     Extracts solar source function, as provided by Kurucz

C     This modified version can handle the new AER solar source function
C     derived from combining high spectral spectral variability from  data
C     from Toon and Kurucz  with  coarser measurement/model data from NRLSSI2
C     NRLSSI2 data also contains FB and SB terms. Three different solar functions
C     can be read in:
C     - Kurucz
C     - average NRLSSI2, calculated with average FB and SB values
C     - a file containing separate values for the quiet, FB and SB terms,
C     which the user can combine as desired in LBLRTM.
C     
C     In all cases, the solar source file should be linked to solar.bin
C     The appropriate flags also need to be set (cflag and cflag_form)

c     ------------------------------------------------------------------
c     ------- VARIABLE DECLARATIONS -------

      implicit real*8 (v)

      real*8 sol,sol_int,sol_int_hd,solrad_tot,sol_int_out
      real*8 sol_fb, sol_sb

      character*8 xid,   hmol,yid 
      real*8          sec

! mji - Parameter nsol increased to accommodate extension of solar source files to 86500 cm-1.
!      parameter (nblock=4000,nsol=4000000)
      parameter (nblock=4000,nsol=8640000)

      dimension sol(nblock),xdum(1)
      dimension sol_fb(nblock),sol_sb(nblock)
       
      dimension sol_temp(nsol,3),vgnu_temp(nsol)
      dimension solrad(2410,3)

      character*80 xdum
      character*60 solar_file
      character*1 cdum(80)
      character*10 cdate, ctime, cdate_hd, ctime_hd
      character*300 cfilhd
      character*7 cflag,cflag_form
      character*80 cfile_out
      character*80 HVR_EXT_SOL

      COMMON /FILHDR/ XID(10),SEC,P0,T0,HMOL(64),W(64),WBROAD,DVT,V1V,
     1 V2V,TBOUND,EMISIV,FSCDID(17),NMOL,NLAYER,
     C            YID1,YID(10)   ,LSTW1 
 
      common /pnlhdr/ vgnu_1,vgnu_2,dvout,nlimo

      DIMENSION FILHDR(2),PNLHDR(2),IW1(2),IW2(2) 

      equivalence (xdum,cdum(1))

      EQUIVALENCE (FILHDR(1),XID(1)), (PNLHDR(1),vgnu_1),
     X            (IW1(1),XID(1)),    (IW2(1),vgnu_1),
     1 (FSCDID(1),IHIRAC),(FSCDID(2),ILBLF4),(FSCDID(3),ICNTNM),
     2 (FSCDID(4),IAERSL),(FSCDID(5),IEMIT ),(FSCDID(6),ISCAN ),
     3 (FSCDID(7),IPLOT ),(FSCDID(8),IPATHL),
     C (FSCDID(9),JRAD),(FSCDID(10),ITEST),(FSCDID(11),IMRG),
     C (FSCDID(12),SCNID),(FSCDID(13),HWHM ),(FSCDID(14),IDABS),
     C (FSCDID(15),IATM),(FSCDID(16),LAYR1 ),(FSCDID(17),NLAYFS)
C
c     ------------------------------------------------------------------
c     ------- SET CONSTANTS -------

      HVR_EXT_SOL = '@(#) $Revision: 32419 $'

      vend = -987654321.
      nend = -987654321

      input = 44
      iread = 55
      iout = 66

c     ------------------------------------------------------------------
c     ------- INITIALIZE VARIABLES -------

      scale_fac = 1.0
      solrad_tot = 0.0
      sol_int_out = 0.0
      sol_min_out = 99999999.
      sol_max_out = 0.0
c
c     ------------------------------------------------------------------
c     ------- OPEN & READ CONTROL FILE -------

      open(iread,file='SOLAR_control',form='formatted',status='old')

c     Read past comment line

      read(iread,900) xdum

c     Read output control flag
c          "extract" extract the data, with no interpolation
c          "ext_nrl": extract from average NRLSSI2 file, which has a uniform grid
c          "extnrl3": extract from  NRLSSI2 file containing all three components,
c                     which will also be output               
c          "interpl" interpolate the data to a given grid
c          "int_nrl": interpolate from average NRLSSI2 file, which has a uniform grid
c          "intnrl3": interpolate from  NRLSSI2 file containing all three components,
c                     which will also be output               
c
      read(iread,900) xdum
      nchar = 7
      call strip_space(cdum,cflag,nchar)

      write(*,*) 'cflag = ',cflag

c     Read output format control flag
c          "ascii2" means an ascii file with frequency and solar source value
c                   per record (two columns) - Kurucz format
c          "ascii1" means an ascii file with one solar source value per record
c                   (one column)
c          "asciib" means an ascii file blocked in 2400 point blocks
c          "binary" means an LBLRTM binary file for cflag = 'intxxxx'
c                   and a solar binary file for cflag = 'extxxxx'
c
      read(iread,900) xdum
      nchar = 7
      call strip_space(cdum,cflag_form,nchar)

      write(*,*) 'cflag_form = ',cflag_form

c     Read output file name
c
      read(iread,900) xdum
      nchar = 80
      call strip_space(cdum,cfile_out,nchar)

      write(*,*) 'cfile_out = ',cfile_out

c     Open output file

      if (cflag_form.eq.'binary') then
         open(iout,file=cfile_out,form='unformatted',
     *        status='unknown')
      else
         open(iout,file=cfile_out,form='formatted',
     *        status='unknown')
      endif


c     Read vmin, vmax, scale_fac, and sun_dist for cflag = "extract"

      if (cflag.eq.'extract' .OR. cflag.eq.'ext_nrl' .OR. 
     *    cflag.eq.'ext3nrl') then
         read(iread,*) vmin,vmax,scale_fac
c         read(iread,*) vmin,vmax,scale_fac,sun_dist
c        Initialize variable for sol_temp array
         idone_temp = 0


c     Read vmin_save, vmax_save, dvout, scale_fac, and sun_dist
c     for cflag = "intrpl"

      else

c         read(iread,*) vmin_save,vmax_save,dvout,scale_fac,sun_dist
         read(iread,*) vmin_save,vmax_save,dvout,scale_fac
         npts_out = nint((vmax_save-vmin_save)/dvout) + 1
         nleft = npts_out

c        open temporary binary file

         open(13,status='scratch',form='unformatted')

c        Find word lengths

         lstw1 = -654321
         nfhdrf=nwdl(iw1,lstw1)
         nlimo = -654321
         nphdrf=nwdl(iw2,nlimo)
         write(*,*) 'nfhdrf, npfdrf = ',nfhdrf,nphdrf
         nphdrf = 6

c        set default values for filhdr

         call filset

c        write temporary file header

         call bufout(13,filhdr(1),nfhdrf)

c        Initialize variable for sol_temp array
         idone_temp = 0
        
      endif

c     Set scale_fac to default value if input file entry is zero or interpolation and binary output are selected

      if (scale_fac.eq.0.) scale_fac = 1.0

      if ((cflag_form.eq.'binary').AND.(cflag.eq.'interpl'.OR.
     *    cflag.eq.'int_nrl'.OR.cflag.eq.'int3nrl')) then
         scale_flag = 1.0
      endif

c     ------------------------------------------------------------------
c     ------- OPEN SOLAR SOURCE FUNCTION FILE -------
      
      if (cflag.eq.'extract'.OR.cflag.eq.'interpl') then 
         solar_file=
     *   'solar.kurucz.rad.mono.full_disk.bin.little_Endian'
      else if (cflag.eq.'ext_nrl'.OR.cflag.eq.'int_nrl') then
          solar_file='build_comb_solar_rad_avg.bin'
      else if (cflag.eq.'ext3nrl'.OR.cflag.eq.'int3nrl') then
          solar_file='build_comb_solar_rad_multi_comp.bin'
      endif
      open(input,file=solar_file,form='unformatted',status='old')

c     Read in file header info:
c          cdate = date file was made
c          ctime = time file was made
c          vgnu_min = minimum wavenumber
c          vgnu_max = maximum wavenumber
c          vc1_file = constant used to determine dv
c          npts_tot = total number of points
c          sol_min = minimum value
c          sol_max = maximum value
c          sol_int = integrated value
c          


c     ------------------------------------------------------------------
c     ------- READ IN SOLAR SOURCE FUNCTION FILE AND SELECT -------
c     -------------------- APPROPRIATE DATA -----------------------


      read(input) cfilhd
c     read(cfilhd,890) cdate_hd,ctime_hd,vgnu_min_hd,vgnu_max_hd,
c    *     vc1_hd,npts_tot_hd,
c    *     sol_min_hd,sol_max_hd,sol_int_hd

      read(input) cdate,ctime,vgnu_min,vgnu_max,vc1_file,npts_tot,
     *     sol_min,sol_max,sol_int

      write(*,*) cdate,ctime,vgnu_min,vgnu_max,vc1_file,npts_tot,
     *     sol_min,sol_max,sol_int



c     Calculate vmin, vmax for the interpl option to ensure accurate 4
c     point interpolation at endpoints.

      if (cflag.eq.'interpl' .OR. cflag.eq.'int_nrl' 
     *    .OR. cflag.eq.'int3nrl') then

         if (cflag.eq.'interpl') then
	    dvfirst = vmin_save*vc1_file*1.e-06
	    dvlast = vmax_save*vc1_file*1.e-06
         else
	    dvfirst = vc1_file
	    dvlast =  vc1_file
         endif
 

         vmin = vmin_save-3*dvfirst
         vmax = vmax_save+3*dvlast
      endif

c     Read values in blocks of size nblock and output according to flags

c     ------------------------------------------------------------------
c     ------- GET SPECTRUM AND OUTPUT AS SELECTED -------

c     ~~~~~~~~~~~~~~~~
c     "EXTRACT" Option
c     ~~~~~~~~~~~~~~~~

      if (cflag.eq.'extract' .OR. cflag.eq.'ext_nrl' .OR. 
     *     cflag.eq.'ext3nrl') then


 10      continue

         read(input) v1,v2,vc1,nlim

         if (v1.eq.vend) then
            write(*,*) 
     *       'End of file encountered before reaching VMAX = ',
     *       vmax
            goto 888
         endif

c        Skip to selected spectral region

         if (v2.lt.vmin) then
            read(input) cdum(1)
            goto 10
         endif

         read(input) (sol(i),i=1,nlim)
         if (cflag.eq.'ext3nrl') then 
	    read(input) (sol_fb(i),i=1,nlim)
	    read(input) (sol_sb(i),i=1,nlim)
         endif

         istart = 1
         iend = nlim
         vgnu_start = vmin
         vgnu_end = vmax
         if ((v1.le.vmin).and.(v2.ge.vmin)) then
            if (cflag.eq.'extract') then 
	       istart = ((dlog(vmin/v1))/(vc1*1.e-06))+1.001
	       vgnu_start = v1*dexp(vc1*(float(istart-1)*1.e-06))
            else if (cflag.eq.'ext_nrl' .OR. cflag.eq.'ext3nrl') then
               istart = (vmin-v1)/vc1+1.0  
               vgnu_start = v1+(istart-1)*vc1 
            endif
            write(*,*) 'Extract: STARTING point = ',vgnu_start
         endif
         if ((v1.le.vmax).and.(v2.ge.vmax)) then
            if (cflag.eq.'extract') then 
	       iend = ((dlog(vmax/v1))/(vc1*1.e-06))+1.001
	       vgnu_end = v1*dexp(vc1*(float(iend-1)*1.e-06))
            else if (cflag.eq.'ext_nrl' .OR. cflag.eq.'ext3nrl') then
               iend = (vmax-v1)/vc1+1
               vgnu_end = v1+(iend-1)*vc1 
            endif
            write(*,*) 'Extract: ENDING point = ',vgnu_end
         endif


c        Load into sol_temp array, for output as selected

         do 50 itemp = istart,iend
            ilocation = (itemp-istart+1)+idone_temp
            sol_temp(ilocation,1) = sol(itemp)*scale_fac
            if (cflag.eq.'ext3nrl') then
	       sol_temp(ilocation,2) = sol_fb(itemp)*scale_fac
	       sol_temp(ilocation,3) = sol_sb(itemp)*scale_fac
            endif
            if (cflag .eq. 'extract') then
	       vgnu_temp(ilocation) = 
     *           v1*dexp(vc1*(float(itemp-1)*1.e-06))
            else if (cflag .eq. 'ext_nrl' .OR. cflag.eq.'ext3nrl') then
               vgnu_temp(ilocation) = v1+vc1*(float(itemp-1)) 
            endif

C Note that sol_int only includes the first NRLSSI2 term (the "quiet" term).
            sol_min_out = min(sol_min_out,sol_temp(ilocation,1))
            sol_max_out = max(sol_max_out,sol_temp(ilocation,1))
 50      continue

         idone_temp = idone_temp+(iend-istart+1)

c        Get more points, if needed

         if (v2.lt.vmax) goto 10


c     Integrate function

         do 80 i=1,idone_temp

            if (i.eq.1) then
               dv_int = vgnu_temp(i+1)-vgnu_temp(i)
            else if (i.eq.idone_temp) then
               dv_int = vgnu_temp(i)-vgnu_temp(i-1)
            else
               dv_int = 0.5*(vgnu_temp(i+1)-vgnu_temp(i-1))
            endif
            sol_int_out = sol_int_out + sol_temp(i,1)*dv_int
c            write(86,*) i,dv_int,sol_temp(i)
 80      continue


         write(*,*) 'Extract: sol_int_out = ',sol_int_out

c        Output extracted data


         if (cflag_form.eq.'ascii2') then
            write(iout,*) vgnu_start,vgnu_end,vc1,idone_temp,
     *           sol_min_out,sol_max_out,sol_int_out
            do 100 ientry=1,idone_temp
               if (cflag.ne.'ext3nrl') then 
                  write(iout,910) vgnu_temp(ientry),sol_temp(ientry,1)
               else
                  write(iout,910) vgnu_temp(ientry),sol_temp(ientry,1),
     *              sol_temp(ientry,2),sol_temp(ientry,3)
               endif
 100        continue
         else if (cflag_form.eq.'ascii1') then
            write(iout,*) vgnu_start,vgnu_end,vc1,idone_temp,
     *           sol_min_out,sol_max_out,sol_int_out
            do 105 ientry=1,idone_temp
               if (cflag.ne.'ext3nrl') then 
                  write(iout,915) sol_temp(ientry,1)
               else 
                  write(iout,915) sol_temp(ientry,1),  
     *              sol_temp(ientry,2),sol_temp(ientry,3)
               endif 
 105        continue
         else if (cflag_form.eq.'asciib') then
            write(iout,*) vgnu_start,vgnu_end,vc1,idone_temp,
     *           sol_min_out,sol_max_out,sol_int_out
            write(iout,960) (sol_temp(i,1),i=1,idone_temp)
            if (cflag.eq.'ext3nrl') then
               do ic=2,3
                  write(iout,960) (sol_temp(i,ic),i=1,idone_temp)
               end do
            endif
         else
            write(cfilhd,890) cdate,ctime,vgnu_start,vgnu_end,vc1,
     *           idone_temp,sol_min_out,sol_max_out,sol_int_out
            write(iout) cfilhd
            write(iout) cdate,ctime,vgnu_start,vgnu_end,vc1,idone_temp,
     *           sol_min_out,sol_max_out,sol_int_out

c           Write out in binary in blocks of size nblock

            nstart = 1
            nend = nblock
            nleft = idone_temp

 110        continue
            nlim = nend-nstart+1
            v1 = vgnu_temp(nstart)
            v2 = vgnu_temp(nend)
            write(iout) v1,v2,vc1,nlim
            write(iout) (sol_temp(i,1),i=nstart,nend)
            if (cflag.eq.'ext3nrl') then
               do ic=2,3
                  write(iout) (sol_temp(i,ic),i=nstart,nend)
               end do
             endif

            if (nend.lt.npts_tot) then
               nleft = npts_tot-nend
               nstart = nend+1
               nend = nend+min(nblock,nleft)
               goto 110
            endif

            write(iout) vend,vend,vend,nend
         endif


c     ~~~~~~~~~~~~~~~~
c     "INTERPL" Option
c     ~~~~~~~~~~~~~~~~
      else

 115     continue

         read(input) v1,v2,vc1,nlim

         if (v1.eq.vend) then
            write(*,*) 
     *       'End of file encountered before reaching VMAX = ',
     *       vmax
            goto 777
         endif

c        Skip to selected spectral region

         if (v2.lt.vmin) then
            read(input) cdum(1)
	    if (cflag.eq.'int3nrl') then
	       read(input) cdum(1)
	       read(input) cdum(1)
            endif
            goto 115
         endif

         read(input) (sol(i),i=1,nlim)
         if (cflag.eq.'int3nrl') then
	    read(input) (sol_fb(i),i=1,nlim)
	    read(input) (sol_sb(i),i=1,nlim)
         endif


         istart = 1
         iend = nlim
         if ((v1.lt.vmin).and.(v2.gt.vmin)) then
            if (cflag.eq.'interpl') then 
	       istart = ((dlog(vmin/v1))/(vc1*1.e-06))+1.001
	       vgnu_start = v1*dexp(vc1*(float(istart-1)*1.e-06))
            else if (cflag.eq.'int_nrl' .OR. cflag.eq.'int3nrl') then
               istart = nint((vmin-v1)/vc1)+1  
               vgnu_start = v1+(istart-1)*vc1 
            endif
            write(*,*) 'Interp STARTING point = ',vgnu_start
         endif
         if ((v1.lt.vmax).and.(v2.gt.vmax)) then
            if (cflag.eq.'interpl') then 
	       iend = ((dlog(vmax/v1))/(vc1*1.e-06))+1.001
	       vgnu_end = v1*dexp(vc1*(float(iend-1)*1.e-06))
            else if (cflag.eq.'int_nrl' .OR. cflag.eq.'int3nrl') then
               iend = nint((vmax-v1)/vc1)+1
               vgnu_end = v1+(iend-1)*vc1 
            endif
            write(*,*) 'Interp ENDING point = ',vgnu_end
         endif

c        Interpolate and output as selected

c        Load into sol_temp array, for use in interpolation

         do 120 itemp = istart,iend
            sol_temp((itemp-istart+1)+idone_temp,1) = sol(itemp)
            if (cflag.eq.'int3nrl') then 
	       sol_temp((itemp-istart+1)+idone_temp,2) = sol_fb(itemp)
	       sol_temp((itemp-istart+1)+idone_temp,3) = sol_sb(itemp)
	    endif
 120     continue

         idone_temp = idone_temp+(iend-istart+1)

c        Get more points, if needed

         if (v2.lt.vmax) goto 115


 777     continue

c     Set up interpolation loop


         write(*,*) '******* INTERPOLATION PART *********'

C
C     Determine the first point to be used.
C     It should be among between points 2-5 that we have extracted
C     using vmin.

         do 780 itest=2,5
            if (cflag.eq.'interpl') then 
	       vtest = vgnu_start*dexp(vc1*(float(itest-1)*1.e-06))
            else if (cflag.eq.'int_nrl' .OR. cflag.eq.'int3nrl') then 
               vtest = vgnu_start+vc1*float(itest-1)
            endif
            write(*,*) 'vtest = ',vtest
            if (vmin_save.le.vtest) then
               j = itest-1
               vmin_out = vmin_save
               goto 790
            endif
 780     continue
             
c        If this line is reached, then we couldn't find the first point

         write(*,*) 'First point not found: VMIN, VTEST = ',
     *        vmin_save,vtest
         stop


 790  continue

c     ------------------------------------------------------------------
c     ------- LAGRANGE 4 POINT INTERPOLATION SECTION -------

         nlimo = min(2400,nleft)

         n_increment = npts_out - nleft

         do 800 i = 1,nlimo
            vgnu_i = vmin_out+(i-1+n_increment)*dvout

c           Determine the DV for a given set of four points.
C           IMPORTANT NOTE: This assumes a constant DV among the four
C               points, which is not correct, but the difference in DV
c               is sufficiently small to make this assumption.


 795        continue

            if (cflag.eq.'interpl') then 
	       vgnu_j = vgnu_start*dexp(vc1*(float(j-1)*1.e-06))
	       dv_temp = vgnu_j*vc1*1.e-6
            else if (cflag.eq.'int_nrl' .OR. cflag.eq.'int3nrl') then 
               vgnu_j = vgnu_start+vc1*float(j-1)
               dv_temp = vc1
            endif

c           Test ratio dvout/dv_temp to be certain output dv is not
c           greater than input dv

            if ((dvout/dv_temp).gt.1.) then
               write(*,*) 'dvout is greater than input solar dv: ',
     *              dvout,dv_temp
               stop
            endif
            recdv_temp = 1./dv_temp
            p = recdv_temp*(vgnu_i-vgnu_j)

c           When reaching P ge 1., move to the next point in the array,
c           and redetermine the DV and P.

            if (p.ge.1.) then
               j = j+1
               goto 795
            endif


c           Calculate coefficients and interpolate

            C = (3.-2.*P)*P*P
            B = 0.5*P*(1.-P)
            B1 = B*(1.-P)
            B2 = B*P
            solrad(i,1) = scale_fac*(-sol_temp(J-1,1)*B1+sol_temp(J,1)*
     *           (1.-C+B2)+sol_temp(J+1,1)*(C+B1)-sol_temp(J+2,1)*B2)
            if (cflag.eq.'int3nrl') then
               do ic=2,3 
                  solrad(i,ic) = scale_fac*(-sol_temp(J-1,ic)*B1+     
     *                   sol_temp(J,ic)* (1.-C+B2)      
     *                 +sol_temp(J+1,ic)*(C+B1)-sol_temp(J+2,ic)*B2)
               end do
            endif

            sol_min_out = min(sol_min_out,solrad(i,1))
            sol_max_out = max(sol_max_out,solrad(i,1))
            solrad_tot = solrad_tot+dble(solrad(i,1))
c            write(88,*) scale_fac,sol_temp(j-1),sol_temp(j),
c     *           sol_temp(j+1),sol_temp(j+2),b1,b2,c
c            write(87,*) i,dvout,solrad(i)

c            write(89,*) vgnu_i,solrad(i)

 800     continue
c
c        Write out in binary temp file

         vgnu_1 = vmin_out+(n_increment)*dvout
         vgnu_2 = vmin_out+(nlimo-1+n_increment)*dvout

         call bufout(13,pnlhdr(1),nphdrf)
         call bufout(13,solrad(1,1),nlimo)
         if (cflag.eq.'int3nrl') then
            call bufout(13,solrad(1,2),nlimo)
            call bufout(13,solrad(1,3),nlimo)
         endif
         

         nleft = nleft - nlimo

c        Return for another 2400 points

         if (nleft.gt.0) goto 790

c        Calculate integral

         sol_int_out = solrad_tot*dble(dvout)

         write(*,*) 'Interp: sol_int_out = ',sol_int_out

c        Put end of file flag on temp file

         call endfil(13)

         rewind(13)

         
c        Copy info from scratch file to output file, including appropriate
c        head info.


         call bufin(13,ieof,filhdr,nfhdrf)

         v1v = vmin_out
         v2v = vgnu_2
         dvt = dvout
         n_out = nint((v2v-v1v)/dvt)+1

         if (cflag_form.eq.'ascii2') then
            write(iout,*) v1v,v2v,dvt,n_out,
     *           sol_min_out,sol_max_out,sol_int_out
         else if (cflag_form.eq.'ascii1') then
            write(iout,*) v1v,v2v,dvt,n_out,
     *           sol_min_out,sol_max_out,sol_int_out
         else if (cflag_form.eq.'asciib') then
            write(iout,*) v1v,v2v,dvt,n_out,
     *           sol_min_out,sol_max_out,sol_int_out
         else
            write(xdum,920) sol_min_out,sol_max_out,sol_int_out
            read(xdum,930) xid
            call bufout(iout,filhdr(1),nfhdrf)
         endif

 810     continue


         call bufin(13,ieof,pnlhdr,nphdrf)
         if (ieof.le.0) then
            goto 888
         endif

         call bufin(13,ieof,solrad(1,1),nlimo)
         if (cflag.eq.'int3nrl') then
            do ic=2,3
               call bufin(13,ieof,solrad(1,ic),nlimo)
            end do
         endif



         if (cflag_form.eq.'ascii2') then
            do 820 i=1,nlimo
               if (cflag.eq.'interpl'.OR.cflag.eq.'int_nrl') 
     *          write(iout,940) vgnu_1+(i-1)*dvout,solrad(i,1)
               if (cflag.eq.'int3nrl') then
                 write(iout,940) vgnu_1+(i-1)*dvout,
     *            (solrad(i,ic),ic=1,3)
               endif
 820        continue
         else if (cflag_form.eq.'ascii1') then
            do 825 i=1,nlimo
               write(iout,950) solrad(i,1)
 825        continue
            if (cflag.eq.'int3nrl') then
                do ic=2,3
                   do i=1,nlimo
                     write(iout,950) solrad(i,ic)
                  end do
               end do
            endif
         else if (cflag_form.eq.'asciib') then
            write(iout,960) solrad(1:nlimo,1)
            if (cflag.eq.'int3nrl') then
                do ic=2,3
		  write(iout,960) (solrad(1:nlimo,ic),i=1,nlimo)
               end do
            endif
         else
            call bufout(iout,pnlhdr(1),nphdrf)
            call bufout(iout,solrad(1,1),nlimo)
            if (cflag.eq.'int3nrl') then
                do ic=2,3
                   call bufout(iout,solrad(1,ic),nlimo)
                end do
            endif
         endif


         goto 810


      endif


c     End of solar file

 888  continue
      if (cflag_form.eq.'binary'.AND.(cflag.eq.'int_nrl'.OR.
     *   cflag.eq.'int3nrl')) then
          call endfil(iout)
      endif
             
      close(iread)
      close(input)
      close(iout)


 890  format(2(a8,2x),3(f15.7,2x),i7,2x,3(e12.5))
 900  format(a80)
 910  format(f13.7,1x,1p,3e15.5,0p)
 915  format(1p,3e15.3,0p)
 920  format(3(e12.5,2x),38x)
 930  format(10a8)
 940  format(f13.7,1p,3e14.5,0p)
 950  format(1p,3e14.5,0p)
 960  format(80(1p,3e14.5,0p))
      end

c     ---------------------------------------------------

      subroutine strip_space(cdum,cflag,nchar)

c     Interprets input for extraction/interpolation of data

      character*1 cdum(80),cspace
      character*1 cflag(*)

      cspace = ' ' 

      do 10 i= 1,nchar
         cflag(i)=cspace
 10   continue

      do 100 i=1,80
         jlo=i
         if (cdum(i).ne.cspace) goto 110
 100  continue

 110  do 120 i=jlo,80
         jhi=i
         if (cdum(i).eq.cspace) goto 130
 120  continue
      
 130  continue

      jhi=jhi-1

      nchar=jhi-jlo+1

      do 140 j=jlo,jhi
         cflag(j-jlo+1)=cdum(j)
 140  continue

      return

      end

         
      subroutine filset
c
c     this subroutine sets up the filhdr common block
c
      implicit real*8 (v)
      character*8 xid,   hmol,yid 
      real*8          sec
      common /filhdr/ xid(10),sec,p0,t0,hmol(64),w(64),wbroad,dvt,v1v,
     1 v2v,tbound,emisiv,fscdid(17),nmol,nlayer,
     c            yid1,yid(10)   ,lstw1 
      equivalence         (fscdid(1),ihirac),(fscdid(2),ilblf4),
     2 (fscdid(3),icntnm),(fscdid(4),iaersl),(fscdid(5),iemit ),
     3 (fscdid(6),iscan ),(fscdid(7),iplot ),(fscdid(8),ipathl),
     c (fscdid(9),jrad),(fscdid(10),itest),(fscdid(11),imrg),
     c (fscdid(12),scnid),(fscdid(13),hwhm ),(fscdid(14),idabs),
     c (fscdid(15),iatm),(fscdid(16),layr1 ),(fscdid(17),nlayfs)
c
c
      ihirac=1
      ilblf4=1
      icntnm=1
      iaersl=0
      iemit=1
      iscan=1
      iplot=0
      ipathl=0
      jrad=0
      itest=0
      imrg=0
      scnid=float(iscan)*100. + .01
      idabs=0
      iatm=0
      layr1=0
      nlayfs=0
      sec=0
      p0=0
      t0=0
      wbroad=0
      tbound=0
      emisiv=0
      nmol=1
      nlayer=1
      yid1=0
c
      return
      end 




      block data init
c
c     this subroutine sets up the hmol, xid, yid, and w variables
c
      implicit real*8 (v)
      character*8 xid,   hmol,yid 
      real*8          sec
      common /filhdr/ xid(10),sec,p0,t0,hmol(64),w(64),wbroad,dvt,v1v,
     1 v2v,tbound,emisiv,fscdid(17),nmol,nlayer,
     c            yid1,yid(10)   ,lstw1 


      data hmol  /'  h2o ','  co2 ','   o3 ','  n2o ','   co ',
     c  '  ch4 ','   o2 ',57* '      ' /
      data xid /'        ','        ','        ','        ','        ',
     1          '        ','        ','        ','        ','        '/
      data yid(1)/ '        '/, yid(2)/ '        ' /
      data w/64*0./ 

      end

      function nwdl(iwd,ilast)

      dimension iwd(*)
      ilast=-654321
      do 10 i=1,900
      if(iwd(i).ne.ilast) go to 10
      nwdl=i-1
      go to 12
10    continue
12    return

      end


      SUBROUTINE BUFIN (IFILE,IEOF,IARRAY,IWORDS)
C
C     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING
C     AT LOCATION IARRAY
C
C     IFILE IS THE FILE DESIGNATION
C                                  
      DIMENSION IARRAY(IWORDS)
C                          
      IEOF = 1             
C                          
C#    BUFFER IN (IFILE,1) (IARRAY(ILO),IARRAY(IHI))
C#    IF (UNIT(IFILE).EQ.0.) GO TO 10              
C                                               
      READ (IFILE,END=10) IARRAY
      ITEST = MIN(IWORDS,4)                 
      IF (IARRAY(ITEST).EQ.-99) IEOF = -99      
C                                               
      RETURN                                    
C                                               
   10 IEOF = 0                                  
C                                               
      RETURN                                    
C                                               
      END                                       
      SUBROUTINE BUFOUT (IFILE,IARRAY,IWORDS)
C                                                 
C     THIS SUBROUTINE BUFFERS OUT (WRITES) IWORDS FROM IARRAY STARTING
C     AT LOCATION IARRAY                                                 
C                                                                     
C     IFILE IS THE FILE DESIGNATION                                   
C                                                                     
      DIMENSION IARRAY(IWORDS)
C                                                   
C#    BUFFER OUT (IFILE,1) (IARRAY(ILO),IARRAY(IHI))
C#    IF (UNIT(IFILE).EQ.0.) STOP ' ERROR IN BUFOUT '
C                                                    
      WRITE (IFILE) IARRAY
C                                                    
      RETURN                                         
C                                                    
      END                                            


      SUBROUTINE ENDFIL(IFILE)                                              5160      6770
      DIMENSION IDUM(6)                                                     5170      6780
      DATA IDUM /6*-99/                                                     5180      6790
      CALL BUFOUT(IFILE,IDUM(1),6)                                          5190      6800
      RETURN                                                                5200      6810
      END                                                                   5210      6820






