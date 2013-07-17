pro spinmeround, data, x = x, y = y, z = z, ct = ct, rotx = rotx, roty = roty, rotz = rotz, slices = slices, nrots = nrots, delay = delay, theta = theta, norot = norot,  manual = manual, help = help ;user should pick a rotation axis and either an nrot and delay, or manual or norot
   if keyword_set(help) then begin
       print, "This procedure iterates the JENKY3D procedure (also included) while rotating the data represented around a specified axis*. As such, it includes all the options necessary to modify the 3D view (for which, you should look at the help file for JENKY3D, but they are as before: viewing axis /x, /y, and /z; color table ct; number of slices), as well as the axis around which to rotate the data, independent of the viewing axis, /rotx, /roty, and /rotz, and an option for no rotation /norot (in which case the program merely replicates JENKY3D); the number of rotations to perform nrot; the angle [in radians] by which to rotate the data each frame; the delay in tenths of a second for the program to wait after plotting each frame before plotting the next; and an option to controll the rotation by hand, by means of a 'desktop gaming-style' control interface. as shown below:"
       print, ""
       print, "Rotation Direction:   <-   ->    ^  v     /  \        -       +/=""
       print, "Key Command:           A   D     W  S     Q  E    ZOOM OUT  ZOOM IN"
       print, ""
       print, "     * as mentioned in the write-up to JENKY3D, because of the limitations of the oplot function, this plots succesive layers of the out-of-screen axis twoard the viewer, from the back. While this may create a less noticable issue with smaller data sets, it caused a significant delay in tests involving of order 5000 datapoints, and so it is suggested that a noticable delay (or manual) is set for data of this magnitude, in order for the display to finish plotting and be viewed"
       print, ""
       print, ""
       print, "The options for viewing axis and rotation axis are both exclusive - at risk of great confusion, do not call, for example, 'SPINMEROUND, data, /x, /y, /z, ... ' In the case of multiple called rotation axes, x will take precidence, so why bother? Also, obviously, some options, like /norot and /theta should not go together. The calls should therefore have one of the following forms:"
       print, ""
       print, "spinmeround, data, /[viewaxis], /[norot]"
       print, ""
       print, "spinmeround, data, /[viewaxis], /[rotationaxis], /manual"
       print, ""
       print, "spinmeround, data, /[viewaxis], /[rotationaxis], nrots=[number of rotations], delay=[delay], theta=[theta]"
       print, ""
       print, "...and ct and slices are optional input in all cases. Delay and /manual may be used together, but it only gives a delay before you're allowed to tell it to continue."
       print, ""
       print, "If you you gave an incomplete call, or would like to quit, press X; to continue, press ENTER."
       pause
   endif

   if ~keyword_set(ct) then begin
       ct = 1 ;this looks better for a rotating object
   endif
   if ~keyword_set(nrots) then begin
       nrots = 9999
   endif
   if keyword_set(norot) then begin
       jenky3d, data, x=x, y=y, z=z, ct=ct, slices=slices
   endif else begin
      totrot = 0
      datasize = size(data)
      if ~keyword_set(theta) then begin
          theta = !pi/16 ;the program will default to rotating the data by 1/32 of a full revolution every time
      endif

      jenky3d, data, x=x, y=y, z=z, ct=ct, slices=slices, /nooffset

      if keyword_set(x) then begin
          if keyword_set(rotz) then begin
              rotationmatrix = [[cos(theta), sin(theta), 0],[-sin(theta), cos(theta), 0], [0,0,1]]
          endif
          if keyword_set(roty) then begin
              rotationmatrix = [[cos(theta), 0, -sin(theta)],[0,1,0],[sin(theta), 0, cos(theta)]]
          endif
          if keyword_set(rotx) then begin
              rotationmatrix = [[1,0,0],[0, cos(theta), sin(theta)],[0, -sin(theta), cos(theta)]]
          endif
      endif

      if keyword_set(y) then begin
          if keyword_set(rotz) then begin
              rotationmatrix = [[cos(theta), sin(theta), 0],[-sin(theta), cos(theta), 0], [0,0,1]]
          endif
          if keyword_set(rotx) then begin
              rotationmatrix = [[cos(theta), 0, -sin(theta)],[0,1,0],[sin(theta), 0, cos(theta)]]
          endif
          if keyword_set(roty) then begin
              rotationmatrix = [[1,0,0],[0, cos(theta), sin(theta)],[0, -sin(theta), cos(theta)]]
          endif
      endif

      if keyword_set(z) then begin
          if keyword_set(rotz) then begin
              rotationmatrix = [[cos(theta), sin(theta), 0],[-sin(theta), cos(theta), 0], [0,0,1]]
          endif
          if keyword_set(roty) then begin
              rotationmatrix = [[cos(theta), 0, -sin(theta)],[0,1,0],[sin(theta), 0, cos(theta)]]
          endif
          if keyword_set(rotx) then begin
              rotationmatrix = [[1,0,0],[0, cos(theta), sin(theta)],[0, -sin(theta), cos(theta)]]
          endif
      endif

      while totrot LE 2*!pi*nrots do begin
          if keyword_set(manual) then begin
              if keyword_set(x) then begin
                  rotationmatrix = rotpause(theta,  /xaxis)
              endif
              if keyword_set(y) then begin
                  rotationmatrix = rotpause(theta, /yaxis)
              endif
              if keyword_set(z) then begin
                  rotationmatrix = rotpause(theta, /zaxis)
              endif

          endif
       ;   print, data[*,0:10]  ;;;
        ;  stop  ;;;
         ; print, rotationmatrix  ;;;  ok so it thinks that the rotation matrix is zero... hmmm...
         ; stop  ;;;
          rotdata = [[transpose(rotationmatrix##data[*,0])]]
         ; print, rotdata  ;;;
         ; stop  ;;;
          n = 1

          while n LT datasize[2] do begin
              point = data[*, n] ;idl doesn't want to do a rotation on the entire array at once, so we do each point individually and then put them back together
              rotpoint = transpose(rotationmatrix##point)
              rotdata = [[rotdata], [rotpoint]]
              n ++ 
          endwhile
         ; print, rotdata[*, 0:10]  ;;;
         ; stop   ;;;
          data = rotdata
          jenky3d, data, x=x, y=y, z=z, ct=ct, slices=slices, /noresize, /nooffset
          totrot += theta
          if keyword_set(delay) then begin
              wait, delay/10   ;delay here set in tenths of seconds
          endif
        
      endwhile
  endelse
end


pro jenky3d, data, x = x, y = y, z = z, ct = ct, slices = slices, noresize = noresize, nooffset = nooffset, help = help ;specify viewing axis, color table, and number of slices
   if keyword_set(help) then begin
       print, "This program is a (somewhat lazy) attempt to create a 3D image viewer. Due to the constraints of the OPLOT function, of which it makes extensive use, it plots from the back of the field of view foreward, however through the use of color tables, it should be obvious what goes where."
       print, ""
       pause
       print, "the available color tables are as follows:"
       print, ""
       loadct
       print, ""
       print, "In this function, the user is able to specify the axis from which the 3D data is viewed, the color table used to indicate distance, and the number of slices of data taken to plot the out-of-screen axis. Although the latter two have defaults, the viewing axis does not. The NORESIZE option is of less interest when using this procedure on its own, but in the case of the parent procedure, it will set the axes to a set width (1.5 times the radius of the furthest point in each direction). /NOOFFSET is also less interesting here, although may be useful. It overrides the default of this program to plot a 3/4 view from slightly above the specified axis, and instead plots a truly face-on view. Therefore, typical calls will look like the following:"
       print, ""
       print, "jenky3d, data, /x"
       print, ""
       print, "or,"
       print, ""
       print, "jenky3d, data, /z, ct=30, slices=20"
       print, ""
       print, "(/ct, /noresize, /nooffset, and /slices are optional inputs that can be added to any call string.)"
       print, ""
       print, "If you have called this procedure with too little information (for example, 'jenky3d, /help', or neglected to include a viewing axis), press X and try again. Otherwise, press ENTER."
       pause
   endif
   if ~keyword_set(ct) then begin
       loadct, 20, /silent ;setting the default colortable to rainbow faiding into white, which may or may not look really ugly when I actually run this
   endif else begin
       loadct, ct, /silent
   endelse
   if ~keyword_set(slices) then begin
       slices = 256 ;default number of slices is 256
   endif else begin
       slices = slices
   endelse
   maxx = max(data[0,*])
   maxy = max(data[1,*])
   maxz = max(data[2,*])
   minx = min(data[0,*])
   miny = min(data[1,*])
   minz = min(data[2,*])
   maxrad = max(sqrt(total((data)^2, 1)))
   i = 0
   if keyword_set(x) then begin
       slicesize = (maxx - minx)/slices
   endif
   if keyword_set(y) then begin
       slicesize = (maxy - miny)/slices
   endif
   if keyword_set(z) then begin
       slicesize = (maxz - minz)/slices ;lalala bookkeeping
   endif
   
   ;time for the meat

   if keyword_set(x) then begin
       if ~keyword_set(noresize) then begin
           plot, -data[1,*], data[2,*], xtitle='Y', ytitle='Z', xrange=[miny - slices*slicesize, maxy + slices*slicesize], yrange=[minz - 1.5*slices*slicesize, maxz], /nodata
       endif else begin
            plot, -data[1,*], data[2,*], xtitle='Y', ytitle='Z', xrange=[-1.5*maxrad, 1.5*maxrad], yrange=[-1.5*maxrad, 1.5*maxrad], /nodata
       endelse
   endif

   if keyword_set(y) then begin
       if ~keyword_set(noresize) then begin
           plot, data[0,*], data[2,*], xtitle='X', ytitle='Z', xrange=[minx - slices*slicesize, maxx + 1.5*slices*slicesize], yrange=[minz - 1.5*slices*slicesize, maxz], /nodata
       endif else begin
           plot, data[0,*], data[2,*], xtitle='X', ytitle='Z', xrange=[-1.5*maxrad, 1.5*maxrad], yrange=[-1.5*maxrad, 1.5*maxrad], /nodata
       endelse
   endif

   if keyword_set(z) then begin
       if ~keyword_set(noresize) then begin
           plot, -data[0,*], data[1,*], xtitle='X', ytitle='Y', xrange=[minx - slices*slicesize, maxx + 1.5*slices*slicesize], yrange=[miny - 1.5*slices*slicesize, maxy], /nodata ;priming the display window
       endif else begin
           plot, -data[0,*], data[1,*], xtitle='X', ytitle='Y', xrange=[-1.5*maxrad, 1.5*maxrad], yrange=[-1.5*maxrad, 1.5*maxrad], /nodata
       endelse
   endif


   while i LE slices do begin
       if ~keyword_set(nooffset) then begin
           current = i*slicesize
       endif
       if keyword_set(nooffset) then begin
           current = 0
       endif

       if keyword_set(x) then begin
           mark = minx+current
           cslice = currentslice(data, minx, slicesize, i, /x)
           if cslice[0] EQ 720 AND cslice[1] EQ 5040 AND cslice[2] EQ 40320 then begin
           endif else begin
               oplot, -cslice[1,*]+current, cslice[2,*]-current, psym=3, color=256*i/slices, /noclip
           endelse ;thus, it will only plot the slice if it is not a single point composed of [[6!, 7!, 8!]], which I judge to be an unlikely enough occurance that this should not cause any major problems.

       endif
       if keyword_set(y) then begin
           mark = miny+current
           cslice = currentslice(data, minx, slicesize, i, /y)
           if cslice[0] EQ 720 AND cslice[1] EQ 5040 AND cslice[2] EQ 40320 then begin
           endif else begin
               oplot, cslice[1,*]+current, cslice[2,*]-current, psym=3, color=256*i/slices, /noclip 
           endelse

       endif
       if keyword_set(z) then begin
           mark = minz+current
           cslice = currentslice(data, minx, slicesize, i, /z)
           if cslice[0] EQ 720 AND cslice[1] EQ 5040 AND cslice[2] EQ 40320 then begin
           endif else begin
               oplot, -cslice[0,*]+current, cslice[1,*]-current, psym=3, color=256*i/slices, /noclip ;being general is really boring, without knowing how to permute things... well actually if I was being clever I could probably permute this without too much difficulty but meh.
           endelse
       endif
       i ++
   endwhile
end

function currentslice, data, min, slicesize, i, x = x, y = y, z = z ;finds the points in the data that are within half a slice of the current slice
   mark = min + i*slicesize
   datasize = size(data)
   n = 0
   currentslice = [[0,0,0]] ;this is just here because idl is being annoying and I need something easy to get rid of later to prime this array
   if keyword_set(x) then begin
       while n LT datasize[2] do begin
           if data[0,n] GT mark - slicesize/2. AND data[0,n] LE mark + slicesize/2. then begin
               currentslice = [[currentslice],[data[*,n]]]
           endif
           n ++
       endwhile
   endif
   if keyword_set(y) then begin
       while n LT datasize[2] do begin
           if data[1,n] GT mark - slicesize/2. AND data[1,n] LE mark + slicesize/2. then begin
               currentslice = [[currentslice],[data[*,n]]]
           endif
           n ++
       endwhile
   endif
   if keyword_set(z) then begin
       while n LT datasize[2] do begin
           if data[2,n] GT mark - slicesize/2. AND data[2,n] LE mark + slicesize/2. then begin
               currentslice = [[currentslice],[data[*,n]]]
           endif
           n ++
       endwhile
   endif
   howbig = size(currentslice) 
   finalsize = howbig[2]-1
   finalsize = reform(finalsize)
   ;print, currentslice, "which is an array containing", howbig[2], "elements."
   ;stop
   
   if finalsize GE 1 AND howbig[0] EQ 2 then begin
       currentslice = currentslice[*, 1:finalsize]
   endif else begin
       currentslice = [[720, 5040, 40320]] ;I find it unlikely that the factorial series will be in a plot, and even if it is, almost certainly not in a single slice. Therefore it is a good dummy array to tell idl to ignore that plot.
   endelse
   return, currentslice
end

pro PAUSE ;this procedure I found online, but believe I understand it after much scrutiny.
    ON_ERROR, 2

    prompt_save = !prompt
    rp = ''
    read,rp,prompt='Press ENTER to continue, X to break.'
    rp = strupcase(rp)
    !prompt=prompt_save
    if rp eq 'X' then begin
         close,/all
         message,'USER-BREAK'
    endif
end

function ROTPAUSE, theta, xaxis = xaxis, yaxis = yaxis, zaxis = zaxis ;an extended version of the above.
    ON_ERROR, 2

    prompt_save = !prompt
    rp = ''
    read,rp,prompt='Press A-D, W-S, or Q-E to rotate the view, or X to break.'
    rp = strupcase(rp)
    !prompt=prompt_save
    if rp eq 'X' then begin
         close,/all
         message,'USER-BREAK'
     endif

     if keyword_set(xaxis) then begin
         if rp EQ 'A' then begin
             rotationmatrix = [[cos(-theta), sin(-theta), 0],[-sin(-theta), cos(-theta), 0], [0,0,1]]
         endif
         if rp EQ 'D' then begin
             rotationmatrix = [[cos(theta), sin(theta), 0],[-sin(theta), cos(theta), 0], [0,0,1]]
         endif
         if rp EQ 'W' then begin
             rotationmatrix = [[cos(theta), 0, -sin(theta)],[0,1,0],[sin(theta), 0, cos(theta)]]
         endif
         if rp EQ 'S' then begin
             rotationmatrix = [[cos(-theta), 0, -sin(-theta)],[0,1,0],[sin(-theta), 0, cos(-theta)]]
         endif
         if rp EQ 'E' then begin
             rotationmatrix = [[1,0,0],[0, cos(-theta), sin(-theta)],[0, -sin(-theta), cos(-theta)]]
         endif
         if rp EQ 'Q' then begin
             rotationmatrix = [[1,0,0],[0, cos(theta), sin(theta)],[0, -sin(theta), cos(theta)]]
         endif
         if rp EQ '=' then begin
            rotationmatrix = [[1.5, 0, 0], [0, 1.5, 0], [0, 0, 1.5]]
         endif
         if rp EQ '-' then begin
            rotationmatrix = [[1/1.5, 0, 0], [0, 1/1.5, 0], [0, 0, 1/1.5]]
         endif
         return, rotationmatrix
     endif

     if keyword_set(yaxis) then begin
         if rp EQ 'D' then begin
             rotationmatrix = [[cos(-theta), sin(-theta), 0],[-sin(-theta), cos(-theta), 0], [0,0,1]]
         endif
         if rp EQ 'A' then begin
             rotationmatrix = [[cos(theta), sin(theta), 0],[-sin(theta), cos(theta), 0], [0,0,1]]
         endif
         if rp EQ 'E' then begin
             rotationmatrix = [[1,0,0],[0, cos(theta), sin(theta)],[0, -sin(theta), cos(theta)]]
         endif
         if rp EQ 'Q' then begin
             rotationmatrix = [[1,0,0],[0, cos(-theta), sin(-theta)],[0, -sin(-theta), cos(-theta)]]
         endif
         if rp EQ 'W' then begin
             rotationmatrix = [[cos(-theta), 0, -sin(-theta)],[0,1,0],[sin(-theta), 0, cos(-theta)]]
         endif
         if rp EQ 'S' then begin
             rotationmatrix = [[cos(theta), 0, -sin(theta)],[0,1,0],[sin(theta), 0, cos(theta)]]
         endif
         if rp EQ '=' then begin
            rotationmatrix = [[1.5, 0, 0], [0, 1.5, 0], [0, 0, 1.5]]
         endif
         if rp EQ '-' then begin
            rotationmatrix = [[1/1.5, 0, 0], [0, 1/1.5, 0], [0, 0, 1/1.5]]
         endif
         return, rotationmatrix
     endif

     if keyword_set(zaxis) then begin
         if rp EQ 'A' then begin
             rotationmatrix = [[cos(-theta), 0, -sin(-theta)],[0,1,0],[sin(-theta), 0, cos(-theta)]]
         endif
         if rp EQ 'D' then begin
             rotationmatrix = [[cos(theta), 0, -sin(theta)],[0,1,0],[sin(theta), 0, cos(theta)]]
         endif
         if rp EQ 'W' then begin
             rotationmatrix = [[1,0,0],[0, cos(theta), sin(theta)],[0, -sin(theta), cos(theta)]]
         endif
         if rp EQ 'S' then begin
             rotationmatrix = [[1,0,0],[0, cos(-theta), sin(-theta)],[0, -sin(-theta), cos(-theta)]]
         endif
         if rp EQ 'Q' then begin
             rotationmatrix = [[cos(theta), sin(theta), 0],[-sin(theta), cos(theta), 0], [0,0,1]]
         endif
         if rp EQ 'E' then begin
             rotationmatrix = [[cos(-theta), sin(-theta), 0],[-sin(-theta), cos(-theta), 0], [0,0,1]]
         endif
         if rp EQ '=' then begin
            rotationmatrix = [[1.5, 0, 0], [0, 1.5, 0], [0, 0, 1.5]]
         endif
         if rp EQ '-' then begin
            rotationmatrix = [[1/1.5, 0, 0], [0, 1/1.5, 0], [0, 0, 1/1.5]]
         endif
        ; print, rotationmatrix
        ; stop
         return, rotationmatrix
     endif
     ;return, rotationmatrix
end
