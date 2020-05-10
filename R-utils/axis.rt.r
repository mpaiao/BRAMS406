#==========================================================================================#
#==========================================================================================#
#      This function works similarly to axis, but it allows rotating the labels.  If las   #
# is not provided, or it is between 1 and 4, then it will plot the axis with the           #
# default settings.  Otherwise, if las = 5, it will rotate the labels 45 degrees, and if   #
# las is 6, it will rotate the labels -45 degrees.                                         #
#------------------------------------------------------------------------------------------#
axis.rt <<- function( side
                    , at        = NULL
                    , labels    = TRUE
                    , las       = NULL
                    , off       = 0.15
                    , tick      = TRUE
                    , line      = NA
                    , pos       = NA
                    , outer     = FALSE
                    , font      = NA
                    , lty       = "solid"
                    , lwd       = 1
                    , lwd.ticks = lwd
                    , col       = NULL
                    , col.ticks = NULL
                    , hadj      = NA
                    , padj      = NA
                    , ...
                    ){


   #----- Make sure side is correct. ------------------------------------------------------#
   stopifnot(side %in% c(1,2,3,4))
   #---------------------------------------------------------------------------------------#


   #----- Get the original "par". ---------------------------------------------------------#
   par.orig = par(no.readonly=FALSE)
   #---------------------------------------------------------------------------------------#


   #----- Get the list of default arguments to be passed to axis (excluding ...). ---------#
   defarg          = as.list(match.call(expand.dots=FALSE))
   axisarg         = c(names(formals(axis)),"las")
   axisarg         = axisarg[! axisarg %in% "..."]
   defarg          = defarg[axisarg]
   defarg          = lapply(X=defarg,FUN=eval)
   #---------------------------------------------------------------------------------------#

   #----- Save the "dots" argument to a list. ---------------------------------------------#
   dots = list(...)
   #---------------------------------------------------------------------------------------#



   #---------------------------------------------------------------------------------------#
   #     Grab las from par.orig in case it hasn't been passed.                             #
   #---------------------------------------------------------------------------------------#
   if (is.null(las) || (is.logical(labels) && ! labels)){
      las = par.orig$las
      defarg$las = las
   }#end if (is.null(las) || (is.logical(labels) && ! labels))
   #---------------------------------------------------------------------------------------#


   #----- Check whether art has been given and that labels need to be written. ------------#
   if ( las %in% c(1,2,3,4) || (is.logical(labels) && ! labels)){
      #------------------------------------------------------------------------------------#
      #      Normal las, or labels aren't to be displayed.  Keep it simple and use         #
      # the default axis function.                                                         #
      #------------------------------------------------------------------------------------#
      arguments = modifyList(x=defarg,val=dots)
      do.call(what="axis",args=arguments)
      #------------------------------------------------------------------------------------#

   }else{
      #------------------------------------------------------------------------------------#
      #     las is either 5 or 6, and labels is not FALSE.  We first plot the axis ticks,  #
      # using the default axis function, then add the labels using rotation.               #
      #------------------------------------------------------------------------------------#
      defarg$las = NULL
      #------------------------------------------------------------------------------------#


      #------------------------------------------------------------------------------------#
      #     Set the horizontal and vertical adjustment in case they are not set.           #
      #------------------------------------------------------------------------------------#
      if (is.na(hadj)){
         if (las %in% 5){
            hadj = if (side %in% c(1,2)){1.0}else{0.0}
         }else{
            hadj = if (side %in% c(2,3)){1.0}else{0.0}
         }#end if (las %in% 5)
      }#end if (is.na(hadj))
      if (is.na(padj)){
         if (las %in% 5){
            padj = if (side %in% c(1,2)){1.0}else{0.0}
         }else{
            padj = if (side %in% c(1,4)){1.0}else{0.0}
         }#end if (las %in% 5)
      }#end if (is.na(hadj))
      #------------------------------------------------------------------------------------#


      #------ In case "at" is NULL, find where to place the tick marks. -------------------#
      if (is.null(at)){
         if (side %in% c(1,3)){
            usr     = par.orig$usr[1:2]
            axp     = par.orig$xaxp
            is.log  = par.orig$xlog
            nint    = par.orig$lab[1]
         }else if (side %in% c(2,4)){
            usr     = par.orig$usr[3:4]
            axp     = par.orig$yaxp
            is.log  = par.orig$ylog
            nint    = par.orig$lab[2]
         }#end if
         at   = axisTicks(usr=usr,log=is.log,axp=axp,nint=nint)
      }#end if
      #------------------------------------------------------------------------------------#
      

      #------ In case "labels" is TRUE, set it to "at". -----------------------------------#
      if (is.logical(labels) && labels) labels = at
      #------------------------------------------------------------------------------------#
   

      #------ Plot the tick marks. --------------------------------------------------------#
      arguments = modifyList(x=defarg,val=dots)
      arguments = modifyList(x=arguments,val=list(at=at,labels=FALSE))
      do.call(what="axis",args=arguments)
      #------------------------------------------------------------------------------------#



      #------ Find the x and y positions to add text. -------------------------------------#
      if (side == 1){
         x   = at
         y   = rep(par.orig$usr[3]-off*diff(par.orig$usr[3:4]),times=length(x))
      }else if (side == 2){
         y   = at
         x   = rep(par.orig$usr[1]-off*diff(par.orig$usr[1:2]),times=length(y))
      }else if (side == 3){
         x   = at
         y   = rep(par.orig$usr[4]+off*diff(par.orig$usr[3:4]),times=length(x))
      }else if (side == 4){
         y   = at
         x   = rep(par.orig$usr[2]+off*diff(par.orig$usr[1:2]),times=length(y))
      }#end if
      #----- Set adjsutment to add text. --------------------------------------------------#
      adj = c(hadj,padj)
      #----- If the other axis is in log scale, par$usr must be adjusted. -----------------#
      if (side %in% c(1,3) && par.orig$ylog) y = 10^y
      if (side %in% c(2,4) && par.orig$xlog) x = 10^x
      #----- Additional options. ----------------------------------------------------------#
      srt  = if(las == 5          ){45                }else{-45      }
      col  = if(is.null(dots$col )){par.orig$col.axis }else{dots$col }
      cex  = if(is.null(dots$cex )){par.orig$cex.axis }else{dots$cex }
      font = if(is.null(dots$font)){par.orig$font.axis}else{dots$font}
      #------------------------------------------------------------------------------------#


      #------------------------------------------------------------------------------------#
      #    Change "dots" to have the right configuration.                                  #
      #------------------------------------------------------------------------------------#
      arguments = modifyList( x   = dots
                            , val = list( x      = x
                                        , y      = y
                                        , labels = labels
                                        , adj    = adj
                                        , pos    = NULL
                                        , cex    = cex
                                        , col    = col
                                        , font   = font
                                        , srt    = srt
                                        , xpd    = TRUE
                                        )#end list
                            )#end modifyList
      do.call(what="text",args=arguments)
      #------------------------------------------------------------------------------------#
   }#end if
   #---------------------------------------------------------------------------------------#

   invisible()
}#end function
#==========================================================================================#
#==========================================================================================#
